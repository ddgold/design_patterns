/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008-2013, University of Amsterdam
			      VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/*#define O_DEBUG 1*/
#define _GNU_SOURCE			/* get pipe2() */
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "error.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static atom_t ATOM_stdin;
static atom_t ATOM_stdout;
static atom_t ATOM_stderr;
static atom_t ATOM_std;
static atom_t ATOM_null;
static atom_t ATOM_process;
static atom_t ATOM_detached;
static atom_t ATOM_cwd;
static atom_t ATOM_env;
static atom_t ATOM_priority;
static atom_t ATOM_window;
static atom_t ATOM_timeout;
static atom_t ATOM_release;
static atom_t ATOM_infinite;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_process_error2;
static functor_t FUNCTOR_system_error2;
static functor_t FUNCTOR_pipe1;
static functor_t FUNCTOR_exit1;
static functor_t FUNCTOR_killed1;
static functor_t FUNCTOR_eq2;		/* =/2 */

#define MAYBE 2

#if O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g) (void)0
#endif

#ifdef __WINDOWS__
#include <windows.h>
#include <stdio.h>
#include <fcntl.h>
#include <io.h>

#if !defined(__MINGW32__)
typedef DWORD  pid_t;
#endif

typedef wchar_t echar;			/* environment character */

#ifndef CREATE_BREAKAWAY_FROM_JOB
#define CREATE_BREAKAWAY_FROM_JOB 0x1000000
#endif

#else
typedef char echar;
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISSUES:
	- Deal with child errors (no cwd, cannot execute, etc.)
	- Complete test suite
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	       ADMIN		*
		 *******************************/

typedef enum std_type
{ std_std,
  std_null,
  std_pipe
} std_type;


typedef struct p_stream
{ term_t   term;			/* P in pipe(P) */
  std_type type;			/* type of stream */
#ifdef __WINDOWS__
  HANDLE   fd[2];			/* pipe handles */
#else
  int      fd[2];			/* pipe handles */
#endif
  int	   cloexec;			/* close on exec activated */
} p_stream;


typedef struct ecbuf
{ echar *buffer;
  size_t size;
  size_t allocated;
} ecbuf;


typedef struct p_options
{ atom_t exe_name;			/* exe as atom */
#ifdef __WINDOWS__
  wchar_t *exe;				/* Executable */
  wchar_t *cmdline;			/* Command line */
  wchar_t *cwd;				/* CWD of new process */
#else
  char *exe;				/* Executable */
  char **argv;				/* argument vector */
  char *cwd;				/* CWD of new process */
  char **envp;				/* New environment */
#endif
  ecbuf  envbuf;			/* environment buffer */
  term_t pid;				/* process(PID) */
  int pipes;				/* #pipes found */
  p_stream streams[3];
  int   detached;			/* create as detached */
  int   window;				/* Show a window? */
  int   priority;			/* Process priority */
} p_options;


typedef struct wait_options
{ double timeout;
  int	 has_timeout;
  int	 release;
} wait_options;


#ifdef __WINDOWS__
static int win_command_line(term_t t, int arity,
			    const wchar_t *exepath, wchar_t **cmdline);
#endif

		 /*******************************
		 *	  STRING BUFFER		*
		 *******************************/

static void
free_ecbuf(ecbuf *b)
{ if ( b->buffer )
  { PL_free(b->buffer);
    b->buffer = NULL;
  }
}


static int
add_ecbuf(ecbuf *b, echar *data, size_t len)
{ if ( b->size + len > b->allocated )
  { size_t newsize = (b->allocated ? b->allocated * 2 : 2048);

    while( b->size + len > newsize )
      newsize *= 2;

    if ( b->buffer )
    { b->buffer = PL_realloc(b->buffer, newsize*sizeof(echar));
    } else
    { b->buffer = PL_malloc(newsize*sizeof(echar));
    }

    b->allocated = newsize;
  }

  memcpy(b->buffer+b->size, data, len*sizeof(echar));
  b->size += len;

  return TRUE;
}

		 /*******************************
		 *	ENVIRONMENT PARSING	*
		 *******************************/

static int
get_echars_arg_ex(int i, term_t from, term_t arg, echar **sp, size_t *lenp)
{ const echar *s, *e;

  if ( !PL_get_arg(i, from, arg) )
    return FALSE;

#ifdef __WINDOWS__
  if ( !PL_get_wchars(arg, lenp, sp,
		      CVT_ATOMIC|CVT_EXCEPTION) )
#else
  if ( !PL_get_nchars(arg, lenp, sp,
		      CVT_ATOMIC|CVT_EXCEPTION|REP_FN) )
#endif
    return FALSE;

  for(s = *sp, e = s+*lenp; s<e; s++)
  { if ( !*s )
      return PL_domain_error("text_non_zero_code", arg);
  }

  return TRUE;
}

#ifdef __WINDOWS__
#define ECHARS(s) L##s
#else
#define ECHARS(s) s
#endif

static int
parse_environment(term_t t, p_options *info)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();
  ecbuf *eb   = &info->envbuf;
  int count = 0;
#ifndef __WINDOWS__
  echar *q;
  char **ep;
  int c = 0;
#endif

  assert(eb->size == 0);
  assert(eb->allocated == 0);
  assert(eb->buffer == NULL);

  while( PL_get_list(tail, head, tail) )
  { echar *s;
    size_t len;

    if ( !PL_is_functor(head, FUNCTOR_eq2) )
      return PL_type_error("environment_variable", head);

    if ( !get_echars_arg_ex(1, head, tmp, &s, &len) )
      return FALSE;
    add_ecbuf(eb, s, len);
    add_ecbuf(eb, ECHARS("="), 1);
    if ( !get_echars_arg_ex(2, head, tmp, &s, &len) )
      return FALSE;
    add_ecbuf(eb, s, len);
    add_ecbuf(eb, ECHARS("\0"), 1);

    count++;
  }

  if ( !PL_get_nil_ex(tail) )
    return FALSE;

#ifdef __WINDOWS__
  add_ecbuf(eb, ECHARS("\0"), 1);
#else
  info->envp = PL_malloc((count+1)*sizeof(char*));

  for(ep=info->envp, c=0, q=eb->buffer; c<count; c++, ep++)
  { *ep = q;
    q += strlen(q)+1;
  }
  assert((size_t)(q-eb->buffer) == eb->size);
  *ep = NULL;
#endif

  return TRUE;
}


static int
get_stream(term_t t, p_options *info, p_stream *stream)
{ atom_t a;
  int i;
  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_null )
    { stream->type = std_null;
      return TRUE;
    } else if ( a == ATOM_std )
    { stream->type = std_std;
      return TRUE;
    } else
    { return PL_domain_error("process_stream", t);
    }
  } else if ( PL_is_functor(t, FUNCTOR_pipe1) )
  { stream->term = PL_new_term_ref();
    _PL_get_arg(1, t, stream->term);
    if ( !PL_is_variable(stream->term) )
    { for (i = 0; i < info->pipes; i++)
      { if (PL_compare(info->streams[i].term, t) == 0)
          break;
      }
      if (i == info->pipes)
        return PL_uninstantiation_error(stream->term);
    }
    stream->type = std_pipe;
    info->pipes++;
    return TRUE;
  } else
    return PL_type_error("process_stream", t);
}


static int
parse_options(term_t options, p_options *info)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg = PL_new_term_ref();

  info->window = MAYBE;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);

    if ( name == ATOM_stdin )
    { if ( !get_stream(arg, info, &info->streams[0]) )
	return FALSE;
    } else if ( name == ATOM_stdout )
    { if ( !get_stream(arg, info, &info->streams[1]) )
	return FALSE;
    } else if ( name == ATOM_stderr )
    { if ( !get_stream(arg, info, &info->streams[2]) )
	return FALSE;
    } else if ( name == ATOM_process )
    { info->pid = PL_copy_term_ref(arg);
    } else if ( name == ATOM_detached )
    { if ( !PL_get_bool_ex(arg, &info->detached) )
	return FALSE;
    } else if ( name == ATOM_cwd )
    {
#ifdef __WINDOWS__
      if ( !PL_get_wchars(arg, NULL, &info->cwd,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC) )
	return FALSE;
#else
      if ( !PL_get_chars(arg, &info->cwd,
			 CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
	return FALSE;
#endif
    } else if ( name == ATOM_window )
    { if ( !PL_get_bool_ex(arg, &info->window) )
	return FALSE;
    } else if ( name == ATOM_env )
    { if ( !parse_environment(arg, info) )
	return FALSE;
    } else if ( name == ATOM_priority )
    { int tmp;

      if ( !PL_get_integer_ex(arg, &tmp) )
	return FALSE;
      if ( tmp < -20 || tmp > 19 )
	return PL_domain_error("priority_option", arg);

      info->priority = tmp;
    } else
      return PL_domain_error("process_option", head);
  }

  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  return TRUE;
}


static int
get_exe(term_t exe, p_options *info)
{ int arity;
  term_t arg = PL_new_term_ref();

  if ( !PL_get_name_arity(exe, &info->exe_name, &arity) )
    return PL_type_error("callable", exe);

  PL_put_atom(arg, info->exe_name);

#ifdef __WINDOWS__
  if ( !PL_get_wchars(arg, NULL, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC) )
    return FALSE;
  if ( !win_command_line(exe, arity, info->exe, &info->cmdline) )
    return FALSE;
#else /*__WINDOWS__*/
  if ( !PL_get_chars(arg, &info->exe, CVT_ATOM|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
    return FALSE;

  if ( !(info->argv = PL_malloc((arity+2)*sizeof(char*))) )
    return PL_resource_error("memory");
  memset(info->argv, 0, (arity+2)*sizeof(char*));
  if ( !(info->argv[0] = PL_malloc(strlen(info->exe)+1)) )
    return PL_resource_error("memory");
  strcpy(info->argv[0], info->exe);

  { int i;

    for(i=1; i<=arity; i++)
    { _PL_get_arg(i, exe, arg);

      if ( !PL_get_chars(arg, &info->argv[i],
			 CVT_ATOMIC|CVT_EXCEPTION|BUF_MALLOC|REP_FN) )
	return FALSE;
    }
    info->argv[i] = NULL;
  }
#endif /*__WINDOWS__*/

  return TRUE;
}


static void
free_options(p_options *info)		/* TBD: close streams */
{ if ( info->exe )
  { PL_free(info->exe);
    info->exe = NULL;
  }
  if ( info->cwd )
  { PL_free(info->cwd);
    info->cwd = NULL;
  }
#ifndef __WINDOWS__
  if ( info->envp )
  { PL_free(info->envp);
    info->envp = NULL;
  }
#endif
  free_ecbuf(&info->envbuf);
#ifdef __WINDOWS__
  if ( info->cmdline )
  { PL_free(info->cmdline);
    info->cmdline = NULL;
  }

#else /*__WINDOWS__*/

  if ( info->argv )
  { char **a;
    for(a=info->argv; *a; a++)
    { if ( *a )
	PL_free(*a);
    }
    PL_free(info->argv);

    info->argv = NULL;
  }

#endif /*__WINDOWS__*/
}


		 /*******************************
		 *	   PROCESS READS	*
		 *******************************/

#define	PROCESS_MAGIC	0x29498001

typedef struct process_context
{ int	magic;				/* PROCESS_MAGIC */
#ifdef __WINDOWS__
  HANDLE handle;			/* process handle */
#else
  pid_t	pid;				/* the process id */
#endif
  int   open_mask;			/* Open streams */
  int   pipes[3];			/* stdin/stdout/stderr */
  atom_t exe_name;			/* exe as atom */
} process_context;

static int wait_for_process(process_context *pc);

static int
process_fd(void *handle, process_context **PC)
{ process_context *pc = (process_context*) ((uintptr_t)handle&~(uintptr_t)0x3);
  int pipe = (int)(uintptr_t)handle & 0x3;

  if ( pc->magic == PROCESS_MAGIC )
  { if ( PC )
      *PC = pc;
    return pc->pipes[pipe];
  }

  return -1;
}


static ssize_t
Sread_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.read)((void*)(uintptr_t)fd, buf, size);
}


static ssize_t
Swrite_process(void *handle, char *buf, size_t size)
{ int fd = process_fd(handle, NULL);

  return (*Sfilefunctions.write)((void*)(uintptr_t)fd, buf, size);
}


static int
Sclose_process(void *handle)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  if ( fd >= 0 )
  { int which = (int)(uintptr_t)handle & 0x3;
    int rc;

    rc = (*Sfilefunctions.close)((void*)(uintptr_t)fd);
    pc->open_mask &= ~(1<<which);

    DEBUG(Sdprintf("Closing fd[%d]; mask = 0x%x\n", which, pc->open_mask));

    if ( !pc->open_mask )
    { int rcw = wait_for_process(pc);

      return rcw ? 0 : -1;
    }

    return rc;
  }

  return -1;
}


static int
Scontrol_process(void *handle, int action, void *arg)
{ process_context *pc;
  int fd = process_fd(handle, &pc);

  switch(action)
  { case SIO_GETFILENO:
    { int *fdp = arg;
      *fdp = fd;
      return 0;
    }
    default:
      return (*Sfilefunctions.control)((void*)(uintptr_t)fd, action, arg);
  }
}


static IOFUNCTIONS Sprocessfunctions =
{ Sread_process,
  Swrite_process,
  NULL,					/* seek */
  Sclose_process,
  Scontrol_process,
  NULL					/* seek64 */
};


static IOSTREAM *
#ifdef __WINDOWS__
open_process_pipe(process_context *pc, int which, HANDLE fd)
#else
open_process_pipe(process_context *pc, int which, int fd)
#endif
{ void *handle;
  int flags;

  pc->open_mask |= (1<<which);
#ifdef __WINDOWS__
  pc->pipes[which] = _open_osfhandle((intptr_t)fd, _O_BINARY);
#else
  pc->pipes[which] = fd;
#endif

#define ISO_FLAGS (SIO_RECORDPOS|SIO_FBUF|SIO_TEXT)

  if ( which == 0 )
    flags = SIO_OUTPUT|ISO_FLAGS;
  else
    flags = SIO_INPUT|ISO_FLAGS;

  handle = (void *)((uintptr_t)pc | (uintptr_t)which);

  return Snew(handle, flags, &Sprocessfunctions);
}


		 /*******************************
		 *	       OS STUFF		*
		 *******************************/


#ifdef __WINDOWS__

CRITICAL_SECTION process_lock;
#define LOCK()   EnterCriticalSection(&process_lock);
#define UNLOCK() LeaveCriticalSection(&process_lock);

static void
win_init()
{ InitializeCriticalSection(&process_lock);
}


#include "win_error.c"


typedef struct arg_string
{ size_t  len;
  wchar_t *text;
  wchar_t quote;
} arg_string;

#define QMISC	0x1
#define QDBLQ	0x2
#define QSBLQ	0x4

static int
set_quote(arg_string *as)
{ int needq = 0;
  const wchar_t *s = as->text;

  for(; *s; s++)
  { if ( !iswalnum(*s) )
    { if ( *s == '"' )
	needq |= QDBLQ;
      else if ( *s == '\'' )
	needq |= QSBLQ;
      else
	needq |= QMISC;
    }
  }

  if ( !needq )
  { as->quote = 0;
    return TRUE;
  }
  needq &= ~QMISC;
  switch( needq )
  { case QDBLQ:
      as->quote = '\'';
      return TRUE;
    case 0:
    case QSBLQ:
      as->quote = '"';
      return TRUE;
    default:
      return FALSE;
  }
}


static int
win_command_line(term_t t, int arity, const wchar_t *exe, wchar_t **cline)
{ if ( arity > 0 )
  { arg_string *av = PL_malloc((arity+1)*sizeof(*av));
    term_t arg = PL_new_term_ref();
    size_t cmdlen;
    wchar_t *cmdline, *o;
    const wchar_t *b;
    int i;

    if ( (b=wcsrchr(exe, '\\')) )
      b++;
    else
      b = exe;
    av[0].text = (wchar_t*)b;
    av[0].len = wcslen(av[0].text);
    set_quote(&av[0]);
    cmdlen = av[0].len+(av[0].quote?2:0)+1;

    for( i=1; i<=arity; i++)
    { _PL_get_arg(i, t, arg);

      if ( !PL_get_wchars(arg, &av[i].len, &av[i].text,
			  CVT_ATOMIC|CVT_EXCEPTION|BUF_MALLOC) )
	return FALSE;

      if ( wcslen(av[i].text) != av[i].len )
	return PL_domain_error("no_zero_code_atom", arg);

      if ( !set_quote(&av[i]) )
	return PL_domain_error("dos_quotable_atom", arg);

      cmdlen += av[i].len+(av[i].quote?2:0)+1;
    }

    cmdline = PL_malloc(cmdlen*sizeof(wchar_t));
    for( o=cmdline,i=0; i<=arity; )
    { wchar_t *s = av[i].text;

      if ( av[i].quote )
	*o++ = av[i].quote;
      wcsncpy(o, s, av[i].len);
      o += av[i].len;
      if ( i > 0 )
	PL_free(s);			/* do not free shared exename */
      if ( av[i].quote )
	*o++ = av[i].quote;

      if (++i <= arity)
	*o++ = ' ';
    }
    *o = 0;
    PL_free(av);

    *cline = cmdline;
  } else
  { *cline = NULL;
  }

  return TRUE;
}


typedef struct win_process
{ DWORD pid;
  HANDLE handle;
  struct win_process *next;
} win_process;


static win_process *processes;

static void
register_process(DWORD pid, HANDLE h)
{ win_process *wp = PL_malloc(sizeof(*wp));

  wp->pid = pid;
  wp->handle = h;
  LOCK();
  wp->next = processes;
  processes = wp;
  UNLOCK();
}


static int
unregister_process(DWORD pid)
{ win_process **wpp, *wp;

  LOCK();
  for(wpp=&processes, wp=*wpp; wp; wpp=&wp->next, wp=*wpp)
  { if ( wp->pid == pid )
    { *wpp = wp->next;
      PL_free(wp);
      UNLOCK();
      return TRUE;
    }
  }

  UNLOCK();
  return FALSE;
}


static HANDLE
find_process_from_pid(DWORD pid, const char *pred)
{ win_process *wp;

  LOCK();
  for(wp=processes; wp; wp=wp->next)
  { if ( wp->pid == pid )
    { HANDLE h = wp->handle;
      UNLOCK();
      return h;
    }
  }

  UNLOCK();

  if ( pred )
  { term_t ex = PL_new_term_ref();

    if ( PL_put_integer(ex, pid) )
      PL_existence_error("process", ex);
  }

  return (HANDLE)0;
}


#define WP_TIMEOUT 2

static int
wait_process_handle(HANDLE process, ULONG *rc, DWORD timeout)
{ DWORD wc;

retry:
  wc = MsgWaitForMultipleObjects(1,
				 &process,
				 FALSE,	/* return on any event */
				 timeout,
				 QS_ALLINPUT);

  switch(wc)
  { case WAIT_OBJECT_0:
      if ( !GetExitCodeProcess(process, rc) )
      { win_error("GetExitCodeProcess");
	CloseHandle(process);
	return FALSE;
      }
      CloseHandle(process);
      return TRUE;
    case WAIT_OBJECT_0+1:
    { MSG msg;

      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
	if ( PL_handle_signals() < 0 )
	  return FALSE;
      }
      goto retry;
    }
    case WAIT_TIMEOUT:
      return WP_TIMEOUT;
    default:
      win_error("WaitForSingleObject");
      CloseHandle(process);
      return FALSE;
  }
}


static int
wait_for_pid(pid_t pid, term_t code, wait_options *opts)
{ HANDLE *h;

  if ( (h=find_process_from_pid(pid, "process_wait")) )
  { ULONG rc;
    DWORD timeout;
    int wc;

    if ( opts->has_timeout )
      timeout = (DWORD)(opts->timeout * 1000.0);
    else
      timeout = INFINITE;

    if ( !(wc=wait_process_handle(h, &rc, timeout)) )
      return FALSE;
    if ( wc == WP_TIMEOUT )
      return PL_unify_atom(code, ATOM_timeout);

    unregister_process(pid);

    return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_LONG, rc);
  } else
  { return FALSE;
  }
}


static int
win_wait_success(atom_t exe, HANDLE process)
{ ULONG rc;

  if ( !wait_process_handle(process, &rc, INFINITE) )
    return FALSE;

  if ( rc != 0 )
  { term_t ex = PL_new_term_ref();

    if ( PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
		         PL_FUNCTOR, FUNCTOR_process_error2,
		           PL_ATOM, exe,
		           PL_FUNCTOR, FUNCTOR_exit1,
		             PL_LONG, rc,
		         PL_VARIABLE) )
      return PL_raise_exception(ex);
    return FALSE;
  }

  return TRUE;
}


static int
wait_for_process(process_context *pc)
{ int rc = win_wait_success(pc->exe_name, pc->handle);

  PL_unregister_atom(pc->exe_name);
  PL_free(pc);

  return rc;
}


static int
create_pipes(p_options *info)
{ int i;
  SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);          /* Length in bytes */
  sa.bInheritHandle = 1;            /* the child must inherit these handles */
  sa.lpSecurityDescriptor = NULL;

  for(i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->term )
    { if ( i == 2 && info->streams[1].term &&
	   PL_compare(info->streams[1].term, info->streams[2].term) == 0 )
      { s->fd[0] = info->streams[1].fd[0];
	s->fd[1] = info->streams[1].fd[1];
      } else
      { if ( !CreatePipe(&s->fd[0], &s->fd[1], &sa, 1<<13) )
	{ return win_error("CreatePipe");
	}
      }
    }
  }

  return TRUE;
}


static IOSTREAM *
Sopen_handle(HANDLE h, const char *mode)
{ return Sfdopen(_open_osfhandle((intptr_t)h, _O_BINARY), mode);
}


static HANDLE
open_null_stream(DWORD access)
{ SECURITY_ATTRIBUTES sa;

  sa.nLength = sizeof(sa);          /* Length in bytes */
  sa.bInheritHandle = 1;            /* the child must inherit these handles */
  sa.lpSecurityDescriptor = NULL;

  return CreateFile("nul",
		    access,
		    FILE_SHARE_READ|FILE_SHARE_WRITE,
		    &sa,		/* security */
		    OPEN_EXISTING,
		    0,
		    NULL);
}


static int
console_app(void)
{ HANDLE h;

  if ( (h = GetStdHandle(STD_OUTPUT_HANDLE)) != INVALID_HANDLE_VALUE )
  { DWORD mode;

    if ( GetConsoleMode(h, &mode) )
      return TRUE;
  }

  return FALSE;
}


static int
do_create_process(p_options *info)
{ int flags = 0;
  PROCESS_INFORMATION pi;
  STARTUPINFOW si;

  switch(info->window)
  { case MAYBE:
      if ( !console_app() )
	flags |= CREATE_NO_WINDOW;
      break;
    case TRUE:
      break;
    case FALSE:
      flags |= CREATE_NO_WINDOW;
      break;
  }

  if ( info->detached )
    flags |= CREATE_BREAKAWAY_FROM_JOB;
  if ( info->envbuf.buffer )
    flags |= CREATE_UNICODE_ENVIRONMENT;

  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;

				      /* stdin */
  switch( info->streams[0].type )
  { case std_pipe:
      si.hStdInput = info->streams[0].fd[0];
      SetHandleInformation(info->streams[0].fd[1],
			   HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdInput = open_null_stream(GENERIC_READ);
      break;
    case std_std:
      si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
      break;
  }
				      /* stdout */
  switch( info->streams[1].type )
  { case std_pipe:
      si.hStdOutput = info->streams[1].fd[1];
      SetHandleInformation(info->streams[1].fd[0],
			   HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdOutput = open_null_stream(GENERIC_WRITE);
      break;
    case std_std:
      si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
      break;
  }
				      /* stderr */
  switch( info->streams[2].type )
  { case std_pipe:
      si.hStdError = info->streams[2].fd[1];
      SetHandleInformation(info->streams[2].fd[0],
                           HANDLE_FLAG_INHERIT, FALSE);
      break;
    case std_null:
      si.hStdError = open_null_stream(GENERIC_WRITE);
      break;
    case std_std:
      si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
      break;
  }

  if ( CreateProcessW(info->exe,
		      info->cmdline,
		      NULL,		/* Process security */
		      NULL,		/* Thread security */
		      TRUE,		/* Inherit handles */
		      flags,		/* Creation flags */
		      info->envbuf.buffer, /* Environment */
		      info->cwd,	/* Directory */
		      &si,		/* Startup info */
		      &pi) )		/* Process information */
  { int rc = TRUE;

    CloseHandle(pi.hThread);

    if ( info->pipes > 0 && info->pid == 0 )
    { IOSTREAM *s;
      process_context *pc = PL_malloc(sizeof(*pc));

      DEBUG(Sdprintf("Wait on pipes\n"));

      memset(pc, 0, sizeof(*pc));
      pc->magic    = PROCESS_MAGIC;
      pc->handle   = pi.hProcess;
      pc->exe_name = info->exe_name;
      PL_register_atom(pc->exe_name);

      if ( info->streams[0].type == std_pipe )
      { CloseHandle(info->streams[0].fd[0]);
	if ( (s = open_process_pipe(pc, 0, info->streams[0].fd[1])) )
	  rc = PL_unify_stream(info->streams[0].term, s);
	else
	  CloseHandle(info->streams[0].fd[0]);
      }
      if ( info->streams[1].type == std_pipe )
      { CloseHandle(info->streams[1].fd[1]);
	if ( rc && (s = open_process_pipe(pc, 1, info->streams[1].fd[0])) )
	  PL_unify_stream(info->streams[1].term, s);
	else
	  CloseHandle(info->streams[1].fd[0]);
      }
      if ( info->streams[2].type == std_pipe &&
           ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
      { CloseHandle(info->streams[2].fd[1]);
	if ( rc && (s = open_process_pipe(pc, 2, info->streams[2].fd[0])) )
	  rc = PL_unify_stream(info->streams[2].term, s);
	else
	  CloseHandle(info->streams[2].fd[0]);
      }

      return rc;
    } else if ( info->pipes > 0 )
    { IOSTREAM *s;

      if ( info->streams[0].type == std_pipe )
      { CloseHandle(info->streams[0].fd[0]);
	if ( (s = Sopen_handle(info->streams[0].fd[1], "w")) )
	  rc = PL_unify_stream(info->streams[0].term, s);
	else
	  CloseHandle(info->streams[0].fd[1]);
      }
      if ( info->streams[1].type == std_pipe )
      { CloseHandle(info->streams[1].fd[1]);
	if ( rc && (s = Sopen_handle(info->streams[1].fd[0], "r")) )
	  rc = PL_unify_stream(info->streams[1].term, s);
	else
	  CloseHandle(info->streams[1].fd[0]);
      }
      if ( info->streams[2].type == std_pipe &&
           ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
      { CloseHandle(info->streams[2].fd[1]);
	if ( rc && (s = Sopen_handle(info->streams[2].fd[0], "r")) )
	  rc = PL_unify_stream(info->streams[2].term, s);
	else
	  CloseHandle(info->streams[2].fd[0]);
      }
    }

    if ( !rc )
    { Sdprintf("FATAL ERROR: create_process/3\n");
      PL_halt(1);
    }

    if ( info->pid )
    { register_process(pi.dwProcessId, pi.hProcess);
      return PL_unify_integer(info->pid, pi.dwProcessId);
    }

    return win_wait_success(info->exe_name, pi.hProcess);
  } else
  { return win_error("CreateProcess");
  }
}

#else /*__WINDOWS__*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note the descriptors created using pipe()   are  inherited by the child.
This implies that if two threads  call process_create/3 using pipes, the
pipe created for one thread may also  end   up  in  the child created by
another thread. We  can  avoid  this   using  FD_CLOEXEC  on  the pipe's
descriptor. Note that we can do that on   both  because the flag will be
cleared on the duplicated  descriptor  after   dup2  (which  is executed
before the exec, so the descriptors are still valid).

This can be implemented safely on systems   that  have pipe2(). On other
systems, safety can be enhanced by   reducing  the window using fcntl(),
but this needs to be synced with fork() in other threads.

(*) It seems that some systems implement  the function, but the function
returns ENOSYS to indicate it is not   implemented. Hence, we need to go
for a runtime solution.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
create_pipes(p_options *info)
{ int i;

  for(i=0; i<3; i++)
  { p_stream *s = &info->streams[i];

    if ( s->term )
    { if ( i == 2 && info->streams[1].term &&
	   PL_compare(info->streams[1].term, info->streams[2].term) == 0 )
      { s->fd[0] = info->streams[1].fd[0];
	s->fd[1] = info->streams[1].fd[1];
      } else
      { int my_side;

#ifdef HAVE_PIPE2
	if ( pipe2(s->fd, O_CLOEXEC) == 0 )
	{ s->cloexec = TRUE;
	  continue;
	} else if ( errno != ENOSYS )	/* See (*) */
	{ if ( errno != EMFILE )
	    Sdprintf("pipe2(): unexpected error: %s\n", strerror(errno));
	  return PL_resource_error("open_files");
	}
#endif
	if ( pipe(s->fd) )
	{ if ( errno != EMFILE )
	    Sdprintf("pipe(): unexpected error: %s\n", strerror(errno));
	  return PL_resource_error("open_files");
	}
	my_side = (i == 0 ? s->fd[1] : s->fd[0]);
#ifdef F_SETFD
        if ( fcntl(my_side, F_SETFD, FD_CLOEXEC) == 0 )
	  s->cloexec = TRUE;
#endif
      }
    }
  }

  return TRUE;
}


static int
unify_exit_status(term_t code, int status)
{ if ( WIFEXITED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_exit1,
			   PL_INT, (int)WEXITSTATUS(status));
  } else if ( WIFSIGNALED(status) )
  { return PL_unify_term(code,
			 PL_FUNCTOR, FUNCTOR_killed1,
			   PL_INT, (int)WTERMSIG(status));
  } else
  { assert(0);
    return FALSE;
  }
}


static int
wait_for_pid(pid_t pid, term_t code, wait_options *opts)
{ pid_t p2;
  int status;

  if ( opts->has_timeout && opts->timeout == 0.0 )
  { if ( (p2=waitpid(pid, &status, WNOHANG)) == pid )
      return unify_exit_status(code, status);
    else if ( p2 == 0 )
      return PL_unify_atom(code, ATOM_timeout);
    else
    { term_t PID;

    error:
      return ((PID = PL_new_term_ref()) &&
	      PL_put_integer(PID, pid) &&
	      pl_error(NULL, 0, "waitpid", ERR_ERRNO,
		       errno, "wait", "process", PID));
    }
  }

  for(;;)
  { if ( (p2=waitpid(pid, &status, 0)) == pid )
      return unify_exit_status(code, status);

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    } else
    { goto error;
    }
  }
}


static int
wait_success(atom_t name, pid_t pid)
{ pid_t p2;

  for(;;)
  { int status;

    if ( (p2=waitpid(pid, &status, 0)) == pid )
    { if ( WIFEXITED(status) && WEXITSTATUS(status) == 0 )
      { return TRUE;
      } else
      { term_t code, ex;

	if ( (code = PL_new_term_ref()) &&
	     (ex = PL_new_term_ref()) &&
	     unify_exit_status(code, status) &&
	     PL_unify_term(ex,
			   PL_FUNCTOR, FUNCTOR_error2,
			     PL_FUNCTOR, FUNCTOR_process_error2,
			       PL_ATOM, name,
			       PL_TERM, code,
			     PL_VARIABLE) )
	  return PL_raise_exception(ex);
	return FALSE;
      }
    }

    if ( p2 == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
    }
  }
}


static int
wait_for_process(process_context *pc)
{ int rc = wait_success(pc->exe_name, pc->pid);

  PL_unregister_atom(pc->exe_name);
  PL_free(pc);

  return rc;
}


#ifndef HAVE_VFORK
#define vfork fork
#endif

static int
close_ok(int fd)
{ int rc;

  do
  { rc = close(fd);
  } while ( rc == -1 && errno == EINTR );

  DEBUG(if ( rc == -1 )
	  perror("close"));

  return rc;
}


static int
do_create_process(p_options *info)
{ int pid;

  if ( !(pid=vfork()) )			/* child */
  { int fd;

    PL_cleanup_fork();

#if defined(HAVE_SYS_RESOURCE_H) && defined(PRIO_PROCESS)
    if ( info->priority != 255 )
      setpriority(PRIO_PROCESS, pid, info->priority);
#endif

    if ( info->detached )
      setsid();

    if ( info->cwd )
    { if ( chdir(info->cwd) )
      { perror(info->cwd);
	exit(1);
      }
    }

					/* stdin */
    switch( info->streams[0].type )
    { case std_pipe:
	dup2(info->streams[0].fd[0], 0);
        if ( !info->streams[0].cloexec )
	  close(info->streams[0].fd[1]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_RDONLY)) >= 0 )
	  dup2(fd, 0);
        break;
      case std_std:
	break;
    }
					/* stdout */
    switch( info->streams[1].type )
    { case std_pipe:
	dup2(info->streams[1].fd[1], 1);
        if ( !info->streams[1].cloexec )
	  close(info->streams[1].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 1);
        break;
      case std_std:
	break;
    }
					/* stderr */
    switch( info->streams[2].type )
    { case std_pipe:
	dup2(info->streams[2].fd[1], 2);
	if ( !info->streams[2].cloexec )
	  close(info->streams[2].fd[0]);
	break;
      case std_null:
	if ( (fd = open("/dev/null", O_WRONLY)) >= 0 )
	  dup2(fd, 2);
        break;
      case std_std:
	break;
    }

    if ( info->envp )
      execve(info->exe, info->argv, info->envp);
    else
      execv(info->exe, info->argv);

    perror(info->exe);
    exit(1);
  } else if ( pid < 0 )			/* parent */
  { term_t exe = PL_new_term_ref();
    PL_put_atom_chars(exe, info->exe);

    return pl_error(NULL, 0, "fork", ERR_ERRNO, errno, "fork", "process", exe);
  } else
  { int rc = TRUE;

    if ( info->pipes > 0 && info->pid == 0 )
    { IOSTREAM *s;			/* no pid(Pid): wait */
      process_context *pc = PL_malloc(sizeof(*pc));

      DEBUG(Sdprintf("Wait on pipes\n"));

      memset(pc, 0, sizeof(*pc));
      pc->magic = PROCESS_MAGIC;
      pc->pid = pid;
      pc->exe_name = info->exe_name;
      PL_register_atom(pc->exe_name);

      if ( info->streams[0].type == std_pipe )
      { close_ok(info->streams[0].fd[0]);
	if ( (s = open_process_pipe(pc, 0, info->streams[0].fd[1])) )
	  rc = PL_unify_stream(info->streams[0].term, s);
	else
	  close_ok(info->streams[0].fd[1]);
      }
      if ( info->streams[1].type == std_pipe )
      { close_ok(info->streams[1].fd[1]);
	if ( rc && (s = open_process_pipe(pc, 1, info->streams[1].fd[0])) )
	  rc = PL_unify_stream(info->streams[1].term, s);
	else
	  close_ok(info->streams[1].fd[0]);
      }
      if ( info->streams[2].type == std_pipe &&
           ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
      { close_ok(info->streams[2].fd[1]);
	if ( rc && (s = open_process_pipe(pc, 2, info->streams[2].fd[0])) )
	  rc = PL_unify_stream(info->streams[2].term, s);
	else
	  close_ok(info->streams[2].fd[0]);
      }

      return rc;
    } else if ( info->pipes > 0 )
    { IOSTREAM *s;

      if ( info->streams[0].type == std_pipe )
      { close_ok(info->streams[0].fd[0]);
	if ( (s = Sfdopen(info->streams[0].fd[1], "w")) )
	  rc = PL_unify_stream(info->streams[0].term, s);
	else
	  close_ok(info->streams[0].fd[1]);
      }
      if ( info->streams[1].type == std_pipe )
      { close_ok(info->streams[1].fd[1]);
	if ( rc && (s = Sfdopen(info->streams[1].fd[0], "r")) )
	  rc = PL_unify_stream(info->streams[1].term, s);
	else
	  close_ok(info->streams[1].fd[0]);
      }
      if ( info->streams[2].type == std_pipe &&
           ( !info->streams[1].term || PL_compare(info->streams[1].term, info->streams[2].term) != 0 ) )
      { close_ok(info->streams[2].fd[1]);
	if ( rc && (s = Sfdopen(info->streams[2].fd[0], "r")) )
	  PL_unify_stream(info->streams[2].term, s);
	else
	  close_ok(info->streams[2].fd[0]);
      }
    }

    assert(rc);				/* What else? */

    if ( info->pid )
      return PL_unify_integer(info->pid, pid);

    return wait_success(info->exe_name, pid);
  }
}

#endif /*__WINDOWS__*/


		 /*******************************
		 *	      BINDING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Running out of resources after we created  the process is really hard to
handle gracefully, so we first make  a   list  to  ensure we have enough
resources to bind the return value.

Ideally, we need a call to ask for sufficient resources.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
ensure_stack_resources(int count)
{ fid_t fid = PL_open_foreign_frame();
  term_t list = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(list);

  while ( count-- > 0 )
  { term_t head;

    if ( !(head = PL_new_term_ref()) ||
	 !PL_unify_list(tail, head, tail) )
    { PL_close_foreign_frame(fid);
      return FALSE;
    }
  }

  PL_discard_foreign_frame(fid);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Basic process creation interface takes

	* Exe file
	* List of arguments
	* standard streams		% std, null, pipe(S)
	* Working directory
	* detached
	* window			% Windows
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
process_create(term_t exe, term_t options)
{ p_options info;
  int rc = FALSE;

  if ( !ensure_stack_resources(10) )	/* max 3 stream structures */
    return FALSE;

  memset(&info, 0, sizeof(info));

  info.priority = 255;			/* zero is a valid priority */

  if ( !get_exe(exe, &info) )
    goto out;
  if ( !parse_options(options, &info) )
    goto out;
  if ( !create_pipes(&info) )
    goto out;

  rc = do_create_process(&info);

out:
  free_options(&info);

  return rc;
}


static int
get_pid(term_t pid, pid_t *p)
{ int n;

  if ( !PL_get_integer_ex(pid, &n) )
    return FALSE;
  if ( n < 0 )
    return PL_domain_error("not_less_than_zero", pid);

  *p = n;
  return TRUE;
}


static foreign_t
process_wait(term_t pid, term_t code, term_t options)
{ pid_t p;
  wait_options opts;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();

  if ( !get_pid(pid, &p) )
    return FALSE;

  memset(&opts, 0, sizeof(opts));
  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);
    if ( name == ATOM_timeout )
    { atom_t a;

      if ( !(PL_get_atom(arg, &a) && a == ATOM_infinite) )
      { if ( !PL_get_float(arg, &opts.timeout) )
	  return PL_type_error("timeout", arg);
	opts.has_timeout = TRUE;
      }
    } else if ( name == ATOM_release )
    { if ( !PL_get_bool_ex(arg, &opts.release) )
	return FALSE;
      if ( opts.release == FALSE )
	return PL_domain_error("true", arg);
    } else
      return PL_domain_error("process_wait_option", head);
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  return wait_for_pid(p, code, &opts);
}


static foreign_t
process_kill(term_t pid, term_t signal)
{ pid_t p;

  if ( !get_pid(pid, &p) )
    return FALSE;

{
#ifdef __WINDOWS__
  HANDLE h;

  if ( !(h=find_process_from_pid(p, "process_kill")) )
    return FALSE;

  if ( TerminateProcess(h, 255) )
    return TRUE;

  return win_error("TerminateProcess");
#else /*__WINDOWS__*/
  int sig;

  if ( !PL_get_signum_ex(signal, &sig) )
    return FALSE;

  if ( kill(p, sig) == 0 )
    return TRUE;

  switch(errno)
  { case EPERM:
      return pl_error("process_kill", 2, NULL, ERR_PERMISSION,
		      pid, "kill", "process");
    case ESRCH:
      return pl_error("process_kill", 2, NULL, ERR_EXISTENCE,
		      "process", pid);
    default:
      return pl_error("process_kill", 2, "kill", ERR_ERRNO, errno, "kill", "process", pid);
  }
#endif /*__WINDOWS__*/
}
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_process()
{
#ifdef __WINDOWS__
  win_init();
#endif

  MKATOM(stdin);
  MKATOM(stdout);
  MKATOM(stderr);
  MKATOM(std);
  MKATOM(null);
  MKATOM(process);
  MKATOM(detached);
  MKATOM(cwd);
  MKATOM(env);
  MKATOM(priority);
  MKATOM(window);
  MKATOM(timeout);
  MKATOM(release);
  MKATOM(infinite);

  MKFUNCTOR(pipe, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(process_error, 2);
  MKFUNCTOR(system_error, 2);
  MKFUNCTOR(exit, 1);
  MKFUNCTOR(killed, 1);

  FUNCTOR_eq2 = PL_new_functor(PL_new_atom("="), 2);

  PL_register_foreign("process_create", 2, process_create, 0);
  PL_register_foreign("process_wait", 3, process_wait, 0);
  PL_register_foreign("process_kill", 2, process_kill, 0);
}