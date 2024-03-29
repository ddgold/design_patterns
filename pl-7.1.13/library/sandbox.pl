/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sandbox,
	  [ safe_goal/1,		% :Goal
	    safe_call/1			% :Goal
	  ]).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply_macros), [expand_phrase/2]).
:- use_module(library(prolog_format)).

:- multifile
	safe_primitive/1,		% Goal
	safe_meta/2.			% Goal, Calls

% :- debug(sandbox).

/** <module> Sandboxed Prolog code

Prolog is a full-featured Turing complete  programming language in which
it is easy to write programs that can   harm your computer. On the other
hand, Prolog is a logic based _query language_ which can be exploited to
query data interactively from, e.g.,  the   web.  This  library provides
safe_goal/1, which determines whether it is safe to call its argument.

@tbd	Handling of ^ and // meta predicates
@tbd	Complete set of whitelisted predicates
@see	http://www.swi-prolog.org/pldoc/package/pengines.html
*/


:- meta_predicate
	safe_goal(:),
	safe_call(0).

%%	safe_call(:Goal)
%
%	Call Goal if it  complies  with   the  sandboxing  rules. Before
%	calling   Goal,   it   performs   expand_goal/2,   followed   by
%	safe_goal/1. Expanding is done explicitly  because situations in
%	which safe_call/1 typically concern goals that  are not known at
%	compile time.
%
%	@see safe_goal/1.

safe_call(Goal0) :-
	expand_goal(Goal0, Goal),
	safe_goal(Goal),
	call(Goal).

%%	safe_goal(:Goal) is det.
%
%	True if calling Goal provides  no   security  risc. This implies
%	that:
%
%	  - The call-graph can be fully expanded. Full expansion *stops*
%	  if a meta-goal is found for   which we cannot determine enough
%	  details to know which predicate will be called.
%
%	  - All predicates  referenced  from   the  fully  expanded  are
%	  whitelisted by the predicate safe_primitive/1 and safe_meta/2.
%
%	  - It is not allowed to make explicitly qualified calls into
%	  modules to predicates that are not exported or declared
%	  public.
%
%	@error	instantiation_error if the analysis encounters a term in
%		a callable position that is insufficiently instantiated
%		to determine the predicate called.
%	@error	permission_error(call, sandboxed, Goal) if Goal is in
%		the call-tree and not white-listed.

:- thread_local
	last_error/1.

safe_goal(M:Goal) :-
	empty_assoc(Safe0),
	catch(safe(Goal, M, [], Safe0, _), E, true), !,
	retractall(last_error(_)),
	(   var(E)
	->  true
	;   throw(E)
	).
safe_goal(_) :-
	last_error(E), !,
	retractall(last_error(_)),
	throw(E).
safe_goal(G) :-
	debug(sandbox(fail), 'safe_goal/1 failed for ~p', [G]),
	throw(error(instantiation_error, sandbox(G, []))).


%%	safe(+Goal, +Module, +Parents, +Safe0, -Safe) is semidet.
%
%	Is true if Goal can only call safe code.

safe(V, _, Parents, _, _) :-
	var(V), !,
	Error = error(instantiation_error, sandbox(V, Parents)),
	asserta(last_error(Error)),
	throw(Error).
safe(M:G, _, Parents, Safe0, Safe) :- !,
	must_be(atom, M),
	must_be(callable, G),
	(   (   predicate_property(M:G, exported)
	    ;	predicate_property(M:G, public)
	    ;	predicate_property(M:G, multifile)
	    ;	predicate_property(M:G, iso)
	    ;	memberchk(M:_, Parents)
	    )
	->  safe(G, M, Parents, Safe0, Safe)
	;   throw(error(permission_error(call, sandboxed, M:G),
			sandbox(M:G, Parents)))
	).
safe(G, _, Parents, _, _) :-
	debugging(sandbox(show)),
	length(Parents, Level),
	debug(sandbox(show), '[~D] SAFE ~q?', [Level, G]),
	fail.
safe(G, _, _, Safe, Safe) :-
	safe_primitive(G),
	predicate_property(G, iso), !.
safe(G, M, _, Safe, Safe) :-
	(   predicate_property(M:G, imported_from(M2))
	->  true
	;   M2 = M
	),
	safe_primitive(M2:G), !.
safe(G, M, Parents, Safe0, Safe) :-
	predicate_property(G, iso),
	safe_meta_call(G, Called), !,
	safe_list(Called, M, Parents, Safe0, Safe).
safe(G, M, Parents, Safe0, Safe) :-
	expand_phrase(G, Goal), !,
	safe(Goal, M, Parents, Safe0, Safe).
safe(G, M, Parents, Safe0, Safe) :-
	(   predicate_property(M:G, imported_from(M2))
	->  true
	;   M2 = M
	),
	safe_meta_call(M2:G, Called), !,
	safe_list(Called, M, Parents, Safe0, Safe).
safe(G, M, Parents, Safe0, Safe) :-
	goal_id(M:G, Id, Gen),
	(   get_assoc(Id, Safe0, _)
	->  Safe = Safe0
	;   put_assoc(Id, Safe0, true, Safe1),
	    (	Gen == M:G
	    ->	safe_clauses(Gen, M, [Id|Parents], Safe1, Safe)
	    ;	catch(safe_clauses(Gen, M, [Id|Parents], Safe1, Safe),
		      error(instantiation_error, _),
		      fail)
	    )
	).

safe_clauses(G, M, Parents, Safe0, Safe) :-
	predicate_property(M:G, interpreted), !,
	def_module(M:G, MD:QG),
	findall(Body, clause(MD:QG, Body), Bodies),
	safe_list(Bodies, MD, Parents, Safe0, Safe).
safe_clauses(G, M, [_|Parents], _, _) :-
	predicate_property(M:G, visible), !,
	throw(error(permission_error(call, sandboxed, G),
		    sandbox(M:G, Parents))).
safe_clauses(_, _, [G|Parents], _, _) :-
	throw(error(existence_error(procedure, G),
		    sandbox(G, Parents))).

safe_list([], _, _, Safe, Safe).
safe_list([H|T], M, Parents, Safe0, Safe) :-
	(   H = M2:H2,
	    M == M2
	->  copy_term(H2, H3)
	;   copy_term(H, H3)
	),
	safe(H3, M, Parents, Safe0, Safe1),
	safe_list(T, M, Parents, Safe1, Safe).


def_module(M:G, MD:QG) :-
	predicate_property(M:G, imported_from(MD)), !,
	meta_qualify(MD:G, M, QG).
def_module(M:G, M:QG) :-
	meta_qualify(M:G, M, QG).

%%	meta_qualify(:G, +M, -QG) is det.
%
%	Perform meta-qualification of the goal-argument

meta_qualify(MD:G, M, QG) :-
	predicate_property(MD:G, meta_predicate(Head)), !,
	G =.. [Name|Args],
	Head =.. [_|Q],
	qualify_args(Q, M, Args, QArgs),
	QG =.. [Name|QArgs].
meta_qualify(_:G, _, G).

qualify_args([], _, [], []).
qualify_args([H|T], M, [A|AT], [Q|QT]) :-
	qualify_arg(H, M, A, Q),
	qualify_args(T, M, AT, QT).

qualify_arg(S, M, A, Q) :-
	q_arg(S), !,
	qualify(A, M, Q).
qualify_arg(_, _, A, A).

q_arg(I) :- integer(I), !.
q_arg(:).
q_arg(^).
q_arg(//).

qualify(A, M, MZ:Q) :-
	strip_module(M:A, MZ, Q).

%%	goal_id(:Goal, -Id, -Gen) is nondet.
%
%	Generate an identifier for the goal proven to be safe. We
%	first try to prove the most general form of the goal.  If
%	this fails, we try to prove more specific versions.
%
%	@tbd	Do step-by-step generalisation instead of the current
%		two levels (most general and most specific).
%	@tbd	We could also use variant_sha1 for the goal ids.

goal_id(M:Goal, M:Id, Gen) :- !,
	goal_id(Goal, Id, Gen).
goal_id(Var, _, _) :-
	var(Var), !,
	instantiation_error(Var).
goal_id(Atom, Atom, Atom) :-
	atom(Atom), !.
goal_id(Term, _, _) :-
	\+ compound(Term), !,
	type_error(callable, Term).
goal_id(Term, Skolem, Gen) :-		% most general form
	compound_name_arity(Term, Name, Arity),
	compound_name_arity(Skolem, Name, Arity),
	compound_name_arity(Gen, Name, Arity),
	copy_goal_args(1, Term, Skolem, Gen),
	(   Gen =@= Term
	->  !				% No more specific one; we can commit
	;   true
	),
	numbervars(Skolem, 0, _).
goal_id(Term, Skolem, Term) :-		% most specific form
	debug(sandbox(specify), 'Retrying with ~p', [Term]),
	copy_term(Term, Skolem),
	numbervars(Skolem, 0, _).

%%	copy_goal_args(+I, +Term, +Skolem, +Gen) is det.
%
%	Create  the  most  general  form,   but  keep  module  qualified
%	arguments because they will likely be called anyway.

copy_goal_args(I, Term, Skolem, Gen) :-
	arg(I, Term, TA), !,
	arg(I, Skolem, SA),
	arg(I, Gen, GA),
	copy_goal_arg(TA, SA, GA),
	I2 is I + 1,
	copy_goal_args(I2, Term, Skolem, Gen).
copy_goal_args(_, _, _, _).

copy_goal_arg(Arg, SArg, Arg) :-
	copy_goal_arg(Arg), !,
	copy_term(Arg, SArg).
copy_goal_arg(_, _, _).

copy_goal_arg(Var) :- var(Var), !, fail.
copy_goal_arg(_:_).


%%	verify_safe_declaration(+Decl)
%
%	See whether a  safe  declaration  makes   sense.  That  is,  the
%	predicate must be defined (such that  the attacker cannot define
%	the predicate), must be sufficiently   instantiated and only ISO
%	declared predicates may omit a module qualification.
%
%	@tbd	Verify safe_meta/2 declarations.  It is a bit less clear
%		what the rules are.

term_expansion(safe_primitive(Goal), Term) :-
	(   verify_safe_declaration(Goal)
	->  Term = safe_primitive(Goal)
	;   Term = []
	).

system:term_expansion(sandbox:safe_primitive(Goal), Term) :-
	(   verify_safe_declaration(Goal)
	->  Term = sandbox:safe_primitive(Goal)
	;   Term = []
	).

verify_safe_declaration(Var) :-
	var(Var), !,
	instantiation_error(Var).
verify_safe_declaration(Module:Goal) :-
	must_be(atom, Module),
	must_be(callable, Goal),
	(   (   predicate_property(Module:Goal, visible)
	    ->	true
	    ;	predicate_property(Module:Goal, foreign)
	    ),
	    \+ predicate_property(Module:Goal, imported_from(_)),
	    \+ predicate_property(Module:Goal, meta_predicate())
	->  true
	;   permission_error(declare, safe_goal, Module:Goal)
	).
verify_safe_declaration(Goal) :-
	must_be(callable, Goal),
	(   predicate_property(system:Goal, iso),
	    \+ predicate_property(system:Goal, meta_predicate())
	->  true
	;   permission_error(declare, safe_goal, Goal)
	).

verify_predefined_safe_declarations :-
	forall(clause(safe_primitive(Goal), Ref),
	       ( catch(verify_safe_declaration(Goal), E, true),
		 (   nonvar(E)
		 ->  clause_property(Ref, file(File)),
		     clause_property(Ref, line_count(Line)),
		     print_message(error, bad_safe_declaration(Goal, File, Line))
		 ;   true
		 )
	       )).

:- initialization(verify_predefined_safe_declarations, now).

%%	safe_primitive(?Goal) is nondet.
%
%	True if Goal is safe  to   call  (i.e.,  cannot access dangerous
%	system-resources and cannot upset  other   parts  of  the Prolog
%	process). There are two  types  of   facts.  ISO  built-ins  are
%	declared without a module prefix. This is safe because it is not
%	allowed to (re-)define these  primitives   (i.e.,  give  them an
%	unsafe     implementation)     and     the       way      around
%	(redefine_system_predicate/1) is unsafe.  The   other  group are
%	module-qualified and only match if the   system  infers that the
%	predicate is imported from the given module.

% First, all ISO system predicates that are considered safe

safe_primitive(true).
safe_primitive(fail).
safe_primitive(system:false).
safe_primitive(repeat).
safe_primitive(!).
					% types
safe_primitive(var(_)).
safe_primitive(nonvar(_)).
safe_primitive(integer(_)).
safe_primitive(float(_)).
safe_primitive(system:rational(_)).
safe_primitive(number(_)).
safe_primitive(atom(_)).
safe_primitive(system:blob(_,_)).
safe_primitive(system:string(_)).
safe_primitive(atomic(_)).
safe_primitive(compound(_)).
safe_primitive(callable(_)).
safe_primitive(ground(_)).
safe_primitive(system:cyclic_term(_)).
safe_primitive(acyclic_term(_)).
safe_primitive(system:is_stream(_)).
					% ordering
safe_primitive(@>(_,_)).
safe_primitive(@>=(_,_)).
safe_primitive(==(_,_)).
safe_primitive(@<(_,_)).
safe_primitive(@=<(_,_)).
safe_primitive(compare(_,_,_)).
safe_primitive(sort(_,_)).
safe_primitive(keysort(_,_)).
safe_primitive(system: =@=(_,_)).
					% unification and equivalence
safe_primitive(=(_,_)).
safe_primitive(\==(_,_)).
					% arithmetic
safe_primitive(is(_,_)).
safe_primitive(>(_,_)).
safe_primitive(>=(_,_)).
safe_primitive(=:=(_,_)).
safe_primitive(=\=(_,_)).
safe_primitive(=<(_,_)).
safe_primitive(<(_,_)).
					% term-handling
safe_primitive(arg(_,_,_)).
safe_primitive(system:setarg(_,_,_)).
safe_primitive(system:nb_setarg(_,_,_)).
safe_primitive(functor(_,_,_)).
safe_primitive(_ =.. _).
safe_primitive(copy_term(_,_)).
safe_primitive(system:duplicate_term(_,_)).
safe_primitive(numbervars(_,_,_)).
					% dicts
safe_primitive(system:is_dict(_)).
safe_primitive(system:is_dict(_,_)).
safe_primitive(system:get_dict(_,_,_)).
safe_primitive(system:get_dict(_,_,_,_,_)).
safe_primitive(system:'$get_dict_ex'(_,_,_)).
safe_primitive(system:dict_create(_,_,_)).
safe_primitive(system:dict_pairs(_,_,_)).
safe_primitive(system:put_dict(_,_,_)).
safe_primitive(system:put_dict(_,_,_,_)).
safe_primitive(system:del_dict(_,_,_,_)).
safe_primitive(system:select_dict(_,_,_)).
safe_primitive(system:b_set_dict(_,_,_)).
safe_primitive(system:nb_set_dict(_,_,_)).
safe_primitive(system:nb_link_dict(_,_,_)).
safe_primitive(system:(:<(_,_))).
safe_primitive(system:(>:<(_,_))).
					% atoms
safe_primitive(atom_chars(_, _)).
safe_primitive(atom_codes(_, _)).
safe_primitive(sub_atom(_,_,_,_,_)).
safe_primitive(atom_concat(_,_,_)).
safe_primitive(atom_length(_,_)).
safe_primitive(system:atomic_list_concat(_,_,_)).
					% numbers
safe_primitive(number_codes(_,_)).
safe_primitive(number_chars(_,_)).
safe_primitive(system:atom_number(_,_)).
					% strings
safe_primitive(system:atom_string(_,_)).
safe_primitive(system:number_string(_,_)).
safe_primitive(system:string_chars(_, _)).
safe_primitive(system:string_codes(_, _)).
safe_primitive(system:sub_string(_,_,_,_,_)).
safe_primitive(system:split_string(_,_,_,_)).
safe_primitive(system:atomics_to_string(_,_,_)).
safe_primitive(system:atomics_to_string(_,_)).
safe_primitive(system:string_concat(_,_,_)).
safe_primitive(system:string_length(_,_)).
					% Lists
safe_primitive(length(_,_)).
					% exceptions
safe_primitive(throw(_)).
					% misc
safe_primitive(current_prolog_flag(_,_)).
safe_primitive(system:sleep(_)).
safe_primitive(system:thread_self(_)).
safe_primitive(system:get_time(_)).
safe_primitive(system:format_time(_,_,_)).
safe_primitive('$messages':message_to_string(_,_)).

safe_primitive(clause(_,_)).
safe_primitive(asserta(X)) :- safe_assert(X).
safe_primitive(assertz(X)) :- safe_assert(X).
safe_primitive(retract(X)) :- safe_assert(X).
safe_primitive(retractall(X)) :- safe_assert(X).

% The non-ISO system predicates.  These can be redefined, so we must
% be careful to ensure the system ones are used.

safe_primitive(system:false).
safe_primitive(system:cyclic_term(_)).
safe_primitive(system:msort(_,_)).
safe_primitive(system:between(_,_,_)).
safe_primitive(system:succ(_,_)).
safe_primitive(system:plus(_,_,_)).
safe_primitive(system:term_variables(_,_)).
safe_primitive(system:atom_to_term(_,_,_)).
safe_primitive(system:term_to_atom(_,_)).
safe_primitive(system:atomic_list_concat(_,_,_)).
safe_primitive(system:atomic_list_concat(_,_)).
safe_primitive(system:downcase_atom(_,_)).
safe_primitive(system:upcase_atom(_,_)).
safe_primitive(system:is_list(_)).
safe_primitive(system:memberchk(_,_)).
safe_primitive(system:'$skip_list'(_,_,_)).
					% attributes
safe_primitive(system:get_attr(_,_,_)).
safe_primitive(system:del_attr(_,_)).
					% globals
safe_primitive(system:b_getval(_,_)).
safe_primitive(system:b_setval(Var,_)) :-
	safe_global_variable(Var).
safe_primitive(system:nb_getval(_,_)).
safe_primitive(system:nb_setval(Var,_)) :-
	safe_global_variable(Var).
safe_primitive(system:nb_current(_,_)).
					% database
safe_primitive(system:assert(X)) :-
	safe_assert(X).

% use_module/1.  We only allow for .pl files that are loaded from
% relative paths that do not contain /../

safe_primitive(system:use_module(Spec)) :-
	ground(Spec),
	(   atom(Spec)
	->  Path = Spec
	;   Spec =.. [_Alias, Segments],
	    phrase(segments_to_path(Segments), List),
	    atomic_list_concat(List, Path)
	),
	\+ is_absolute_file_name(Path),
	\+ sub_atom(Path, _, _, _, '/../'),
	absolute_file_name(Spec, AbsFile,
			   [ access(read),
			     file_type(prolog),
			     file_errors(fail)
			   ]),
	file_name_extension(_, Ext, AbsFile),
	save_extension(Ext).

% support predicates for safe_primitive, validating the safety of
% arguments to certain goals.

segments_to_path(A/B) --> !,
	segments_to_path(A),
	[/],
	segments_to_path(B).
segments_to_path(X) -->
	[X].

save_extension(pl).

%%	safe_assert(+Term) is semidet.
%
%	True if assert(Term) is safe,  which   means  it  asserts in the
%	current module. Cross-module asserts are   considered unsafe. We
%	only allow for adding facts. In theory,  we could also allow for
%	rules if we prove the safety of the body.

safe_assert(C) :- cyclic_term(C), !, fail.
safe_assert(X) :- var(X), !, fail.
safe_assert(_Head:-_Body) :- !, fail.
safe_assert(_:_) :- !, fail.
safe_assert(_).

%%	safe_global_variable(+Name) is semidet.
%
%	True if Name  is  a  global   variable  to  which  assertion  is
%	considered safe.

safe_global_variable(Name) :-
	var(Name), !,
	fail.
safe_global_variable('$clpfd_queue_status').
safe_global_variable('$clpfd_current_propagator').
safe_global_variable('$clpfd_queue').


%%	safe_meta(+Goal, -Called:list(callable)) is semidet.
%
%	Hook. True if Goal is a   meta-predicate that is considered safe
%	iff all elements in Called are safe.

safe_meta(system:put_attr(_,M,A), [M:attr_unify_hook(A, _)]) :- !,
	(   atom(M)
	->  true
	;   instantiation_error(M)
	).
safe_meta(system:with_output_to(Output, G), [G]) :-
	safe_output(Output), !.
safe_meta(system:format(Format, Args), Calls) :-
	format_calls(Format, Args, Calls).
safe_meta(system:format(Output, Format, Args), Calls) :-
	safe_output(Output),
	format_calls(Format, Args, Calls).
safe_meta(prolog_debug:debug(_Term, Format, Args), Calls) :-
	format_calls(Format, Args, Calls).

%%	safe_meta_call(+Goal, -Called:list(callable)) is semidet.
%
%	True if Goal is a   meta-predicate that is considered safe
%	iff all elements in Called are safe.

safe_meta_call(Goal, Called) :-
	safe_meta(Goal, Called), !.	% call hook
safe_meta_call(M:Goal, Called) :- !,
	generic_goal(Goal, Gen),
	safe_meta(M:Gen),
	findall(C, called(Gen, Goal, C), Called).
safe_meta_call(Goal, Called) :-
	generic_goal(Goal, Gen),
	safe_meta(Gen),
	findall(C, called(Gen, Goal, C), Called).

called(Gen, Goal, Called) :-
	arg(I, Gen, Spec),
	integer(Spec),
	arg(I, Goal, Called0),
	extend(Spec, Called0, Called).

generic_goal(G, Gen) :-
	functor(G, Name, Arity),
	functor(Gen, Name, Arity).

extend(0, G, G) :- !.
extend(I, M:G0, M:G) :- !,
	G0 =.. List,
	length(Extra, I),
	append(List, Extra, All),
	G =.. All.
extend(I, G0, G) :-
	G0 =.. List,
	length(Extra, I),
	append(List, Extra, All),
	G =.. All.

safe_meta((0,0)).
safe_meta((0;0)).
safe_meta((0->0)).
safe_meta(apply:forall(0,0)).
safe_meta(catch(0,_,0)).
safe_meta(findall(_,0,_)).
safe_meta(findall(_,0,_,_)).
safe_meta(setof(_,0,_)).		% TBD
safe_meta(bagof(_,0,_)).
safe_meta(^(_,0)).
safe_meta(\+(0)).
safe_meta(apply:maplist(1, _)).
safe_meta(apply:maplist(2, _, _)).
safe_meta(apply:maplist(3, _, _, _)).
safe_meta(call(0)).
safe_meta(call(1, _)).
safe_meta(call(2, _, _)).
safe_meta(call(3, _, _, _)).
safe_meta(call(4, _, _, _, _)).
safe_meta(call(5, _, _, _, _, _)).
safe_meta(call(6, _, _, _, _, _, _)).


%%	safe_output(+Output)
%
%	True if something is a safe output argument for with_output_to/2
%	and friends. We do not want writing to streams.

safe_output(Output) :-
	var(Output), !,
	instantiation_error(Output).
safe_output(atom(_)).
safe_output(string(_)).
safe_output(codes(_)).
safe_output(codes(_,_)).
safe_output(chars(_)).
safe_output(chars(_,_)).
safe_output(current_output).
safe_output(current_error).

%%	format_calls(+Format, +FormatArgs, -Calls)
%
%	Find ~@ calls from Format and Args.

format_calls(Format, Args, Calls) :-
	format_types(Format, Types),
	format_callables(Types, Args, Calls).

format_callables([], [], []).
format_callables([callable|TT], [G|TA], [G|TG]) :- !,
	format_callables(TT, TA, TG).
format_callables([_|TT], [_|TA], TG) :- !,
	format_callables(TT, TA, TG).
format_callables(Types, Args, _) :-		% TBD: Proper error
	throw(error(format_error(Types, Args), _)).


		 /*******************************
		 *    SAFE COMPILATION HOOKS	*
		 *******************************/

:- multifile
	prolog:sandbox_allowed_directive/1,
	prolog:sandbox_allowed_expansion/1.

%%	prolog:sandbox_allowed_directive(:G) is det.
%
%	Throws an exception if G is not considered a safe directive.

prolog:sandbox_allowed_directive(M:PredAttr) :-
	safe_directive(PredAttr),
	(   prolog_load_context(module, M)
	->  PredAttr =.. [Attr, Preds],
	    safe_pattr(Preds, Attr)
	;   permission_error(directive, sandboxed, (:- M:PredAttr))
	).
prolog:sandbox_allowed_directive(G) :-
	safe_goal(G).

safe_directive(dynamic(_)).
safe_directive(thread_local(_)).
safe_directive(discontiguous(_)).
safe_directive(public(_)).

safe_pattr(Var, _) :-
	var(Var), !,
	instantiation_error(Var).
safe_pattr((A,B), Attr) :- !,
	safe_pattr(A, Attr),
	safe_pattr(B, Attr).
safe_pattr(M:G, Attr) :- !,
	(   atom(M),
	    prolog_load_context(module, M)
	->  true
	;   Goal =.. [Attr,M:G],
	    permission_error(directive, sandboxed, (:- Goal))
	).
safe_pattr(_, _).


%%	prolog:sandbox_allowed_expansion(:G) is det.
%
%	Throws an exception if G  is   not  considered  a safe expansion
%	goal. This deals with call-backs from the compiler for
%
%	  - goal_expansion/2
%	  - term_expansion/2
%	  - Quasi quotations.

prolog:sandbox_allowed_expansion(G) :-
	safe_goal(G).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.
	prolog:message_context//1.

prolog:message_context(sandbox(_G, Parents)) -->
	[ nl, 'Reachable from:'-[] ],
	callers(Parents, 10).

callers([], _) --> !.
callers(_,  0) --> !.
callers([G|Parents], Level) -->
	{ NextLevel is Level-1
	},
	[ nl, '	  ~p'-[G] ],
	callers(Parents, NextLevel).

prolog:message(bad_safe_declaration(Goal, File, Line)) -->
	[ '~w:~d: Invalid safe_primitive/1 declaration: ~p'-
	  [File, Line, Goal] ].
