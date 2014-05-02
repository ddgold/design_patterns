:- discontiguous bcClass/4, bcMembers/7, bcImplements/2.

bcClass(1,true,'yparser.connection','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(1,1,true,false,'yparser.connection',null,'connection(String,String,String,String,String,String)').

/* public Fields */
bcMembers(1,2,true,true,'String',null,'quote').
bcMembers(1,3,true,true,'String',null,'comma').
bcMembers(1,4,false,true,'String',null,'name1').
bcMembers(1,5,false,true,'String',null,'role1').
bcMembers(1,6,false,true,'String',null,'end1').
bcMembers(1,7,false,true,'String',null,'name2').
bcMembers(1,8,false,true,'String',null,'role2').
bcMembers(1,9,false,true,'String',null,'end2').

/* public Methods */
bcMembers(1,10,true,false,'void',null,'dump()').

/*-------------*/
bcClass(2,true,'yparser.klass','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(2,11,true,false,'yparser.klass',null,'klass(String[])').
bcMembers(2,12,true,false,'yparser.klass',null,'klass(String,String,String)').

/* public Fields */
bcMembers(2,13,true,true,'String',null,'comma').
bcMembers(2,14,true,true,'String',null,'quote').
bcMembers(2,15,false,true,'String',null,'id').
bcMembers(2,16,false,true,'String',null,'name').
bcMembers(2,17,false,true,'String',null,'fields').
bcMembers(2,18,false,true,'String',null,'methods').

/* public Methods */
bcMembers(2,19,true,false,'void',null,'dump()').
bcMembers(2,20,true,false,'String',null,'toId(String)').

/*-------------*/
bcClass(3,true,'yparser.Main','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(3,21,true,false,'yparser.Main',null,'Main()').

/* public Fields */
bcMembers(3,22,true,true,'String',null,'gen').
bcMembers(3,23,true,true,'String',null,'comp').
bcMembers(3,24,true,true,'String',null,'agg').
bcMembers(3,25,true,true,'String',null,'none').
bcMembers(3,26,true,true,'String',null,'arrow1').
bcMembers(3,27,true,true,'String',null,'arrow2').
bcMembers(3,28,true,true,'String',null,'comma').
bcMembers(3,29,true,true,'String',null,'commaApost').
bcMembers(3,30,true,true,'String',null,'apostComma').
bcMembers(3,31,true,true,'String',null,'equals').
bcMembers(3,32,true,true,'String',null,'apost').
bcMembers(3,33,true,true,'int',null,'lineCount').
bcMembers(3,34,true,true,'String',null,'line').
bcMembers(3,35,true,true,'String',null,'original').
bcMembers(3,36,true,true,'String',null,'name1').
bcMembers(3,37,true,true,'String',null,'end1').
bcMembers(3,38,true,true,'String',null,'role1').
bcMembers(3,39,true,true,'String',null,'name2').
bcMembers(3,40,true,true,'String',null,'end2').
bcMembers(3,41,true,true,'String',null,'role2').
bcMembers(3,42,true,true,'String','[]','array1').
bcMembers(3,43,true,true,'String','[]','array2').
bcMembers(3,44,true,true,'java.util.HashMap',null,'klassS').
bcMembers(3,45,true,true,'java.util.LinkedList',null,'connectionS').
bcMembers(3,46,true,true,'int',null,'kounter').
bcMembers(3,47,true,true,'java.io.PrintStream',null,'out').

/* public Methods */
bcMembers(3,48,true,false,'void',null,'main(String[])').
bcMembers(3,49,true,false,'void',null,'err(String)').
bcMembers(3,50,true,false,'void',null,'printArray(String,String[])').
bcMembers(3,51,true,false,'String','[]','checkNameStructure(String)').

/*-------------*/
