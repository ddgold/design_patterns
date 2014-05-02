:- discontiguous bcClass/4, bcMembers/7, bcImplements/2.

bcClass(1,true,'Visitor.Shape','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(1,1,false,true,'void',null,'wait(long,int)').
bcMembers(1,2,false,true,'void',null,'wait(long)').
bcMembers(1,3,false,true,'void',null,'wait()').
bcMembers(1,4,false,true,'boolean',null,'equals(Object)').
bcMembers(1,5,false,true,'String',null,'toString()').
bcMembers(1,6,false,true,'int',null,'hashCode()').
bcMembers(1,7,false,true,'Class',null,'getClass()').
bcMembers(1,8,false,true,'void',null,'notify()').
bcMembers(1,9,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(2,true,'Visitor.DrawVisitor','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(2,10,false,true,'float',null,'visit(Circle)').
bcMembers(2,11,false,true,'float',null,'visit(Triangle)').
bcMembers(2,12,false,true,'float',null,'visit(Rectangle)').
bcMembers(2,13,false,true,'void',null,'wait(long,int)').
bcMembers(2,14,false,true,'void',null,'wait(long)').
bcMembers(2,15,false,true,'void',null,'wait()').
bcMembers(2,16,false,true,'boolean',null,'equals(Object)').
bcMembers(2,17,false,true,'String',null,'toString()').
bcMembers(2,18,false,true,'int',null,'hashCode()').
bcMembers(2,19,false,true,'Class',null,'getClass()').
bcMembers(2,20,false,true,'void',null,'notify()').
bcMembers(2,21,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(3,true,'Visitor.Triangle','Shape').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(3,22,false,true,'Visitor.Triangle',null,'Triangle(int,int)').

/* public Fields */
bcMembers(3,23,false,false,'int',null,'_h').
bcMembers(3,24,false,false,'int',null,'_b').

/* public Methods */
bcMembers(3,25,false,true,'float',null,'accept(DrawVisitor)').
bcMembers(3,26,false,true,'void',null,'wait(long,int)').
bcMembers(3,27,false,true,'void',null,'wait(long)').
bcMembers(3,28,false,true,'void',null,'wait()').
bcMembers(3,29,false,true,'boolean',null,'equals(Object)').
bcMembers(3,30,false,true,'String',null,'toString()').
bcMembers(3,31,false,true,'int',null,'hashCode()').
bcMembers(3,32,false,true,'Class',null,'getClass()').
bcMembers(3,33,false,true,'void',null,'notify()').
bcMembers(3,34,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(4,true,'Visitor.Rectangle','Shape').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(4,35,false,true,'Visitor.Rectangle',null,'Rectangle(int,int)').

/* public Fields */
bcMembers(4,36,false,false,'int',null,'_h').
bcMembers(4,37,false,false,'int',null,'_w').

/* public Methods */
bcMembers(4,38,false,true,'float',null,'accept(DrawVisitor)').
bcMembers(4,39,false,true,'void',null,'wait(long,int)').
bcMembers(4,40,false,true,'void',null,'wait(long)').
bcMembers(4,41,false,true,'void',null,'wait()').
bcMembers(4,42,false,true,'boolean',null,'equals(Object)').
bcMembers(4,43,false,true,'String',null,'toString()').
bcMembers(4,44,false,true,'int',null,'hashCode()').
bcMembers(4,45,false,true,'Class',null,'getClass()').
bcMembers(4,46,false,true,'void',null,'notify()').
bcMembers(4,47,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(5,true,'Visitor.Circle','Shape').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(5,48,false,true,'Visitor.Circle',null,'Circle(int)').

/* public Fields */
bcMembers(5,49,false,false,'int',null,'_r').

/* public Methods */
bcMembers(5,50,false,true,'float',null,'accept(DrawVisitor)').
bcMembers(5,51,false,true,'void',null,'wait(long,int)').
bcMembers(5,52,false,true,'void',null,'wait(long)').
bcMembers(5,53,false,true,'void',null,'wait()').
bcMembers(5,54,false,true,'boolean',null,'equals(Object)').
bcMembers(5,55,false,true,'String',null,'toString()').
bcMembers(5,56,false,true,'int',null,'hashCode()').
bcMembers(5,57,false,true,'Class',null,'getClass()').
bcMembers(5,58,false,true,'void',null,'notify()').
bcMembers(5,59,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(6,true,'Visitor.Visitor','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(6,60,true,true,'void',null,'main(String[])').
bcMembers(6,61,false,true,'void',null,'wait(long,int)').
bcMembers(6,62,false,true,'void',null,'wait(long)').
bcMembers(6,63,false,true,'void',null,'wait()').
bcMembers(6,64,false,true,'boolean',null,'equals(Object)').
bcMembers(6,65,false,true,'String',null,'toString()').
bcMembers(6,66,false,true,'int',null,'hashCode()').
bcMembers(6,67,false,true,'Class',null,'getClass()').
bcMembers(6,68,false,true,'void',null,'notify()').
bcMembers(6,69,false,true,'void',null,'notifyAll()').

/*-------------*/
