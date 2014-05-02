:- discontiguous bcClass/4, bcMembers/7, bcImplements/2.

bcClass(1,true,'Prototype.Button','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(1,1,false,true,'Prototype.Button',null,'Button(String)').

/* public Fields */
bcMembers(1,2,false,false,'String',null,'_label').

/* public Methods */
bcMembers(1,3,false,true,'Button',null,'clone()').
bcMembers(1,4,false,true,'Object',null,'clone()').
bcMembers(1,5,false,true,'int',null,'size()').
bcMembers(1,6,false,true,'String',null,'display()').
bcMembers(1,7,false,true,'void',null,'wait(long,int)').
bcMembers(1,8,false,true,'void',null,'wait(long)').
bcMembers(1,9,false,true,'void',null,'wait()').
bcMembers(1,10,false,true,'boolean',null,'equals(Object)').
bcMembers(1,11,false,true,'String',null,'toString()').
bcMembers(1,12,false,true,'int',null,'hashCode()').
bcMembers(1,13,false,true,'Class',null,'getClass()').
bcMembers(1,14,false,true,'void',null,'notify()').
bcMembers(1,15,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(2,true,'Prototype.BigButton','Button').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(2,16,false,true,'Prototype.BigButton',null,'BigButton(String)').

/* public Fields */
bcMembers(2,17,false,false,'String',null,'_label').

/* public Methods */
bcMembers(2,18,false,true,'int',null,'size()').
bcMembers(2,19,false,true,'Button',null,'clone()').
bcMembers(2,20,false,true,'Object',null,'clone()').
bcMembers(2,21,false,true,'String',null,'display()').
bcMembers(2,22,false,true,'void',null,'wait(long,int)').
bcMembers(2,23,false,true,'void',null,'wait(long)').
bcMembers(2,24,false,true,'void',null,'wait()').
bcMembers(2,25,false,true,'boolean',null,'equals(Object)').
bcMembers(2,26,false,true,'String',null,'toString()').
bcMembers(2,27,false,true,'int',null,'hashCode()').
bcMembers(2,28,false,true,'Class',null,'getClass()').
bcMembers(2,29,false,true,'void',null,'notify()').
bcMembers(2,30,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(3,true,'Prototype.SmallButton','Button').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(3,31,false,true,'Prototype.SmallButton',null,'SmallButton(String)').

/* public Fields */
bcMembers(3,32,false,false,'String',null,'_label').

/* public Methods */
bcMembers(3,33,false,true,'int',null,'size()').
bcMembers(3,34,false,true,'Button',null,'clone()').
bcMembers(3,35,false,true,'Object',null,'clone()').
bcMembers(3,36,false,true,'String',null,'display()').
bcMembers(3,37,false,true,'void',null,'wait(long,int)').
bcMembers(3,38,false,true,'void',null,'wait(long)').
bcMembers(3,39,false,true,'void',null,'wait()').
bcMembers(3,40,false,true,'boolean',null,'equals(Object)').
bcMembers(3,41,false,true,'String',null,'toString()').
bcMembers(3,42,false,true,'int',null,'hashCode()').
bcMembers(3,43,false,true,'Class',null,'getClass()').
bcMembers(3,44,false,true,'void',null,'notify()').
bcMembers(3,45,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(4,true,'Prototype.TextBox','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(4,46,false,true,'Prototype.TextBox',null,'TextBox(String)').

/* public Fields */
bcMembers(4,47,false,false,'String',null,'_hint').

/* public Methods */
bcMembers(4,48,false,true,'TextBox',null,'clone()').
bcMembers(4,49,false,true,'Object',null,'clone()').
bcMembers(4,50,false,true,'String',null,'display()').
bcMembers(4,51,false,true,'boolean',null,'showText()').
bcMembers(4,52,false,true,'void',null,'wait(long,int)').
bcMembers(4,53,false,true,'void',null,'wait(long)').
bcMembers(4,54,false,true,'void',null,'wait()').
bcMembers(4,55,false,true,'boolean',null,'equals(Object)').
bcMembers(4,56,false,true,'String',null,'toString()').
bcMembers(4,57,false,true,'int',null,'hashCode()').
bcMembers(4,58,false,true,'Class',null,'getClass()').
bcMembers(4,59,false,true,'void',null,'notify()').
bcMembers(4,60,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(5,true,'Prototype.PasswordBox','TextBox').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(5,61,false,true,'Prototype.PasswordBox',null,'PasswordBox(String)').

/* public Fields */
bcMembers(5,62,false,false,'String',null,'_hint').

/* public Methods */
bcMembers(5,63,false,true,'boolean',null,'showText()').
bcMembers(5,64,false,true,'TextBox',null,'clone()').
bcMembers(5,65,false,true,'Object',null,'clone()').
bcMembers(5,66,false,true,'String',null,'display()').
bcMembers(5,67,false,true,'void',null,'wait(long,int)').
bcMembers(5,68,false,true,'void',null,'wait(long)').
bcMembers(5,69,false,true,'void',null,'wait()').
bcMembers(5,70,false,true,'boolean',null,'equals(Object)').
bcMembers(5,71,false,true,'String',null,'toString()').
bcMembers(5,72,false,true,'int',null,'hashCode()').
bcMembers(5,73,false,true,'Class',null,'getClass()').
bcMembers(5,74,false,true,'void',null,'notify()').
bcMembers(5,75,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(6,true,'Prototype.EmailBox','TextBox').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(6,76,false,true,'Prototype.EmailBox',null,'EmailBox(String)').

/* public Fields */
bcMembers(6,77,false,false,'String',null,'_hint').

/* public Methods */
bcMembers(6,78,false,true,'TextBox',null,'clone()').
bcMembers(6,79,false,true,'Object',null,'clone()').
bcMembers(6,80,false,true,'String',null,'display()').
bcMembers(6,81,false,true,'boolean',null,'showText()').
bcMembers(6,82,false,true,'void',null,'wait(long,int)').
bcMembers(6,83,false,true,'void',null,'wait(long)').
bcMembers(6,84,false,true,'void',null,'wait()').
bcMembers(6,85,false,true,'boolean',null,'equals(Object)').
bcMembers(6,86,false,true,'String',null,'toString()').
bcMembers(6,87,false,true,'int',null,'hashCode()').
bcMembers(6,88,false,true,'Class',null,'getClass()').
bcMembers(6,89,false,true,'void',null,'notify()').
bcMembers(6,90,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(7,true,'Prototype.Form','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */
bcMembers(7,91,false,false,'ArrayList',null,'_buttons').
bcMembers(7,92,false,false,'ArrayList',null,'_boxes').

/* public Methods */
bcMembers(7,93,false,true,'void',null,'addButton(Button)').
bcMembers(7,94,false,true,'void',null,'addBox(TextBox)').
bcMembers(7,95,false,true,'Button',null,'button(int)').
bcMembers(7,96,false,true,'TextBox',null,'box(int)').
bcMembers(7,97,false,true,'void',null,'wait(long,int)').
bcMembers(7,98,false,true,'void',null,'wait(long)').
bcMembers(7,99,false,true,'void',null,'wait()').
bcMembers(7,100,false,true,'boolean',null,'equals(Object)').
bcMembers(7,101,false,true,'String',null,'toString()').
bcMembers(7,102,false,true,'int',null,'hashCode()').
bcMembers(7,103,false,true,'Class',null,'getClass()').
bcMembers(7,104,false,true,'void',null,'notify()').
bcMembers(7,105,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(8,true,'Prototype.FormPrototype','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(8,106,false,true,'Prototype.FormPrototype',null,'FormPrototype(Button,TextBox)').

/* public Fields */
bcMembers(8,107,false,false,'Button',null,'_button').
bcMembers(8,108,false,false,'TextBox',null,'_box').

/* public Methods */
bcMembers(8,109,false,true,'Button',null,'createButton()').
bcMembers(8,110,false,true,'TextBox',null,'createBox()').
bcMembers(8,111,false,true,'void',null,'wait(long,int)').
bcMembers(8,112,false,true,'void',null,'wait(long)').
bcMembers(8,113,false,true,'void',null,'wait()').
bcMembers(8,114,false,true,'boolean',null,'equals(Object)').
bcMembers(8,115,false,true,'String',null,'toString()').
bcMembers(8,116,false,true,'int',null,'hashCode()').
bcMembers(8,117,false,true,'Class',null,'getClass()').
bcMembers(8,118,false,true,'void',null,'notify()').
bcMembers(8,119,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(9,true,'Prototype.Prototype','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(9,120,true,true,'void',null,'main(String[])').
bcMembers(9,121,true,true,'Form',null,'createForm(FormPrototype)').
bcMembers(9,122,false,true,'void',null,'wait(long,int)').
bcMembers(9,123,false,true,'void',null,'wait(long)').
bcMembers(9,124,false,true,'void',null,'wait()').
bcMembers(9,125,false,true,'boolean',null,'equals(Object)').
bcMembers(9,126,false,true,'String',null,'toString()').
bcMembers(9,127,false,true,'int',null,'hashCode()').
bcMembers(9,128,false,true,'Class',null,'getClass()').
bcMembers(9,129,false,true,'void',null,'notify()').
bcMembers(9,130,false,true,'void',null,'notifyAll()').

/*-------------*/
