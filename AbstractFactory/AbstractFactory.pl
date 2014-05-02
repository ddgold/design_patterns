:- discontiguous bcClass/4, bcMembers/7, bcImplements/2.

bcClass(1,true,'AbstractFactory.BigButton','Button').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(1,1,false,true,'AbstractFactory.BigButton',null,'BigButton(String)').

/* public Fields */
bcMembers(1,2,false,false,'String',null,'_label').

/* public Methods */
bcMembers(1,3,false,true,'int',null,'size()').
bcMembers(1,4,false,true,'String',null,'display()').
bcMembers(1,5,false,true,'void',null,'wait(long,int)').
bcMembers(1,6,false,true,'void',null,'wait(long)').
bcMembers(1,7,false,true,'void',null,'wait()').
bcMembers(1,8,false,true,'boolean',null,'equals(Object)').
bcMembers(1,9,false,true,'String',null,'toString()').
bcMembers(1,10,false,true,'int',null,'hashCode()').
bcMembers(1,11,false,true,'Class',null,'getClass()').
bcMembers(1,12,false,true,'void',null,'notify()').
bcMembers(1,13,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(2,true,'AbstractFactory.SmallButton','Button').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(2,14,false,true,'AbstractFactory.SmallButton',null,'SmallButton(String)').

/* public Fields */
bcMembers(2,15,false,false,'String',null,'_label').

/* public Methods */
bcMembers(2,16,false,true,'int',null,'size()').
bcMembers(2,17,false,true,'String',null,'display()').
bcMembers(2,18,false,true,'void',null,'wait(long,int)').
bcMembers(2,19,false,true,'void',null,'wait(long)').
bcMembers(2,20,false,true,'void',null,'wait()').
bcMembers(2,21,false,true,'boolean',null,'equals(Object)').
bcMembers(2,22,false,true,'String',null,'toString()').
bcMembers(2,23,false,true,'int',null,'hashCode()').
bcMembers(2,24,false,true,'Class',null,'getClass()').
bcMembers(2,25,false,true,'void',null,'notify()').
bcMembers(2,26,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(3,true,'AbstractFactory.TextBox','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(3,27,false,true,'AbstractFactory.TextBox',null,'TextBox(String)').

/* public Fields */
bcMembers(3,28,false,false,'String',null,'_hint').

/* public Methods */
bcMembers(3,29,false,true,'boolean',null,'showText()').
bcMembers(3,30,false,true,'String',null,'display()').
bcMembers(3,31,false,true,'void',null,'wait(long,int)').
bcMembers(3,32,false,true,'void',null,'wait(long)').
bcMembers(3,33,false,true,'void',null,'wait()').
bcMembers(3,34,false,true,'boolean',null,'equals(Object)').
bcMembers(3,35,false,true,'String',null,'toString()').
bcMembers(3,36,false,true,'int',null,'hashCode()').
bcMembers(3,37,false,true,'Class',null,'getClass()').
bcMembers(3,38,false,true,'void',null,'notify()').
bcMembers(3,39,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(4,true,'AbstractFactory.PasswordBox','TextBox').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(4,40,false,true,'AbstractFactory.PasswordBox',null,'PasswordBox(String)').

/* public Fields */
bcMembers(4,41,false,false,'String',null,'_hint').

/* public Methods */
bcMembers(4,42,false,true,'boolean',null,'showText()').
bcMembers(4,43,false,true,'String',null,'display()').
bcMembers(4,44,false,true,'void',null,'wait(long,int)').
bcMembers(4,45,false,true,'void',null,'wait(long)').
bcMembers(4,46,false,true,'void',null,'wait()').
bcMembers(4,47,false,true,'boolean',null,'equals(Object)').
bcMembers(4,48,false,true,'String',null,'toString()').
bcMembers(4,49,false,true,'int',null,'hashCode()').
bcMembers(4,50,false,true,'Class',null,'getClass()').
bcMembers(4,51,false,true,'void',null,'notify()').
bcMembers(4,52,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(5,true,'AbstractFactory.EmailBox','TextBox').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(5,53,false,true,'AbstractFactory.EmailBox',null,'EmailBox(String)').

/* public Fields */
bcMembers(5,54,false,false,'String',null,'_hint').

/* public Methods */
bcMembers(5,55,false,true,'boolean',null,'showText()').
bcMembers(5,56,false,true,'String',null,'display()').
bcMembers(5,57,false,true,'void',null,'wait(long,int)').
bcMembers(5,58,false,true,'void',null,'wait(long)').
bcMembers(5,59,false,true,'void',null,'wait()').
bcMembers(5,60,false,true,'boolean',null,'equals(Object)').
bcMembers(5,61,false,true,'String',null,'toString()').
bcMembers(5,62,false,true,'int',null,'hashCode()').
bcMembers(5,63,false,true,'Class',null,'getClass()').
bcMembers(5,64,false,true,'void',null,'notify()').
bcMembers(5,65,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(6,true,'AbstractFactory.Form','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */
bcMembers(6,66,false,false,'ArrayList',null,'_buttons').
bcMembers(6,67,false,false,'ArrayList',null,'_boxes').

/* public Methods */
bcMembers(6,68,false,true,'void',null,'addBox(TextBox)').
bcMembers(6,69,false,true,'Button',null,'button(int)').
bcMembers(6,70,false,true,'TextBox',null,'box(int)').
bcMembers(6,71,false,true,'void',null,'addButton(Button)').
bcMembers(6,72,false,true,'void',null,'wait(long,int)').
bcMembers(6,73,false,true,'void',null,'wait(long)').
bcMembers(6,74,false,true,'void',null,'wait()').
bcMembers(6,75,false,true,'boolean',null,'equals(Object)').
bcMembers(6,76,false,true,'String',null,'toString()').
bcMembers(6,77,false,true,'int',null,'hashCode()').
bcMembers(6,78,false,true,'Class',null,'getClass()').
bcMembers(6,79,false,true,'void',null,'notify()').
bcMembers(6,80,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(7,true,'AbstractFactory.BigButtonEmailBoxFactory','FormFactory').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(7,81,false,true,'Button',null,'createButton(String)').
bcMembers(7,82,false,true,'TextBox',null,'createBox(String)').
bcMembers(7,83,false,true,'void',null,'wait(long,int)').
bcMembers(7,84,false,true,'void',null,'wait(long)').
bcMembers(7,85,false,true,'void',null,'wait()').
bcMembers(7,86,false,true,'boolean',null,'equals(Object)').
bcMembers(7,87,false,true,'String',null,'toString()').
bcMembers(7,88,false,true,'int',null,'hashCode()').
bcMembers(7,89,false,true,'Class',null,'getClass()').
bcMembers(7,90,false,true,'void',null,'notify()').
bcMembers(7,91,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(8,true,'AbstractFactory.ButtonPasswordBoxFactory','FormFactory').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(8,92,false,true,'Button',null,'createButton(String)').
bcMembers(8,93,false,true,'TextBox',null,'createBox(String)').
bcMembers(8,94,false,true,'void',null,'wait(long,int)').
bcMembers(8,95,false,true,'void',null,'wait(long)').
bcMembers(8,96,false,true,'void',null,'wait()').
bcMembers(8,97,false,true,'boolean',null,'equals(Object)').
bcMembers(8,98,false,true,'String',null,'toString()').
bcMembers(8,99,false,true,'int',null,'hashCode()').
bcMembers(8,100,false,true,'Class',null,'getClass()').
bcMembers(8,101,false,true,'void',null,'notify()').
bcMembers(8,102,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(9,true,'AbstractFactory.SmallButtonTextBoxFactory','FormFactory').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(9,103,false,true,'Button',null,'createButton(String)').
bcMembers(9,104,false,true,'TextBox',null,'createBox(String)').
bcMembers(9,105,false,true,'void',null,'wait(long,int)').
bcMembers(9,106,false,true,'void',null,'wait(long)').
bcMembers(9,107,false,true,'void',null,'wait()').
bcMembers(9,108,false,true,'boolean',null,'equals(Object)').
bcMembers(9,109,false,true,'String',null,'toString()').
bcMembers(9,110,false,true,'int',null,'hashCode()').
bcMembers(9,111,false,true,'Class',null,'getClass()').
bcMembers(9,112,false,true,'void',null,'notify()').
bcMembers(9,113,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(10,true,'AbstractFactory.AbstractFactory','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(10,114,true,true,'Form',null,'createForm(FormFactory,String,String)').
bcMembers(10,115,true,true,'void',null,'main(String[])').
bcMembers(10,116,false,true,'void',null,'wait(long,int)').
bcMembers(10,117,false,true,'void',null,'wait(long)').
bcMembers(10,118,false,true,'void',null,'wait()').
bcMembers(10,119,false,true,'boolean',null,'equals(Object)').
bcMembers(10,120,false,true,'String',null,'toString()').
bcMembers(10,121,false,true,'int',null,'hashCode()').
bcMembers(10,122,false,true,'Class',null,'getClass()').
bcMembers(10,123,false,true,'void',null,'notify()').
bcMembers(10,124,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(11,true,'AbstractFactory.FormFactory','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */

/* public Fields */

/* public Methods */
bcMembers(11,125,false,true,'void',null,'wait(long,int)').
bcMembers(11,126,false,true,'void',null,'wait(long)').
bcMembers(11,127,false,true,'void',null,'wait()').
bcMembers(11,128,false,true,'boolean',null,'equals(Object)').
bcMembers(11,129,false,true,'String',null,'toString()').
bcMembers(11,130,false,true,'int',null,'hashCode()').
bcMembers(11,131,false,true,'Class',null,'getClass()').
bcMembers(11,132,false,true,'void',null,'notify()').
bcMembers(11,133,false,true,'void',null,'notifyAll()').

/*-------------*/
bcClass(12,true,'AbstractFactory.Button','Object').

/* implements declarations */
:- dynamic bcImplements/3.

/* public Constructors */
bcMembers(12,134,false,true,'AbstractFactory.Button',null,'Button(String)').

/* public Fields */
bcMembers(12,135,false,false,'String',null,'_label').

/* public Methods */
bcMembers(12,136,false,true,'String',null,'display()').
bcMembers(12,137,false,true,'int',null,'size()').
bcMembers(12,138,false,true,'void',null,'wait(long,int)').
bcMembers(12,139,false,true,'void',null,'wait(long)').
bcMembers(12,140,false,true,'void',null,'wait()').
bcMembers(12,141,false,true,'boolean',null,'equals(Object)').
bcMembers(12,142,false,true,'String',null,'toString()').
bcMembers(12,143,false,true,'int',null,'hashCode()').
bcMembers(12,144,false,true,'Class',null,'getClass()').
bcMembers(12,145,false,true,'void',null,'notify()').
bcMembers(12,146,false,true,'void',null,'notifyAll()').

/*-------------*/
