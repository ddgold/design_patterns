package Prototype;

import java.util.ArrayList;

// Buttons
class Button
{
	public String _label;

	public Button (String label)
	{
		_label = label;
	}

	public String display ()
	{
		return _label;
	}

	public int size ()
	{
		return 2;
	}

	public Button clone ()
	{
		try
		{
			return (Button) super.clone();
		}
		catch (CloneNotSupportedException e)
		{
			return null;
		}
	}
}
class BigButton extends Button
{
	public BigButton (String label)
	{
		super(label);
	}

	public int size ()
	{
		return 3;
	}
}
class SmallButton extends Button
{
	public SmallButton (String label)
	{
		super(label);
	}

	public int size ()
	{
		return 1;
	}
}


// TextBoxes
class TextBox
{
	public String _hint;

	public TextBox (String hint)
	{
		_hint = hint;
	}

	public String display ()
	{
		return _hint;
	}

	public boolean showText ()
	{
		return true;
	}

	public TextBox clone ()
	{
		try
		{
			return (TextBox) super.clone();
		}
		catch (CloneNotSupportedException e)
		{
			return null;
		}
	}
}
class PasswordBox extends TextBox 
{
	public PasswordBox (String hint)
	{
		super(hint);
	}

	public boolean showText ()
	{
		return false;
	}
}
class EmailBox extends TextBox
{
	public EmailBox (String hint)
	{
		super(hint);
	}
}


// Form
class Form
{
	public ArrayList<Button> _buttons = new ArrayList<Button>();
	public ArrayList<TextBox> _boxes = new ArrayList<TextBox>();

	public void addButton (Button b)
	{
		_buttons.add(b);
	}

	public void addBox (TextBox b)
	{
		_boxes.add(b);
	}

	public Button button (int i)
	{
		return _buttons.get(i);
	}

	public TextBox box (int i)
	{
		return _boxes.get(i);
	}
}


final class FormPrototype
{
	public Button _button;
	public TextBox _box;

	public FormPrototype (Button button, TextBox box)
	{
		_button = button;
		_box = box;
	}

	public Button createButton ()
	{
		return _button.clone();
	}

	public TextBox createBox ()
	{
		return _box.clone();
	}
}

final class Prototype
{
	public static Form createForm (FormPrototype prototype)
	{
		Form form = new Form();
		form.addBox(prototype.createBox());
		form.addBox(prototype.createBox());
		form.addButton(prototype.createButton());
		return form;
	}

	public static void main (String[] args)
	{
		System.out.println("AbstractFactory.java");
		{
			String button = "Big";
			String box = "Email";

			FormPrototype prototype = new FormPrototype(new BigButton(button), new EmailBox(box));
			Form form = createForm(prototype);

			assert (!form.box(0).showText());
			assert (!form.box(1).showText());
			assert (form.button(0).size() == 3);
			assert (form.box(0).display() == box);
			assert (form.box(1).display() == box);
			assert (form.button(0).display() == button);

			System.out.println("  BigButtonEmailBoxPrototype");
		}
		{
			String button = "Normal";
			String box = "Password";

			FormPrototype prototype = new FormPrototype(new Button(button), new PasswordBox(box));
			Form form = createForm(prototype);

			assert (!form.box(0).showText());
			assert (!form.box(1).showText());
			assert (form.button(0).size() == 3);
			assert (form.box(0).display() == box);
			assert (form.box(1).display() == box);
			assert (form.button(0).display() == button);

			System.out.println("  ButtonPasswordBoxPrototype");
		}
		{
			String button = "Small";
			String box = "Normal";

			FormPrototype prototype = new FormPrototype(new SmallButton(button), new TextBox(box));
			Form form = createForm(prototype);

			assert (!form.box(0).showText());
			assert (!form.box(1).showText());
			assert (form.button(0).size() == 3);
			assert (form.box(0).display() == box);
			assert (form.box(1).display() == box);
			assert (form.button(0).display() == button);

			System.out.println("  SmallButtonTextBoxPrototype");
		}
		System.out.println("Done.");
	}
}