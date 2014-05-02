package AbstractFactory;

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


// Factories
abstract class FormFactory
{
	abstract Button createButton(String label);
	abstract TextBox createBox(String hint);
}
class BigButtonEmailBoxFactory extends FormFactory
{
	public Button createButton(String label)
	{
		return new BigButton(label);
	}

	public TextBox createBox(String hint)
	{
		return new EmailBox(hint);
	}
}
class ButtonPasswordBoxFactory extends FormFactory
{
	public Button createButton(String label)
	{
		return new Button(label);
	}

	public TextBox createBox(String hint)
	{
		return new PasswordBox(hint);
	}
}
class SmallButtonTextBoxFactory extends FormFactory
{
	public Button createButton(String label)
	{
		return new SmallButton(label);
	}

	public TextBox createBox(String hint)
	{
		return new TextBox(hint);
	}
}

final class AbstractFactory
{
	public static Form createForm (FormFactory factory, String button, String box)
	{
		Form form = new Form();
		form.addBox(factory.createBox(box));
		form.addBox(factory.createBox(box));
		form.addButton(factory.createButton(button));
		return form;
	}

	public static void main (String[] args)
	{
		System.out.println("AbstractFactory.java");
		{
			String button = "Big";
			String box = "Email";

			FormFactory factory = new BigButtonEmailBoxFactory();
			Form form = createForm(factory, button, box);

			assert (!form.box(0).showText());
			assert (!form.box(1).showText());
			assert (form.button(0).size() == 3);
			assert (form.box(0).display() == box);
			assert (form.box(1).display() == box);
			assert (form.button(0).display() == button);

			System.out.println("  BigButtonEmailBoxFactory");
		}
		{
			String button = "Normal";
			String box = "Password";

			FormFactory factory = new ButtonPasswordBoxFactory();
			Form form = createForm(factory, button, box);

			assert (form.box(0).showText());
			assert (form.box(1).showText());
			assert (form.button(0).size() == 2);
			assert (form.box(0).display() == box);
			assert (form.box(1).display() == box);
			assert (form.button(0).display() == button);

			System.out.println("  ButtonPasswordBoxFactory");
		}
		{
			String button = "Small";
			String box = "Normal";

			FormFactory factory = new SmallButtonTextBoxFactory();
			Form form = createForm(factory, button, box);

			assert (!form.box(0).showText());
			assert (!form.box(1).showText());
			assert (form.button(0).size() == 1);
			assert (form.box(0).display() == box);
			assert (form.box(1).display() == box);
			assert (form.button(0).display() == button);

			System.out.println("  SmallButtonTextBoxFactory");
		}
		System.out.println("Done.");
	}
}