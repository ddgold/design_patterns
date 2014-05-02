package Visitor;

abstract class Shape
{
	abstract float accept (DrawVisitor v);

	float area ()
	{
		return accept(new DrawVisitor());
	}
}

class Triangle extends Shape
{
	public int _h;
	public int _b;

	public Triangle (int h, int b)
	{
		_h = h;
		_b = b;
	}

	public float accept (DrawVisitor v)
	{
		return v.visit(this);
	}
} 

class Rectangle extends Shape
{
	public int _h;
	public int _w;

	public Rectangle (int h, int w)
	{
		_h = h;
		_w = w;
	}

	public float accept (DrawVisitor v)
	{
		return v.visit(this);
	}
}

class Circle extends Shape
{
	public int _r;

	public Circle (int r)
	{
		_r = r;
	}

	public float accept (DrawVisitor v)
	{
		return v.visit(this);
	}
}


class DrawVisitor
{
	public float visit (Triangle t)
	{
		return t._h * t._b / 2;
	}

	public float visit (Rectangle r)
	{
		return r._h * r._w;
	}

	public float visit (Circle c)
	{
		return 3.14f * c._r * c._r;
	}
}


final class Visitor
{
	public static void main (String[] args)
	{
		System.out.println("Visitor.java");

		// Triangle
		Shape triangle = new Triangle(2, 3);
		System.out.println("Triangle Area  = " + triangle.area());
		assert (triangle.area() == 3.0);

		// Rectangle
		Shape rectangle = new Rectangle(4, 3);
		System.out.println("Rectangle Area = " + rectangle.area());
		assert (rectangle.area() == 12.0);

		// Circle
		Shape circle = new Circle(2);
		System.out.println("Circle Area    = " + circle.area());
		assert (circle.area() == 3.14 * 4);

		System.out.println("Done.");
	}
}