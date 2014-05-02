package FeatureModel;

public class Relation
{
	private Feature _parent;
	private Feature _child;
	private RelationType _type;

	public Relation (Feature parent, Feature child, RelationType type)
	{
		_parent = parent;
		_child = child;
		_type = type;
	}

	public boolean isParent (Feature feature)
	{
		return (_parent == feature);
	}

	public boolean isChild (Feature feature)
	{
		return (_child == feature);
	}

	public RelationType getType ()
	{
		return _type;
	}

	public String getLabel ()
	{
		String output = " Relation between '" + _parent.getLabel() + "' and '" + _child.getLabel() + "'.";
		switch(_type)
		{
			case ANDmandatory:
				return "'ANDmandatory'" + output;
			case ANDoptional:
				return "'ANDoptional'" + output;
			case XOR:
				return "'XOR'" + output;
			case OR:
				return "'OR'" + output;
			default:
				assert false;
				return "Invalid Relation Type";
		}
	}
}