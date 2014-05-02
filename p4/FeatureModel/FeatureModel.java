package FeatureModel;

import java.util.ArrayList;

public class FeatureModel
{
	private ArrayList<Feature> _features;
	private ArrayList<Relation> _relations;
	private Feature _root;

	public FeatureModel ()
	{
		_features = new ArrayList<Feature>();
		_relations = new ArrayList<Relation>();
		_root = null;
	}

	public Feature addFeature (Feature feature)
	{
		if (_root == null)
		{
			_root = feature;
		}
		_features.add(feature);
		return feature;
	}

	public void addRelation (Relation relation)
	{
		_relations.add(relation);
	}

	public boolean isRoot (Feature feature)
	{
		return _root == feature;
	}

	public boolean validate ()
	{
		// Verify Relations are all valid, with non-null parent, child, and type
		for (Relation relation : _relations)
		{
			if (relation.isParent(null))
			{
				System.out.println("Invalid Relation, parent is null.");
				return false;
			}
			if (relation.isChild(null))
			{
				System.out.println("Invalid Relation, child is null.");
				return false;
			}
			if (relation.getType() == null)
			{
				System.out.println("Invalid Relation, type is null.");
				return false;
			}
		}

		for (Feature feature : _features)
		{
			boolean foundParent = false;
			int childCount = 0;
			RelationType type = null;
			for (Relation relation : _relations)
			{
				// Check for multiple parents
				if (relation.isChild(feature))
				{
					if (foundParent)
					{
						System.out.println("Feature '" + feature.getLabel() + "' has multiple parents.");
						return false;
					}
					else
					{
						foundParent = true;
					}
				}

				// Look for invalid mismatching Relations and count children
				if (relation.isParent(feature))
				{
					// Turn ANDoptional into ANDmandatory 
					RelationType temp = relation.getType();
					if (temp == RelationType.ANDoptional)
					{
						temp = RelationType.ANDmandatory;
					}

					if (type == null)
					{
						type = temp;
					}
					else
					{
						if (type != temp)
						{
							System.out.println("Feature '" + feature.getLabel() + "' has mismatched Relations.");
							return false;
						}
					}

					++childCount;
				}

			}

			// Verift Root Feature have no parent, but all Non-Root Feature do
			if (isRoot(feature))
			{
				if (foundParent)
				{
					System.out.println("Root Feature '" + feature.getLabel() + "' has parent.");
					return false;
				}
			}
			else
			{
				if (!foundParent)
				{
					System.out.println("Non-Root Feature '" + feature.getLabel() + "' has no parent.");
					return false;
				}
			}

			// Fail if Feature has childen, but only one
			if (childCount == 1)
			{
				System.out.println("Feature '" + feature.getLabel() + "' has exactly one child.");
				return false;
			}
		}

		System.out.println("Feature Model is valid.");
		return true;
	}
}