import FeatureModel.*;

public class TestFeatureModel
{
	public static void main (String[] args)
	{
		testCreateFeature();
		testCreateRelation();
		testCreateFeatureModel();
		testInvalidRelations();
		testInvalidParents();
		testInvalidChildren();
		testInvalidMatch();
		testFeatureDiagramA();
		testFeatureDiagramB();
		testFeatureDiagramC();

		System.out.println("Done.");
	}

	public static void testCreateFeature ()
	{
		System.out.println("--- testCreateFeatureModel ---");
		Feature root = new Feature ("NewFeature");
		System.out.println(root.getLabel());
		System.out.println();
	}

	public static void testCreateRelation ()
	{
		System.out.println("--- testCreateRelation ---");
		Feature parent = new Feature("Parent");
		Feature child = new Feature("Child");
		Relation relation = new Relation(parent, child, RelationType.OR);

		assert relation.isParent(parent);
		assert !relation.isParent(child);

		assert relation.isChild(child);
		assert !relation.isChild(parent);

		assert relation.getType() != RelationType.ANDmandatory;
		assert relation.getType() != RelationType.ANDoptional;
		assert relation.getType() != RelationType.XOR;
		assert relation.getType() == RelationType.OR;

		System.out.println(relation.getLabel());
		System.out.println();
	}

	public static void testCreateFeatureModel ()
	{
		System.out.println("--- testCreateFeatureModel ---");
		FeatureModel fm = new FeatureModel();

		Feature first = fm.addFeature(new Feature("First"));
		Feature second = fm.addFeature(new Feature("Second"));
		Feature third = fm.addFeature(new Feature("Third"));

		fm.addRelation(new Relation(first, second, RelationType.ANDmandatory));
		fm.addRelation(new Relation(first, third, RelationType.ANDoptional));

		assert fm.isRoot(first);
		assert !fm.isRoot(second);
		assert fm.validate();

		System.out.println();
	}

	public static void testInvalidRelations ()
	{
		System.out.println("--- testInvalidRelations ---");
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));

			fm.addRelation(new Relation(null, second, RelationType.ANDmandatory));
			assert !fm.validate();
		}
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));

			fm.addRelation(new Relation(first, null, RelationType.ANDmandatory));
			assert !fm.validate();
		}
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));

			fm.addRelation(new Relation(first, second, null));
			assert !fm.validate();
		}
		System.out.println();
	}

	public static void testInvalidParents ()
	{
		System.out.println("--- testInvalidParents ---");
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));
			Feature third = fm.addFeature(new Feature("Third"));

			fm.addRelation(new Relation(first, second, RelationType.ANDmandatory));
			fm.addRelation(new Relation(first, third, RelationType.ANDmandatory));
			fm.addRelation(new Relation(second, third, RelationType.ANDmandatory));
			assert !fm.validate();
		}
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));

			fm.addRelation(new Relation(second, first, RelationType.ANDmandatory));
			assert !fm.validate();
		}
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));
			Feature third = fm.addFeature(new Feature("Third"));

			fm.addRelation(new Relation(first, second, RelationType.ANDmandatory));
			fm.addRelation(new Relation(third, second, RelationType.ANDmandatory));
			assert !fm.validate();
		}
		System.out.println();
	}

	public static void testInvalidChildren ()
	{
		System.out.println("--- testInvalidChildren ---");
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));

			fm.addRelation(new Relation(first, second, RelationType.ANDmandatory));
			assert !fm.validate();
		}
		System.out.println();
	}

	public static void testInvalidMatch ()
	{
		System.out.println("--- testInvalidMatch ---");
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));
			Feature third = fm.addFeature(new Feature("Third"));

			fm.addRelation(new Relation(first, second, RelationType.ANDmandatory));
			fm.addRelation(new Relation(first, third, RelationType.ANDoptional));
			assert fm.validate();
		}
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));
			Feature third = fm.addFeature(new Feature("Third"));

			fm.addRelation(new Relation(first, second, RelationType.ANDmandatory));
			fm.addRelation(new Relation(first, third, RelationType.XOR));
			assert !fm.validate();
		}
		{
			FeatureModel fm = new FeatureModel();

			Feature first = fm.addFeature(new Feature("First"));
			Feature second = fm.addFeature(new Feature("Second"));
			Feature third = fm.addFeature(new Feature("Third"));
			Feature fourth = fm.addFeature(new Feature("Fourth"));
			Feature fifth = fm.addFeature(new Feature("Fifth"));
			Feature sixth = fm.addFeature(new Feature("Sixth"));
			Feature seventh = fm.addFeature(new Feature("Seventh"));
			Feature eighth = fm.addFeature(new Feature("Eighth"));
			Feature ninth = fm.addFeature(new Feature("Ninth"));
			Feature tenth = fm.addFeature(new Feature("Tenth"));

			fm.addRelation(new Relation(first, second, RelationType.ANDmandatory));
			fm.addRelation(new Relation(first, third, RelationType.ANDoptional));
			fm.addRelation(new Relation(first, fourth, RelationType.ANDoptional));
			fm.addRelation(new Relation(third, fifth, RelationType.OR));
			fm.addRelation(new Relation(third, sixth, RelationType.OR));
			fm.addRelation(new Relation(fourth, seventh, RelationType.XOR));
			fm.addRelation(new Relation(fourth, eighth, RelationType.XOR));
			fm.addRelation(new Relation(sixth, ninth, RelationType.OR));
			fm.addRelation(new Relation(sixth, tenth, RelationType.XOR));
			assert !fm.validate();
		}
		System.out.println();
	}

	public static void testFeatureDiagramA ()
	{
		System.out.println("--- testFeatureDiagramA ---");
		FeatureModel fm = new FeatureModel();

		Feature a = fm.addFeature(new Feature("EX"));
		Feature b = fm.addFeature(new Feature("directX"));
		Feature c = fm.addFeature(new Feature("API"));
		Feature d = fm.addFeature(new Feature("base"));
		Feature e = fm.addFeature(new Feature("get"));
		Feature f = fm.addFeature(new Feature("put"));

		fm.addRelation(new Relation(a, b, RelationType.ANDoptional));
		fm.addRelation(new Relation(a, c, RelationType.ANDoptional));
		fm.addRelation(new Relation(a, d, RelationType.ANDmandatory));
		fm.addRelation(new Relation(c, e, RelationType.OR));
		fm.addRelation(new Relation(c, f, RelationType.OR));

		assert fm.validate();
		System.out.println();
	}

	public static void testFeatureDiagramB ()
	{
		System.out.println("--- testFeatureDiagramB ---");
		FeatureModel fm = new FeatureModel();

		Feature a = fm.addFeature(new Feature("Car"));
		Feature b = fm.addFeature(new Feature("Cruise"));
		Feature c = fm.addFeature(new Feature("Engine"));
		Feature d = fm.addFeature(new Feature("Transmission"));
		Feature e = fm.addFeature(new Feature("CarBody"));
		Feature f = fm.addFeature(new Feature("Gasoline"));
		Feature g = fm.addFeature(new Feature("Electric"));
		Feature h = fm.addFeature(new Feature("Manual"));
		Feature i = fm.addFeature(new Feature("Automatic"));

		fm.addRelation(new Relation(a, b, RelationType.ANDoptional));
		fm.addRelation(new Relation(a, c, RelationType.ANDmandatory));
		fm.addRelation(new Relation(a, d, RelationType.ANDmandatory));
		fm.addRelation(new Relation(a, e, RelationType.ANDmandatory));
		fm.addRelation(new Relation(c, f, RelationType.OR));
		fm.addRelation(new Relation(c, g, RelationType.OR));
		fm.addRelation(new Relation(d, h, RelationType.XOR));
		fm.addRelation(new Relation(d, i, RelationType.XOR));

		assert fm.validate();
		System.out.println();
	}

	public static void testFeatureDiagramC ()
	{
		System.out.println("--- testFeatureDiagramC ---");
		FeatureModel fm = new FeatureModel();

		Feature a = fm.addFeature(new Feature("GraphicLibrary"));
		Feature b = fm.addFeature(new Feature("Edge Type"));
		Feature c = fm.addFeature(new Feature("Search"));
		Feature d = fm.addFeature(new Feature("Weighted"));
		Feature e = fm.addFeature(new Feature("Algorithm"));
		Feature f = fm.addFeature(new Feature("Directed"));
		Feature g = fm.addFeature(new Feature("Undirected"));
		Feature h = fm.addFeature(new Feature("BFS"));
		Feature i = fm.addFeature(new Feature("DFS"));
		Feature j = fm.addFeature(new Feature("Cycle"));
		Feature k = fm.addFeature(new Feature("ShortestPath"));
		Feature l = fm.addFeature(new Feature("MST"));
		Feature m = fm.addFeature(new Feature("Transpose"));
		Feature n = fm.addFeature(new Feature("Prim"));
		Feature o = fm.addFeature(new Feature("Kruskal"));

		fm.addRelation(new Relation(a, b, RelationType.ANDmandatory));
		fm.addRelation(new Relation(a, c, RelationType.ANDoptional));
		fm.addRelation(new Relation(a, d, RelationType.ANDoptional));
		fm.addRelation(new Relation(a, e, RelationType.ANDmandatory));
		fm.addRelation(new Relation(b, f, RelationType.XOR));
		fm.addRelation(new Relation(b, g, RelationType.XOR));
		fm.addRelation(new Relation(c, h, RelationType.XOR));
		fm.addRelation(new Relation(c, i, RelationType.XOR));
		fm.addRelation(new Relation(e, j, RelationType.OR));
		fm.addRelation(new Relation(e, k, RelationType.OR));
		fm.addRelation(new Relation(e, l, RelationType.OR));
		fm.addRelation(new Relation(e, m, RelationType.OR));
		fm.addRelation(new Relation(l, n, RelationType.XOR));
		fm.addRelation(new Relation(l, o, RelationType.XOR));

		assert fm.validate();
		System.out.println();
	}
}