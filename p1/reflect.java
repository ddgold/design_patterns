import java.io.File;
import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.sql.*;  
import java.net.URL;
import java.net.URLClassLoader;


public class reflect 
{
    /***********/
    /* Globals */
    /***********/
    static int classCount = 0;
    static int memberCount = 0;
    static int interfaceCount = 0;

    /********/
    /* Main */
    /********/
    public static void main (String[] args)
    {
        File directory = new File(args[1]);

        if (directory.isDirectory())
        {
            System.out.println(":- discontiguous bcClass/4, bcMembers/7, bcImplements/2.\n");

            String files[] = directory.list();
            for (int i = 0; i < files.length; i++)
            {
                int l = files[i].length();
                if (l > 5 && files[i].substring(l - 5, l).equals("class"))
                {
                    System.out.println(reflectClass(args[0] + "." + files[i].substring(0, l - 6)));
                }
            }
        }
        else
        {
            System.out.println(args[1] + " is not a valid directory.");
        }
    }

    private static String reflectClass (String className)
    {
        try
        {
            Class<?> c = Class.forName(className);
            String o = "bcClass(" + ++classCount + "," + !c.isInterface() + ",\'" + c.getName() + "\',\'" + c.getSuperclass().getSimpleName() + "\').\n\n";
            o += "/* implements declarations */\n" + reflectInterfaces(c, c.getInterfaces()) + "\n";
            o += "/* public Constructors */\n" + reflectConstructors(c.getConstructors()) +"\n";
            o += "/* public Fields */\n" + reflectFields(c.getFields()) +"\n";
            o += "/* public Methods */\n" + reflectMethods(c.getMethods()) +"\n";
            o += "/*-------------*/";

            return o;
        }
        catch (ClassNotFoundException e)
        {
            return e.getMessage() + " not found";
        }
    }
    
    private static StringBuilder reflectInterfaces (Class<?> class1, Class<?>[] interfaces)
    {
    	if (interfaces.length == 0)
    	{
    		return new StringBuilder(":- dynamic bcImplements/3.\n");
    	}
    	StringBuilder o = new StringBuilder();
    	for (Class<?> interface1 : interfaces)
    	{
    		o.append(++interfaceCount + ",");
    		o.append(class1.getName() + ",");
    		o.append(interface1.getName() + ".\n");
    	}
    	
    	return o;
    }

    private static StringBuilder reflectConstructors (Constructor<?>[] constructors)
    {
    	if (constructors.length == 0)
    	{
    		return new StringBuilder(":- dynamic bcMembers/7.\n");
    	}
    	StringBuilder o = new StringBuilder();
    	for (Constructor<?> constructor : constructors)
    	{
    		o.append("bcMembers(");
    		o.append(classCount + ",");
    		o.append(++memberCount + ",");
    		o.append(Modifier.isStatic(constructor.getModifiers()) + ",");
    		o.append("true,");
    		o.append("'" + constructor.getName() + "',");
    		o.append("null,");
    		o.append("'" + constructor.getDeclaringClass().getSimpleName() + "(");
    		Class<?>[] parameters = constructor.getParameterTypes();
    		for (Class<?> parameter : parameters)
    		{
    			o.append(parameter.getSimpleName() + ",");
    		}
    		if (parameters.length > 0)
    		{
    			o.setCharAt(o.length() - 1, ')');
    		}
    		else
    		{
    			o.append(')');
    		}
    		o.append("').\n");
    	}
        return o;
    }

    private static StringBuilder reflectFields (Field[] fields)
    {
    	if (fields.length == 0)
    	{
    		return new StringBuilder(":- dynamic bcMembers/7.\n");
    	}
    	StringBuilder o = new StringBuilder();
    	for (Field field : fields)
    	{
    		o.append("bcMembers(");
    		o.append(classCount + ",");
    		o.append(++memberCount + ",");
    		o.append(Modifier.isStatic(field.getModifiers()) + ",");
    		o.append("false,");
    		o.append("'" + field.getType().getSimpleName() + "',");
    		o.append("null,");
    		o.append("'" + field.getName() + "').\n");
    	}
        return o;
    }

    private static StringBuilder reflectMethods (Method[] methods)
    {
    	if (methods.length == 0)
    	{
    		return new StringBuilder(":- dynamic bcMembers/7.");
    	}
    	StringBuilder o = new StringBuilder();
    	for (Method method: methods)
    	{
    		o.append("bcMembers(");
    		o.append(classCount + ",");
    		o.append(++memberCount + ",");
    		o.append(Modifier.isStatic(method.getModifiers()) + ",");
    		o.append("true,");
    		String extras = "null";
    		Class<?> returnType = method.getReturnType();
    		if (returnType.isArray())
    		{
    			extras = "[]";
    		}
    		o.append("'" + returnType.getSimpleName() + "',");
    		o.append(extras + ",");
    		o.append("'" + method.getName() + "(");
    		Class<?>[] parameters = method.getParameterTypes();
    		for (Class<?> parameter : parameters)
    		{
    			o.append(parameter.getSimpleName() + ",");
    		}
    		if (parameters.length > 0)
    		{
    			o.setCharAt(o.length() - 1, ')');
    		}
    		else
    		{
    			o.append(')');
    		}
    		o.append("').\n");
    	}
        return o;
    }
}
