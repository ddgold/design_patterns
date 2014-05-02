import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;


public class reflectpy
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
    @SuppressWarnings("unchecked")
	public static void main (String[] args)
    {   	
        File directory = new File(args[1]);

        if (directory.isDirectory())
        {
        	try
        	{
	        	JSONArray bcClass = new JSONArray();
	        	JSONArray bcImplements = new JSONArray();
	        	JSONArray bcMembers = new JSONArray();
	            String files[] = directory.list();
	            for (int i = 0; i < files.length; i++)
	            {
	                int l = files[i].length();
	                if (l > 5 && files[i].substring(l - 5, l).equals("class"))
	                {
	                	Class<?> c = Class.forName(args[0] + "." + files[i].substring(0, l - 6));
	                	bcClass.add(reflectClass(c));
	                	bcImplements.addAll(reflectInterfaces(c, c.getInterfaces()));
	                	bcMembers.addAll(reflectConstructors(c.getConstructors()));
	                	bcMembers.addAll(reflectFields(c.getFields()));
	                	bcMembers.addAll(reflectMethods(c.getMethods()));
	                }
	            }
	            
	            JSONObject obj = new JSONObject();
	            obj.put("bcClass", bcClass);
	            obj.put("bcImplements", bcImplements);
	            obj.put("bcMembers", bcMembers);
	            
	            System.out.print(obj.toJSONString());
        	}
        	catch (ClassNotFoundException e)
            {
        		 System.out.println(e.getMessage() + " not found");
            }
        }
        else
        {
            System.out.println(args[1] + " is not a valid directory.");
        }
        
    }
    
    @SuppressWarnings("unchecked")
	private static JSONObject reflectClass (Class<?> c)
    {
    	JSONObject obj = new JSONObject();
    	obj.put("classID", ++classCount);
    	obj.put("notInterface", !c.isInterface());
    	obj.put("name", c.getName());
    	obj.put("parent", c.getSuperclass().getSimpleName());
        return obj;
    }
    
    @SuppressWarnings("unchecked")
	private static JSONArray reflectInterfaces (Class<?> class1, Class<?>[] interfaces)
    {
    	JSONArray list = new JSONArray();
    	for (Class<?> interface1 : interfaces)
    	{
    		JSONObject obj = new JSONObject();
    		obj.put("interfaceID", ++interfaceCount);
    		obj.put("class", class1.getName());
    		obj.put("interface", interface1.getName());
    		list.add(obj);
    	}
    	return list;
    }
    
    @SuppressWarnings("unchecked")
	private static JSONArray reflectConstructors (Constructor<?>[] constructors)
    {
    	JSONArray list = new JSONArray();
    	for (Constructor<?> constructor : constructors)
    	{
    		JSONObject obj = new JSONObject();
    		obj.put("classID", classCount);
    		obj.put("memberID", ++memberCount);
    		obj.put("isStatic", Modifier.isStatic(constructor.getModifiers()));
    		obj.put("isMethod", true);
    		obj.put("type", constructor.getName());
    		obj.put("extras", "null");
    		String name = constructor.getDeclaringClass().getSimpleName() + "(";
    		Class<?>[] parameters = constructor.getParameterTypes();
    		for (Class<?> parameter : parameters)
    		{
    			name += parameter.getSimpleName() + ",";
    		}
    		if (parameters.length > 0)
    		{
    			name = name.substring(0, name.length() - 1) + ')';
    		}
    		else
    		{
    			name += ')';
    		}
    		obj.put("name", name);
    		list.add(obj);
    	}
        return list;
    }
    
    @SuppressWarnings("unchecked")
	private static JSONArray reflectFields (Field[] fields)
    {
    	JSONArray list = new JSONArray();
    	for (Field field : fields)
    	{
    		JSONObject obj = new JSONObject();
    		obj.put("classID", classCount);
    		obj.put("memberID", ++memberCount);
    		obj.put("isStatic", Modifier.isStatic(field.getModifiers()));
    		obj.put("isMethod", false);
    		obj.put("type", field.getType().getSimpleName());
    		obj.put("extras", "null");
    		obj.put("name", field.getName());
    		list.add(obj);
    	}
    	return list;
    }
    
    @SuppressWarnings("unchecked")
	private static JSONArray reflectMethods (Method[] methods)
    {
    	JSONArray list = new JSONArray();
    	for (Method method: methods)
    	{
    		JSONObject obj = new JSONObject();
    		obj.put("classID", classCount);
    		obj.put("memberID", ++memberCount);
    		obj.put("isStatic", Modifier.isStatic(method.getModifiers()));
    		obj.put("isMethod", true);
    		String extras = "null";
    		Class<?> returnType = method.getReturnType();
    		if (returnType.isArray())
    		{
    			extras = "[]";
    		}
    		obj.put("type", returnType.getSimpleName());
    		obj.put("extras", extras);
    		String name =  method.getName() + "(";
    		Class<?>[] parameters = method.getParameterTypes();
    		for (Class<?> parameter : parameters)
    		{
    			name += parameter.getSimpleName() + ",";
    		}
    		if (parameters.length > 0)
    		{
    			name = name.substring(0, name.length() - 1) + ')';
    		}
    		else
    		{
    			name += ')';
    		}
    		obj.put("name", name);
    		list.add(obj);
    	}
        return list;
    }
}
