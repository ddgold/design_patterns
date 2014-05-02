package quark;

abstract class G extends tree {

   // set of default methods common to advice
   // may be overridden by specific types
 
   public tree apply( I i ) {
      System.out.println( "cannot apply introduction to A node" );
      System.exit(1);
      return null;
   }

   public tree apply( A a ) {
      System.out.println( "cannot apply local advice to G node" );
      System.exit(1);
      return null;
   }
}
