package quark;

abstract class A extends tree {

   // set of default methods common to advice
   // may be overridden by specific types
 
   public tree apply( I i ) {
      System.out.println( "cannot apply introduction to A node" );
      System.exit(1);
      return null;
   }

   public tree apply( A a ) {
      return new advsum( a, this );
   }

   public tree apply( G g ) {
      System.out.println( "cannot apply global advice to A node" );
      System.exit(1);
      return null;
   }

   public tree apply( H h ) {
      return new hoaadv( h, this );
   }
}
