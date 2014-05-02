package quark;

abstract class H extends tree {

   // set of methods common to all higher-order advice
 
   public tree apply( I i ) {
      System.out.println( "cannot apply introduction to higher order advice" );
      System.exit(1);
      return null;
   }

   public tree apply( A a ) {
      System.out.println( "cannot apply local advice to higher order advice" );
      System.exit(1);
      return null;
   }

   public tree apply( G g ) {
      System.out.println( "cannot apply global advice to higher order advice" );
      System.exit(1);
      return null;
   }
}
