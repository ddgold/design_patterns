package quark;

abstract class tree {
   public abstract tree apply( I i );
   public abstract tree apply( A a );
   public abstract tree apply( H h );
   public abstract tree apply( G g );
   public abstract String toString();
   public abstract String eval( String fnc );
   public void print() { System.out.println( this.toString() ); }
}
