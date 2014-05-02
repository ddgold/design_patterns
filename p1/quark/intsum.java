package quark;

class intsum extends I {
   public I left;
   public I right;

   intsum( I left, I right ) { this.left = left; this.right = right; }

   public tree apply( I i ) { return new intsum( i, this ); }

   public tree apply( A a ) { return new advprog( a, this ); }

   public tree apply( G g ) { return new gadvprog( g, this ); }

   public tree apply( H h ) {
      return new intsum( (I) left.apply(h), (I) right.apply(h) );
   }

   public String toString() { return left + " + " + right; }

   public String eval( String fnc ) {
      return left.eval(fnc) + " + " + right.eval(fnc); }
}
