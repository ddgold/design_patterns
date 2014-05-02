package quark;

class advprog extends I {
   public A left;
   public I right;

   advprog( A left, I right ) { this.left = left; this.right = right; }

   public tree apply( I i ) { return new intsum( i, this ); }

   public tree apply( G g ) { return new gadvprog( g, this ); }

   public tree apply(A a) { return new advprog( new advsum( a, left ), right); }

   public tree apply(H h) { return new advprog((A) left.apply(h), (I) right.apply(h)); }

   public String toString() { return left + "(" + right + ")"; }

   public String eval( String fnc ) {  return right.eval(fnc+left.eval("")); }
}

