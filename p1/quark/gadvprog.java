package quark;

class gadvprog extends I {
   public G left;
   public I right;

   gadvprog( G left, I right ) { this.left = left; this.right = right; }

   public tree apply(I i) { return new gadvprog( left, (I) right.apply(i)); }

   public tree apply(A a) { return new gadvprog( left, (I) right.apply(a)); }

   public tree apply( G g ) { return new gadvprog(new gadvsum(g,left), right) ; }

   public tree apply(H h) { return new gadvprog((G) left.apply(h), (I) right.apply(h)); }

   public String toString() { return left + "(" + right + ")"; }

   public String eval( String fnc ) {  return right.eval(fnc+left.eval("")); }
}
