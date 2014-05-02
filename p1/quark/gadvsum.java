package quark;

class gadvsum extends G {
   public G left;
   public G right;

   gadvsum( G left, G right ) { this.left = left; this.right = right; }

   public tree apply(A a) { return new gadvsum( left, (G) right.apply(a)); }

   public tree apply( G g ) { return new gadvsum( g, this ); }

   public tree apply( H h ) { return new ghoaadv( h, this ); }

   public String toString() { return left + " . " + right; }

   public String eval( String fnc ) { return left.eval(fnc)+right.eval(fnc); }
}
