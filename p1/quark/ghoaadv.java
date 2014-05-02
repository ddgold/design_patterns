package quark;

class ghoaadv extends G {
   public H left;
   public G right;

   ghoaadv( H left, G right ) { this.left = left; this.right = right; }

   public tree apply( G g ) { return new gadvsum( g, this ); }

   public tree apply(H h) { return new ghoaadv( new hoasum(h, left), right ); }

   public String toString() { return left + "[" + right + "]"; }

   public String eval( String fnc ) { return right.eval(left.eval(fnc)); }
}
