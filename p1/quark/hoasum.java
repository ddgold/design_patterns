package quark;

class hoasum extends H {
   public H left;
   public H right;

   hoasum( H left, H right ) { this.left = left; this.right = right; }

   public tree apply( H h ) { return new hoasum( h, this ); }

   public String toString() { return left + " * " + right; }

   public String eval( String fnc ) { return left.eval(fnc)+right.eval(fnc); }
}
