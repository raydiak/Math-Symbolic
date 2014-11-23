use v6;

use Test;
plan 6;

use Math::Symbolic;

isa_ok Math::Symbolic.new('0'), Math::Symbolic, ".new() works";

is Math::Symbolic.new('x+y=1').isolate('x').Str, 'x=1-y', '.isolate() works';

is Math::Symbolic.new('x+y').evaluate(y => 2).Str, 'x+2', '.evaluate() works';

is Math::Symbolic.new('a^3+b*2').expand.Str, 'a*a*a+b+b', '.expand() works';

is
    Math::Symbolic.new('y=m*x+b').isolate('x').evaluate(:m(1), :b(0)).Str,
    'x=y',
    'README example works';

is
    Math::Symbolic.new('a²+b²=c²').var('c').evaluate(:a<x2-x1>, :b<y2-y1>)\
        .compile(<x1 y1 x2 y2>).( -1,-1, 2,3 ),
    5,
    'can convert Pythagorean theorem into Perl distance subroutine';

done;
