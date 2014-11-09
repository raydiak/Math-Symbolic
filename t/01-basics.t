use v6;

use Test;
plan 4;

use Math::Symbolic;

isa_ok Math::Symbolic.new('0'), Math::Symbolic, ".new works";

is Math::Symbolic.new('x+y=1').isolate('x').Str, 'x=1-y', '.isolate works';

is Math::Symbolic.new('x+y').evaluate(y => 2).Str, 'x+2', '.evaluate works';

is
    Math::Symbolic.new('y=m*x+b').isolate('x').evaluate(:m(1), :b(0)).Str,
    'x=y',
    'README example works';

done;
