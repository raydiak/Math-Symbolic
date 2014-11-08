use v6;
use Math::Symbolic;
use Test;
plan 3;

isa_ok Math::Symbolic.new('0'), Math::Symbolic, ".new works";

is Math::Symbolic.new('x+y=1').isolate('x').Str, 'x=1-y', '.isolate works';

is Math::Symbolic.new('x+y').evaluate(:y<2>).Str, 'x+2', '.evaluate works';

done;
