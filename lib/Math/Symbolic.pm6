unit class Math::Symbolic;

use Math::Symbolic::Tree;
use Math::Symbolic::Grammar;
use Math::Symbolic::Language;

my %ops := Math::Symbolic::Language.by_name;
my %syn := Math::Symbolic::Language.by_syntax;
my %syn_syn := Math::Symbolic::Language.syntax_by_syntax;

has $.tree handles <Numeric Str count>;

multi method new ($in, *%args is copy) {

    my $parse = Math::Symbolic::Grammar.parse(~$in);

    die 'Parse failure: invalid expression' unless $parse;

    %args<tree> = self!convert_parse($parse);
    my $obj = self.bless: |%args;

    $obj;
}

method clone () {
    self.new: tree => self.tree.clone;
}

method child (|args) {
    self.new: tree => $!tree.child: |args;
}

method list () {
    my @vars = $.tree.list;

    @vars > 1 ??
        @vars.map: { self.new: tree => $_ } !!
        self;
}

method expression (Str:D $var) {
    my $new := self.clone.isolate($var);
    my $tree := $new.tree;
    $tree.set: $tree.children[1];
    $new;
}

method code ($language = 'perl6', $tree = $!tree) {
    $tree.translate: $language;
}

method routine ($positional = False, $defaults? is copy, $tree = $!tree) {
    my @vars = $tree.find_all(:type<symbol>)».content.sort.squish;
    if defined $defaults {
        $defaults = Hash.new: @vars.map: * => $defaults
            unless $defaults ~~ Associative;
    } else {
        $defaults = {};
    }
    if $positional === True { @$positional = @vars; @vars = (); }
    elsif $positional { @vars .= grep: * !∈ @$positional; }

    my @sig;
    if $positional {
        for @$positional {
            my $default = $defaults{$_};
            $default = $default.defined ?? " = $default" !! '';
            @sig.push: "Numeric:D \$$_$default";
        }
    }

    for @vars {
        my $default = $defaults{$_};
        $default = $default.defined ?? " = $default" !! '!';
        @sig.push: "Numeric:D :\$$_$default";
    }

    my $sig = '--> Numeric:D';
    $sig = @sig.join(', ') ~ " $sig" if @sig;

    "sub ($sig) is pure \{ " ~ $tree.translate('perl6') ~ ' };';
}

method compile (|args) {
    EVAL self.routine(|args);
}

method evaluate (*%vals is copy) {
    for %vals.values {
        when Math::Symbolic::Tree {}
        when Math::Symbolic {
            $_ = $_.tree;
        }
        default {
            $_ = self.new(~$_).tree;
        }
    }

    for $!tree.find_all: :type<symbol>, :content(%vals.keys.any) {
        $_.set: %vals{$_.content}.clone;
    }

    self.simplify;
}

# TODO need more property-based generic manipulations and per-op special cases, like *0, ^0, etc
    # but avoid adding code in the ops
    # iow properties are good for Operations (as long as they're optional with sane defaults), but code in those properties is bad, because the point of the Operation class is to be a simple declarative way to express the language, so minimizing complexity in the public API of ::Operation is central to its intended purpose
    # TODO BUG speaking of minimizing complexity in ::Operation, please convert the .function/.syntax/.syntaxes/BUILD mess to Roles or something soon
# this is also highly inefficient
method simplify () {
    my $tree = $!tree;
    my $hit = True;
    while $hit {
        $hit = False;
        my $node;

        # x^-n = 1/x^n
        if $node = $tree.find( :type<operation>, :content('power'|'root'), :children(
            *,
            {:type<operation>, :content<neg>}
        ) ) {
            $node.children[1] = $node.children[1].children[0];
            $node.children[1] = $node.clone;
            $node.content = %ops<div>;
            $node.children[0] = Math::Symbolic::Tree.new(:type<value>, :content(1));
            $hit = True;
        }

        # x^-n = 1/x^n
        elsif $node = $tree.find( :type<operation>, :content('power'|'root'), :children(
            *,
            {:type<value>, :content(* < 0)}
        ) ) {
            $node.children[1].content *= -1;
            $node.children[1] = $node.clone;
            $node.content = %ops<div>;
            $node.children[0] = Math::Symbolic::Tree.new(:type<value>, :content(1));
            $hit = True;
        }

        # sqr
        elsif $node = $tree.find( :type<operation>, :content<power>, :children(
            *,
            {:type<value>, :content(2)}
        ) ) {
            $node.content = %ops<sqr>;
            $node.children[1] :delete;
            $hit = True;
        }

        # sqrt
        elsif $node = $tree.find( :type<operation>, :content<root>, :children(
            *,
            {:type<value>, :content(2)}
        ) ) {
            $node.content = %ops<sqrt>;
            $node.children[1] :delete;
            $hit = True;
        }

        # neg
        elsif $node = $tree.find( :type<operation>, :content('mul'|'div'), :children(
            *,
            {:type<value>, :content(-1)}
        ) ) {
            $node.content = %ops<neg>;
            $node.children[1] :delete;
            $hit = True;
        }

        # neg
        elsif $node = $tree.find( :type<operation>, :content<mul>, :children(
            {:type<value>, :content(-1)},
            *
        ) ) {
            $node.content = %ops<neg>;
            $node.children[0] = $node.children[1]:delete;
            $hit = True;
        }

        # invert -> division
        elsif $node = $tree.find( :type<operation>, :content<inv> ) {
            $node.content = %ops<div>;
            $node.children[1] = $node.children[0];
            $node.children[0] = $tree.new(:type<value>, :content(1));
            $hit = True;
        }

        # a/b/c -> a/(b*c)
        elsif $node = $tree.find( :type<operation>, :content<div>, :children(
            {:type<operation>, :content<div>},
            *
        ) ) {
            $node.content = %ops<div>;
            $node.children[1] = $tree.new(:type<operation>, :content(%ops<mul>), :children(
                $node.children[0].children[1],
                $node.children[1]
            ));
            $node.children[0] = $node.children[0].children[0];
            $hit = True;
        }

        # a/(b/c) -> a*c/b
        elsif $node = $tree.find( :type<operation>, :content<div>, :children(
            *,
            {:type<operation>, :content<div>}
        ) ) {
            $node.content = %ops<div>;
            $node.children[0] = Math::Symbolic::Tree.new(:type<operation>, :content(%ops<mul>), :children(
                $node.children[0],
                $node.children[1].children[1]
            ));
            $node.children[1] = $node.children[1].children[0];
            $hit = True;
        }

        # a*(b/c) -> a*b/c
        elsif $node = $tree.find( :type<operation>, :content<mul>, :children(
            *,
            {:type<operation>, :content<div>}
        ) ) {
            $node.content = %ops<div>;
            $node.children[0] = Math::Symbolic::Tree.new(:type<operation>, :content(%ops<mul>), :children(
                $node.children[0],
                $node.children[1].children[0]
            ));
            $node.children[1] = $node.children[1].children[1];
            $hit = True;
        }

        elsif my @nodes = $tree.find_all( :type<operation> ) {
            # TODO we could use a smarter pattern to not have to re-test every single op in the tree repeatedly
            # except now we're doing many things in here
            while @nodes && !$hit {
                $node = @nodes.shift;
                my $op = $node.content;
                my $func = $op.function;

                # identity value stuff
                my $ident = $func.identity;
                if defined $ident {
                    my $do = False;
                    my $val = $node.children[1];
                    $do = ($val.type eq 'value' && $val.content ~~ $ident);
                    my $flip = False;
                    unless $do {
                        $val = $node.children[0];
                        $do = (
                            $func.commute ~~ Bool &&
                            $func.commute &&
                            $val.type eq 'value' &&
                            $val.content ~~ $ident
                        );
                        $flip = True;
                    }
                    if $do {
                        my $new;
                        if $flip {
                            $new = $node.children[1];
                        } else {
                            $new = $node.children[0];
                        }
                        $node.type = $new.type;
                        $node.content = $new.content;
                        $node.children = $new.children;
                        $hit = True;
                    }
                }

                # inversion
                if !$hit {
                    if $func.arity == 1 {
                        my $child = $node.children[0];
                        if $child.type eq 'operation' {
                            my $child_op = $child.content;
                            my $child_func = $child_op.function;
                            if $child_op.arity == 1 && $func.inverse === $child_op {
                                $node.type = $child.children[0].type;
                                $node.content = $child.children[0].content;
                                $node.children = $child.children;
                                $hit = True;
                            } elsif $child_op.arity == 2 && $child_func.commute !=== True &&
                                $child_func.invert-via === $node.content {
                                $node.content = $child_op;
                                $node.children = $child.children.reverse;
                                #self.dump_tree($node);
                                $hit = True;
                            }
                        }
                    } elsif $func.arity == 2 {
                        my $inv_via = $func.invert-via;
                        if $inv_via {
                            for 1, 0 -> $i {
                                my $child := $node.children[$i];

                                my $do = False;
                                if $child.type eq 'operation' &&
                                    $child.content === $inv_via {
                                    $child = $child.children[0];
                                    $do = True;
                                } elsif $inv_via eq 'neg' &&
                                    $child.type eq 'value' &&
                                    $child.content < 0 {
                                    $child.content *= -1;
                                    $do = True;
                                }

                                if $do {
                                    $node.content = $func.inverse;
                                    $node.children .= reverse unless $i;
                                    $hit = True;
                                    last;
                                }

                                last unless $func.commute === True;
                            }
                        }
                    }
                }
            }
        }

        # invert -> division - we put this last, so that inversion is preserved for matching against inv-via
        elsif $node = $tree.find( :type<operation>, :content<inv> ) {
            $node.content = %ops<div>;
            $node.children[1] = $node.children[0];
            $node.children[0] = $tree.new(:type<value>, :content(1));
            $hit = True;
        }
    }

    self.fold;
}

method fold ($tree = $!tree) {
    my $hit = True;
    while $hit && my @nodes = $tree.find_all: :type<operation> {
        $hit = False;
        for @nodes -> $node {
            my $func = $node.content.function;
            if $func && (my &eval := $func.eval) &&
                $node.children.all.type eq 'value' {
                $node.type = 'value';
                $node.content = eval( |@($node.children».content) );
                $node.children = ();
                $hit = True;
            }
        }
    }

    self;
}

method poly ($var?, :$coef) {
    self.normalize;
    self.expand;

    my $tree = $!tree;

    my $work = $tree;

    if $tree.type eq 'relation' {
        my $side = 0;
        if defined $var {
            my @paths = $tree.find_all: :type<symbol>, :content($var), :path;
            die "Error: variable '$var' not found in '$tree'" unless @paths;

            my %side_var := @paths»[0].Bag;
            $side = +(%side_var{0} < %side_var{1});

            if %side_var.keys == 1 {
                my @path;
                my $i = 0;
                my $max_elems = @paths».elems.min;
                while $max_elems > $i && @paths»[$i].unique == 1 {
                    @path[$i] = @paths[0][$i];
                    $i++;
                }

                if @path > 1 {
                    self.isolate: :@path;
                    $side = 0;
                }
            } # TODO else do something clever? can't solve t=√t, but can solve t²=t...
        } else {
            $side = ($tree.children[0].count < $tree.children[1].count);
        }

        $tree.children .= reverse if $side;
        $work = $tree.children[0];
        my $opp := $tree.children[1];

        my %zero = :type<value>, :content(0);
        unless $opp.match: |%zero {
            @($work.children) = $work.clone, $tree.new(
                :type<operation>, :content(%ops<mul>),
                :children[ $tree.new(:type<value>,:content(-1)), $opp ]
            );
            $work.type = 'operation';
            $work.content = %ops<add>;

            $opp = $tree.new: |%zero;

            self.expand;
        }
    }

    my \ret = self.condense($var, $work, :$coef);
    self.simplify;
    ret;
}

method condense ($var?, $tree = $!tree, :$coef = False) {
    use MultiHash;

    # self.normalize: $tree;

    my $type = $tree.type;

    if $type eq 'relation' {
        self.condense: $var, $_ for $tree.children;
    }

    if $type ne 'operation' {
        die 'Error: coefficient analysis is only available for operation nodes'
            if $coef;
        return self;
    }

    my $op = $tree.content;
    my $func = $op.function;
    my $up = $func.up;
    my @c = $tree.children;

    unless $up {
        self.condense: $var, $_ for @c;
        return self;
    }

    my @parts;
    my $upup = $up.function.up;
    my $vars = MultiHash.new;
    my $n := $vars.elem();
    for $tree.chain {
        my $type = $_.type;
        my $content = $_.content;
        if $type eq 'symbol' {
            $vars.elem($content => 1)[0]++;
        } elsif $type eq 'value' {
            if defined $n[0] {
                $n[0] = ($func.eval)($n[0], $content);
            } else {
                $n[0] = $content;
            }
        } elsif $type eq 'operation' {
            if $content eq $up {
                my %subvar_count;
                my @subparts;
                my $subfunc = $up.function;

                if $subfunc.commute {
                    for $_.chain -> $sub {
                        my $subtype = $sub.type;
                        my $subcontent = $sub.content;
                        if $subtype eq 'symbol' {
                            %subvar_count{$subcontent}++;
                        } elsif $subtype eq 'value' {
                            if %subvar_count{''}:exists {
                                %subvar_count{''} = ($subfunc.eval)(
                                    %subvar_count{''}, $subcontent );
                            } else {
                                %subvar_count{''} = $subcontent;
                            }
                        } elsif $upup && $subtype eq 'operation' && $subcontent eq $upup &&
                            $sub.match: :children({:type<symbol>,}, {:type<value>,}) {
                            %subvar_count{$sub.children[0].content} += $sub.children[1].content;
                        } else {
                            @subparts.push: $sub;
                        }
                    }
                } elsif $_.match: :children({:type<symbol>,}, {:type<value>,}) {
                    %subvar_count{$_.children[0].content}++;
                    %subvar_count{''} += $_.children[1].content;
                } else {
                    @subparts.push: $_;
                }

                my $count = %subvar_count{''}:delete // 1;

                %subvar_count .= grep: *.value;
                my $elem := $vars.elem(|%subvar_count);
                if %subvar_count {
                    $elem[0] += $count;
                } else {
                    unshift @subparts: $tree.new: :type<value>, :content($count)
                        unless $count == $subfunc.identity;
                }

                $elem.push: $tree.new-chain: $up, @subparts if @subparts;
            } elsif $upup && $content eq $upup && $_.match:
                :children({:type<symbol>,}, {:type<value>,}) {
                $vars.elem($_.children[0].content => $_.children[1].content)[0] += 1;
            } else {
                @parts.push: $_;
            }
        } else {
            die "Error: cannot manipulate nodes of type '$type'";
        }
    }

    if @parts {
        $n.push: 1 unless @$n;
        $n.push: @parts;
    }

    $vars.hash .= grep: { my $v := .value; $v[0] || $v.elems > 1 };

    self.condense: Any, $_ for $vars.values».grep: Math::Symbolic::Tree;

    if $var {
        my $newvars = $vars.new;
        # transform for requested $var here
        for $vars.kv -> $keyhash, $vals {
            my $power = $keyhash{$var} :delete // 0;
            my @subparts;
            if $upup {
                push @subparts: $keyhash.map: {
                    .value == $upup.function.identity ?? $tree.new-sym(.key) !!
                    $tree.new-op($upup, $tree.new-sym(.key), $tree.new-val(.value))
                };
            } else {
                push @subparts: $keyhash.map: -> $p {
                    my @sub;
                    push @sub: $tree.new-sym($p.key) for ^($p.value);
                    @sub;
                };
            }
            my $elem := $newvars.elem($var => $power);
            my $co = @$vals.shift;
            unshift @subparts: $tree.new-val: $co;# if $co != $up.function.identity;
            $elem.push: $tree.new-chain: $up, @subparts if @subparts;
            $elem.push: @$vals;
        }

        $vars := $newvars;
    }

    # then convert back into a ::Tree and $tree.set
    my @new_parts;
    for $vars.keys -> $keyhash {
        my $vals := $vars.hash{$keyhash};
        my @subparts;

        for $keyhash.keys.sort -> $kk {
            my $kv := $keyhash{$kk};
            if (defined $var and $kk eq $var) || $kv != 1 {
                my $new;
                if $upup {
                    $new = $tree.new-op: $upup, $tree.new-sym($kk), $tree.new-val($kv);
                } elsif $kv == $kv.Int && $kv > 0 {
                    $new.push: $tree.new-sym($kk) for 1..$kv;
                    $new = $tree.new-chain: $up, |@$new;
                } else {
                    die "Error: this transformation would require the Knuth up arrow (NYI)";
                }
                if defined($var) && $kk eq $var && $up.function.commute {
                    @subparts.unshift: $new;
                } else {
                    @subparts.push: $new;
                }
            } else {
                @subparts.push: $tree.new-sym($kk);
            }
        }

        if @$vals && $vals[0] ~~ Numeric &&
            (my $v = $vals.shift) != $op.function.identity {
            if $op.function.commute { $vals.unshift: $tree.new-val: $v }
            else { $vals.push: $tree.new-val: $v }
        }

        @subparts.push: $tree.new-chain: $op, @$vals if @$vals;

        @new_parts.push: ($vals = $tree.new-chain: $up, @subparts);
    }

    $tree.set: $tree.new-chain($op, @new_parts);

    if $coef {
        die 'Error: returning coefficients is only supported for a single specific variable'
            unless defined $var;
        die "Error: couldn't reduce '$tree' to a polynomial in $var"
            unless $vars.keys».keys.all eq $var;

        my %return;
        for $vars.kv -> $k, $v {
            %return{$k.values[0]} = $v.children[1];
        }
        return %return;
    }

    self;
}

# roughly opposite of simplify
# expands shorthand ops (eg a² -> a^2)
# inverts non-commutative ops to commutable forms (eg a-b -> a+-1*b)
# folds constants
method normalize ($tree = $!tree) {
    my $hit = True;
    while $hit {
        $hit = False;
        my $node;

        # sqr -> power
        if $node = $tree.find( :type<operation>, :content<sqr> ) {
            $node.content = %ops<power>;
            $node.children[1] = $tree.new( :type<value>, :content(2) );
            $hit = True;
        }

        # sqrt -> power
        elsif $node = $tree.find( :type<operation>, :content<sqrt> ) {
            $node.content = %ops<power>;
            $node.children[1] = $tree.new( :type<value>, :content(.5) );
            $hit = True;
        }

        elsif my @nodes = $tree.find_all( :type<operation> ) {
            # TODO we could use a smarter pattern to not have to re-test every single op in the tree repeatedly
            # except now we're doing many things in here
            while @nodes && !$hit {
                $node = @nodes.shift;
                my $op = $node.content;
                my $func = $op.function;

                # inversion stuff
                my $inv = $func.inverse;
                my $inv-via = $func.invert-via;
                if !$func.normal && $func.arity == 2 && $inv && $inv.function.normal && $inv-via {
                    $node.children[1] = $tree.new(
                        :type<operation>, :content($inv-via),
                        :children($node.children[1])
                    );
                    $node.content = $inv;

                    $hit = True;
                }

                # identity value stuff
                if !$hit && defined (my $ident = $func.identity) {
                    my $do = False;
                    my $val = $node.children[1];
                    $do = ($val.type eq 'value' && $val.content ~~ $ident);
                    my $flip = False;
                    unless $do {
                        $val = $node.children[0];
                        $do = (
                            $func.commute ~~ Bool &&
                            $func.commute &&
                            $val.type eq 'value' &&
                            $val.content ~~ $ident
                        );
                        $flip = True;
                    }
                    if $do {
                        my $new;
                        if $flip {
                            $new = $node.children[1];
                        } else {
                            $new = $node.children[0];
                        }
                        $node.type = $new.type;
                        $node.content = $new.content;
                        $node.children = $new.children;
                        $hit = True;
                    }
                }
            }
        }

        # these two should come last to allow the former to match inverse ops

        # invert -> power
        if !$hit && ($node = $tree.find( :type<operation>, :content<inv> )) {
            $node.content = %ops<power>;
            $node.children[1] = $tree.new(:type<value>, :content(-1));
            $hit = True;
        }

        # neg -> mul
        elsif $node = $tree.find( :type<operation>, :content<neg> ) {
            $node.content = %ops<mul>;
            $node.children[1] = $tree.new(:type<value>, :content(-1));
            $hit = True;
        }
    }

    self.fold;
}

# need sink_ops and float_ops (transform via down/up)
# expand is sink
# does .float need a param like .poly? .float=.poly? .poly=.isolate?
# .poly = {.sink; .arrange;? .float;}
method expand () {
    my $tree = $!tree;

    my $hit = True;
    while $hit {
        $hit = False;

        for $tree.find_all(:children(
            {:type<value>,},
            {:type<value>,}
        )) -> $node {
            $node.content = +$node;
            $node.type = 'value';
            $node.children = ();
            $hit = True;
        }

        for $tree.find_all(:type<operation>) -> $node {
            my $op = $node.content;
            next unless $op.arity == 2;
            next unless my $func = $op.function;
            next unless my $down = $func.down;
            my $i;
            if $node.match: :children( *,
                {
                    :type<value>,
                    :content( {$_ == $_.Int and $_ > 0} )
                }
            ) {
                $i = 0;
            } elsif $func.commute &&
                $node.match: :children(
                    {
                        :type<value>,
                        :content( {$_ == $_.Int and $_ > 0} )
                    }, *
            ) {
                $i = 1;
            }

            if defined $i {
                my $child = $node.children[$i];
                my $ident = $func.identity;
                my $rep = $node.children[1-$i].content - $ident;
                next unless $rep-- > 0;
                my $val = $child.content;
                my $tmpl = $tree.new: :type<operation>, :content($down),
                    :children($child, $child.clone);
                my $new = $tmpl.clone;

                while $rep-- {
                    my $old = $new;
                    $new = $tmpl.clone;
                    $new.children[1] = $old;
                }

                $node.content = $new.content;
                $node.children = $new.children;
                $hit = True;
                last;
            }

            # distribution
            my $content = $down.name | $down.function.inverse.name;
            if $node.children[0].match: :type<operation>, :$content
                { $i = 0 }
            elsif $func.commute === True &&
                $node.children[1].match: :type<operation>, :$content
                { $i = 1 }
            if defined $i {
                my $child = $node.children[$i];
                $child.children .= map: {
                    my $new = $node.clone;
                    $new.children[$i] = $_;
                    $new;
                };
                $node.content = $child.content;
                $node.children = $child.children;
                $hit = True;
                last;
            }
        }

        # btw this tree/while/hit/find thing is looking familiar...wrap?
    }

    self;
}

our ($det_template, $quad_template_det, $quad_template_nodet);
method isolate_quadratic ($var, $a, $b, $c, :$tree = $!tree) {
    $det_template //= Math::Symbolic.new('b^2-4*a*c');

    my $det = $det_template.clone();
    $det.evaluate(:$a, :$b, :$c).fold;
    my $detval = $det.tree.type eq 'value' ?? +$det !! Any;

    my $new = $detval.defined && $detval == 0 ??
        ($quad_template_nodet //=
            Math::Symbolic.new('x = -b / 2*a')).clone !!
        ($quad_template_det //=
            Math::Symbolic.new('x = (-b ± √det) / (2*a)')).clone;

    $new.evaluate: :$a, :$b, :$c, :$det,
         :x(Math::Symbolic::Tree.new-sym: $var);

    $tree.set: $new.tree;

    self;
}

proto method isolate (|) {*}

multi method isolate (Str:D $var) {
    my $tree = $!tree;

    my @paths = $tree.find_all: :type<symbol>, :content($var), :path;
    if @paths > 1 {
        my %coeffs = self.poly($var, :coef);
        die "Error: cannot isolate $var in '{self}': " ~
            'the polynomial must have only one variable term, or be degree 0, 1, or 2'
            unless %coeffs.keys.grep(* ne 0) <= 1 || %coeffs.keys.all == 0|1|2;

        if %coeffs{1 & 2} :exists {
            %coeffs<0> //= Math::Symbolic::Tree.new-val: 0;
            self.isolate_quadratic($var, |%coeffs{2...0});
        } else {
            # removes extraneous x^0 before re-calling isolate for a single instance of $var
            for $tree.find_all: :type<operation>, :content<power>, :children(
                { :type<symbol>, :content($var) },
                { :type<value>, :content(0) }
            ) {
                $_.set: :type<value>, :content(1), :children();
            }

            self.isolate: $var;
        }
    } elsif !@paths {
        die "Error: symbol '$var' not found in relation '$tree'";
    } else {
        self.isolate: :path(@paths[0]);
    }

    self.simplify;
}

multi method isolate (:@path) {
    my $tree = $!tree;

    die 'Error: can only isolate variables in relations'
        unless $tree.type eq 'relation';

    my $i = @path.shift;
    $tree.children .= reverse if $i != 0;
    my $work = $tree.children[0];

    my $complete = !$work.children;
    while defined($i = @path.shift) {
        my $next = $work.children[$i];
        die 'Error: encountered non-operation parent node'
            unless $work.type eq 'operation';

        my $op = $work.content;
        my $func = $op.function;
        my $invop = $func.inverse;
        die "Error: inversion of '$op' is NYI" unless $invop;
        my $new;

        if $i != 0 {
            my $commute = $func.commute;
            if $commute {
                if $commute eq 'inverse' {
                    $next = $work;
                    $next.children .= reverse;
                    $next.children[0] = $tree.new(
                        :type<operation>, :content($func.invert-via),
                        :children($next.children[0])
                    );
                    @path.unshift: 0, 0;
                    $new = False;
                } else {
                    $work.children .= reverse;
                }
            } else {
                die "Error: reversing '$op' is NYI";
            }
        }

        if !defined $new {
            die "Error: inversion of '$op' is NYI" if $op.arity > 2;
            my @children = $tree.children[1];

            @children.push: $work.children[1] if $op.arity > 1;

            $new = Math::Symbolic::Tree.new(
                type => 'operation',
                content => $invop,
                :@children
            );
        }

        $tree.children[0] = $next;
        $tree.children[1] = $new if $new;

        $work = $next;
    }

    self;
}

# TODO this whole thing is done much more concisely by grammar actions...learn about those
method !convert_parse ($parse, $part = '') {
    my $is_array = !($parse ~~ Match);
    my $str = $parse.Str;
    $str = (@$parse».Str).join: ', ' if $is_array;
    my @branches = $parse;
    @branches = @$parse unless $parse ~~ Match;
    my @results;
    for @branches -> $branch {
        for $branch.hash.keys {
            my $result = self!convert_parse($branch.hash{$_}, $_);
            if $result {
                if $result.type {
                    @results.push: $result;
                } elsif $result.children {
                    @results.push: $result.children.list;
                }
            }
        }
    }

    my @children;
    @children = @results if @results;

    my ($type, $content);
    if $part eq 'equation' {
        $type = 'relation';
        $content = '=';
    } elsif $part eq 'constant' {
        $type = 'value';
        $content = +$str;
    } elsif $part eq 'variable' {
        $type = 'symbol';
        $content = $str;
    } elsif $part ~~ /^infix_chain_/ {
        $type = 'operation';
        my @ops = $parse<op>.list;
        my $node;
        while @ops {
            my $op = @ops.shift;
            my $term = @children.shift;
            my $children;
            if $node {
                $children = [$node, $term];
            } else {
                $children = [$term, @children.shift];
            }
            my $op_obj = %syn<infix>{$op};
            my $syn = %syn_syn<infix>{$op};
            $children .= reverse if $syn.reverse;
            $node = Math::Symbolic::Tree.new(
                :type<operation>,
                :content($op_obj),
                :$children
            );
        }
        return $node;
    } elsif $part eq 'circumfix_operation' {
        my $key = "$parse[0]$parse[1]";
        my $op = %syn<circumfix>{$key};
        if $op.function {
            $type = 'operation';
            $content = $op;
        }
    } elsif $part eq 'prefix_operation' {
        my $key = $parse<prefix_operator>.Str;
        my $op = %syn<prefix>{$key};
        if $op.function {
            $type = 'operation';
            $content = $op;
        }
    } elsif $part eq 'postfix_operation_chain' {
        my @keys = $parse<postfix_operator>.list».Str;
        my $node;
        while @keys {
            my $op = %syn<postfix>{@keys.shift};
            $node = Math::Symbolic::Tree.new-op: $op, $node // @children.shift
                if $op.function;
        }
        return $node;
    }

    my $node;
    if !($type || $content) && @children == 1 {
        $node = @children[0];
    } elsif $type || $content || @children {
        $node = Math::Symbolic::Tree.new(:$type, :$content, :@children);
    }
    return $node;
}

method !dump_parse ($tree, $level = 0, $anno is copy = '') {
    $anno = "$anno: " if $anno;
    my $is_array = !($tree ~~ Match);
    my $str = $tree.Str;
    $str = (@$tree».Str).join: ', ' if $is_array;
    say(' ' x 4*$level ~ $anno ~ $str);
    my @matches = $tree;
    @matches = @$tree unless $tree ~~ Match;
    for @matches -> $match {
        for $match.hash.keys {
            self!dump_parse($match.hash{$_}, $level+1, $_);
        }
    }
}

method dump_tree ($tree? is copy, $level = 0) {
    $tree //= $!tree;
    say( ' ' x 4*$level ~ $tree.type ~ ': ' ~ $tree.content);
    for $tree.children.list {
       self.dump_tree($_, $level+1);
    }
}

method perl () {
    my $str = self.HOW.name(self);
    $str ~= '.new(' ~ self.Str.perl ~ ')' if defined self;
    $str;
}

method gist () { self.Str }



#`[[[

              Inverse
                 ↔
     Negative         Positive
+----------------+----------------+
|                |                |
|          √     |        ^       |
|         √      |       ^ ^      |
|    √   √       |      ^   ^     | 3
|     √ √        |                |
|      √         |                |
|                |                |
+----------------+----------------+     ↑ Up
|                |                |
|       ÷        |     ×     ×    |
|                |      ×× ××     |
|    ÷÷÷÷÷÷÷     |        ×       | 2
|                |      ×× ××     |
|       ÷        |     ×     ×    |
|                |                |
+----------------+----------------+     ↓ Down
|                |                |
|                |        +       |
|                |        +       |
|    -------     |    +++++++++   | 1
|                |        +       |
|                |        +       |
|                |                |
+----------------+----------------+

An op distributes over either op 1 level down from it.

Positive ops associate and commute, except level 3.

Level 1 has identity 0, while the rest have identity 1.

Each subsequent positive op is a repetition of the op directly below it. However, no obvious similar relation seems to exist between the negative ops.

A positive op with an integer second argument is equal to the op 1 level down iterated that many times on its own identity value, given the original first arg as the second.

An op with a negative second arg is equal to the negative op 1 level down with it's first arg set to its own identity value, and the second arg set to the original op with a negd (positive) second arg.

]]]



