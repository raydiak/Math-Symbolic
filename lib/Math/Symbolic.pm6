class Math::Symbolic;

# so this should be a class which represents the entire problem
    # encapsulates both Tree and Language so they can be used together
        # implements features which require both, like (de)serialization and isolating a variable in a relation
    # this is the main public interface

# other manipulations to support:
    # arrange as a polynomial in terms of <var>
    # factor polynomials in simplify and isolate

use Math::Symbolic::Tree;
use Math::Symbolic::Grammar;
use Math::Symbolic::Language;

my %ops := Math::Symbolic::Language.by_name;
my %syn := Math::Symbolic::Language.by_syntax;
my %syn_syn := Math::Symbolic::Language.syntax_by_syntax;

has $.tree handles <Numeric count>;

method Str () {
    self.simplify;
    $.tree.Str;
}

method new (Str:D $in, *%args is copy) {

    # TODO this is by far the slowest part, over 2.4 seconds for 'x=(-b+(b^2-4*a*c)^.5)/(2*a)' on an A4-3305M
    my $parse = Math::Symbolic::Grammar.parse($in);

    die 'Parse failure: invalid expression' unless $parse;

    #self.dump_parse $parse;
    %args<tree> = self!convert_parse($parse);
    my $obj = self.bless: |%args;
    #$obj.dump_tree;
    #$obj.simplify();
    #$obj.normalize();
    #$obj.dump_tree;

    $obj;
}

method clone () {
    self.bless: :tree(self.tree.clone);
}

method evaluate (*%vals is copy) {
    for %vals.values {
        $_ = self.new(~$_).tree;
    }
    my $hit = True;
    while $hit {
        $hit = False;
        for %vals.kv -> $var, $val {
            for $!tree.find_all( :type<symbol>, :content($var) ) {
                set $_: $val;
                $hit = True;
            }
        }
    }

    #self.simplify;
    #self.normalize;
    self;
}

# this whole routine is very dumb and could be made smarter with a limited search through a (lazy?) network of possible manipulations
# TODO for now as a simpler stopgap, we could at least do more property-based generic manipulations, like the identity value stuff
    # but avoid adding more code in the ops like how commute is done for division and subtraction now, it feels messy and poorly encapsulated
    # iow properties are good for Operations (as long as they're optional with sane defaults), but code in those properties is bad, because the point of the Operation class is to be a simple declarative way to express the language, so minimizing complexity in the public API of ::Operation is central to its intended purpose
    # TODO BUG speaking of minimizing complexity in ::Operation, please convert the .function/.syntax/.syntaxes/BUILD mess to Roles soon
# this is also highly inefficient
# there is a lot more that could be done with constants here esp. 0 like *0, 0*, 0/, ^0, ^/0...need special cases on ops or something
# TODO really need to split this into 2
    # normalize, for making ready for manipulation
    # simplify, for maximal reduction, probably for display to the user
method simplify () {
    my $tree = $!tree;
    my $hit = True;
    while $hit {
        $hit = False;
        my $node;

        #`[[[
        if $node = $tree.find( :type<operation>, :content('add'|'subtract'), :children(
            *,
            {:type<operation>, :content<negate>}
        ) ) {
            $node.content = $node.content.function.inverse;
            $node.children[1] = $node.children[1].children[0];
            $hit = True;
        }
        ]]]

        # x^-n = 1/x^n
        if $node = $tree.find( :type<operation>, :content('power'|'root'), :children(
            *,
            {:type<operation>, :content<negate>}
        ) ) {
            $node.children[1] = $node.children[1].children[0];
            $node.children[1] = $node.clone;
            $node.content = %ops<divide>;
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
            $node.content = %ops<divide>;
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

        # negate
        elsif $node = $tree.find( :type<operation>, :content('multiply'|'divide'), :children(
            *,
            {:type<value>, :content(-1)}
        ) ) {
            $node.content = %ops<negate>;
            $node.children[1] :delete;
            $hit = True;
        }

        # negate
        elsif $node = $tree.find( :type<operation>, :content<multiply>, :children(
            {:type<value>, :content(-1)},
            *
        ) ) {
            $node.content = %ops<negate>;
            $node.children[0] = $node.children[1]:delete;
            $hit = True;
        }

        # invert -> division
        elsif $node = $tree.find( :type<operation>, :content<invert> ) {
            $node.content = %ops<divide>;
            $node.children[1] = $node.children[0];
            $node.children[0] = $tree.new(:type<value>, :content(1));
            $hit = True;
        }

        # a/b/c -> a/(b*c)
        elsif $node = $tree.find( :type<operation>, :content<divide>, :children(
            {:type<operation>, :content<divide>},
            *
        ) ) {
            $node.content = %ops<divide>;
            $node.children[1] = $tree.new(:type<operation>, :content(%ops<multiply>), :children(
                $node.children[0].children[1],
                $node.children[1]
            ));
            $node.children[0] = $node.children[0].children[0];
            $hit = True;
        }

        # a/(b/c) -> a*c/b
        elsif $node = $tree.find( :type<operation>, :content<divide>, :children(
            *,
            {:type<operation>, :content<divide>}
        ) ) {
            $node.content = %ops<divide>;
            $node.children[0] = Math::Symbolic::Tree.new(:type<operation>, :content(%ops<multiply>), :children(
                $node.children[0],
                $node.children[1].children[1]
            ));
            $node.children[1] = $node.children[1].children[0];
            $hit = True;
        }

        # a*(b/c) -> a*b/c
        elsif $node = $tree.find( :type<operation>, :content<multiply>, :children(
            *,
            {:type<operation>, :content<divide>}
        ) ) {
            $node.content = %ops<divide>;
            $node.children[0] = Math::Symbolic::Tree.new(:type<operation>, :content(%ops<multiply>), :children(
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
                                self.dump_tree($node);
                                $hit = True;
                            }
                        }
                    } elsif $func.arity == 2 {
                        my $inv_via = $func.invert-via;
                        if $inv_via {
                            for 1, 0 -> $i {
                                my $child := $node.children[$i];

                                if $child.type eq 'operation' &&
                                    $child.content === $inv_via {
                                    $node.content = $func.inverse;
                                    $child = $child.children[0];

                                    $hit = True;
                                    last;
                                }

                                last if !$func.commute
                            }
                        }
                    }
                }
            }
        }

        # invert -> division - we put this last, so that inversion is preserved for matching against inv-via
        elsif $node = $tree.find( :type<operation>, :content<invert> ) {
            $node.content = %ops<divide>;
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

method poly ($var) {
    self.normalize;
    self.expand;

    my $tree = $!tree;

    my @paths = $tree.find_all: :type<symbol>, :content($var), :path
        unless @paths;
    die "Error: variable '$var' not found in '$tree'" unless @paths;

    my $work = $tree;

    if $tree.type eq 'relation' {
        my %side_var := @paths»[0].Bag;
        my $side = +(%side_var{0} < %side_var{1});
        $tree.children .= reverse if $side;

        $work = $tree.children[0];
        my $opp := $tree.children[1];

        if %side_var.keys == 1 {
            @paths = $tree.find_all: :type<symbol>, :content($var), :path;
            my @path;
            my $i = 0;
            my $max_elems = @paths».elems.min;
            while $max_elems > $i && @paths»[$i].uniq == 1 {
                @path[$i] = @paths[0][$i];
                $i++;
            }

            if @path > 1 {
                self.isolate: :@path;
                $work = $tree.children[0];
            }
        }
        
        my %zero = :type<value>, :content(0);
        unless $opp.match: |%zero {
            @($work.children) = $work.clone, $tree.new(
                :type<operation>, :content(%ops<multiply>),
                :children[ $tree.new(:type<value>,:content(-1)), $opp ]
            );
            $work.type = 'operation';
            $work.content = %ops<add>;

            $opp = $tree.new: |%zero;

            self.expand;

            # TODO reduce & report: if this isn't here, rakudo gives
            # "Internal error: zeroed target thread ID in work pass"
            print '';
        }
    }

    # assumptions at this point:
    # there is more than one instance of var
    # var is contained within only + * and ^ operations from the root
        # hope var isn't on the right of any ^
    die "Error: cannot arrange '$work' as a polynomial" unless $work.type eq 'operation';

    self.condense($var, $work);

    my %coeffs;
    my @parts = $work.match(:type<operation>, :content<add>) ??
        $work.chain !!
        $work;
    my %var = :type<symbol>, :content($var);
    for @parts {
        when $_.match: |%var {
            %coeffs{1} = $tree.new-val: 1;
        }
        when $_.match: :type<operation>, :content<multiply>,
            :children(%var, *) {
            %coeffs{1} = $_.children[1];
        }
        when $_.match: :type<operation>, :content<power>,
            :children(%var, {:type<value>,}) {
            %coeffs{$_.children[1]} = $tree.new-val: 1;
        }
        #`[[[ TODO BUG
        when $_.match( :type<operation>, :content<multiply> ) &&
            $_.children[0].match(
                :type<operation>,
                :content<power>,
                :children(
                    %var,
                    # {:type<symbol>, :content($var)},
                    # { :type<value>, }
                    %( :type<value> )
                )
            )
        ]]]
        when $_.match( :type<operation>, :content<multiply> ) &&
            $_.children[0].match( :type<operation>, :content<power> ) &&
            $_.children[0].children[0].match(|%var) &&
            $_.children[0].children[1].type eq 'value'
        {
            %coeffs{$_.children[0].children[1]} = $_.children[1];
        }
        default {
            my $z := %coeffs{0};
            if $z {
                $z = $tree.new-op(%ops<add>, $z, $_);
            } else {
                $z = $_;
            }
        }
    }

    for %coeffs.keys.sort.reverse {
        say "$_\t%coeffs{$_}";
    }
    exit;

    #@paths = $tree.find_all: :type<symbol>, :content($var), :path;

    #`[[[
    my $levels = {
        when 'add' { 3 }
        when 'multiply' { 2 }
        # when 'power' { 1 } in this case there could only be 1 var, or var^var
        die "Cannot arrange '$work' as a polynomial";
    }($work.content.Str);
    #die "levels: $levels";

    my $one = $tree.new: :type<value>, :content(1);
    my %by_power;
    my @parts = ($levels == 3 ?? $work.chain !! $work);
    for @parts {
        my $type = $_.type;
        if $type eq 'symbol' {
            if $_.content eq $var {
                %by_power.push: 1 => $one.clone;
            } else {
                %by_power.push: 0 => $_;
            }
        } elsif $type eq 'value' {
            %by_power.push: 0 => $_;
        } elsif $type eq 'operation' {
            my $content = $_.content;
            if $content eq 'multiply' {
                my $power = 0;
                my @subparts;
                for $_.chain -> $sub {
                    my $subtype = $sub.type;
                    my $subcontent = $sub.content;
                    if $subtype eq 'symbol' && $subcontent eq $var {
                        $power++;
                    } elsif $subtype eq 'operation' && $subcontent eq 'power' && $sub.match:
                        :children({:type<symbol>, :content($var)}, {:type<value>,}) {
                        $power += $sub.children[1].content;
                    } else {
                        @subparts.push: $sub;
                    }
                }
                my $subresult = @subparts ?? @subparts.shift !! $one.clone;
                $subresult = $tree.new: :type<operation>, :content(%ops<multiply>),
                    :children($subresult, @subparts.shift) while @subparts;
                %by_power.push: $power => $subresult;
            } elsif $content eq 'power' && $_.match:
                :children({:type<symbol>, :content($var)}, {:type<value>,}) {
                %by_power.push: $_.children[1].content => $one.clone;
            } else {
                %by_power.push: 0 => $_;
            }
        }
    }

    for %by_power.keys -> $k {
        my $v := %by_power{$k};

        next unless $v ~~ Positional;

        if $v.elems == 1 {
            $v = $v[0];
            next;
        }

        my $new = $v.shift;
        $new = $tree.new: :type<operation>, :content(%ops<add>),
            :children($new, $v.shift) while @$v;

        $v = $new;
        
        say "$k: $v";
    };
    ]]]

    #@paths = $tree.find_all: :type<symbol>, :content($var), :path;
    #$tree.child(|@$_).Str.say for @paths;

    self;
}

my class MultiHash is rw {
    has %.hash{Any} handles <keys values kv pairs>;

    method elem (*@pairs, *%keyhash is copy) is rw {
        %keyhash{.key} = .value for @pairs;

        my $found_key;
        my %key;

        for %!hash.keys {
            if %$_ eqv %keyhash {
                %key := $_;
                $found_key = True;
                last;
            }
        }

        %key := %keyhash unless $found_key;

        %!hash{ $%key };
    }

    method matching (*%keyhash) {
        my @hits;

        for %!hash.kv -> $k, $v {
            my $hit = True;

            for %keyhash.kv -> $kt, $vt { # t is for test
                next if $k{$kt}:exists && $k{$kt} eqv $vt;

                $hit = False;
                last;
            }

            push @hits, $v if $hit;
        }

        return |@hits;
    }
}

method condense ($var?, $tree = $!tree) {
    my $type = $tree.type;

    if $type eq 'relation' {
        self.condense: $var, $_ for $tree.children;
    }

    return self if $type ne 'operation';

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
                    #%subvar_count{$_.children[0].content} += $_.children[1].content;
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

                $elem.push: $tree.new-chain: $up, |@subparts if @subparts;

                # TODO reduce & report: if this isn't here, rakudo gives
                # "Internal error: zeroed target thread ID in work pass"
                print '';
            } elsif $upup && $content eq $upup && $_.match:
                :children({:type<symbol>,}, {:type<value>,}) {
                #%var_count{$_.children[0].content}{$_.children[1].content} += 1;
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
            #$elem[0] = @$elem ??
            #    ($up.function.eval)($elem[0], @$vals.shift) !!
            #    @$vals.shift;
            my $co = @$vals.shift;
            unshift @subparts: $tree.new-val: $co if $co != $up.function.identity;
            $elem.push: $tree.new-chain: $up, @subparts if @subparts;
            $elem.push: @$vals;
        }

        $vars := $newvars;
    }

    # then convert back into a ::Tree and $tree.set
    my @new_parts;
    for $vars.kv -> $keyhash, $vals {
        my @subparts;

        if @$vals && $vals[0] ~~ Numeric {
            my $v = $vals.shift;
            @subparts.push: $tree.new-val: $v
                if $v != $up.function.identity;
        }

        for $keyhash.keys.sort -> $kk {
            my $kv := $keyhash{$kk};
            if (defined $var and $kk eq $var) || $kv != 1 {
                my $new;
                if $upup {
                    $new = $tree.new-op: $upup, $tree.new-sym($kk), $tree.new-val($kv);
                } elsif $kv == $kv.Int && $kv > 0 {
                    $new = $tree.new-sym($kk) for 1..$kv;
                } else {
                    # really, checking for this is sorta inane...how would we get here?
                    die "Error: this transformation would require the Knuth up arrow (NYI)";
                }
                if $kk eq $var {
                    @subparts.unshift: $new;
                } else {
                    @subparts.push: $new;
                }
            } else {
                @subparts.push: $tree.new-sym($kk);
            }
        }

        @subparts.push: $tree.new-chain: $op, @$vals if @$vals;

        @new_parts.push: $tree.new-chain: $up, |@subparts;
    }

    $tree.set: $tree.new-chain: $op, |@new_parts;

    self;
}

# roughly opposite of simplify
# expands shorthand ops (eg a² -> a^2)
# inverts non-commutative ops to commutable forms (eg a-b -> a+-1*b)
# folds constants
method normalize () {
    my $tree = $!tree;
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

        # invert -> power
        elsif $node = $tree.find( :type<operation>, :content<invert> ) {
            $node.content = %ops<power>;
            $node.children[1] = $tree.new(:type<value>, :content(-1));
            $hit = True;
        }

        # negate -> multiply
        elsif $node = $tree.find( :type<operation>, :content<negate> ) {
            $node.content = %ops<multiply>;
            $node.children[1] = $tree.new(:type<value>, :content(-1));
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

        # do some more stuff in here
        # btw this tree/while/hit/find thing is looking familiar...wrap?
    }

    self;
}

# this routine is pretty dumb too, and could be enhanced with a similar network approach
    # simply peeling back all the ops by applying the inverse to the other side won't always work
    # we also don't currently have any way of solving for a variable which appears more than once
# each node represents a possible arrangement of the expression
# each edge represents a possible manipulation of the expression
# both isolate & simplify involve walking branches outwards
# nodes/arrangements/representations:
    # need a hash function to track and not re-walk them
    # need a 'complexity' metric...just count all the nodes in the expression? we even already have a count method...what about tie-breaking?
# cognition-inspired ideas:
    # some sort of "distance" metric between arrangements, to more efficiently prioritize search branches
        # all truths are connected, but only a small number of them are closer to the answer we seek than the question we start with
    # pick some other points known to be in the "right area", and path-find to them first, or outwards from them, to have established territory to work from
        # I'm sure Sun Tzu must have said something about awareness of your surroundings
        # this is a geometric way to talk about implementing foreknown optimization hints
    # prioritize solving more difficult/restrictive parts first...iow plan the tricky parts of the path first, then fill in the boring parts in between
        # don't waste time bikeshedding until you can know the requirements of the bikeshed
    # mix and balance these approaches
proto method isolate (|) {*}

multi method isolate (Str:D $var) {
    my $tree = $!tree;

    my @paths = $tree.find_all: :type<symbol>, :content($var), :path;
    if @paths > 1 {
        self.poly($var);
        $tree.Str.say;
        die 'Error: isolating multiple instances of a variable is NYI';
    } elsif !@paths {
        die "Error: symbol '$var' not found in relation '$tree'";
    } else {
        self.isolate: :path(@paths[0]);
    }

    self;
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

    #self.simplify();
    #self.normalize();
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
    } elsif $part eq 'postfix_operation' {
        my $key = $parse<postfix_operator>.Str;
        my $op = %syn<postfix>{$key};
        if $op.function {
            $type = 'operation';
            $content = $op;
        }
    }

    my $node;
    #if !($part || $type || $content) && @children == 1 {
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

An op with a negative second arg is equal to the negative op 1 level down with it's first op set to its own identity value, and the second arg set to the original op with a negated (positive) second arg.

]]]



