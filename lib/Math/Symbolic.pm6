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

has $.tree handles <Str Numeric count>;

method new (Str:D $in, *%args is copy) {
    # TODO this is by far the slowest part, over 2.4 seconds for 'x=(-b+(b^2-4*a*c)^.5)/(2*a)' on an A4-3305M
        # by comparison, it only takes .55 seconds to simplify, isolate 'c', and simplify again, once parsing is complete
    my $parse = Math::Symbolic::Grammar.parse($in);

    die 'Parse failure: invalid expression' unless $parse;

    #self.dump_parse $parse;
    %args<tree> = self!convert_parse($parse);
    my $obj = self.bless: |%args;
    #$obj.dump_tree;
    $obj.simplify();
    #$obj.dump_tree;

    $obj;
}

method clone () {
    self.bless: :tree(self.tree.clone);
}
method evaluate (*%vals) {
    for %vals.kv -> $var, $val {
        my $subtree = self.new(~$val).tree;
        for $!tree.find_all( :type<symbol>, :content($var) ) {
            $_.type = $subtree.type;
            $_.content = $subtree.content;
            $_.children = $subtree.children;
        }
    }

    self.simplify;
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

        if $node = $tree.find( :type<operation>, :content('add'|'subtract'), :children(
            *,
            {:type<operation>, :content<negate>}
        ) ) {
            $node.content = $node.content.function.inverse;
            $node.children[1] = $node.children[1].children[0];
            $hit = True;
        }

        elsif $node = $tree.find( :type<operation>, :content('power'|'root'), :children(
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

        # a/b/c -> a*c/b
        elsif $node = $tree.find( :type<operation>, :content<divide>, :children(
            {:type<operation>, :content<divide>},
            *
        ) ) {
            $node.content = %ops<divide>;
            my $d = $node.children[0].children[1];
            $node.children[0] = Math::Symbolic::Tree.new(:type<operation>, :content(%ops<multiply>), :children(
                $node.children[0].children[0],
                $node.children[1]
            ));
            $node.children[1] = $d;
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
                if !$hit && $func.arity == 1 {
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
                }

                # constant folding
                if !$hit && (my $eval = &($func.eval)) && $node.children.all.type eq 'value' {
                    $node.type = 'value';
                    $node.content = $eval( |@($node.children».content) );
                    $node.children = ();
                    $hit = True;
                }
            }
        }
    }

    self;
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
method isolate (Str:D $var) {
    my $tree = $!tree;

    die 'Error: can only isolate variables in relations'
        unless $tree.type eq 'relation';

    my @paths = find_all $tree: :type<symbol>, :content($var), :path;
    if @paths > 1 {
        self.expand;
        $tree.Str.say;
        die 'Error: isolating multiple instances of a variable is NYI';
    } elsif !@paths {
        die "Error: symbol '$var' not found in relation '$tree'";
    }

    my @path := @paths[0];

    my $i = @path.shift;
    $tree.children .= reverse if $i != 0;
    my $work = $tree.children[0];

    my $complete = !$work.children;
    until $complete {
        $i = @path.shift;
        my $next = $work.children[$i];
        if $work.type eq 'operation' {
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
                            :type<operation>, :content(%ops{$func.invert-via}),
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
        } else {
            die 'Error: encountered non-operation parent node';
        }

        $complete = True unless $work.children;
    }

    self.simplify();
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

