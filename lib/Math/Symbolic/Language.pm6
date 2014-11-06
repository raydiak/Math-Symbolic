class Math::Symbolic::Language;

# ::Language (ops/syntax)
    # basically a collection of ::Operations and APIs thereto
    # if all syn and ops are just ::Operations, do we need a whole namespace for ::Language?
    # probably, so as to facilitate looking them up in the various ways needed, if nothing else
    # think about the ways in which both tree and grammar use these
    # also think about future internal and external use cases, API-wise

use Math::Symbolic::Tree;
use Math::Symbolic::Operation;

sub Op (|args) { Math::Symbolic::Operation.new(|args) }

my @operations = (
    Op(
        name => 'add',
        :function{
            :eval( * + * ),
            inverse => 'subtract',
            :identity(0),
            :commute,
            :associative,
        },
        :syntax{
            type => 'infix',
            precedence => 3,
            :parts< + >
        }
    ),
    Op(
        name => 'subtract',
        :function{
            :eval( * - * ),
            inverse => 'add',
            :identity(0),
            commute => sub ($tree) {
                Math::Symbolic::Tree.new(
                    :type<operation>,
                    :content(Math::Symbolic::Language.by_name<add>),
                    children => (
                        Math::Symbolic::Tree.new(
                            :type<operation>,
                            :content(Math::Symbolic::Language.by_name<negate>),
                            children => ($tree.children[1])
                        ),
                        $tree.children[0]
                    )
                )
            }
        },
        :syntax{
            type => 'infix',
            precedence => 3,
            :parts< - >
        }
    ),
    Op(
        name => 'multiply',
        :function{
            :eval( * * * ),
            inverse => 'divide',
            :identity(1),
            :commute,
            :associative
        },
        :syntax{
            type => 'infix',
            precedence => 2,
            :parts< * >
        }
    ),
    Op(
        name => 'divide',
        :function{
            :eval( * / * ),
            inverse => 'multiply',
            :identity(1),
            commute => sub ($tree) {
                Math::Symbolic::Tree.new(
                    :type<operation>,
                    :content(Math::Symbolic::Language.by_name<multiply>),
                    children => (
                        Math::Symbolic::Tree.new(
                            :type<operation>,
                            :content(Math::Symbolic::Language.by_name<power>),
                            children => (
                                $tree.children[1],
                                Math::Symbolic::Tree.new(
                                    :type<value>,
                                    :content(-1)
                                )
                            )
                        ),
                        $tree.children[0]
                    )
                )
            }
        },
        :syntax{
            type => 'infix',
            precedence => 2,
            :parts< / >
        }
    ),
    Op(
        name => 'power',
        :function{
            :eval( * ** * ),
            inverse => 'root',
            :identity(1)
        },
        :syntax{
            type => 'infix',
            precedence => 1,
            :parts< ^ >
        },
    ),
    Op(
        name => 'root',
        arity => 2,
        :function{
            :eval( * ** (1/*) ),
            inverse => 'power',
            :identity(1)
        },
        :syntaxes(
            {
                type => 'infix',
                precedence  => 1,
                :reverse,
                :parts< √ >
            },
            {
                type => 'infix',
                precedence  => 1,
                :parts< ^/ >
            }
        )
    ),
    Op(
        name => 'sqrt',
        arity => 1,
        :function{
            :eval( * ** .5 ),
            inverse => 'sqr'
        },
        :syntax{
            type => 'prefix',
            :parts< √ >
        }
    ),
    Op(
        name => 'sqr',
        arity => 1,
        :function{
            :eval( * ** 2 ),
            inverse => 'sqrt'
        },
        :syntax{
            type => 'postfix',
            :parts< ² >
        }
    ),
    Op(
        name => 'factorial',
        :syntax{
            type => 'postfix',
            :parts< ! >
        }
    ),
    Op(
        name => 'absolute',
        :function{
            :eval( *.abs )
        },
        :syntax{
            type => 'circumfix',
            :parts< | | >
        }
    ),
    Op(
        name => 'negate',
        :function{
            :eval( -* ),
            inverse => 'negate'
        },
        :syntax{
            type => 'prefix',
            :parts< - >
        }
    ),
    Op(
        :syntax{
            type => 'circumfix',
            :parts< ( ) >
        }
    ),
);

our @.operations := @operations;

my %by_name = @operations.grep({.name}).map: {.name => $_};
our %.by_name := %by_name;

for @operations {
    my $func = $_.function;
    next unless $func;
    my $inv := $func.inverse;
    next unless $inv ~~ Str;
    my $op = %by_name{$inv};
    $inv = $op if $op;
}

#my %by_syntax = build_by_syntax();
our %.by_syntax = build_by_syntax();

sub build_by_syntax () {
    my %by_syntax;

    for @operations {
        my @syn = $_.syntaxes;
        for @syn.keys -> $syn_i {
            my $syn = @syn[$syn_i];
            next unless $syn && (my $type = $syn.type);
            my %syn_type := (%by_syntax{$type} //= Hash.new);
            my $key = $syn.key;
            my $entry := %syn_type{$key};
            die "Error: Syntax conflict for $type $key between {$entry.name} and {$_.name}" if $entry;
            $entry = $_ but $syn_i.Int;
        }
    }

    return %by_syntax;
}

