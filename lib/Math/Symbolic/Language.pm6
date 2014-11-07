class Math::Symbolic::Language;

# ::Language (ops/syntax)
    # basically a collection of ::Operations and APIs thereto
    # if all syn and ops are just ::Operations, do we need a whole namespace for ::Language?
    # probably, so as to facilitate looking them up in the various ways needed, if nothing else
    # think about the ways in which both tree and grammar use these
    # also think about future internal and external use cases, API-wise

use Math::Symbolic::Operation;

sub Op (|args) { Math::Symbolic::Operation.new(|args) }

my @operations = (
    Op(
        :name<add>,
        :function{
            :eval( &infix:<+> ),
            :inverse<subtract>,
            :invert-via<negate>,
            :identity(0),
            :commute,
            :associative,
        },
        :syntax{
            :type<infix>,
            :precedence(3),
            :parts< + >
        }
    ),
    Op(
        :name<subtract>,
        :function{
            :eval( &infix:<-> ),
            :inverse<add>,
            :invert-via<negate>,
            :identity(0)
        },
        :syntax{
            :type<infix>,
            :precedence(3),
            :parts< - >
        }
    ),
    Op(
        :name<multiply>,
        :function{
            :eval( &infix:<*> ),
            :inverse<divide>,
            :invert-via<invert>,
            :identity(1),
            :commute,
            :associative
        },
        :syntax{
            :type<infix>,
            :precedence(2),
            :parts< * >
        }
    ),
    Op(
        :name<divide>,
        :function{
            :eval( &infix:</> ),
            :inverse<multiply>,
            :invert-via<invert>,
            :identity(1)
        },
        :syntax{
            :type<infix>,
            :precedence(2),
            :parts< / >
        }
    ),
    Op(
        :name<power>,
        :function{
            :eval( &infix:<**> ),
            :inverse<root>,
            :invert-via<invert>,
            :identity(1)
        },
        :syntax{
            :type<infix>,
            :precedence(1),
            :parts< ^ >
        },
    ),
    Op(
        :name<root>,
        :arity(2),
        :function{
            :eval( * ** (1/*) ),
            :inverse<power>,
            :invert-via<invert>,
            :identity(1)
        },
        :syntaxes(
            {
                :type<infix>,
                :precedence(1),
                :reverse,
                :parts< √ >
            },
            {
                :type<infix>,
                :precedence(1),
                :parts< ^/ >
            }
        )
    ),
    Op(
        :name<sqrt>,
        :arity(1),
        :function{
            :eval( * ** .5 ),
            :inverse<sqr>
        },
        :syntax{
            :type<prefix>,
            :parts< √ >
        }
    ),
    Op(
        :name<sqr>,
        :arity(1),
        :function{
            :eval( * ** 2 ),
            :inverse<sqrt>
        },
        :syntax{
            :type<postfix>,
            :parts< ² >
        }
    ),
    Op(
        :name<factorial>,
        :syntax{
            :type<postfix>,
            :parts< ! >
        }
    ),
    Op(
        :name<absolute>,
        :function{
            :eval( *.abs )
        },
        :syntax{
            :type<circumfix>,
            :parts< | | >
        }
    ),
    Op(
        :name<negate>,
        :function{
            :eval( &prefix:<-> ),
            :inverse<negate>
        },
        :syntax{
            :type<prefix>,
            :parts< - >
        }
    ),
    Op(
        :name<invert>,
        :function{
            :eval( * ** -1 ),
            :inverse<invert>
        },
        :syntax{
            :type<postfix>,
            :parts< ⁻¹ >
        }
    ),
    Op(
        :syntax{
            :type<circumfix>,
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

    if my $inv := $func.inverse {
        my $op = %by_name{$inv};
        $inv = $op if $op;
    }

    if my $inv_via := $func.invert-via {
        my $op = %by_name{$inv_via};
        die "Cannot find '$inv_via' operation" unless $op;
        $inv_via = $op;

        my $comm := $func.commute;
        if all(!$comm, $inv_via, $inv, $inv.function.commute === True) {
            $comm = 'inverse';
        }
    }
}

#my %by_syntax = build_by_syntax();
our %.by_syntax = build_by_syntax();
our %.syntax_by_syntax = build_by_syntax(:syntax);

sub build_by_syntax (Bool :$syntax = False) {
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
            $entry = $syntax ?? $syn !! $_;
        }
    }

    return %by_syntax;
}

