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
            :up<multiply>,
            :inverse<subtract>,
            :invert-via<negate>,
            :identity(0),
            :commute,
            :associative,
            :normal
        },
        :syntaxes(
            {
                :type<infix>,
                :precedence(3),
                :parts< + >
            },
            {
                :language<perl6>,
                :type<infix>,
                :parts< + >
            },
        ),
    ),
    Op(
        :name<subtract>,
        :function{
            :eval( &infix:<-> ),
            :inverse<add>,
            :invert-via<negate>,
            :identity(0)
        },
        :syntaxes(
            {
                :type<infix>,
                :precedence(3),
                :parts< - >
            },
            {
                :language<perl6>,
                :type<infix>,
                :parts< - >
            },
        ),
    ),
    Op(
        :name<multiply>,
        :function{
            :eval( &infix:<*> ),
            :up<power>,
            :down<add>,
            :inverse<divide>,
            :invert-via<invert>,
            :identity(1),
            :commute,
            :associative,
            :normal
        },
        :syntaxes(
            {
                :type<infix>,
                :precedence(2),
                :parts< * >
            },
            {
                :language<perl6>,
                :type<infix>,
                :parts< * >
            },
        ),
    ),
    Op(
        :name<divide>,
        :function{
            :eval( &infix:</> ),
            :inverse<multiply>,
            :invert-via<invert>,
            :identity(1)
        },
        :syntaxes(
            {
                :type<infix>,
                :precedence(2),
                :parts< / >
            },
            {
                :language<perl6>,
                :type<infix>,
                :parts< / >
            },
        ),
    ),
    Op(
        :name<power>,
        :function{
            :eval( &infix:<**> ),
            :down<multiply>,
            :inverse<root>,
            :invert-via<invert>,
            :identity(1),
            :normal
        },
        :syntaxes(
            {
                :type<infix>,
                :precedence(1),
                :parts< ^ >
            },
            {
                :language<perl6>,
                :type<infix>,
                :parts< ** >
            },
        ),
    ),
    Op(
        :name<root>,
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
            },
            {
                :language<perl6>,
                :type<infix>,
                :parts< **1/ >
            },
        )
    ),
    Op(
        :name<sqrt>,
        :function{
            :eval( * ** .5 ),
            :inverse<sqr>
        },
        :syntaxes(
            {
                :type<prefix>,
                :parts< √ >
            },
            {
                :language<perl6>,
                :type<postfix>,
                :parts< **.5 >
            },
        ),
    ),
    Op(
        :name<sqr>,
        :function{

            # TODO code in here receives null for the argument(s?) when precompiled
            # all evals in this file are broken when installed normally via panda :P
            # except those which point to external routines like &infix:<+>
            :eval( * ** 2 ),

            :inverse<sqrt>
        },
        :syntaxes(
            {
                :type<postfix>,
                :parts< ² >
            },
            {
                :language<perl6>,
                :type<postfix>,
                :parts< **2 >
            },
        ),
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
        :syntaxes(
            {
                :type<circumfix>,
                :parts< | | >
            },
            {
                :language<perl6>,
                :type<postfix>,
                :parts< .abs >
            },
        ),
    ),
    Op(
        :name<negate>,
        :function{
            :eval( &prefix:<-> ),
            :inverse<negate>
        },
        :syntaxes(
            {
                :type<prefix>,
                :parts< - >
            },
            {
                :language<perl6>,
                :type<prefix>,
                :parts< - >
            },
        ),
    ),
    Op(
        :name<invert>,
        :function{
            :eval( * ** -1 ),
            :inverse<invert>
        },
        :syntaxes(
            {
                :type<postfix>,
                :parts< ⁻¹ >
            },
            {
                :language<perl6>,
                :type<postfix>,
                :parts< **-1 >
            },
        ),
    ),
    Op(
        :name<addsubtract>,
        :function{
            :identity(0),
        },
        :syntax{
            :type<infix>,
            :precedence(3),
            :parts< ± >
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

    for <inverse invert-via up down> -> $prop {
        if my $val := $func."$prop"() {
            next unless $val ~~ Str;
            my $op = %by_name{$val};
            die "Cannot find '$val' operation" unless $op;
            $val = $op;
        }
    }

    if !(my $comm := $func.commute) && $func.invert-via && (my $inv = $func.inverse) {
        $comm = 'inverse' if $inv.function.commute === True;
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
            next if defined $syn.language;
            my %syn_type := (%by_syntax{$type} //= Hash.new);
            my $key = $syn.key;
            my $entry := %syn_type{$key};
            die "Error: Syntax conflict for $type $key between {$entry.name} and {$_.name}" if $entry;
            $entry = $syntax ?? $syn !! $_;
        }
    }

    return %by_syntax;
}

