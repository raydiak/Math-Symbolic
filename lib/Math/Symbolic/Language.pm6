unit class Math::Symbolic::Language;

# should be instances, singletons are bad
# think about dependency injection
# ::Language (ops/syntax)
    # basically a collection of ::Operations and APIs thereto
    # if all syn and ops are just ::Operations, do we need a whole namespace for ::Language?
    # probably, so as to facilitate looking them up in the various ways needed, if nothing else
    # think about the ways in which both tree and grammar use these
    # also think about future internal and external use cases, API-wise

# might prefer to turn this into an abstract base class for languages
#`[[[ maybe reduce an entire language to a single table w/columns like: 
name/function
sequence of static and dynamic parts
    not sure how best to represent this yet
    this one list will serve the purposes of arity, type, reverse, parts, and key
        though many of those may still exist as ro attrs/methods (like key is now), but won't need to be directly specified
precedence and language are an interesting question: in an ideal world, they wouldn't be specified here at all,
    instead a precedence level would be an unordered set of its syntaxes, and a language would be an ordered set of precedence groups
    this is interesting, because things like -fixness position, arity, and rtl vs ltr should be consistent across a whole precedence group anyway
    so really, the sequence structure is a property of the precedence group, only the choice of characters for the static parts in that sequence varies per op
    btw rtl vs ltr isn't addressed here yet either...just another flag? associativity too

currently:
    has $.name;
    has $.arity;
    has $.type; # prefix, postfix, infix, circumfix
    has $.precedence;
    has Bool $.reverse = False;
    has @.parts;
    has $.key = @!parts.join: '';
    has $.language;
]]]

use Math::Symbolic::Operation;

sub Op (|args) { Math::Symbolic::Operation.new(|args) }

my @operations = BEGIN (
    Op(
        :name<add>,
        :function{
            :eval( &infix:<+> ),
            :up<mul>,
            :inverse<sub>,
            :invert-via<neg>,
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
        :name<sub>,
        :function{
            :eval( &infix:<-> ),
            :inverse<add>,
            :invert-via<neg>,
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
        :name<mul>,
        :function{
            :eval( &infix:<*> ),
            :up<power>,
            :down<add>,
            :inverse<div>,
            :invert-via<inv>,
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
        :name<div>,
        :function{
            :eval( &infix:</> ),
            :inverse<mul>,
            :invert-via<inv>,
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
            :down<mul>,
            :inverse<root>,
            :invert-via<inv>,
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
            :invert-via<inv>,
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
        :name<fact>,
        :syntax{
            :type<postfix>,
            :parts< ! >
        }
    ),
    Op(
        :name<abs>,
        :function{
            :eval( &abs )
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
        :name<neg>,
        :function{
            :eval( &prefix:<-> ),
            :inverse<neg>
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
        :name<inv>,
        :function{
            :eval( * ** -1 ),
            :inverse<inv>
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
        :name<addsub>,
        :function{
            :identity(0),
            :variants(<add sub>)
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
    Op(
        :name<sin>,
        :function{
            :eval( &sin ),
            :inverse<asin>
        },
    ),
    Op(
        :name<asin>,
        :function{
            :eval( &asin ),
            :inverse<sin>
        },
    ),
    Op(
        :name<cos>,
        :function{
            :eval( &cos ),
            :inverse<acos>
        },
    ),
    Op(
        :name<acos>,
        :function{
            :eval( &acos ),
            :inverse<cos>
        },
    ),
    Op(
        :name<tan>,
        :function{
            :eval( &tan ),
            :inverse<atan>
        },
    ),
    Op(
        :name<atan>,
        :function{
            :eval( &atan ),
            :inverse<tan>
        },
    ),
);

our @.operations := @operations;

my %by_name = @operations.grep({.name}).map: {.name => $_};
our %.by_name := %by_name;

for @operations {
    my $func = $_.function;
    next unless $func;

    for <inverse invert-via up down variants> -> $prop {
        for $func."$prop"() <-> $val {
            next unless $val && $val ~~ Str;
            my $op = %by_name{$val};
            die "Cannot find '$val' operation" unless $op;
            $val = $op;
        }
    }

    if !(my $comm := $func.commute) && $func.invert-via && (my $inv = $func.inverse) {
        $comm = 'inverse' if $inv.function.commute === True;
    }

    next unless $func.arity == 1;
    my $name = $_.name;
    my @syn := $_.syntaxes;
    @syn.push( Math::Symbolic::Syntax.new:
        :$name,
        :arity(1),
        :type<circumfix>,
        :parts( "$name\(", ')' )
    );
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

