grammar Math::Symbolic::Grammar;

#use Grammar::Debugger;

# should have planned precedence levels:
    # circumfix ()
    # postfix   !
    # prefix    - √
    # infix
        # root  √
            # special case: operands are backwards from its inverse op ( ^ )
        # power ^
            # special case: right-to-left
        # scale * /
        # shift + -

use Math::Symbolic::Language;

my (@circ, @pre, @post, %in);
for Math::Symbolic::Language.operations.grep({.syntaxes}) -> $op {
    for $op.syntaxes -> $syn {
        next if defined $syn.language;
        given $syn.type {
            when 'circumfix' {
                @circ.push: $syn;
            }
            when 'prefix' {
                @pre.push: $syn;
            }
            when 'postfix' {
                @post.push: $syn;
            }
            when 'infix' {
                # would .push autovivify here?
                %in{$syn.precedence}[*-0] = $syn;
            }
        }
    }
}
my @prec = %in.keys.sort: +*;
my %circ = @circ».parts;

my %syn = %(
    in => $%(%in{*}.map({@$_}).map: {.key => $_}),
    circ => $%(@circ.map: {.key => $_}),
    pre => $%(@pre.map: {.key => $_}),
    post => $%(@post.map: {.key => $_})
);

token TOP { <equation> | <expression> }

token equation { <before <-[ = ]>+ \= > <expression> \= <expression> }

# putting term first here would speed things up, think about negate ramifications
# also other places like infix_term etc
# this ties in with the - prefix op vs negative constant question
rule expression { \s* [<operation>|<term>] }

token sign { \+ | \- }
token value { <sign>? [ \.\d+ | \d+[\.\d*]? ] }
token constant { <value> [:i e <value>]? }

token variable { <alpha> <alnum>* }
rule term { \s* [<variable>|<constant>] }

token operation {
    <circumfix_operation> |
    <postfix_operation_chain> |
    <prefix_operation> |
    <infix_operation_chain>
}

token infix_term {
    <circumfix_operation> |
    <postfix_operation_chain> |
    <prefix_operation> |
    <term>
}

token postfix_term {
    <circumfix_operation> |
    # <prefix_operation> | # TODO BUG postfix is hard-coded to take precedence over prefix for now
    <term>
}

token prefix_term {
    <circumfix_operation> |
    <postfix_operation_chain> | # TODO BUG postfix is hard-coded to take precedence over prefix for now
    <prefix_operation> |
    <infix_chain_c> |
    <term>
}

# for now, we will have to use a hard-coded number of precedence levels
# until the problem with the following block can be resolved
my $in_ops_a = '[' ~ @(%in{@prec[2]}».parts»[0]).map("'" ~ * ~ "'").join('|') ~ ']';
my $in_ops_b = '[' ~ @(%in{@prec[1]}».parts»[0]).map("'" ~ * ~ "'").join('|') ~ ']';
my $in_ops_c = '[' ~ @(%in{@prec[0]}».parts»[0]).map("'" ~ * ~ "'").join('|') ~ ']';
rule infix_operation_chain { <infix_chain_a> | <infix_chain_b> | <infix_chain_c> }
rule infix_chain_a {<before .+? <$in_ops_a>><infix_term_a>[ $<op>=<$in_ops_a> <infix_term_a>]+}
rule infix_chain_b {<before .+? <$in_ops_b>><infix_term_b>[ $<op>=<$in_ops_b> <infix_term_b>]+}
rule infix_chain_c {<before .+? <$in_ops_c>><infix_term_c>[ $<op>=<$in_ops_c> <infix_term_c>]+}
token infix_term_a { <infix_chain_b> | <infix_term_b> }
token infix_term_b { <infix_chain_c> | <infix_term_c> }
token infix_term_c { <infix_term> }

#`[[[
my (@in_terms, @in_chains);
for ^@prec {
    my ($term, $chain);
    if $_ == 0 {
        $term = token { <Math::infix_term> };
    } else {
        $term = token { <{ @in_chains[*-1] }> | <{ @in_terms[*-1] }> };
    }
    @in_terms.push: $term;
    @in_chains.push: rule {<$term>[ (<@(%in{@prec[$_]}».parts»[0])>) <$term>]+};
}
rule infix_operation_chain { <@( reverse @in_chains )> }
]]]

rule prefix_operation { <.ws> <prefix_operator><prefix_term> }
my $pre_ops = '[' ~ @(@pre».parts»[0]).map("'" ~ * ~ "'").join('|') ~ ']';
token prefix_operator { <$pre_ops> }

rule postfix_operation_chain {
    <.ws> <before .* <postfix_operator> >
    <postfix_term><postfix_operator>+
}
my $post_ops = '[' ~ @(@post».parts»[0]).map("'" ~ * ~ "'").join('|') ~ ']';
token postfix_operator { <$post_ops> }

rule circumfix_operation { (<circumfix_open>) <expression> (<circumfix_close>) <?{ %circ{$0} eq $1 }> }
my $circ_open = '[' ~ @(%circ.keys).map("'" ~ * ~ "'").join('|') ~ ']';
my $circ_close = '[' ~ @(%circ.values).map("'" ~ * ~ "'").join('|') ~ ']';
token circumfix_open { <$circ_open> }
token circumfix_close { <$circ_close> }

