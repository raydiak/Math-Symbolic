grammar Math::Symbolic::Grammar;

# should have planned precedence levels:
    # circumfix ()
    # postfix   !
    # prefix    - √
    # infix
        # root  √
            # I made this one up, as it's not really 'infix' in normal math notation
            # special case: operands are backwards from its inverse op ( ^ )
        # power ^
            # special case: right-to-left
        # scale * /
        # shift + -

use Math::Symbolic::Language;

my (@circ, @pre, @post, %in);
for Math::Symbolic::Language.operations.grep({.syntaxes}) -> $op {
    for $op.syntaxes -> $syn {
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

token equation { <expression> \= <expression> }
rule expression { \s* [<operation>|<term>] }

token sign { \+ | \- }
token value { <sign>? [ \.\d+ | \d+[\.\d*]? ] }
token constant { <value> [:i e <value>]? }

token variable { <alpha> <alnum>* }
rule term { \s* [<variable>|<constant>] }

token operation {
    <infix_operation_chain> |
    <circumfix_operation> |
    <postfix_operation> |
    <prefix_operation>
}

token infix_term {
    <circumfix_operation> |
    <postfix_operation> |
    <prefix_operation> |
    <term>
}

token postfix_term {
    <circumfix_operation> |
    # <postfix_operation> | # TODO BUG cannot chain postfix ops
            # recognize postfix chains, similar to infix...prefix too?
            # or a lookahead assertion, but this doesn't reduce the recursion as above would
    # <prefix_operation> | # TODO BUG postfix is hard-coded to take precedence over prefix for now
    <term>
}

token prefix_term {
    <circumfix_operation> |
    <postfix_operation> | # TODO BUG postfix is hard-coded to take precedence over prefix for now
    <prefix_operation> |
    <term>
}

# for now, we will have to use a hard-coded number of precedence levels
# until the problem with the following block can be resolved
rule infix_operation_chain { <infix_chain_a> | <infix_chain_b> | <infix_chain_c> }
rule infix_chain_a {<infix_term_a>[ $<op>=(@(%in{@prec[2]}».parts»[0])) <infix_term_a>]+}
rule infix_chain_b {<infix_term_b>[ $<op>=(@(%in{@prec[1]}».parts»[0])) <infix_term_b>]+}
rule infix_chain_c {<infix_term_c>[ $<op>=(@(%in{@prec[0]}».parts»[0])) <infix_term_c>]+}
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

token prefix_operation { <prefix_operator> <prefix_term> }
token prefix_operator { @(@pre».parts[0]) }

token postfix_operation { <postfix_term> <postfix_operator> }
token postfix_operator { @(@post».parts[0]) }

rule circumfix_operation { (<circumfix_open>) <expression> (<circumfix_close>) <?{ %circ{$0} eq $1 }> }
token circumfix_open { @(%circ.keys) }
token circumfix_close { @(%circ.values) }



