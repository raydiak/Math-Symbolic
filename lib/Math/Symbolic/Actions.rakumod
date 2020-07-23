unit class Math::Symbolic::Actions;

use Math::Symbolic::Tree;
use Math::Symbolic::Language;
my %ops_by_syn = Math::Symbolic::Language.by_syntax;

method TOP ($/) {
    make $<equation>.made // $<expression>.made;
}

method equation ($/) {
    make Math::Symbolic::Tree.new:
        type => 'relation',
        content => '=',
        :children($<expression>[0].made, $<expression>[1].made);
}

method expression ($/) {
    make $<operation>.made // $<term>.made;
}

method term ($/) {
    $<variable> ??
        make Math::Symbolic::Tree.new-sym($<variable>.Str) !!
        make Math::Symbolic::Tree.new-val($<constant>.Str);
}

method operation ($/) {
    make
        $<postfix_operation_chain>.made //
        $<circumfix_operation>.made //
        $<prefix_operation>.made //
        $<infix_operation_chain>.made;
}

method circumfix_operation ($/) {
    my $op = %ops_by_syn<circumfix>{"$0$1"};
    my $made;
    
    $op && $op.function ??
        make Math::Symbolic::Tree.new-op:
            %ops_by_syn<circumfix>{"$0$1"},
            $<expression>.made !!
        make $<expression>.made;
}

method postfix_term ($/) {
    make $<circumfix_operation>.made //
        $<term>.made;
}

method postfix_operation_chain ($/) {
    my @ops = @<postfix_operator>».made;
    my $tree = $<postfix_term>.made;
    $tree = Math::Symbolic::Tree.new-op(%ops_by_syn<postfix>{@ops.shift}, $tree)
        while @ops;
    make $tree;
}

method postfix_operator ($/) {
    make $/.Str;
}

method prefix_operation ($/) {
    make Math::Symbolic::Tree.new-op:
        %ops_by_syn<prefix>{$<prefix_operator>.made},
        $<prefix_term>.made;
}

method prefix_operator ($/) {
    make $/.Str;
}

method prefix_term ($/) {
    make
        $<postfix_operation_chain>.made //
        $<circumfix_operation>.made //
        $<prefix_operation>.made //
        $<infix_chain_c>.made //
        $<term>.made;
}

method infix_operation_chain ($/) {
    make
        $<infix_chain_a>.made //
        $<infix_chain_b>.made //
        $<infix_chain_c>.made;
}

method infix_chain_a ($/) {
    my @terms = @<infix_term_a>».made;
    my @ops = @<op>;
    my $tree = @terms.shift;
    $tree = Math::Symbolic::Tree.new-op:
        %ops_by_syn<infix>{@ops.shift.Str},
        $tree,
        @terms.shift
        while @ops;
    make $tree;
}

method infix_chain_b ($/) {
    my @terms = @<infix_term_b>».made;
    my @ops = @<op>;
    my $tree = @terms.shift;
    $tree = Math::Symbolic::Tree.new-op:
        %ops_by_syn<infix>{@ops.shift.Str},
        $tree,
        @terms.shift
        while @ops;
    make $tree;
}

method infix_chain_c ($/) {
    my @terms = @<infix_term_c>».made;
    my @ops = @<op>;
    my $tree = @terms.shift;
    $tree = Math::Symbolic::Tree.new-op:
        %ops_by_syn<infix>{@ops.shift.Str},
        $tree,
        @terms.shift
        while @ops;
    make $tree;
}

method infix_term_a ($/) {
    make $<infix_chain_b>.made // $<infix_term_b>.made;
}

method infix_term_b ($/) {
    make $<infix_chain_c>.made // $<infix_term_c>.made;
}

method infix_term_c ($/) {
    make $<infix_term>.made;
}

method infix_term ($/) {
    make
        $<postfix_operation_chain>.made //
        $<circumfix_operation>.made //
        $<prefix_operation>.made //
        $<term>.made;
}


