class Math::Symbolic::Tree is rw;

has $.type;
has $.content;
has @.children;

# note only looks for first result
method contains (:$type, :$content) {
    die 'At least one of :type or :content is required' unless defined $type || defined $content;
    
    my $hit = True;
    $hit &&= ($type eq $.type) if defined $type;
    $hit &&= ($content eq $.content) if $hit && defined $content;
    
    return self if $hit;

    for @.children {
        return $_ if .contains(:$type, :$content);
    }

    return False;
}

method match (*%s) {
    CATCH {die "Error in match: $_\nMatch: {%s.perl}"};

    for %s.keys {
        next if $_ eq 'children';
        my $criteria = %s{$_};
        my $value = self."$_"();
        return False unless $value ~~ $criteria;
    }

    if %s<children> :exists {
        my @child_criteria = %s<children>;
        for ^@child_criteria -> $child_i {
            my $child_criteria = @child_criteria[$child_i];
            #next unless $child ~~ Positional && $child.keys;
            next if $child_criteria ~~ Whatever;
            my %child_params = %$child_criteria;
            next unless %child_params;
            my $child = self.children[$child_i];
            return False unless $child && $child.match( |%child_params );
        }
    }

    return True;
}

method find (*%s) {
    return self if self.match(|%s);

    for @.children {
        my $result = $_.find(|%s);
        return $result if $result;
    }

    return;
}

method find_all (Bool :$path = False, *%s) {
    my @results;
    @results.push: $path ?? [] !! self if self.match(|%s);

    for @.children.kv -> $i, $child {
        next unless my @child_results = $child.find_all(:$path, |%s);
        @child_results».unshift: $i if $path;
        @results.push: @child_results;
    }

    return @results;
}

method chain (Bool :$ops = False) {
    my $type = self.type;

    die "Error: can only call .chain() on operations; this is a{
        ($type ~~ /^<[aeiou]>/ ?? 'n ' !! ' ') ~ $type
    }" unless $type eq 'operation';

    my @chain;
    @chain.push: self if $ops;
    for @.children {
        if $_.type eq $.type && $_.content eq $.content {
            @chain.push: $_.chain(:$ops);
        } else {
            @chain.push: $_ unless $ops;
        }
    }

    @chain;
}

method count () {
    [+] @.children».count, 1
}

proto method child(|) {*}

multi method child($i) {
    @.children[$i];
}

multi method child ($i, *@i) {
    @.children[$i].child(|@i);
}

proto method set (|) {*}

multi method set (Math::Symbolic::Tree $node, Bool :$type, Bool :$content, Bool :$children) {
    $.type = $node.type unless $type === False;
    $.content = $node.content unless $content === False;
    @.children = $node.children unless $children === False;

    self;
}

multi method set (*%props) {
    self."$_"() = %props{$_} for %props.keys;

    self;
}

method get () {
    %( :$.type, :$.content, :@.children );
}

method swap (Math::Symbolic::Tree $node) {
    my %tmp = self.clone.get;

    self.set: $node;
    $node.set: |%tmp;

    self;
}

method Str () {
    return '' unless defined self;

    given $.type {
        when 'operation' {
            my $op = $.content;
            my $assoc = $op.function.associative;
            my $syn = $op.syntax // die "Error: No syntax to stringify for '{$op.name}' operations";
            my @args = @.children».Str;
            my $prec = $syn.precedence;
            for ^@args -> $child_i {
                my $child = @.children[$child_i];
                if $child.type eq 'operation' {
                    my $this_op = $child.content;
                    my $this_syn = $this_op.syntax // die "Error: No syntax to stringify for '{$.content.name}' operations";
                    my $this_prec = $this_syn.precedence;
                    if defined($prec) && defined($this_prec) && (
                        $this_prec > $prec or
                        $child_i > 0 && ($this_prec == $prec) && (!$assoc or $this_op !=== $op)
                    ) or $syn.type eq none('infix', 'circumfix', $this_syn.type) {
                        # () here is a hard-coded hack and should be looked up as the first defined non-function circumfix
                        @args[$child_i] = '(' ~ @args[$child_i] ~ ')';
                    }
                }
            }
            return $syn.make_str(@args);
        }
        when 'relation' {
            return @.children».Str.join: $.content.Str;
        }
        when 'symbol' | 'value' {
            return $.content.Str;
        }
        default {
            die "Error: Can't stringify nodes of type '$.type'";
        }
    }
}

method Numeric () {
    self.type eq 'operation' ??
        (self.content.function.eval)(|@(self.children».Numeric)) !!
        +self.Str;
}

method clone () {
    self.new( :$.type, :$.content, :children(@.children».clone) );
}

method new-chain ($op, *@children is copy, *%args is copy) {
    die "Error: cannot create a chain with no children" unless @children;

    return @children[0] unless @children > 1;

    %args<type> = 'operation';
    %args<content> = $op;

    my $chain = self.new: |%args, :children(@children.shift, @children.shift);
    $chain = self.new: |%args, :children($chain, @children.shift) while @children;

    $chain;
}

method new-val ($val, *%args is copy) {
    %args<type> = 'value';
    %args<content> = $val;

    self.new: |%args;
}

method new-sym ($sym, *%args is copy) {
    %args<type> = 'symbol';
    %args<content> = $sym;

    self.new: |%args;
}

method new-op ($op, *@children, *%args is copy) {
    %args<type> = 'operation';
    %args<content> = $op;
    %args<children>.push: @children if @children;

    self.new: |%args;
}


