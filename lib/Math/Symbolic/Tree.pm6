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
            return False unless $child.match( |%child_params );
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

method count () {
    [+] @.children».count, 1
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
    +self.Str;
    CATCH { die "Cannot convert '{self}' to a number: a single constant is required; eliminate any remaining variables with .evaluate() first." }
}

method clone () {
    self.new( :$.type, :$.content, :children(@.children».clone) );
}


