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
            my $child = @child_criteria[$child_i];
            #next unless $child ~~ Positional && $child.keys;
            next if $child ~~ Whatever;
            my %child_params = %$child;
            next unless %child_params;
            return False unless self.children[$child_i].match( |%child_params );
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

method find_all (*%s) {
    my @results;
    @results.push: self if self.match(|%s);

    for @.children {
        my @child_results = $_.find_all(|%s);
        @results.push: @child_results;
    }

    return @results;
}

method count () {
    [+] @.children».count, 1
}

method Str () {
    given $.type {
        when 'operation' {
            my $op = $.content;
            my $assoc = $op.function.associative;
            my $syn = $op.syntax // die "Error: No syntax to stringify for '{$op.name}' operations";
            my @args = @.children».Str;
            my $prec = $syn.precedence;
            if defined $prec {
                for ^@args -> $child_i {
                    my $child = @.children[$child_i];
                    if $child.type eq 'operation' {
                        my $this_op = $child.content;
                        my $this_syn = $this_op.syntax // die "Error: No syntax to stringify for '{$.content.name}' operations";
                        my $this_prec = $this_syn.precedence;
                        if defined($prec) && defined($this_prec) and (
                            $this_prec > $prec or
                            $child_i > 0 && ($this_prec == $prec) && (!$assoc or $this_op !=== $op)
                        ) {
                            # () here is a hard-coded hack and should be looked up as the first defined non-function circumfix
                            @args[$child_i] = '(' ~ @args[$child_i] ~ ')';
                        }
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


