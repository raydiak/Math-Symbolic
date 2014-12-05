class MultiHash is rw {
    has %.hash{Any} handles <keys values kv pairs>;

    method elem (*@pairs, *%keyhash is copy) is rw {
        %keyhash{.key} = .value for @pairs;

        my $found_key;
        my %key;

        for %!hash.keys {
            if %$_ eqv %keyhash {
                %key := $_;
                $found_key = True;
                last;
            }
        }

        %key := %keyhash unless $found_key;

        %!hash{ $%key };
    }

    method matching (*%keyhash) {
        my @hits;

        for %!hash.kv -> $k, $v {
            my $hit = True;

            for %keyhash.kv -> $kt, $vt { # t is for test
                next if $k{$kt}:exists && $k{$kt} eqv $vt;

                $hit = False;
                last;
            }

            push @hits, $v if $hit;
        }

        return |@hits;
    }
}

