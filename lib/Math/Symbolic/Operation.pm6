class Math::Symbolic::Operation { ... };

class Math::Symbolic::Function {
    has $.name;
    has $.arity;
    has &.eval;

    # rw so it can be specified as a string but vivified to an object later
    has $.inverse is rw;
    has $.invert-via is rw;

    # rw so it can be pointed at its inverse if appropriate
    has $.commute is rw;

    has $.identity;
    has $.associative;

    has $.normal;

    has $.up is rw;
    has $.down is rw;

    has @.variants is rw;

    method Str () { $.name }

    multi method perl(::CLASS:D:) {
        my @attrs;
        for self.^attributes().grep: { .has_accessor } -> $attr {
            my $name := $attr.Str.substr(2);

            my $value = self."$name"();
            if $value ~~ Math::Symbolic::Operation && $value.defined && (my $op_name := $value.name) {
                $value = "Math::Symbolic::Language.by_name\{{$op_name.perl}}";
            } else {
                $value = $value.perl;
            }

            @attrs.push: "$name => $value";
        }
        self.HOW.name(self) ~ '.new(' ~  @attrs.join(', ') ~ ')';
    }
}

class Math::Symbolic::Syntax {
    has $.name;
    has $.arity;
    has $.type; # prefix, postfix, infix, circumfix
    has $.precedence;
    has Bool $.reverse = False;
    has @.parts;
    has $.key = @!parts.join: '';
    has $.language;

    method make_str (@args) {
        my $str;

        given $.type {
            when 'infix' {
                $str = [@args];
                $str .= reverse if $.reverse;
                $str .= join: @.parts[0];
            }
            when 'prefix' {
                $str = @.parts[0] ~ @args[0];
            }
            when 'postfix' {
                $str = @args[0] ~ @.parts[0];
            }
            when 'circumfix' {
                $str = @.parts.join: @args[0];
            }
            default {
                die "Error: cannot generate a string for syntax of type '$.type'";
            }
        }

        return $str;
    }
}

class Math::Symbolic::Operation {
    has $.name;
    has Int $.arity;
    has Math::Symbolic::Function $.function;
    has Math::Symbolic::Syntax @.syntaxes;

    method Str () { $.name }

    method syntax (Int $i? is copy, :$language --> Math::Symbolic::Syntax) is rw {
        my $use-lang = defined $language;

        unless defined $i {
            my &check = $use-lang ??
                { defined $_.value.language && $_.value.language eq $language } !!
                { !defined $_.value.language };
            $i = @!syntaxes.pairs.first(&check).key;
        }

        die "Error: could not find {$use-lang ?? "$language " !! ''}syntax for $!name"
            unless defined $i;

        @!syntaxes[$i] // die "Error: could not find {
            $use-lang ?? "$language " !! ''}syntax for $!name";
    }

    submethod BUILD (:%function, :%syntax, :@syntaxes, :$!name, :$arity is copy) {
        unless defined $arity {
            my $syn = @syntaxes[0] // %syntax;
            if $syn && $syn<type> eq 'infix' {
                $arity = 2;
            } else {
                $arity = 1;
            }
        }

        $!arity = $arity;

        $!function = Math::Symbolic::Function.new(
            |%(%function // ()),
            :$!name,
            :$!arity
        ) if $!name || %function;

        if @syntaxes {
            for @syntaxes {
                @!syntaxes.push: Math::Symbolic::Syntax.new(|%$_, :$!name, :$!arity);
            }
        } elsif %syntax {
            @!syntaxes[0] = Math::Symbolic::Syntax.new(|%syntax, :$!name, :$!arity);
        }
    }
}



