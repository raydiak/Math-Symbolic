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

    has $.up is rw;
    has $.down is rw;
    
    method Str () { $.name }
}

class Math::Symbolic::Syntax {
    has $.name;
    has $.arity;
    has $.type; # prefix, postfix, infix, circumfix
    has $.precedence;
    has Bool $.reverse = False;
    has @.parts;
    has $.key = @!parts.join: '';

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

    method syntax (Int:D $i = 0 --> Math::Symbolic::Syntax) is rw {
        @!syntaxes[$i]
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



