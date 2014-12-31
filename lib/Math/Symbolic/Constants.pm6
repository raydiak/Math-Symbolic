module Math::Symbolic::Constants;

our enum Representation is export <Tree Matrix>;
our enum Expression is export <Relation Operation Symbol Value>;
our enum Language is export <Math Perl>;

our subset NumIn of Numeric is export;
our subset NumOut of Real:D is export;

