module Math::Symbolic::Constants;

# is this just another node type?
#enum Representation is export <Tree Matrix>;

enum Expression is export <Relation Operation Symbol Value>;
enum Language is export <Math Perl>;

subset NumIn of Numeric is export;
subset NumOut of Real:D is export;

