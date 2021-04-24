unit module Math::Symbolic::Constants;

# is this just another node type?
#enum Representation is export <Tree Matrix>;

enum Node is export <Relation Operation Symbol Value>;
enum Language is export <Math Raku>;

subset NumIn of Numeric is export;
subset NumOut of Real:D is export;

