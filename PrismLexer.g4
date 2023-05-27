lexer grammar PrismLexer
    ;

options
{
    superClass = PrismLexerBase;
}
// Variable declarations
KEYWORD_LET         : 'let';
KEYWORD_VAR         : 'var';

// Function declarations
KEYWORD_DYN         : 'dyn';
KEYWORD_ANY         : 'any';
KEYWORD_EXT         : 'ext';
KEYWORD_FN          : 'fn';

// Control flow statements
KEYWORD_IF          : 'if';
KEYWORD_ELSE        : 'else';
KEYWORD_FOR         : 'for';
KEYWORD_FOREACH     : 'foreach';
KEYWORD_IN          : 'in';
KEYWORD_WHILE       : 'while';
KEYWORD_DO          : 'do';
KEYWORD_BREAK       : 'break';
KEYWORD_CONTINUE    : 'continue';
KEYWORD_RETURN      : 'return';
KEYWORD_THROW       : 'throw';
KEYWORD_TRY         : 'try';
KEYWORD_CATCH       : 'catch';
KEYWORD_FINALLY     : 'finally';
KEYWORD_SWITCH      : 'switch';
KEYWORD_CASE        : 'case';
KEYWORD_DEFAULT     : 'default';
KEYWORD_ASSERT      : 'assert';
KEYWORD_YIELD       : 'yield';
KEYWORD_INSTANCEOF  : 'instanceof';

// Other keywords
KEYWORD_IS          : 'is';
KEYWORD_NEW         : 'new';
KEYWORD_OBJECT      : 'object';
KEYWORD_NAMESPACE   : 'namespace';
KEYWORD_USING       : 'using';
KEYWORD_PARALLEL    : 'parallel';
KEYWORD_PRIVATE     : 'priv';
KEYWORD_PUBLIC      : 'pub';
KEYWORD_PROTECTED   : 'prot';
KEYWORD_CLASS       : 'class';
KEYWORD_STRUCT      : 'struct';
KEYWORD_INTERFACE   : 'interface';
KEYWORD_ENUM        : 'enum';
KEYWORD_TRAIT       : 'trait';
KEYWORD_IMPL        : 'impl';
KEYWORD_MODULE      : 'module';
KEYWORD_TYPE        : 'type';
KEYWORD_AS          : 'as';
KEYWORD_SIZEOF      : 'sizeof';
KEYWORD_ALIGNOF     : 'alignof';
KEYWORD_TYPEOF      : 'typeof';
KEYWORD_ASYNC       : 'async';
KEYWORD_AWAIT       : 'await';


KEYWORD_INT         : 'Int';
KEYWORD_UINT        : 'UInt';
KEYWORD_BYTE        : 'Byte';
KEYWORD_FLOAT       : 'Float';
KEYWORD_DOUBLE      : 'Double';
KEYWORD_CHAR        : 'Char';
KEYWORD_BOOL        : 'Bool';
KEYWORD_STRING      : 'String';
KEYWORD_VOID        : 'Void';
KEYWORD_ANYTYPE     : 'Any';

// Visibility keywords
KEYWORD_INTERNAL    : 'internal';
KEYWORD_VIRTUAL     : 'virtual';
KEYWORD_THIS        : 'this';
KEYWORD_SUPER       : 'super';
KEYWORD_STATIC      : 'static';
KEYWORD_ABSTRACT    : 'abstract';
KEYWORD_MACRO       : 'macro';
KEYWORD_OVERRIDE    : 'override';
KEYWORD_EXTERNAL    : 'externalise';

// Additional keywords
KEYWORD_UNION            : 'union';
KEYWORD_STATIC_LIFETIME  : '\'static';
KEYWORD_MUT              : 'mut';

// Macro-related keywords
KEYWORD_MACRO_RULES : 'macro_rules';

// Rule itself allows any identifier, but keywords have been matched before
IDENTIFIER : XID_Start XID_Continue* | '_' XID_Continue+;

fragment XID_Start       : [\p{L}\p{Nl}] | UNICODE_Other_ID_Start;
fragment XID_Continue    : XID_Start | [\p{Mn}\p{Mc}\p{Nd}\p{Pc}] | UNICODE_Other_ID_Continue;

fragment UNICODE_Other_ID_Start : UNICODE_L | UNICODE_Nl;
fragment UNICODE_Other_ID_Continue : XID_Start | [\p{Mn}\p{Mc}\p{Nd}\p{Pc}] | UNICODE_Other_ID_Start;

fragment UNICODE_L  : '\u1885'..'\u1886' | '\u2118' | '\u212e' | '\u309b'..'\u309c';
fragment UNICODE_Nl : '\u00b7' | '\u0387' | '\u1369'..'\u1371' | '\u19da';

RAW_IDENTIFIER         : 'r#' IDENTIFIER;

LINE_COMMENT    : '//' ~[\r\n]* -> channel(HIDDEN);
BLOCK_COMMENT   : '/*' .*? '*/' -> channel(HIDDEN);
WHITESPACE      : [ \t\r\n\u000C]+ -> channel(HIDDEN);
NEWLINE         : '\r'? '\n' -> channel(HIDDEN);

CHARACTER_LITERAL       : '\'' ( ~['\\\r\n] | QUOTE_ESCAPE | UNICODE_ESCAPE | ASCII_ESCAPE ) '\'';
STRING_LITERAL          : '"' ( ~['"\\\r\n] | QUOTE_ESCAPE | UNICODE_ESCAPE | ASCII_ESCAPE | ESCAPED_NEWLINE )* '"';
PARAMETRIC_STRING       : '$' STRING_LITERAL;
BYTE_LITERAL            : 'b\'' ( . | QUOTE_ESCAPE | BYTE_ESCAPE ) '\'';
BYTE_STRING_LITERAL     : 'b"' ( . | QUOTE_ESCAPE | BYTE_ESCAPE | ESCAPED_NEWLINE )*? '"';

fragment BYTE_ESCAPE              : '\\' [0-9] [0-9]? [0-9]?;
fragment BYTE_STRING_ESCAPE       : '\\' ['"\\];
fragment QUOTE_ESCAPE             : '\\' ['"\\];
fragment UNICODE_ESCAPE           : '\\u{' HEX_DIGIT HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? HEX_DIGIT? '}';
fragment ASCII_ESCAPE             : '\\' [0-7] [0-7]? [0-7]?;
fragment ESCAPED_NEWLINE          : '\\' '\r'? '\n';
fragment HEX_DIGIT                : [0-9a-fA-F];
fragment DEC_DIGIT                : [0-9];

INTEGER_LITERAL     : ( DEC_LITERAL | BIN_LITERAL | OCT_LITERAL | HEX_LITERAL );
fragment DEC_LITERAL      : DEC_DIGIT+;
fragment BIN_LITERAL      : '0b' [01]+;
fragment OCT_LITERAL      : '0o' [0-7]+;
fragment HEX_LITERAL      : '0x' [0-9a-fA-F]+;

LONG_LITERAL             : INTEGER_LITERAL LONG_SUFFIX?;
fragment LONG_SUFFIX     : 'l' | 'L';

FLOAT_LITERAL         : DEC_LITERAL '.' DEC_LITERAL EXPONENT? FLOAT_SUFFIX?;
fragment EXPONENT     : [eE] [+-]? DEC_LITERAL;
fragment FLOAT_SUFFIX : 'f' | 'F';

DOUBLE_LITERAL         : DEC_LITERAL '.' DEC_LITERAL EXPONENT? DOUBLE_SUFFIX?;
fragment DOUBLE_SUFFIX : 'd' | 'D';

BOOLEAN_LITERAL     : 'true' | 'false';
NULL_LITERAL        : 'null';

// line terminators
LINE_TERMINATOR     : '\r'? '\n';



LIFE_TIME   : '\'' IDENTIFIER;

// Operators
PLUS            : '+';
PLUSPLUS        : '++';
MINUS           : '-';
MINUSMINUS      : '--';
STAR            : '*';
STARSTAR        : '**';
SLASH           : '/';
PERCENT         : '%';
CARET           : '^';
BANG            : '!';
TILDE           : '~';
AND             : '&';
OR              : '|';
ANDAND          : '&&';
OROR            : '||';
EQ              : '=';
EQEQ            : '==';
NE              : '!=';
LTEQ            : '<=';
GTEQ            : '>=';
LT              : '<';
GT              : '>';
PLUSEQ          : '+=';
MINUSEQ         : '-=';
STAREQ          : '*=';
SLASHEQ         : '/=';
PERCENTEQ       : '%=';
CARETEQ         : '^=';
ANDEQ           : '&=';
OREQ            : '|=';
SHLEQ           : '<<=';
SHREQ           : '>>=';
AT              : '@';
DOT             : '.';
DOTDOT          : '..';
DOTDOTDOT       : '...';
DOTDOTEQ        : '..=';
QUESTION        : '?';
QUESTION_QUESTION : '??';
COLON           : ':';
COLONCOLON      : '::';
SEMI            : ';';
COMMA           : ',';
RA_ARROW        : '->';
FAT_ARROW       : '=>';
POUND           : '#';
DOLLAR          : '$';
UNDERSCORE      : '_';

// Delimiters
LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';


