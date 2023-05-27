parser grammar PrismParser
    ;

options {
    tokenVocab = PrismLexer;
    superClass = PrismParserBase;
}

// 1. program
primscript
    : program EOF
    ;

// 2. program
program
    : bodyStmt
    ;

// 3. program body
bodyStmt
    : (statement)*
    ;

// 4. statement
statement
    : blockStmt
    | variableStmt
    | namespaceStmt
    | usingStmt eos
    | externStmt eos
    | emptyStmt
    | objectDeclaration
    | expressionStmt
    | ifStatement
    | iterationStmt
    | continueStmt
    | breakStmt
    | yieldStmt
    | returnStmt
    | throwStmt
    | tryStmt
    | assertStmt
    | switchStmt
    | functionStmt
    ;

// 5. block statement
blockStmt
    : '{' (statement)* '}'
    ;


// 6. <-- Variable Declaration -->
variableStmt
    : KEYWORD_LET variableDeclarationList eos
    ;

variableDeclarationList
    : (variableModifier)*? variableDeclaration (',' variableDeclaration)*
    ;

variableDeclaration
    : variableLeft variableAssignment
    ;

variableLeft
    : assignable (':' '('? type ')'?)?
    ;

variableAssignment
    : ('=' expressionSequence)?
    ;

// 7. <-- Namespace Declaration -->
namespaceStmt
    : KEYWORD_NAMESPACE simplePath (statement | eos)
    ;

// 8. <-- Using Declaration -->
usingStmt
    : KEYWORD_USING usingFrom
    ;

usingFrom
    : usingFromNamespace
    | usingFromType
    ;

usingFromNamespace
    : KEYWORD_NAMESPACE namespacePath
    ;

namespacePath
    : simplePath
    | simplePath '.' '*'
    ;

usingFromType
    : simplePath
    ;

// 9. <-- Extern Declaration -->
externStmt
    : KEYWORD_EXTERNAL externDeclarationList eos
    ;

externDeclarationList
    : externDeclaration (',' externDeclaration)*
    ;

externDeclaration
    : identifier
    ;

// 10. <-- Empty Statement -->
emptyStmt
    : SEMI
    ;

// 11. <-- Object Declaration -->
objectDeclaration
    : jsonObjectDeclaration
    | classDeclaration
    | interfaceDeclaration
    | enumDeclaration
    | typeAliasDeclaration
    ;

constructor
    : '(' formalParameterList? ')'
    | '&' '(' identifier ')' // reference constructor
    ;

extension
    : (':' type (',' type)*)?
    ;

jsonObjectDeclaration
    : (objectModifier)*? KEYWORD_OBJECT identifier '->'? '{' jsonObjectMember? (',' jsonObjectMember)* '}'
    ;

jsonObjectMember
    : identifier ':' expressionSequence
    | identifier '->' '{' jsonObjectMember '}'
    | identifier '->' expressionSequence
    ;

classDeclaration
    : (classModifier)*? KEYWORD_CLASS identifier typeParameters? constructor? extension? classTail
    ;

classTail
    : '{' (classElement)* '}'
    ;

classElement
    : (constructorDefinition | methodDefinition | fieldDefinition)
    | emptyStmt
    ;

methodDefinition
    : functionStmt
    ;

fieldDefinition
    : variableStmt
    ;

constructorDefinition
    : accessModifier? identifier '(' formalParameterList ')' blockStmt
    ;

interfaceDeclaration
    : (objectModifier)*? KEYWORD_INTERFACE identifier typeParameters? extension? interfaceTail
    ;

interfaceTail
    : '{' (interfaceElement)*? '}'
    ;

interfaceElement
    : interfaceField
    | interfaceMethod
    ;

interfaceField
    : accessModifier? variableLeft eos
    | KEYWORD_DEFAULT accessModifier? variableLeft '->' singleExpression
    ;

interfaceMethod
    : accessModifier? functionLeft eos
    | KEYWORD_DEFAULT accessModifier? functionLeft '->'? functionBody
    ;

enumDeclaration
    : (objectModifier)*? KEYWORD_ENUM identifier constructor? enumTrail
    ;

enumTrail
    : (enumElement)*?
    ;

enumElement
    : enumValue
    ;

enumValue
    : identifier (arguementsList)
    ;

typeAliasDeclaration
    : (objectModifier)*? KEYWORD_TYPE identifier typeParameters eos
    ;

// 12. <-- If Statement -->
ifStatement
    : KEYWORD_IF '(' expressionSequence ')' statement elseStatement?
    ;

elseStatement
    : KEYWORD_ELSE ifStatement? statement
    ;

// 13. <-- Iteration Statement -->
iterationStmt
    : KEYWORD_DO statement KEYWORD_WHILE '(' expressionSequence ')' eos
    | KEYWORD_WHILE '(' expressionSequence ')' statement
    | KEYWORD_PARALLEL? KEYWORD_FOR '(' forStatement ')' statement
    | KEYWORD_PARALLEL? KEYWORD_FOREACH '(' forInStatement ')' statement
    ;

forStatement
    : forInitializer? ';' expressionSequence? ';' expressionSequence?
    ;

forInitializer
    : variableDeclarationList
    ;

forInStatement
    : variableDeclarationList (KEYWORD_IN expressionSequence)?
    ;

// 14. <-- Continue Statement -->
continueStmt
    : KEYWORD_CONTINUE
    ;

// 15. <-- Break Statement -->
breakStmt
    : KEYWORD_BREAK
    ;

// 16. <-- Yield Statement -->
yieldStmt
    : yield eos
    | yield KEYWORD_RETURN expressionSequence
    ;

yield
    : KEYWORD_YIELD expressionSequence
    ;

// 17. <-- Return Statement -->
returnStmt
    : KEYWORD_RETURN expressionSequence
    ;

// 18. <-- Throw Statement -->
throwStmt
    : KEYWORD_THROW '(' expressionSequence ')'
    ;

// 19. <-- Try Statement -->
tryStmt
    : KEYWORD_TRY statement catchClause
    ;

catchClause
    : KEYWORD_CATCH '(' catchParameter ')' statement
    ;

catchParameter
    : variableDeclaration
    ;

// 20. <-- Assert Statement -->
assertStmt
    : KEYWORD_ASSERT '(' expressionSequence ')'
    ;

// 21. <-- Switch Statement -->
switchStmt
    : KEYWORD_SWITCH '(' expressionSequence ')' '{' switchCase* '}'
    ;

switchCase
    : KEYWORD_CASE expressionSequence ':' statement
    | KEYWORD_CASE expressionSequence '->' statement
    | KEYWORD_DEFAULT ':' statement
    | KEYWORD_DEFAULT '->' statement
    ;

// 22. <-- Function Declaration -->
functionStmt
    :  (functionModifier)*? KEYWORD_FN '*'? functionDeclaration
    ;

functionDeclaration
    : identifier '(' formalParameterList ')' (':' '('? (type | returnType) ')'?)? functionBody
    | operator '(' formalParameterList ')' (':' '('? (type | returnType) ')'?)? functionBody
    ;

functionLeft
    : KEYWORD_FN identifier '(' formalParameterList ')' (':' '('? (type | returnType) ')'?)?
    ;

functionBody
    : blockStmt
    | '=>' lambdaBody
    | '->' expressionSequence
    ;

formalParameterList
    : (variableDeclaration (',' variableDeclaration)*)?
    ;


// 11. <-- Expression -->
expressionStmt
    : {this.notOpenBraceAndNotFunction()}? expressionSequence eos
    ;

expressionSequence
    : singleExpression (',' singleExpression)*
    ;

// Single Expression
singleExpression
    : lambdaExpression                                                                                      // lambda expression
    | castExpresion                                                                                         // cast expression
    | singleExpression '.' simplePath                                                                       // member access expression
    | KEYWORD_SUPER '.' simplePath                                                                          // member access expression
    | KEYWORD_THIS '.' simplePath                                                                           // member access expression
    | singleExpression '.' '*'                                                                              // member access expression
    | singleExpression '(' arguementsList ')'                                                               // call expression
    | singleExpression '[' expressionSequence ']'                                                           // element access expression
    | newExpression                                                                                         // new expression
    | singleExpression 'as' type                                                                            // cast expression 'as' cast
    | singleExpression 'is' type                                                                            // type test expression 'is' type
    | singleExpression 'in' expressionSequence                                                              // type test expression 'in' expression
    | singleExpression 'instanceof' type                                                                    // type test expression 'instanceof' type
    | singleExpression '++'                                                                                 // unary operator postfix '++' increment
    | singleExpression '--'                                                                                 // unary operator postfix '--' decrement
    | '++' singleExpression                                                                                 // unary operator prefix '++' increment
    | '--' singleExpression                                                                                 // unary operator prefix '--' decrement
    | '+' singleExpression                                                                                  // unary operator prefix '+' positive
    | '-' singleExpression                                                                                  // unary operator prefix '-' negative
    | '~' singleExpression                                                                                  // unary operator prefix '~' complement
    | '!' singleExpression                                                                                  // unary operator prefix '!' logical not
    | <assoc=right> singleExpression '**' singleExpression                                                  // binary operator '**' exponentiation
    | singleExpression ('*' | '/' | '%') singleExpression                                                   // binary operator '*' '/' '%' multiplication division remainder
    | singleExpression ('+' | '-') singleExpression                                                         // binary operator '+' '-' addition subtraction
    | singleExpression '??' singleExpression                                                                // binary operator '??' null coalescing
    | singleExpression ('<' '<' | '>' '>' | '>' '>' '>') singleExpression                                   // binary operator '<<' '>>' '>>>' bitwise shift
    | singleExpression ('<' | '>' | '<=' | '>=') singleExpression                                           // relational operator '<' '>' '<=' '>=' relational
    | singleExpression ('==' | '!=') singleExpression                                                       // equality operator '==' '!=' equality
    | singleExpression '&' singleExpression                                                                 // binary operator '&' bitwise and
    | singleExpression '^' singleExpression                                                                 // binary operator '^' bitwise xor
    | singleExpression '|' singleExpression                                                                 // binary operator '|' bitwise or
    | singleExpression '&&' singleExpression                                                                // comparison operator '&&' logical and
    | singleExpression '||' singleExpression                                                                // comparison operator '||' logical or
    | singleExpression '?' singleExpression ':' singleExpression                                            // ternary operator '?' ':' conditional
    | <assoc=right> singleExpression '=' singleExpression                                                   // assignment operator '=' assignment
    | <assoc=right> singleExpression assignmentOperator singleExpression                                    // assignment operator '+=' '-=' '*=' '/=' '%=' '&=' '|=' '^=' '<<=' '>>=' assignment

    | reference                                                                                             // reference
    | pointer                                                                                               // pointer
    | identifier                                                                                            // identifier
    | literal                                                                                               // literal
    | parenExpression                                                                                       // paren expression
    ;

lambdaExpression
    : '('? lambdaParameters ')' ('=>' lambdaBody | '->' expressionSequence)
    ;

lambdaParameters
    : lambdaParameter? (',' lambdaParameter)*
    ;

lambdaParameter
    : variableDeclaration
    | identifier
    ;

lambdaBody
    : expressionSequence
    | blockStmt
    ;

castExpresion
    : '(' type ')' singleExpression
    ;

newExpression
    : KEYWORD_NEW type '(' arguementsList? ')'
    ;

arguementsList
    : expressionSequence?
    ;


identifier
    : IDENTIFIER
    | RAW_IDENTIFIER
    | reference
    | pointer
    ;

literal
    : numericLiteral
    | stringLiteral
    | booleanLiteral
    | byteLiteral
    | nullLiteral
    ;

nullLiteral
    : NULL_LITERAL
    ;

booleanLiteral
    : BOOLEAN_LITERAL
    ;

byteLiteral
    : BYTE_LITERAL
    ;

stringLiteral
    : STRING_LITERAL
    | PARAMETRIC_STRING
    ;

numericLiteral
    : INTEGER_LITERAL
    | FLOAT_LITERAL
    | DOUBLE_LITERAL
    ;

parenExpression
    : '(' expressionSequence ')'
    ;


simplePath
    : IDENTIFIER ('.' IDENTIFIER)*
    ;


returnType
    : type
    | KEYWORD_VOID
    | KEYWORD_ANYTYPE
    ;

typeParameters
    : '<' typeParameterList? '>'
    ;

typeParameterList
    : typeParameter (',' typeParameter)*
    ;

typeParameter
    : type
    ;


type
    : primitiveType
    | referenceType
    | type '[' ']'
    | type '?'
    | primaryType
    | anyType
    | dynamicType
    ;

anyType
    : KEYWORD_ANYTYPE
    ;

dynamicType
    : KEYWORD_DYN
    ;

primaryType
    : '(' type ')'

    | tupleType
    | thisType
    | typeQuery
    ;

typeQuery
    : KEYWORD_TYPEOF '(' type ')'
    ;

thisType
    : KEYWORD_THIS
    ;

tupleType
    : '(' type typeList? ')'
    ;

typeList
    : type (',' type)*
    ;

primitiveType
    : KEYWORD_INT
    | KEYWORD_UINT
    | KEYWORD_BYTE
    | KEYWORD_FLOAT
    | KEYWORD_DOUBLE
    | KEYWORD_CHAR
    | KEYWORD_BOOL
    | KEYWORD_STRING
    ;

referenceType
    : identifier
    ;


assignmentOperator
    : '+='
    | '-='
    | '*='
    | '/='
    | '%='
    | '&='
    | '|='
    | '^='
    | '<<='
    | '>>='
    ;



accessModifier
    : KEYWORD_PUBLIC
    | KEYWORD_PRIVATE
    | KEYWORD_PROTECTED
    | KEYWORD_INTERNAL
    | KEYWORD_VIRTUAL
    ;

staticModifier
    : KEYWORD_STATIC
    ;

abstractModifier
    : KEYWORD_ABSTRACT
    ;

mutabilityModifier
    : AND? KEYWORD_MUT
    ;

modifier
    : accessModifier
    | staticModifier
    | abstractModifier
    | mutabilityModifier
    ;

variableModifier
    : mutabilityModifier
    | staticModifier
    | accessModifier
    ;

functionModifier
    : staticModifier
    | accessModifier
    | KEYWORD_ASYNC
    | KEYWORD_OVERRIDE
    | KEYWORD_EXT
    ;

objectModifier
    : staticModifier
    | accessModifier
    ;

classModifier
    : staticModifier
    | accessModifier
    | abstractModifier
    ;

operator
    : PLUS
    | MINUS
    | STAR
    | SLASH
    | PERCENT
    | AND
    | OR
    | XOR
    | LTLT
    | GTGT
    | GTEQ
    | LTEQ
    | EQEQ
    | NOTEQ
    | NOT
    | TILDE
    | EQ
    ;
reference
    : AND singleExpression
    ;

pointer
    : STAR singleExpression
    ;

assignable
    : identifier
    ;


eos
    : SEMI
    | EOF
    ;