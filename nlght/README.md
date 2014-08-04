# Grammar
Grammar as programmed in the compiler. TOKEN or 'string' means terminal token.
    Program = TypeDef | Func | Var

    LocalFunc = 'fn' Id ParenArgList EOL Block
    Func = 'fn' BoundId ParenArgList EOL Block
    BoundId = [TYPE '.']? ID
    ParenArgList = '(' ArgList ')'
    ArgList = [MultiArg [',' MultiArg]]*
    MultiArg = ArgName [ ',' ArgName]* TYPE
    ArgName = ID  // ['=' Expr] if we consider optional args and unify with VarName

    Block = | ';' END
            | BlockElem END
    BlockElem = LocalFunc | Var | Statement | Expr
    Var = 'var' VarList
    VarList = VarNameDef [',' VarNameDef]* [TYPE]?
    VarName = ID [ '=' Expr]
