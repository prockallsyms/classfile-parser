/// Compile AST types — lightweight syntax tree mapping directly to bytecode.

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveKind {
    Int,
    Long,
    Float,
    Double,
    Boolean,
    Byte,
    Char,
    Short,
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeName {
    Primitive(PrimitiveKind),
    Class(String),
    Array(Box<TypeName>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    Ushr,
    BitAnd,
    BitOr,
    BitXor,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompareOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    BitNot,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchExprCase {
    pub values: Vec<i64>,
    pub expr: CExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LambdaParam {
    pub ty: Option<TypeName>,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LambdaBody {
    Expr(Box<CExpr>),
    Block(Vec<CStmt>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchCase {
    pub values: Vec<i64>,
    pub body: Vec<CStmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CatchClause {
    pub exception_types: Vec<TypeName>,
    pub var_name: String,
    pub body: Vec<CStmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CStmt {
    LocalDecl {
        ty: TypeName,
        name: String,
        init: Option<CExpr>,
    },
    ExprStmt(CExpr),
    Return(Option<CExpr>),
    If {
        condition: CExpr,
        then_body: Vec<CStmt>,
        else_body: Option<Vec<CStmt>>,
    },
    While {
        condition: CExpr,
        body: Vec<CStmt>,
    },
    For {
        init: Option<Box<CStmt>>,
        condition: Option<CExpr>,
        update: Option<Box<CStmt>>,
        body: Vec<CStmt>,
    },
    Block(Vec<CStmt>),
    Throw(CExpr),
    Break,
    Continue,
    Switch {
        expr: CExpr,
        cases: Vec<SwitchCase>,
        default_body: Option<Vec<CStmt>>,
    },
    TryCatch {
        try_body: Vec<CStmt>,
        catches: Vec<CatchClause>,
        finally_body: Option<Vec<CStmt>>,
    },
    ForEach {
        element_type: TypeName,
        var_name: String,
        iterable: CExpr,
        body: Vec<CStmt>,
    },
    Synchronized {
        lock_expr: CExpr,
        body: Vec<CStmt>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum CExpr {
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    CharLiteral(char),
    NullLiteral,
    LongLiteral(i64),
    DoubleLiteral(f64),
    Ident(String),
    This,
    BinaryOp {
        op: BinOp,
        left: Box<CExpr>,
        right: Box<CExpr>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<CExpr>,
    },
    Comparison {
        op: CompareOp,
        left: Box<CExpr>,
        right: Box<CExpr>,
    },
    LogicalAnd(Box<CExpr>, Box<CExpr>),
    LogicalOr(Box<CExpr>, Box<CExpr>),
    LogicalNot(Box<CExpr>),
    Assign {
        target: Box<CExpr>,
        value: Box<CExpr>,
    },
    CompoundAssign {
        op: BinOp,
        target: Box<CExpr>,
        value: Box<CExpr>,
    },
    PreIncrement(Box<CExpr>),
    PreDecrement(Box<CExpr>),
    PostIncrement(Box<CExpr>),
    PostDecrement(Box<CExpr>),
    MethodCall {
        object: Option<Box<CExpr>>,
        name: String,
        args: Vec<CExpr>,
    },
    StaticMethodCall {
        class_name: String,
        name: String,
        args: Vec<CExpr>,
    },
    FieldAccess {
        object: Box<CExpr>,
        name: String,
    },
    StaticFieldAccess {
        class_name: String,
        name: String,
    },
    NewObject {
        class_name: String,
        args: Vec<CExpr>,
    },
    NewArray {
        element_type: TypeName,
        size: Box<CExpr>,
    },
    NewMultiArray {
        element_type: TypeName,
        dimensions: Vec<CExpr>,
    },
    ArrayAccess {
        array: Box<CExpr>,
        index: Box<CExpr>,
    },
    Cast {
        ty: TypeName,
        operand: Box<CExpr>,
    },
    Instanceof {
        operand: Box<CExpr>,
        ty: TypeName,
    },
    Ternary {
        condition: Box<CExpr>,
        then_expr: Box<CExpr>,
        else_expr: Box<CExpr>,
    },
    SwitchExpr {
        expr: Box<CExpr>,
        cases: Vec<SwitchExprCase>,
        default_expr: Box<CExpr>,
    },
    Lambda {
        params: Vec<LambdaParam>,
        body: LambdaBody,
    },
    MethodRef {
        class_name: String,
        method_name: String,
    },
}
