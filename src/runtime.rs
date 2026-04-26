#![allow(non_snake_case)]

fn main() {
    __mainExpression_0();
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Str(String),
    Ch(char),
    Db(f64),
    PrT(PrimType),
    WorldVal,
    Unit,
    Con(Option<i64>, Vec<Value>),
    Fun(&'static str, Vec<Value>, usize),
}

#[derive(Clone)]
enum PrimType {
    IntType,
    Int8Type,
    Int16Type,
    Int32Type,
    Int64Type,
    IntegerType,
    Bits8Type,
    Bits16Type,
    Bits32Type,
    Bits64Type,
    StringType,
    CharType,
    DoubleType,
    WorldType,
}

fn idris_sub_Integer(val_0: Value, val_1: Value) -> Value {
    let Value::Int(v0) = val_0 else {
        unreachable!();
    };
    let Value::Int(v1) = val_1 else {
        unreachable!();
    };
    Value::Int(v0 - v1)
}

fn idris_lt_Integer(val_0: Value, val_1: Value) -> Value {
    let Value::Int(v0) = val_0 else {
        unreachable!();
    };
    let Value::Int(v1) = val_1 else {
        unreachable!();
    };
    if v0 < v1 {
        Value::Int(1)
    } else {
        Value::Int(0)
    }
}

fn idris_lte_Integer(val_0: Value, val_1: Value) -> Value {
    let Value::Int(v0) = val_0 else {
        unreachable!();
    };
    let Value::Int(v1) = val_1 else {
        unreachable!();
    };
    if v0 <= v1 {
        Value::Int(1)
    } else {
        Value::Int(0)
    }
}

fn idris_eq_Integer(val_0: Value, val_1: Value) -> Value {
    let Value::Int(v0) = val_0 else {
        unreachable!();
    };
    let Value::Int(v1) = val_1 else {
        unreachable!();
    };
    if v0 == v1 {
        Value::Int(1)
    } else {
        Value::Int(0)
    }
}

fn idris_Prelude_dot_IO_prim__putStr(s: String) {
    print!("{s}");
}
