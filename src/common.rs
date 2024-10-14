use lazy_static::lazy_static;

const STRING_BASE: &str = r".*?(?<!\\)(\\\\)*?";
const INT_BASE: &str = r"[0-9](?:[0-9_]*[0-9])?";
const FLOAT_BASE: &str = r"[0-9](?:[0-9_]*[0-9])?(?:[eE][+\-]?[0-9](?:[0-9_]*[0-9])?)|(?:[0-9](?:[0-9_]*[0-9])?\.[0-9]*|\.[0-9]+)(?:[eE][+\-]?[0-9](?:[0-9_]*[0-9])?)?";

lazy_static! {
    pub static ref CHAR: (String, String) = ("char".into(), r"'(?:[^']|\\')'".into());
    pub static ref SINGLE_QUOTED_STRING: (String, String) =
        ("single_quoted_string".into(), format!("'{STRING_BASE}'"));
    pub static ref DOUBLE_QUOTED_STRING: (String, String) =
        ("double_quoted_string".into(), format!("\"{STRING_BASE}\""));
    pub static ref LETTER: (String, String) = ("letter".into(), r"[A-Za-z]".into());
    pub static ref WORD: (String, String) = ("word".into(), r"[A-Za-z]+".into());
    pub static ref C_NAME: (String, String) = ("c_name".into(), r"[_A-Za-z][_A-Za-z\d]*".into());
    pub static ref NEWLINE: (String, String) = ("newline".into(), r"\r?\n".into());
    pub static ref DIGIT: (String, String) = ("digit".into(), r"[0-9]".into());
    pub static ref HEXDIGIT: (String, String) = ("hexdigit".into(), r"[0-9A-Fa-f]".into());
    pub static ref UNSIGNED_INT: (String, String) = ("unsigned_int".into(), INT_BASE.into());
    pub static ref SIGNED_INT: (String, String) =
        ("signed_int".into(), format!(r"[+\-]{INT_BASE}"));
    pub static ref DECIMAL: (String, String) = (
        "decimal".into(),
        format!(r"{INT_BASE}\.(?:[0-9]+)?|\.[0-9]+")
    );
    pub static ref UNSIGNED_FLOAT: (String, String) = ("unsigned_float".into(), FLOAT_BASE.into());
    pub static ref SIGNED_FLOAT: (String, String) =
        ("signed_float".into(), format!(r"[+\-](?:{FLOAT_BASE})"));
    pub static ref STRING: (String, String) = (
        "string".into(),
        format!("\"{STRING_BASE}\"|'{STRING_BASE}'")
    );
    pub static ref UNSIGNED_NUMBER: (String, String) =
        ("unsigned_number".into(), format!("{FLOAT_BASE}|{INT_BASE}"));
    pub static ref SIGNED_NUMBER: (String, String) = (
        "signed_number".into(),
        format!(r"[+\-](?:(?:{FLOAT_BASE})|{INT_BASE})")
    );
    pub static ref INT: (String, String) = ("int".into(), format!(r"[+\-]?{INT_BASE}"));
    pub static ref FLOAT: (String, String) = ("float".into(), format!(r"[+\-]?(?:{FLOAT_BASE})"));
    pub static ref NUMBER: (String, String) = (
        "number".into(),
        format!(r"[+\-]?(?:(?:{FLOAT_BASE})|{INT_BASE})")
    );
}
