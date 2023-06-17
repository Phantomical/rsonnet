//! Individual tokens that make up jsonnet source code.

mod ident;
mod keyword;
mod number;
mod string;
mod symbol;
mod operator;
mod paren;

pub use self::ident::Ident;
pub use self::keyword::*;
pub use self::number::Number;
pub use self::string::StringT;
pub use self::symbol::*;
pub use self::operator::*;
pub use self::paren::*;

macro_rules! Token {
    [assert]    => { $crate::parse::token::Assert };
    [else]      => { $crate::parse::token::Else };
    [error]     => { $crate::parse::token::Error };
    [false]     => { $crate::parse::token::False };
    [for]       => { $crate::parse::token::For };
    [function]  => { $crate::parse::token::Function };
    [if]        => { $crate::parse::token::If };
    [import]    => { $crate::parse::token::Import };
    [importstr] => { $crate::parse::token::ImportStr };
    [importbin] => { $crate::parse::token::ImportBin };
    [in]        => { $crate::parse::token::In };
    [local]     => { $crate::parse::token::Local };
    [null]      => { $crate::parse::token::Null };
    [tailstrict]=> { $crate::parse::token::TailStrict };
    [then]      => { $crate::parse::token::Then };
    [self]      => { $crate::parse::token::SelfT };
    [super]     => { $crate::parse::token::Super };
    [true]      => { $crate::parse::token::True };

    [,] => { $crate::parse::token::Comma };
    [.] => { $crate::parse::token::Dot };
    [;] => { $crate::parse::token::SemiColon };

    [()] => { $crate::parse::token::Paren };
    [{}] => { $crate::parse::token::Brace };
    [[]] => { $crate::parse::token::Bracket };

    [*] => { $crate::parse::token::Mul };
    [/] => { $crate::parse::token::Div };
    [%] => { $crate::parse::token::Mod };
    [+] => { $crate::parse::token::Plus };
    [-] => { $crate::parse::token::Minus };
    [<<] => { $crate::parse::token::Shl };
    [>>] => { $crate::parse::token::Shr };
    [<] => { $crate::parse::token::Less };
    [>] => { $crate::parse::token::Greater };
    [<=] => { $crate::parse::token::LessEqual };
    [>=] => { $crate::parse::token::GreaterEqual };
    [==] => { $crate::parse::token::Equal };
    [!=] => { $crate::parse::token::NotEqual };
    [&] => { $crate::parse::token::And };
    [|] => { $crate::parse::token::Or };
    [^] => { $crate::parse::token::Xor };
    [&&] => { $crate::parse::token::LogicalAnd };
    [||] => { $crate::parse::token::LogicalOr };
    [!] => { $crate::parse::token::Not };
    [~] => { $crate::parse::token::Tilde };
    [:] => { $crate::parse::token::Visible };
    [::] => { $crate::parse::token::Hidden };
    [:::] => { $crate::parse::token::ForceVisible };
    [$] => { $crate::parse::token::Dollar };
    [=] => { $crate::parse::token::Assign };
}

pub(crate) use Token;
