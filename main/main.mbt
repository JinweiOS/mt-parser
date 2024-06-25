// 构造可组合的解析器
type Lexer[S] (String) -> (S, String)?

fn parse[S](self : Lexer[S], str : String) -> (S, String)? {
  // .0提取newType内部表示
  // https://www.moonbitlang.cn/docs/syntax#%E6%96%B0%E7%B1%BB%E5%9E%8B
  (self.0)(str)
}

fn and[S, T](self : Lexer[S], parser2 : Lexer[T]) -> Lexer[(S, T)] {
  Lexer(
    fn(input) {
      let (value, rest) = self.parse(input)?
      let (value2, rest2) = parser2.parse(rest)?
      Some(((value, value2), rest2))
    },
  )
}

fn or[S](self : Lexer[S], parser2 : Lexer[S]) -> Lexer[S] {
  Lexer(
    fn(input) {
      match self.parse(input) {
        None => parser2.parse(input)
        Some(_) as result => result
      }
    },
  )
}

fn many[S](self : Lexer[S]) -> Lexer[@immut/list.List[S]] {
  Lexer(
    fn(input) {
      let mut rest = input
      let mut cumul = @immut/list.List::Nil
      while true {
        match self.parse(rest) {
          None => break
          Some((value, new_rest)) => {
            rest = new_rest
            cumul = Cons(value, cumul)
          }
        }
      }
      Some((cumul.reverse(), rest))
    },
  )
}

// 将解析结果直接映射为枚举值
fn map[I, O](self : Lexer[I], f : (I) -> O) -> Lexer[O] {
  Lexer(
    fn(input) {
      let (value, rest) = self.parse(input)?
      Some((f(value), rest))
    },
  )
}

fn pchar(predicate : (Char) -> Bool) -> Lexer[Char] {
  Lexer(
    fn(input) {
      if input.length() > 0 && predicate(input[0]) {
        Some((input[0], input.substring(start=1, end=input.length())))
      } else {
        None
      }
    },
  )
}

fn pstring(target : String) -> Lexer[String] {
  Lexer(
    fn(input) {
      let len = target.length()
      if input.length() >= len && input.substring(start=0, end=len) == target {
        Some((target, input.substring(start=len, end=input.length())))
      } else {
        None
      }
    },
  )
}

enum Token {
  KeyWord(String)
  Value(Int)
  LParen
  RParen
  Plus
  Minus
  Multiply
  Divide
} derive(Debug)

let symbol : Lexer[Token] = pchar(
  fn {
    '+' | '-' | '*' | '/' | '(' | ')' => true
    _ => false
  },
).map(
  fn {
    '+' => Token::Plus
    '-' => Token::Minus
    '*' => Token::Multiply
    '/' => Token::Divide
    '(' => Token::LParen
    ')' => Token::RParen
  },
)

// 匹配空白字符
let whitespace : Lexer[Char] = pchar(fn { ch => ch == ' ' })

let zero : Lexer[Int] = pchar(fn { ch => ch == '0' }).map(fn { _ => 0 })

let ky_and : Lexer[Token] = pstring("AND").map(Token::KeyWord)

let one_to_nine : Lexer[Int] = pchar(
  fn { ch => ch.to_int() >= 0x31 && ch.to_int() <= 0x39 },
).map(fn { ch => ch.to_int() - 0x30 })

let zero_to_nine : Lexer[Int] = pchar(
  fn { ch => ch.to_int() >= 0x30 && ch.to_int() <= 0x39 },
).map(fn { ch => ch.to_int() - 0x30 })

let num : Lexer[Token] = zero.or(
  one_to_nine.and(zero_to_nine.many()).map(
    fn { (i, ls) => ls.fold_left(fn { i, j => i * 10 + j }, init=i) },
  ),
).map(Token::Value)

let tokens : Lexer[@immut/list.List[Token]] = num.or(ky_and).or(symbol).and(
  whitespace.many(),
).map(fn { (symbols, _) => symbols }).many()

// TEST
// fn main {
//   debug(tokens.parse("-2+--+32849 103   AND   AND   (5  )"))
// }
// 语法解析
enum Expression {
  Number(Int)
  Plus(Expression, Expression)
  Minus(Expression, Expression)
  Multiply(Expression, Expression)
  Divide(Expression, Expression)
} derive(Debug)

type Parser[S] (@immut/list.List[Token]) -> (S, @immut/list.List[Token])?

fn parse[S](
  self : Parser[S],
  tokens : @immut/list.List[Token]
) -> (S, @immut/list.List[Token])? {
  (self.0)(tokens)
}

fn ptoken(predicate : (Token) -> Bool) -> Parser[Token] {
  Parser(
    fn {
      Nil => None
      Cons(token, rest) =>
        if predicate(token) {
          Some((token, rest))
        } else {
          None
        }
    },
  )
}

fn map[S, T](self : Parser[S], f : (S) -> T) -> Parser[T] {
  Parser(
    fn(input) {
      let (value, rest) = self.parse(input)?
      Some((f(value), rest))
    },
  )
}

fn and[S, T](self : Parser[S], parser2 : Parser[T]) -> Parser[(S, T)] {
  Parser(
    fn(input) {
      let (value, rest) = self.parse(input)?
      let (value2, rest2) = parser2.parse(rest)?
      Some(((value, value2), rest2))
    },
  )
}

fn or[Value](self : Parser[Value], parser2 : Parser[Value]) -> Parser[Value] {
  Parser(
    fn(input) {
      match self.parse(input) {
        None => parser2.parse(input)
        Some(_) as result => result
      }
    },
  )
}

fn many[Value](self : Parser[Value]) -> Parser[@immut/list.List[Value]] {
  Parser(
    fn(input) {
      let mut i = input
      let mut cumul = @immut/list.List::Nil
      while true {
        match self.parse(i) {
          None => break
          Some((value, rest)) => {
            i = rest
            cumul = Cons(value, cumul)
          }
        }
      }
      Some((cumul.reverse(), i))
    },
  )
}

fn Parser::ref[Value](ref : Ref[Parser[Value]]) -> Parser[Value] {
  Parser(fn(input) { ref.val.parse(input) })
}

let lparen : Parser[Token] = ptoken(
  fn {
    LParen => true
    _ => false
  },
)

let rparen : Parser[Token] = ptoken(
  fn {
    RParen => true
    _ => false
  },
)

let plus : Parser[Token] = ptoken(
  fn {
    Plus => true
    _ => false
  },
)

let minus : Parser[Token] = ptoken(
  fn {
    Minus => true
    _ => false
  },
)

let multiply : Parser[Token] = ptoken(
  fn {
    Multiply => true
    _ => false
  },
)

let divide : Parser[Token] = ptoken(
  fn {
    Divide => true
    _ => false
  },
)

fn parser() -> Parser[Expression] {
  // 定义空引用
  let expression_ref : Ref[Parser[Expression]] = {
    val: Parser(fn { _ => None }),
  }
  let number : Parser[Expression] = ptoken(
    fn {
      Value(_) => true
      _ => false
    },
  ).map(fn { Value(i) => Expression::Number(i) })

  // atomic = Value / "(" expression ")"
  let automic = lparen.and(ref(expression_ref)).and(rparen).map(
    fn { ((_, expr), _) => expr },
  ).or(number)

  // combine = atomic * ( ("*" / "/") atomic )
  let combine = automic.and(multiply.or(divide).and(automic).many()).map(
    fn {
      (expr, list) => {
        debug(expr)
        debug(
          list.fold_left(
            fn {
              e, (Multiply, expr) => Expression::Multiply(e, expr)
              e, (_, expr) => Expression::Divide(e, expr)
            },
            init=expr,
          ),
        )
        list.fold_left(
          fn {
            e, (Multiply, expr) => Expression::Multiply(e, expr)
            e, (_, expr) => Expression::Divide(e, expr)
          },
          init=expr,
        )
      }
    },
  )

  // expression = combine * ( "+" / "-" combine )
  expression_ref.val = combine.and(plus.or(minus).and(combine).many()).map(
    fn {
      (expr, list) =>
        list.fold_left(
          fn {
            e, (Plus, expr) => Expression::Plus(e, expr)
            e, (_, expr) => Expression::Minus(e, expr)
          },
          init=expr,
        )
    },
  )
  ref(expression_ref)
}

fn parse_string(str : String) -> (Expression, String, @immut/list.List[Token])? {
  let (token_list, rest_string) = tokens.parse(str)?
  let (expr, rest_token) = parser().parse(token_list)?
  Some((expr, rest_string, rest_token))
}

// trait Expr {
//   number(Int) -> Self
//   op_add(Self, Self) -> Self
//   op_sub(Self, Self) -> Self
//   op_mul(Self, Self) -> Self
//   op_div(Self, Self) -> Self
// }

// fn Expression::number(i : Int) -> Expression {
//   Number(i)
// }

// fn Expression::op_add(a : Expression, b : Expression) -> Expression {
//   Plus(a, b)
// }

// fn Expression::op_sub(a : Expression, b : Expression) -> Expression {
//   Minus(a, b)
// }

// fn Expression::op_mul(a : Expression, b : Expression) -> Expression {
//   Multiply(a, b)
// }

// fn Expression::op_div(a : Expression, b : Expression) -> Expression {
//   Divide(a, b)
// }

// 语义：计算表达式
// type BoxedInt Int derive(Debug)

// fn BoxedInt::number(i : Int) -> BoxedInt {
//   BoxedInt(i)
// }

// fn BoxedInt::op_add(a : BoxedInt, b : BoxedInt) -> BoxedInt {
//   BoxedInt(a.0 + b.0)
// }

// fn BoxedInt::op_sub(a : BoxedInt, b : BoxedInt) -> BoxedInt {
//   BoxedInt(a.0 - b.0)
// }

// fn BoxedInt::op_mul(a : BoxedInt, b : BoxedInt) -> BoxedInt {
//   BoxedInt(a.0 * b.0)
// }

// fn BoxedInt::op_div(a : BoxedInt, b : BoxedInt) -> BoxedInt {
//   BoxedInt(a.0 / b.0)
// }

// 语义：输出表达式
// struct BoxedString {
//   str : String
//   level : Int
// } derive(Debug)

// fn BoxedString::number(i : Int) -> BoxedString {
//   { str: i.to_string(), level: 0 }
// }

// fn BoxedString::op_add(a : BoxedString, b : BoxedString) -> BoxedString {
//   let stra = a.str
//   let strb = if b.level == 1 { "(" + b.str + ")" } else { b.str }
//   { str: "\(stra) + \(strb)", level: 1 }
// }

// fn BoxedString::op_sub(a : BoxedString, b : BoxedString) -> BoxedString {
//   let stra = a.str
//   let strb = if b.level == 1 { "(" + b.str + ")" } else { b.str }
//   { str: "\(stra) - \(strb)", level: 1 }
// }

// fn BoxedString::op_mul(a : BoxedString, b : BoxedString) -> BoxedString {
//   let stra = if a.level == 1 { "(" + a.str + ")" } else { a.str }
//   let strb = if b.level == 1 { "(" + b.str + ")" } else { b.str }
//   { str: "\(stra) * \(strb)", level: 2 }
// }

// fn BoxedString::op_div(a : BoxedString, b : BoxedString) -> BoxedString {
//   let stra = if a.level == 1 { "(" + a.str + ")" } else { a.str }
//   let strb = if b.level == 1 { "(" + b.str + ")" } else { b.str }
//   { str: "\(stra) / \(strb)", level: 2 }
// }

// fn recursive_parser_with_tagless_final[E : Expr]() -> Parser[E] {
//   let number : Parser[E] = ptoken(
//     fn {
//       Value(_) => true
//       _ => false
//     },
//   ).map(fn { Value(i) => E::number(i) })
//   fn atomic(tokens : @immut/list.List[Token]) -> (E, @immut/list.List[Token])? {
//     lparen.and(Parser(expression)).and(rparen).map(
//       fn { ((_, expr), _) => expr },
//     ).or(number).parse(tokens)
//   }

//   fn combine(
//     tokens : @immut/list.List[Token]
//   ) -> (E, @immut/list.List[Token])? {
//     (Parser(atomic) : Parser[E]).and(
//       multiply.or(divide).and(Parser(atomic)).many(),
//     ).map(
//       fn {
//         (e, list) =>
//           list.fold_left(
//             fn {
//               e, (Multiply, expr) => e * expr
//               e, (_, expr) => e / expr
//             },
//             init=e,
//           )
//       },
//     ).parse(tokens)
//   }

//   fn expression(
//     tokens : @immut/list.List[Token]
//   ) -> (E, @immut/list.List[Token])? {
//     (Parser(combine) : Parser[E]).and(
//       plus.or(minus).and(Parser(combine)).many(),
//     ).map(
//       fn {
//         (e, list) =>
//           list.fold_left(
//             fn {
//               e, (Plus, expr) => e + expr
//               e, (_, expr) => e - expr
//             },
//             init=e,
//           )
//       },
//     ).parse(tokens)
//   }

//   Parser(expression)
// }

// fn parse_string_tagless_final[E : Expr](
//   str : String
// ) -> (E, String, @immut/list.List[Token])? {
//   let (token_list, rest_string) = tokens.parse(str)?
//   let (expr, rest_token) : (E, @immut/list.List[Token]) = recursive_parser_with_tagless_final().parse(
//     token_list,
//   )?
//   Some((expr, rest_string, rest_token))
// }

fn main {
  println("===第二部分===")
  debug(parse_string("1 + 1 * (307 + 7) + 5 - 3 - 2"))
  // debug(
  //   (
  //     parse_string_tagless_final("1 + 1 * (307 + 7) + 5 - 3 - 2") :
  //     (Expression, String, @immut/list.List[Token])?),
  // )
  // debug(
  //   (
  //     parse_string_tagless_final("(307 + 7) + 5 - 3 - 22") :
  //     (BoxedInt, String, @immut/list.List[Token])?),
  // )
  // debug(
  //   (
  //     parse_string_tagless_final("1 + 1 * (307 + 7) + 5 - 3 - 2") :
  //     (BoxedString, String, @immut/list.List[Token])?),
  // )
}
