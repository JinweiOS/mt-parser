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
        debug(input.length())
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

let number : Lexer[Token] = zero.or(
  one_to_nine.and(zero_to_nine.many()).map(
    fn { (i, ls) => ls.fold_left(fn { i, j => i * 10 + j }, init=i) },
  ),
).map(Token::Value)

let tokens : Lexer[@immut/list.List[Token]] = number.or(ky_and).or(symbol).and(
  whitespace.many(),
).map(fn { (symbols, _) => symbols }).many()

fn main {
  debug(tokens.parse("-2+--+32849 103   AND   AND   (5  )"))
}
