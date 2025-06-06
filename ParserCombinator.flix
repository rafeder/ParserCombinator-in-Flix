use Char.isWhitespace
use Char.isLetter
use Char.isDigit
use CodePoint.isAlphabetic

type alias Parser[s] = List[Char] -> List[(s, List[Char])] 

def satisfy(pred : (Char -> Bool)) : Parser[Char] =  
    input -> match input {
        case x::xs => if (pred(x)) ((x, xs)::Nil) else Nil
        case Nil => Nil
    }

def digit() : Parser[Char] = satisfy(isDigit)
def alpha() : Parser[Char] = satisfy(isLetter)
def whitespace() : Parser[Char] = satisfy(isWhitespace)
def string(str : String) : Parser[String]  =
     input -> {
        let t = List.splitAt(String.length(str), input);
        if (String.toList(str) == fst(t)) ((str, snd(t))::Nil) else Nil
     }
def token(str : String, val : s) : Parser[s] =  
    input -> List.map(pair -> (val, snd(pair)), parse(string(str), input))
def parseTrue() : Parser[Bool] = token("true", true)
def parseFalse() : Parser[Bool] = token("false", false)
def some() : Parser[Char] = ???

def parse(parser : Parser[s], input : List[Char]):List[(s, List[Char])] = parser(input)

def main() : List[(Bool, List[Char])] = 
    let input : List[Char] = String.toList("true && false");
    parse(parseTrue(), input)
