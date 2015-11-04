package calendarscript.parser

import scala.util.parsing.combinator._
import calendarscript.ir._
import calendarscript.ir.sugar._

object PiconotParser extends JavaTokenParsers with PackratParsers {
    // parsing interface
    def apply(s: String): ParseResult[AST] = {
      parseAll(multiTransformer, s)
    }
    
    def word(s: String): Parser[String] = {
      val msg = if (s == "else") {
        "Expected the keyword 'else'."
      } else {
        s"Expected the keyword '${s}'."
      }
      ident.filter(_ == s).withFailureMessage(msg)
        
    }
    
    
    //lazy val ident2: PackratParser[String] = ident.filter(s => !(reserved contains s))
    
    def endsInBlock(trans: Transformers): Boolean = {
      trans match {
        case BracedTransformers(_) => true
        case BaseTransformers(trans) => endsInBlock(trans)
      }
    }

    def endsInBlock(trans: Transformer): Boolean = {
      trans match {
        case ElseTransformerBasic(_, t) => endsInBlock(t)
        case ElseTransformerComplex(_, _, t) => endsInBlock(t)
        case AugmentTransformer(_, t) => endsInBlock(t)
        case BaseTransformer(_) => false
      }
    }
    
    def transformerBlockEnd: PackratParser[Transformer] = new PackratParser[Transformer] {
      def apply(in: Input): ParseResult[Transformer] = {
        val parse = transformer(in)
        parse match {
          case Success(trans, _) =>  if (endsInBlock(trans)) {
              parse
            } else {
              Failure("You must end rules with either a ; or a {block}. Perhaps you forgot a ;?", in)
            }
          case _ => parse
        }
      }
    }
    
    def ident2: PackratParser[String] = new PackratParser[String] {
      def apply(in: Input): ParseResult[String] = {
        val reserved = "else move up down left right north south east west n s e w open free blocked closed".split(" ")
        val parse = (ident.withFailureMessage("Expected to see a state name. State names must be valid identifiers (alphanumeric, etc.)"))(in)
        parse match {
          case Success(word, _) => if (reserved contains word) {
              Failure(s"${word} is a reserved word. It cannot be used as a state name.", in)
            } else {
              parse
            }
          case _ => parse
        }
      }
    }
    
    // transformer
    lazy val transformer: PackratParser[Transformer] =
      (  move~newline~word("else")~transformers ^^ {case mov~n~"else"~trans => new ElseTransformerBasic(mov, trans)}  
         | move~separator~transformers~newline~word("else")~transformers ^^ {case mov~s~trans1~n~"else"~trans2 => new ElseTransformerComplex(mov, trans1, trans2)} 
         | move~block~word("else")~transformers ^^ {case mov~b~"else"~trans2 => new ElseTransformerComplex(mov, b, trans2)}
         | augment~separator~transformers ^^ {case aug~s~trans => new AugmentTransformer(aug, trans)}
         | augment~block ^^ {case aug~b => new AugmentTransformer(aug, b)}
         | augment ^^ {aug => new BaseTransformer(aug)}
      )
    
    lazy val augmentList: PackratParser[List[Augment]] =
      ( augment~separator~augmentList ^^ {case a~s~as => a::as}
      | augment ^^ {a => List(a)}
      )

      
    //transformers
    lazy val transformers: PackratParser[Transformers] = 
      ( transformer ^^ {trans => new BaseTransformers(trans)}
      | block
      )
     
    lazy val block: PackratParser[Transformers] =
      (
       ("{".withFailureMessage("You either forgot a separator (your choice of , : ->), or forgot to start a curly-brace block { ... }! For example, 'move left left closed' is missing a separator between the 'left's."))
      ~multiTransformer
      ~("}".withFailureMessage("You must end rules with either a semicolon ; or a curly-braced surrounded block { ... }. Perhaps on the line above you forgot a ; or to close a block?")) ^^ {case "{"~mTrans~"}" => new BracedTransformers(mTrans)}
     )
    //multiTransformer
    lazy val multiTransformer: PackratParser[MultiTransformers] =
      ( transformer~newline~multiTransformer ^^ {case trans~nL~mTrans => new MutlipleMultiTransformers(trans, mTrans)}
      | transformerBlockEnd~multiTransformer ^^ {case trans~mTrans => new MutlipleMultiTransformers(trans, mTrans)}
      | transformer ^^ {trans => new SingleMultiTransformers(trans)}
      )
    
    // augment
    lazy val augment: PackratParser[Augment] =
      (  move ^^ { move => move}
         | word("stay") ^^ { _ => new Stay()}
         | word("state")~ident2 ^^ {case "state"~s => new StateDef(s)}
         | dir~restrictdef ^^ {case d~r => new Restrict(d,r)}
         | ident2 ^^ {case s => new MoveState(s)}
      )
      
    lazy val move: PackratParser[Move] =
      ( word("move")~dir ^^ {case "move"~d => 
        new Move(d)} )
      
    //separator
    lazy val separator: PackratParser[Boolean] = 
      ( ("," | ":" | "->") ^^ {_ => true})
    
    //newline
    lazy val newline: PackratParser[Boolean] =
        ( ";"  ^^ {_ => true}
        //| "\n" ^^ {_ => true}
        ).withFailureMessage("Expected and end-of-rule marker. That's either a ; or closing a curly-brace block { ... }.")
     
    //restrictdef
    lazy val restrictdef: PackratParser[RelativeDescription] =
      ( //environment?
       (word("open") | word("free")) ^^ {_ => Open} 
       | (word("blocked") | word("closed")) ^^ {_ => Blocked}
      ).withFailureMessage("Expected a surroundings descriptor keyword. (open, free, blocked, closed)")
      
    // dir
    lazy val dir: PackratParser[MoveDirection] =
      ( (word("north") | word("n") | word("up")) ^^ {_ => North}
        | (word("east") | word("e") | word("right")) ^^ {_ => East}
        | (word("south") | word("s") | word("down")) ^^ {_ => South}
        | (word("west") | word("w") | word("left")) ^^ {_ => West} )
      
      
//    // expressions
//    lazy val expr: PackratParser[Expr] = 
//      (   "-" ~ expr ^^ {case "-"~e => new Neg(e) }
//        |  expr~"+"~fact ^^ {case l~"+"~r => l |+| r}
//        |  expr~"-"~fact ^^ {case l~"-"~r => l |-| r}
//        | "(" ~ expr ~ ")" ^^ {case "("~e~")" => e}
//        | fact )
//        
//    lazy val fact: PackratParser[Expr] =
//      ( fact~"*"~term ^^ {case l~"*"~r => l |*| r}
//        | term )
//      
//    // factors
//    lazy val term: PackratParser[Expr] =
//      number
//      
//    // numbers
//    def number: Parser[Num] = wholeNumber ^^ {s ⇒ Num(s.toInt)}
    
 }