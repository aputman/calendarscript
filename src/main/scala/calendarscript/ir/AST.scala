package calendarscript.ir

/**
 * @author mvalentine
 */


sealed abstract class AST
//sealed abstract class Expr extends AST
sealed abstract class Transform extends AST
sealed abstract class Transformer extends AST
sealed abstract class Transformers extends AST
sealed abstract class MultiTransformers extends AST
sealed abstract class Augment extends AST

case class ElseTransformerBasic(move: Move, transR: Transformers) extends Transformer
case class ElseTransformerComplex(move: Move, transL: Transformers, transR: Transformers) extends Transformer
case class AugmentTransformer(aug: Augment, trans: Transformers) extends Transformer
case class BaseTransformer(aug: Augment) extends Transformer

case class BaseTransformers(trans: Transformer) extends Transformers
case class BracedTransformers(multiTrans: MultiTransformers) extends Transformers

//case class Transformers(trans: multiTransformers) extends Transform

case class SingleMultiTransformers(t: Transformer) extends MultiTransformers
case class MutlipleMultiTransformers(transL: Transformer, multiT: MultiTransformers) extends MultiTransformers

case class Move(dir: MoveDirection) extends Augment
case class Stay() extends Augment
case class Restrict(dir: MoveDirection, restrict: RelativeDescription) extends Augment
case class StateDef(name: String) extends Augment
case class MoveState(newState: String) extends Augment



