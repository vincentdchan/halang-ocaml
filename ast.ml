
module rec Identifier : sig
  type t = {
    value : string;
    raw : string;
  }
end = Identifier

module rec StringLiteral : sig
  type t = {
    value : string;
    raw : string;
  }
end = StringLiteral

module rec Number : sig
  type t = {
    value: int;
    raw : string;
  }
end = Number

module rec Expression : sig

  type op_type =
  | O_LG_OR | O_LG_AND | O_EQUAL | O_NOT_EQUAL
  | O_LTEQ | O_GTEQ | O_LT | O_GT
  | O_LSHIFT | O_RSHIFT
  | O_PLUS | O_MINUS | O_MULT | O_DIV | O_EXP
  | O_MOD

  module Assign : sig
    type t = {
      left: Identifier.t;
      right: Expression.t;
    }
  end

  module Unary : sig
    type t = {
      op : op_type;
      body : Expression.t;
    }
  end

  module Binary : sig
    type t = {
      op : op_type;
      left: Expression.t;
      right: Expression.t;
    }
  end

  module Call : sig
    type t = {
      body: Expression.t;
      params: Expression.t list;
    }
  end

  module Member : sig
    type t = {
      body: Expression.t;
      property: Expression.t;
    }
  end

  type t =
  | Assign of Assign.t
  | Unary of Unary.t
  | Binary of Binary.t
  | Call of Call.t
  | Member of Member.t
  | Identifier of Identifier.t
  | StringLiteral of StringLiteral.t
  | Number of Number.t
end = Expression

module rec Statement : sig
  module If : sig
   type t = {
     condition : Expression.t;
     true_branch : Statement.t list;
     false_branch : Statement.t list;
   }
  end

  module While : sig
    type t = {
      condition : Expression.t;
      body: Statement.t list;
    }
  end

  module Let : sig
    type t = Expression.Assign.t list
  end

  module Def : sig
    type t = {
      name : Identifier.t;
      params : Identifier.t list;
      body : Statement.t list;
    }
  end

  type t =
  | If of If.t
  | While of While.t
  | Let of Let.t
  | Def of Def.t
  | Break
  | Continue
  | Return of Expression.t option
  | Expression of Expression.t
  | Null
end = Statement

module rec Program : sig
  type t = Statement.t list
end = Program
