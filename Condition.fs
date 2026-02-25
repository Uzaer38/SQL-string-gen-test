module Condition
open Select
open From

type Comparator =
    | LessThan
    | GreaterThan
    | Equals
    | NotEquals
    | LessThanOrEqual
    | GreaterThanOrEqual

type Value =
    | Column of Column
    | Literal of string

type Condition = {
    Comparator : Comparator
    Var1 : Value
    Var2 : Value
}