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

type Condition = {
    Comparator : Comparator
    Var1 : Column
    Var2 : Column
}