module Join
open Select
open From
open Condition

type JoinKind = 
    | Inner
    | LeftOuter
    | RightOuter

type Join = {
    Kind : JoinKind
    Table2 : Table
    OnCondition : Condition
}