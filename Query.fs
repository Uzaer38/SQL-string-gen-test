module Query
open Select
open From
open Join
open Condition

type Query = {
    Select : Select
    From : From
    Join : Join list option
    Where : Condition list option
}