module Query
open Select
open From
open Join
open Condition

type Query = {
    Select : Select option
    From : From
    Join : Join list option
    Where : Condition list option
}