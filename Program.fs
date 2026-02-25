open Query
open Select
open From
open Join
open Condition

let table1 = {
    Name = "table1"
    Alias = Some "t1"
}

let column1 = {
        Table = table1
        Name = "column1"
        Alias = Some "c1"
        }

let column2 =
        {
        Table = table1
        Name = "column2"
        Alias = Some "c2"
        }

let query = {

    Select = Some {

        Fields = [column1; column2]

    }

    From = {
        Table = table1
    }

    Join = None

    Where = Some {
        Comparator = Equals
        Var1 = Column column1
        Var2 = Literal "0"
    }
}

let ToSql query =
    let fields =
        match query.Select with
            | Some select -> 
                select.Fields
                |> List.map (fun col -> col.Name)
                |> String.concat ", "
            | None -> ""

    let selectClause =
        match query.Select with
            | Some select -> "\nSELECT " + fields
            | None -> "\nSELECT *"

    let fromClause = 
        match query.From.Table.Alias with
            | Some alias -> "\nFROM " + query.From.Table.Name + " " + alias
            | None -> "\nFROM " + query.From.Table.Name

    let joinClause = 
        match query.Join with
            | Some join ->
                join
                |> List.map(fun j ->
                    match j.Kind with
                        | Inner -> "\nINNER JOIN "
                        | LeftOuter -> "\nLEFT OUTER JOIN "
                        | RightOuter -> "\nRIGHT OUTER JOIN "
                    +
                    match query.From.Table.Alias with
                        | Some alias -> alias
                        | None -> query.From.Table.Name
                    )
                |> String.concat "\n"
            | None -> ""

    let whereClause = 
        match query.Where with
            | Some condition -> "\nWHERE " + match condition.Var1 with
                                                | Column col -> col.Name
                                                | Literal str -> str 
                                                + match condition.Comparator with
                                                    | LessThan -> " < "
                                                    | GreaterThan -> " > "
                                                    | Equals -> " == "
                                                    | NotEquals -> " != "
                                                    | LessThanOrEqual -> " <= "
                                                    | GreaterThanOrEqual -> " >= "
                                                    + match condition.Var2 with
                                                        | Column col -> col.Name
                                                        | Literal str -> str
            | None -> ""

    let str = selectClause + fromClause + joinClause + whereClause
    str

printfn "%s" (ToSql query)
System.Console.ReadLine() |> ignore