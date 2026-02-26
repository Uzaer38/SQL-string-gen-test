open Query
open Select
open From
open Join
open Condition

let table1 = {
    Name = "table1"
    Alias = Some "t1"
}

let table2 = {
    Name = "table2"
    Alias = Some "t2"
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

    Join = Some [{
        Kind = Inner
        Table2 = table2
        OnCondition = {
            Comparator = Equals
            Var1 = Column column1
            Var2 = Literal "0"
        }
    }]

    Where = Some {
        Comparator = Equals
        Var1 = Column column1
        Var2 = Literal "0"
    }
}

let renderComparator comp =
    match comp with
        | LessThan -> " < "
        | GreaterThan -> " > "
        | Equals -> " = "
        | NotEquals -> " <> "
        | LessThanOrEqual -> " <= "
        | GreaterThanOrEqual -> " >= "

let renderValue value =
    match value with
        | Column col -> col.Name
        | Literal str -> str

let renderAlias alias =
    match alias with
        | Some alias -> " " + alias
        | None -> ""

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
        "\nFROM " + query.From.Table.Name + renderAlias query.From.Table.Alias

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
                    match j.Table2.Alias with
                        | Some alias -> j.Table2.Name + " " + alias
                        | None -> j.Table2.Name
                    +
                    "\n    ON " + query.From.Table.Name + "." + renderValue j.OnCondition.Var1
                                                                    + renderComparator j.OnCondition.Comparator
                                                                        + renderValue j.OnCondition.Var2
                                                                            
                    )
                |> String.concat "\n"
            | None -> ""

    let whereClause = 
        match query.Where with
            | Some condition -> "\nWHERE " + query.From.Table.Name + "." + renderValue condition.Var1
                                                                            + renderComparator condition.Comparator
                                                                                + renderValue condition.Var2
            | None -> ""

    let str = selectClause + fromClause + joinClause + whereClause
    str

printfn "%s" (ToSql query)
System.Console.ReadLine() |> ignore