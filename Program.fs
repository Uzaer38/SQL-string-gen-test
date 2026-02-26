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
            Connector = None
            Comparator = Equals
            Var1 = Column column1
            Var2 = Column column2
        }
    }]

    Where = Some [{
        Connector = None
        Comparator = Equals
        Var1 = Column column1
        Var2 = Column column2
    };
    {
        Connector = Some And
        Comparator = GreaterThan
        Var1 = Column column1
        Var2 = Literal "0"
    }]
}

let renderConnector con =
    match con with
        | Some con ->
            match con with
                | And -> "AND "
                | Or -> "OR "
        | None -> ""

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

let renderAliasOrName alias name =
    match alias with
        | Some alias -> alias
        | None -> name

let ToSql query =
    let fields =
        match query.Select with
            | Some select -> 
                select.Fields
                |> List.map (fun col -> col.Name)
                |> String.concat ", "
            | None -> "*"

    let selectClause = "\nSELECT " + fields

    let fromClause =
        "\nFROM " + query.From.Table.Name + renderAlias query.From.Table.Alias

    let joinClause = 
        match query.Join with
            | Some joins ->
                joins
                |> List.map(fun j ->
                    match j.Kind with
                        | Inner -> "\nINNER JOIN "
                        | LeftOuter -> "\nLEFT OUTER JOIN "
                        | RightOuter -> "\nRIGHT OUTER JOIN "
                    +
                    j.Table2.Name + renderAlias j.Table2.Alias
                    +
                    "\n    ON " 
                    + renderConnector j.OnCondition.Connector
                    + renderAliasOrName query.From.Table.Alias query.From.Table.Name  + "." 
                    + renderValue j.OnCondition.Var1
                    + renderComparator j.OnCondition.Comparator
                    + renderAliasOrName query.From.Table.Alias query.From.Table.Name  + "." 
                    + renderValue j.OnCondition.Var2
                                                                            
                    )
                |> String.concat "\n"
            | None -> ""

    let whereClause = 
        match query.Where with
            | Some conditions
                -> "\nWHERE "
                    + (conditions
                        |> List.map(fun c ->
                            renderConnector c.Connector
                            + renderAliasOrName query.From.Table.Alias query.From.Table.Name + "."
                            + renderValue c.Var1
                            + renderComparator c.Comparator
                            + renderAliasOrName query.From.Table.Alias query.From.Table.Name + "."
                            + renderValue c.Var2
                            )
                        |> String.concat "\n    "
                    )
            | None -> ""

    let str = selectClause + fromClause + joinClause + whereClause
    str

printfn "%s" (ToSql query)
System.Console.ReadLine() |> ignore