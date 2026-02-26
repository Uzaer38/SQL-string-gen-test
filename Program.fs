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

let column3 =
        {
        Table = table2
        Name = "column2"
        Alias = Some "cc"
        }

let query1 = {

    Select = {
        Fields = Some [column1; column2]
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
            Var2 = Column column3
        }
    }]

    Where = Some [{
        Connector = None
        Comparator = Equals
        Var1 = Column column1
        Var2 = Column column3
    };
    {
        Connector = Some And
        Comparator = GreaterThan
        Var1 = Column column1
        Var2 = Literal "0"
    }]
}

let query2 = {

    Select = {
        Fields = Some [column1; column2]

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

let query3 = {

    Select = {
        Fields = Some [column1; column2]
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

let renderAliasOrName alias name =
    match alias with
        | Some alias -> alias
        | None -> name

let renderValue value query =
    match value with
        | Column col -> renderAliasOrName col.Table.Alias col.Table.Name + "." + col.Name
        | Literal str -> str

let renderAlias alias =
    match alias with
        | Some alias -> " " + alias
        | None -> ""

let ToSql query =
    let fields =
        match query.Select.Fields with
            | Some fields -> 
                fields
                |> List.map (fun col -> match col.Alias with
                                        | Some alias -> renderAliasOrName col.Table.Alias col.Table.Name + "." + col.Name + " AS " + alias
                                        | None -> renderAliasOrName col.Table.Alias col.Table.Name + "." + col.Name)
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
                    + renderValue j.OnCondition.Var1 query
                    + renderComparator j.OnCondition.Comparator
                    + renderValue j.OnCondition.Var2 query
                                                                            
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
                            + renderValue c.Var1 query
                            + renderComparator c.Comparator
                            + renderValue c.Var2 query
                            )
                        |> String.concat "\n    "
                    )
            | None -> ""

    let str = selectClause + fromClause + joinClause + whereClause
    str

printfn "%s" (ToSql query1)
printfn "%s" (ToSql query2)
printfn "%s" (ToSql query3)