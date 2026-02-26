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

let table3 = {
    Name = "table3"
    Alias = Some "t3"
}

let t1column1 = {
        Table = table1
        Name = "column1"
        Alias = Some "c1"
        }

let t1column2 =
        {
        Table = table1
        Name = "column2"
        Alias = Some "c2"
        }

let t2column1 =
        {
        Table = table2
        Name = "column1"
        Alias = Some "c3"
        }

let t3column4 =
        {
        Table = table3
        Name = "column4"
        Alias = Some "c4"
        }

let query1 = {

    Select = {
        Fields = None
    }

    From = {
        Table = table1
    }

    Join = None

    Where = Some [{
        Connector = None
        Comparator = Equals
        Var1 = Column t1column1
        Var2 = Literal "1"
    };
    {
        Connector = Some Or
        Comparator = Equals
        Var1 = Column t1column2
        Var2 = Literal "42"
    };
    {
        Connector = Some And
        Comparator = GreaterThan
        Var1 = Column t1column1
        Var2 = Literal "0"
    }]
}

let query2 = {

    Select = {
        Fields = Some [t1column1; t1column2]
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
            Var1 = Column t1column1
            Var2 = Column t2column1
        }
    };
    {
        Kind = Inner
        Table2 = table3
        OnCondition = {
            Connector = None
            Comparator = Equals
            Var1 = Column t1column1
            Var2 = Column t3column4
        }
    }]

    Where = Some [{
        Connector = None
        Comparator = Equals
        Var1 = Column t1column1
        Var2 = Literal "'Bob'"
    };
    {
        Connector = Some Or
        Comparator = NotEquals
        Var1 = Column t1column2
        Var2 = Literal "0"
    }]
}

let query3 = {

    Select = {
        Fields = Some [t1column1; t1column2; t2column1]
    }

    From = {
        Table = table1
    }

    Join = Some [{
        Kind = LeftOuter
        Table2 = table2
        OnCondition = {
            Connector = None
            Comparator = Equals
            Var1 = Column t1column1
            Var2 = Column t2column1
        }
    };
    {
        Kind = RightOuter
        Table2 = table3
        OnCondition = {
            Connector = None
            Comparator = LessThan
            Var1 = Column t1column1
            Var2 = Column t3column4
        }
    }]

    Where = Some [{
        Connector = None
        Comparator = Equals
        Var1 = Column t1column1
        Var2 = Column t1column2
    };
    {
        Connector = Some And
        Comparator = GreaterThan
        Var1 = Column t1column1
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
                "\n"
                +
                (joins
                |> List.map(fun j ->
                    match j.Kind with
                        | Inner -> "INNER JOIN "
                        | LeftOuter -> "LEFT OUTER JOIN "
                        | RightOuter -> "RIGHT OUTER JOIN "
                    +
                    j.Table2.Name + renderAlias j.Table2.Alias
                    +
                    "\n    ON " 
                    + renderConnector j.OnCondition.Connector
                    + renderValue j.OnCondition.Var1 query
                    + renderComparator j.OnCondition.Comparator
                    + renderValue j.OnCondition.Var2 query
                                                                            
                    )
                |> String.concat "\n")
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