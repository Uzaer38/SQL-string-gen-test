module Select
open From

type Column = {
    Table : Table
    Name : string
    Alias : string option
}

type Select = {
    Fields : Column list
}