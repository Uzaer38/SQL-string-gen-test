module From

type Table = {
    Name : string
    Alias : string option
}

type From = {
    Table : Table
}