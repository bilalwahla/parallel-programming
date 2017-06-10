// Operations on collections are key to functional programming
val x = List(1, 3, 8)

// map
x.map(x => x * x)

// fold
x.fold(100)((x, s) => x + s)

// scan
x.scan(100)((x, s) => x + s)