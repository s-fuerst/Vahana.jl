dump(p1)

Meta.show_sexpr(p1)

ex = Meta.parse("struct foo something::Int64 end")

Meta.show_sexpr(ex)
