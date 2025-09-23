shapes<- coo_object
shapes$fac

shapes[1] %>% head()

shp<- shapes[4]
coo_plot(shp)

coo_oscillo(shapes[1], "efourier")

shape.f<- efourier(shapes, nb.h = 3)
