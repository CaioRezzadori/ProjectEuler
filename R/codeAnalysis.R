library(profvis)
library(htmlwidgets)
var <- profvis::profvis(fibonacciIndex(1e4))
saveWidget(var, "var.html")