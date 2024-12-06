library(profvis)
library(htmlwidgets)
var <- profvis::profvis(fibonacciIndexLA(1e3))
saveWidget(var, "var.html")