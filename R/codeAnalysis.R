library(profvis)
library(htmlwidgets)
var <- profvis::profvis(permuteDigits(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 1e6))
saveWidget(var, "var.html")