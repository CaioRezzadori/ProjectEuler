library(profvis)
library(htmlwidgets)
var <- profvis::profvis(sum(numbersNotSumOfAbundant(28123)))
saveWidget(var, "var.html")