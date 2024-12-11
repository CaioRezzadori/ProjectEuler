library(profvis)
library(htmlwidgets)
var <- profvis::profvis(longestCycle(1e3))
saveWidget(var, "var.html")