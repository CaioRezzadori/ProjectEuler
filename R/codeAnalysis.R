library(profvis)
library(htmlwidgets)
var <- profvis::profvis(longestCycle(1e4))
saveWidget(var, "var.html")