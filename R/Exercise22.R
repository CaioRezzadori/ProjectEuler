readData__ <- function(filepath){
    data <- read.table(
                file = filepath,
                sep = ",",
                header = FALSE)

    data <- unlist(data, use.names = FALSE)
    data[is.na(data)] <- "NA"

    return(data[order(data)])
}

alphabetWeights__ <- function(){
    alphabet <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    alphabet <- strsplit(alphabet, "")[[1]]
    return(data.frame(Letter = alphabet,
                      Weight = order(alphabet)))
}

library(dplyr)
scoreCalculation__ <- function(name, index){
    alphabetWeights <- alphabetWeights__()
    nameWeights <- strsplit(name, "")[[1]]
    nameWeights <- data.frame(Letter = nameWeights) %>%
                                        dplyr::left_join(alphabetWeights,
                                                         by = "Letter")
    score <- nameWeights %>%
                        dplyr::pull(Weight) %>%
                        sum()
    return(score*index)
}

scoreSums <- function(data){
    sum <- 0
    i <- 1
    while(i <= length(data)){
        sum <- sum + scoreCalculation__(name = data[i], index = i)
        i <- i + 1
    }
    return(sum)
}

filepath <- "/home/caio/Documentos/github/ProjectEuler/data/Exercise22.txt"

data <- readData__(filepath)



library(profvis)
library(htmlwidgets)
var <- profvis::profvis(scoreSums(data))
saveWidget(var, "var.html")

