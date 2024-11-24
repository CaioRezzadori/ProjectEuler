readData__ <- function(filepath){
    data <- readLines(filepath)

    data <- unlist(strsplit(data, ","))
    data <- gsub("\"", "", data)

    return(data[order(data)])
}

scoreCalculation__ <- function(name, index){
    alphabet <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    alphabet <- unlist(strsplit(alphabet, ""))
    nameLetters <- unlist(strsplit(name, ""))
    letterWeights <- unlist(
                        Map(function(x) which(x == alphabet), nameLetters))
    return(sum(letterWeights)*index)
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

scoreSums(data)

