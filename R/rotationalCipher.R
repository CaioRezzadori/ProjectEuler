ROT <- function(key, sentence){
    Letters <- c(letters, LETTERS)
    arraySentence <- strsplit(sentence, "")[[1]]
    mask <- arraySentence %in% Letters
    index <- sapply(arraySentence[mask], function(x) which(x == Letters), USE.NAMES = FALSE)

    arraySentence[mask] <- Letters[(index + key - 1) %% 26 + 26*floor(index/27) + 1]
    return(paste0(arraySentence, collapse = ""))
}

ROT(13, "abcdefghijklmnopqrstuvwxyz")
ROT(5, "omg")
ROT(0, "c")
ROT(26, "Cool")
ROT(13, "The quick brown fox jumps over the lazy dog.")
ROT(13, "Gur dhvpx oebja sbk whzcf bire gur ynml qbt.")