cryptoSquare <- function(sentence){
    # Removing special characters
    arraySentence <- strsplit(iconv(tolower(sentence),
                              from = "UTF-8",
                              to = "ASCII//TRANSLIT"), "")[[1]]

    normalizedSentence <- arraySentence[arraySentence %in% letters]
    lenSentence <- length(normalizedSentence)

    c <- ceiling(sqrt(lenSentence))
    r <- floor(sqrt(lenSentence))

    spaceComplete <- rep(" ", c*r - lenSentence)

    chunks <- matrix(c(normalizedSentence, spaceComplete),
                     nrow = r,
                     ncol = c,
                     byrow = TRUE)

    output <- apply(t(chunks), 1, function(x) paste0(x, collapse = ""))

    return(matrix(output, nrow = c))
}

sentence <- "If man was meant to stay on the ground, god would have given us roots."
cryptoSquare(sentence)
