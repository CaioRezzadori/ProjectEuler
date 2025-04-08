cryptoSquare <- function(sentence){
    # Removing special characters
    arraySentence <- unlist(strsplit(iconv(tolower(sentence),
                              from = "UTF-8",
                              to = "ASCII//TRANSLIT"), ""))

    normalizedSentence <- arraySentence[arraySentence %in% letters]
    lenSentence <- length(normalizedSentence)


    # Need to correct cases when c*r < lenSequence^2
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
paste0(cryptoSquare(sentence)[,1], collapse = " ") == "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "
