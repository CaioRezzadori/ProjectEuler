timeMeasure <- function(fun, param, nRep = 1){
    timeVec <- c()
    for(i in 1:nRep){
        timeDiff <- Sys.time()
        do.call(fun, param)
        timeDiff <- Sys.time() - timeDiff
        timeVec <- c(timeVec, timeDiff)
    }
    # timeDiff <- Sys.time()
    # do.call(fun, param)
    # timeDiff <- Sys.time() - timeDiff
    return(mean(timeVec))
}

timeComparision <- function(fun1, fun2, param, nRep){
    timeFun1 <- timeMeasure(fun1, param, nRep)
    timeFun2 <- timeMeasure(fun2, param, nRep)
    cat(paste("\n",
    deparse(substitute(fun1)), "execution time: ",
                                    timeFun1, "\n",
    deparse(substitute(fun2)), "execution time: ",
                                    timeFun2, "\n",
    "Time difference:",
                timeFun1 - timeFun2))
}