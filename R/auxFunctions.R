timeMeasure <- function(fun, param){
    timeDiff <- Sys.time()
    do.call(fun, param)
    timeDiff <- Sys.time() - timeDiff
    return(timeDiff)
}

timeComparision <- function(fun1, fun2, param){
    cat(paste("\n",
    deparse(substitute(fun1)), "execution time: ",
                                    timeMeasure(fun1, param), "\n",
    deparse(substitute(fun2)), "execution time: ",
                                    timeMeasure(fun2, param), "\n",
    "Time difference:",
                timeMeasure(fun1, param) - timeMeasure(fun2, param), "\n"))
}