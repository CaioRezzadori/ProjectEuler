randomMethod <- function(size = 100,
                         nTrials = 50,
                         drawerSeed = NULL,
                         prisionersSeed = NULL){

    set.seed(drawerSeed)
    drawers <- sample(1:size)

    prisionerNumber <- 1
    nPrisionersApart <- 0

    while(prisionerNumber <= size){
        set.seed(prisionersSeed[prisionerNumber])
        prisionerChoices <- sample(1:size, nTrials , replace = FALSE)

        if(prisionerNumber %in% drawers[prisionerChoices]){
            nPrisionersApart <- nPrisionersApart + 1
        }

        prisionerNumber <- prisionerNumber + 1
    }
    return(nPrisionersApart)
}

optimalMethod <- function(size = 100,
                          nTrials = 50,
                          drawerSeed = NULL,
                          prisionersSeed = NULL){

    set.seed(drawerSeed)
    drawers <- sample(1:size)

    prisionerNumber <- 1
    nPrisionersApart <- 0

    while(prisionerNumber <= size){
        currentDrawer <- drawers[prisionerNumber]
        j <- 1
        while(j <= nTrials){
            if(drawers[currentDrawer] == prisionerNumber){
                nPrisionersApart <- nPrisionersApart + 1
                break
            }
            currentDrawer <- drawers[currentDrawer]
            j <- j + 1
        }
        prisionerNumber <- prisionerNumber + 1
    }
    return(nPrisionersApart)
}

repeatExperiment <- function(experiment,
                             nExperiments,
                             size = 100,
                             nTrials = 50,
                             experimentSeed = NULL){
    # Generating seeds (reproducible experiments)
    if(!is.null(experimentSeed)){
        set.seed(experimentSeed)
        drawerSeed <- runif(nExperiments, min = 1, max = 99999)
        prisionersSeed <- matrix(runif(nExperiments*size,
                                            min = 1,
                                            max = 99999),
                                    nrow = nExperiments,
                                    ncol = size)
    } else{
        drawerSeed <- prisionersSeed <- NULL
    }
    i <- 1
    result <- integer(nExperiments)
    while(i <= nExperiments){
        result[i] <- match.fun(experiment)(size = size,
                                        nTrials = nTrials,
                                        drawerSeed = drawerSeed[i],
                                        prisionersSeed = prisionersSeed[i,])

        i <- i + 1
    }
    return(result)
}

probabilityCalc <- function(result, size = 100){
    successExperiments <- result[result == size]
    probSuccess <- length(successExperiments)/length(result)
    return(probSuccess)
}


### RUNING EXPERIMENTS ###
##############################################################
nExperiments <- 1e5
# start.time <- Sys.time()
resultRandomMethod <- repeatExperiment(experiment = "randomMethod",
                                       nExperiments = nExperiments,
                                       experimentSeed = 2025,
                                       size = 100,
                                       nTrials = 50)
# end.time <- Sys.time()
# diff.time <- end.time - start.time
probabilityCalc(resultRandomMethod, size = 100)

mean(resultRandomMethod)

###############################################################
nExperiments <- 1e5
resultOptimalMethod <- repeatExperiment(experiment = "optimalMethod",
                                       nExperiments = nExperiments,
                                       experimentSeed = 2025,
                                       size = 100,
                                       nTrials = 50)

probabilityCalc(resultOptimalMethod, size = 100)

mean(resultOptimalMethod)


# start.time <- Sys.time()

# print(diff.time)
