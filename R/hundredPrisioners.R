randomVector <- function(size, seed = NULL){
    if(!is.null(seed)){
        set.seed(seed)
    }
    return(sample(1:size))
}

randomMethod <- function(drawerSeed = NULL, prisionersSeed = NULL){
    drawers <- randomVector(100, seed = drawerSeed)
    prisionerNumber <- 1
    nPrisionersApart <- 0

    while(prisionerNumber <= 100){
        prisionerChoices <- randomVector(100,
                                seed = prisionersSeed[prisionerNumber])[1:50]

        if(prisionerNumber %in% drawers[prisionerChoices]){
            nPrisionersApart <- nPrisionersApart + 1
        }

        prisionerNumber <- prisionerNumber + 1
    }
    return(nPrisionersApart)
}

optimalMethod <- function(drawerSeed = NULL,
                          prisionersSeed = NULL){

    drawers <- randomVector(100, seed = drawerSeed)
    prisionerNumber <- 1
    nPrisionersApart <- 0
    while(prisionerNumber <= 100){
        currentDrawer <- drawers[prisionerNumber]
        j <- 1
        while(j <= 50){
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
                             drawerSeed = NULL,
                             prisionersSeed = NULL){
    i <- 1
    result <- integer(nExperiments)
    while(i <= nExperiments){
        result[i] <- match.fun(experiment)(drawerSeed = drawerSeed[i],
                                           prisionersSeed = prisionersSeed[i,])
        i <- i + 1
    }
    return(result)
}

probabilityCalc <- function(result){
    successExperiments <- result[result == 100]
    probSuccess <- length(successExperiments)/length(result)
    return(probSuccess)
}


optimalMethod()

nExperiments <- 5000

set.seed(2025)
drawerSeed2 <- runif(nExperiments, min = 1, max = 100)
prisionersSeed2 <- round(matrix(runif(nExperiments*100,
                                     min = 1,
                                     max = 1000),
                               nrow = nExperiments,
                               ncol = 100))

resultRandomMethod <- repeatExperiment(experiment = "randomMethod",
                                       nExperiments = nExperiments,
                                       drawerSeed = drawerSeed,
                                       prisionersSeed = prisionersSeed)

mean(resultRandomMethod)
probabilityCalc(resultRandomMethod)



resultOptimalMethod <- repeatExperiment(experiment = "optimalMethod",
                                       nExperiments = nExperiments,
                                       drawerSeed = drawerSeed,
                                       prisionersSeed = prisionersSeed)
mean(resultOptimalMethod)

probabilityCalc(resultOptimalMethod)
# mean(resultRandomMethod)






