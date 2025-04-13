doorsProblem <- function(nDoors){
    doorIndex <- 1:nDoors
    doorsState <- integer(nDoors)
    currentPass <- 2

    while(currentPass <= 100){
        mask <- (doorIndex %% currentPass == 0)
        doorsState[mask] <- doorsState[mask] + 1
        currentPass <- currentPass + 1
    }
    openDoors = which(doorsState %% 2 == 0)
    return(list(openDoors = openDoors,
                closedDoors = doorIndex[!(doorIndex %in% openDoors)]))
}