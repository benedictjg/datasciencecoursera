rankhospital <- function(state,outcome,rank) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[,11] <- suppressWarnings(as.numeric(data[,11]))
    data[,17] <- suppressWarnings(as.numeric(data[,17]))
    data[,23] <- suppressWarnings(as.numeric(data[,23]))
    validoutcomes <- c("heart attack", "heart failure", "pneumonia")
        
    if (!state %in% data$State) {
        stop("invalid state")
    } else if (!outcome %in% validoutcomes) {
        stop("invalid outcome")
    } else {
        if (outcome == "heart attack") {
            statesub <- data[data$State==state,c(2,11)]
            statesub <- statesub[complete.cases(statesub),]
            ordervec <- order(statesub[,2],statesub[,1])
            stateorder <- statesub[ordervec,]
        } else if (outcome == "heart failure") {
            statesub <- data[data$State==state,c(2,17)]
            statesub <- statesub[complete.cases(statesub),]
            ordervec <- order(statesub[,2],statesub[,1])
            stateorder <- statesub[ordervec,]
        } else {
            statesub <- data[data$State==state,c(2,23)]
            statesub <- statesub[complete.cases(statesub),]
            ordervec <- order(statesub[,2],statesub[,1])
            stateorder <- statesub[ordervec,]
        }
    }
    
    if (rank == "best") {
        rank <- 1
    } else if(rank == "worst") {
        rank <- length(stateorder[,1])
    } else {
        rank <-rank
    }
    stateorder[rank,1]
}