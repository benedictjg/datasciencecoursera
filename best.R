best <- function(state, outcome) {
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
            min <- min(statesub[,2],na.rm=T)
            min <- which(statesub[,2] == min)
        } else if (outcome == "heart failure") {
            statesub <- data[data$State==state,c(2,17)]
            min <- min(statesub[,2],na.rm=T)
            min <- which(statesub[,2] == min)
        } else {
            statesub <- data[data$State==state,c(2,23)]
            min <- min(statesub[,2],na.rm=T)
            min <- which(statesub[,2] == min)
        }
    }
    statesub[min,1]
}