rankall <- function(outcome,rank = 1) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
    ##data is now read into R with only 5 columns
    ##col1 = state; col2 = Name; col3 = heart attack; col4 = heart failure
    ##col5 = pneumonia
    
    ##changing the mortality characters to numerics and suppressing warnings
    data[,3] <- suppressWarnings(as.numeric(data[,3]))
    data[,4] <- suppressWarnings(as.numeric(data[,4]))
    data[,5] <- suppressWarnings(as.numeric(data[,5]))
    
    ##checking valid outcomes and stopping if not valid
    validoutcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% validoutcomes) {
        stop("invalid outcome")
    } else {
    ##subsetting data by outcome type and renaming 3rd column to simple name
    if (outcome == "heart attack") {
        data <- data[,c(1,2,3)]
    } else if (outcome == "heart failure") {
        data <- data[,c(1,2,4)]
    } else {
        data <- data[,c(1,2,5)]
    }
    }
    names(data)[3] = "Rate"
    
    ##removing NAs
    data <- data[!is.na(data$Rate),]
    
    ##Ordering all data by State, then Rate, then Name
    data <- data[order(data$State,data$Rate,data$Hospital.Name),]
    
    ##splitting table into a list of data frames by state
    splitted <- split(data,data$State)
    
    ##Gathering the needed rows
    if (rank == "best") {
        rank <- 1
        rows <- lapply(splitted,function(x) x[rank,])
    } else if (rank == "worst") {
        rows <- lapply(splitted,function(x) x[nrow(x),])
    } else {
        rank <- rank
        rows <- lapply(splitted, function(x) x[rank,])
    }
    
    ##Making one solid data frame from the extracted rows
    output <- do.call(rbind, rows)
    
    ##Subsetting DF into just hospital name and state abbrev
    output <- output[,c(1,2)]
    
    ##re-inputting lost state abbreviations for NA's
    output[,2] <- rownames(output)
    
    ##fixing column names
    colnames(output) <- c("hospital","state")
    return(output)
    
}