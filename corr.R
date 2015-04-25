corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of 
  ## length 1 indicating the location of the CSV files
  ## 'threshold' is a numeric vector of 
  ## length 1 indicating the number of completely 
  ## observed observations (on all
  ## variables) required to compute the correlation 
  ## between nitrate and sulfate; the default is 0
  ## Return a numeric vector of correlations
        source("complete.R")
        files <- list.files(path = directory, full.names = TRUE)
  ## creating a list of all the CSV files in the directory

        comp <- complete(directory)
  ## initializing and populating a data frame of the
  ## id and nobs of the complete cases using our
  ## complete() function created earlier

        above <- comp[comp$nobs > threshold, 1]
  ## creating a vector of the id's that have nobs
  ## above the needed threshold

        corr <- rep_len(NA, length(above))
  ## initializing a vector of NAs        
        for (i in above){
                data <- (read.csv(files[i]))
  ## creating a data frame of the ids from the
  ## above threshold group
                comp <- complete.cases(data)
  ## creating a logical vector of complete cases
                sulfate <- data[comp,2]
  ## creating a vector of the sulfate data from
  ## complete cases
                nitrate <- data [comp, 3]
  ## creating a vector of the nitrate data from
  ## complete cases
                corr[i] <- cor(x = sulfate, y = nitrate)
        }
      corr <- corr[complete.cases(corr)]
}