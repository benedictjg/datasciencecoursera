complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
        files <- list.files(path = directory, full.names = TRUE)
  ## creating a list of all CSV files with path

        data <- data.frame()
        compcases <- data.frame()
        nobs <- data.frame();
  ## initializing empty data frames

        for (i in id) { 
                data <- (read.csv(files[i]))
  ## populating data frame with needed CSV files

                nobs <- sum(complete.cases(data))
  ## calculating the number of complete cases

                compcases <- rbind(compcases, data.frame(i,nobs))
  ## creating final data frame with id & nobs
  }
        colnames(compcases) <- c("id","nobs")
  ## resetting the column names to "id" and "nobs"
  
        compcases
  ## printing data frame
}