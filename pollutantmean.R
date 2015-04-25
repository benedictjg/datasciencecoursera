pollutantmean <- function(directory = 'specdata', pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files.
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used.

        files <- list.files(path = directory, full.names = TRUE)
  ## creating a list where each element are the CSV files
  ## full names are needed so the path stays specified when
  ## creating the data fram below.

        data <- data.frame()
  ## initializing an empty dataframe
        for (i in id){
                data <- rbind(data, read.csv(files[i]))    
        }
  ## populating the data frame with rows made up of each
  ## CSV file
        if (pollutant == "sulfate"){
                mean(data$sulfate, na.rm = TRUE)
        } else if (pollutant == "nitrate"){
                mean(data$nitrate, na.rm = TRUE)
        }
  ## finding means of sulfate or nitrate as needed
}