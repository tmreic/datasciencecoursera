pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  #  pollutantmean(directory,pollutant,id)
  (WD <- getwd())
  X <- paste(WD, directory, collapse = '', sep = '/')
  setwd(X)
  alldata = numeric()
  for (i in id){
                 if(i < 10){
                  
                   csvfile <- paste("00", i,".csv", collapse = '', sep = '')
                   
                 } else if ( i >9 & i < 100) {
                   csvfile <- paste("0", i,".csv", collapse = '', sep = '')
                 } else {
                   csvfile <- paste( i,".csv", collapse = '', sep = '')
                 }
       
       newdata = read.csv(csvfile )
       
       alldata = c(alldata, newdata[[pollutant]]) 

                 
  }
  setwd(WD)

  return(mean(alldata, na.rm = TRUE))
}

