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

  (WD <- getwd())
  X <- paste(WD, directory, collapse = '', sep = '/')
  setwd(X)
  nobs = numeric()
  for (i in id){
    if(i < 10){
      
      csvfile <- paste("00", i,".csv", collapse = '', sep = '')
      
    } else if ( i >9 & i < 100) {
      csvfile <- paste("0", i,".csv", collapse = '', sep = '')
    } else {
      csvfile <- paste( i,".csv", collapse = '', sep = '')
    }
    
    newdata = read.csv(csvfile,header = TRUE )
    nobs = c(nobs, sum(complete.cases(newdata)))
    
    
  }
  setwd(WD)
  return(data.frame(id, nobs))
}