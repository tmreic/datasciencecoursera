corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  corr_values = numeric()
  lf <- list.files(path=directory, full.names=TRUE)
  
  for ( file_name in lf){
  
    newdata <- read.csv(file_name, header=TRUE)
    cc <- newdata[complete.cases(newdata),]
   #print( nrow(cc))
   
    if (nrow(cc) > threshold){
      #print(TRUE)
      cv<- cor(newdata$sulfate , newdata$nitrate, use="complete.obs")
      corr_values <- c(corr_values, cv)
    } else {
      #print(FALSE)
    }
    
  }
  
  return(corr_values)
  
  
}