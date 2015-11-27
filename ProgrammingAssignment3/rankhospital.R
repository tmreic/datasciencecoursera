rankhospital <- function(state, outcome, num= "best") {
  
  
  ## Read outcome data
  FileData <- read.csv("outcome-of-care-measures.csv",header = TRUE)
  #str(FileData)
  
  ## Check that state and outcome are valid
  
  #Error in best("BB", "heart attack") : invalid state
  nr <- FileData[, 7] 
  s <-- is.element(state,nr)
  if  ( s == -1)
  {   #valid sate do nothing
  } 
  else
  {
    #x = cat( paste('Error in best("', state,'","',outcome,'") :invalid state', collapse = '', sep = '') )
     stop('invalid state')
    }
  
  #Error in best("NY", "hert attack") : invalid outcome
  st = outcome
  outcomestr = gsub('[[:space:]]', '.', st)
    #print(outcomestr)
  o<-- grep(outcomestr,colnames(FileData), ignore.case = TRUE)
    #print(length(o))
  if  ( length(o) > 0 )
  {   #valid outcome
  } 
  else
  { 
    stop('invalid outcome')
    }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
if (toupper( outcome) == 'HEART ATTACK' ){
            df<- read.csv("outcome-of-care-measures.csv",header = TRUE,stringsAsFactors = FALSE)[ ,c('State','Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')] 
            
            FileDataByState <-subset(df, df$State == state & df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack !="Not Available",select= Hospital.Name:Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack )
            FileDataByState[, 2]  <- as.numeric(FileDataByState[, 2]) 
            
            a = aggregate(FileDataByState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, list(FileDataByState$Hospital.Name), mean)
            newdata <- a[order( a$x,a$Group.1),] 
            
            if (num == 'worst') {
              
               num = dim(newdata)[1]
               v0= num
            } else{
                v0=num
            }
            
            if ( num <= dim(newdata)[1]) { 
                for (i in 1:dim(newdata)[1]) {
                  #v<--   newdata[v0,2]
                  #print(v)
                  if (i==v0){
                    #print(i)
                   return(newdata[v0,1])
                    break
                    }
                  
                }
            } else {print("NA")}
           
} else if  (toupper( outcome) == 'HEART FAILURE' ){
  
  #Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  #Hospital.30.Day.Readmission.Rates.from.Pneumonia
  
            df<- read.csv("outcome-of-care-measures.csv",header = TRUE,stringsAsFactors = FALSE)[ ,c('State','Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')] 
            FileDataByState <-subset(df, df$State == state & df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure !="Not Available",select= Hospital.Name:Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure )
            FileDataByState[, 2]  <- as.numeric(FileDataByState[, 2]) 
            
            a = aggregate(FileDataByState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, list(FileDataByState$Hospital.Name), mean)
            newdata <- a[order( a$x,a$Group.1),] 
           
            if (num == 'worst') {
              
              num = dim(newdata)[1]
              v0= num
            } else{
              v0=num
            }
            
            if ( num <= dim(newdata)[1]) { 
              for (i in 1:dim(newdata)[1]) {
                #v<--   newdata[v0,2]
                #print(v)
                if (i==v0){
                  #print(i)
                  return(newdata[v0,1])
                  break
                }
                
              }
            } else {print("NA")}
  
} else if  (toupper( outcome) == 'PNEUMONIA' ){
  
  #Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  #Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia 
  
  df<- read.csv("outcome-of-care-measures.csv",header = TRUE,stringsAsFactors = FALSE)[ ,c('State','Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')] 
  
            FileDataByState <-subset(df, df$State == state & df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia  !="Not Available",select= Hospital.Name:Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia  )
            FileDataByState[, 2]  <- as.numeric(FileDataByState[, 2]) 
            
            a = aggregate(FileDataByState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia , list(FileDataByState$Hospital.Name), mean)
            newdata <- a[order( a$x,a$Group.1),] 
           
             if (num == 'worst') {
              
              num = dim(newdata)[1]
              v0= num
            } else{
              v0=num
            }
            
            if ( num <= dim(newdata)[1]) { 
              for (i in 1:dim(newdata)[1]) {
                #v<--   newdata[v0,2]
                #print(v)
                if (i==v0){
                  #print(i)
                  return(newdata[v0,1])
                  break
                }
                
              }
            } else {return("NA")}
}






  
}