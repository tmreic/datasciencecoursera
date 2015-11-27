rankall <- function( outcome, num= "best") {
  options(warn = -1)
  
  ## Read outcome data
  FileData <- read.csv("outcome-of-care-measures.csv",header = TRUE)
  #str(FileData)
  
  ## Check that state and outcome are valid
  
#   #Error in best("BB", "heart attack") : invalid state
#   nr <- FileData[, 7] 
#   s <-- is.element(state,nr)
#   if  ( s == -1)
#   {   #valid sate do nothing
#   } 
#   else
#   {
#     #x = cat( paste('Error in best("', state,'","',outcome,'") :invalid state', collapse = '', sep = '') )
#      stop('invalid state')
#     }
  
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
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
 

if (toupper( outcome) == 'HEART ATTACK' ){
  
            df<- read.csv("outcome-of-care-measures.csv",header = TRUE,stringsAsFactors = FALSE)[ ,c('State','Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')] 
            
            FileDataByState <-subset(df,select= State:Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack )
            FileDataByState[, 3]  <- as.numeric(FileDataByState[, 3]) 
            
            #use number of states to drive for loop & use state data
            uniqueStates<- unique(FileDataByState$State)
            usl = length (uniqueStates)
            
            #dataframe to hold data 
            final.data <- data.frame(state1 = character() , hospital = character(),state= character(),stringsAsFactors=FALSE )
               
                for (i in 1:usl) {
                  
                  # subset data and remove hospitials with no data available. 
                  FS <-subset(FileDataByState, FileDataByState$State == uniqueStates[i] & FileDataByState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack !="Not Available",select= Hospital.Name:Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack )
                  
                  # order by values
                  FSS <- FS[order( FS$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),] 
                 
                  # take means and order by smallest value
                  afs  = aggregate(FSS$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, list(FSS$Hospital.Name), mean)
                  safs <- afs[order( afs$x),] 
                  
                  # rank setup.. best =1 , worst = number of records in state, else number
                  if (num == 'worst'){
                    
                    x=nrow(safs)
                  } else if (num == 'best') 
                  {x = 1}
                  else
                  {x = num}
                  #add the data record with right rank to data frame
                  final.data[nrow( final.data) + 1, ] <- c( uniqueStates[i], safs[x,1],uniqueStates[i] )
                  
                  
                }
           
            # rename first column to null and order by state column
            names(final.data)[names(final.data)=="state1"] <- ""
            final.data <- final.data[order( final.data$state),] 
            return (final.data )
            
              
            
            
            
           
} else if  (toupper( outcome) == 'HEART FAILURE' ){
  
  #Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure

  
            df<- read.csv("outcome-of-care-measures.csv",header = TRUE,stringsAsFactors = FALSE)[ ,c('State','Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')] 
            FileDataByState <-subset(df,select= State:Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure )
            FileDataByState[, 3]  <- as.numeric(FileDataByState[, 3]) 
            
            #use number of states to drive for loop & use state data
            uniqueStates<- unique(FileDataByState$State)
            usl = length (uniqueStates)
            
            #dataframe to hold data 
            final.data <- data.frame(state1 = character() , hospital = character(),state= character(),stringsAsFactors=FALSE )
             
            for (i in 1:usl) {
             
              # subset data and remove hospitials with no data available. 
              FS <-subset(FileDataByState, FileDataByState$State == uniqueStates[i] & FileDataByState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure !="Not Available",select= Hospital.Name:Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure )
              
              # order by values
              FSS <- FS[order( FS$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),] 
             
              # take means and order by smallest value
              afs  = aggregate(FSS$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, list(FSS$Hospital.Name), mean)
              safs <- afs[order( afs$x),] 
               
              # rank setup.. best =1 , worst = number of records in state, else number
              if (num == 'worst'){
                
                x=nrow(safs)
              } else if (num == 'best') 
              {x = 1}
              else
              {x = num}
              #add the data record with right rank to data frame
              final.data[nrow( final.data) + 1, ] <- c( uniqueStates[i], safs[x,1],uniqueStates[i] )
               
            }
             
             # rename first column to null and order by state column
             names(final.data)[names(final.data)=="state1"] <- ""
             final.data <- final.data[order( final.data$state),] 
              
             return (final.data)
           

  
} else if  (toupper( outcome) == 'PNEUMONIA' ){
  
  #Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia 
  
          df<- read.csv("outcome-of-care-measures.csv",header = TRUE,stringsAsFactors = FALSE)[ ,c('State','Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')] 
  
          FileDataByState <-subset(df,select= State:Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia )
          FileDataByState[, 3]  <- as.numeric(FileDataByState[, 3]) 
  
            
          #use number of states to drive for loop & use state data
          uniqueStates<- unique(FileDataByState$State)
          usl = length (uniqueStates)
          
          #dataframe to hold data 
          final.data <- data.frame(state1 = character() , hospital = character(),state= character(),stringsAsFactors=FALSE )
          for (i in 1:usl) {
            
            # subset data and remove hospitials with no data available. 
            FS <-subset(FileDataByState, FileDataByState$State == uniqueStates[i] & FileDataByState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia !="Not Available",select= Hospital.Name:Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia )
            
           
            
            # order by values
            FSS <- FS[order( FS$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),] 
            
            # take means and order by smallest value
            afs  = aggregate(FSS$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, list(FSS$Hospital.Name), mean)
            safs <- afs[order( afs$x),] 
            
            # rank setup.. best =1 , worst = number of records in state, else number
            if (num == 'worst'){
              
              x=nrow(safs)
            } else if (num == 'best') 
            {x = 1}
            else
            {x = num}
            #add the data record with right rank to data frame
            final.data[nrow( final.data) + 1, ] <- c( uniqueStates[i], safs[x,1],uniqueStates[i] )
            
            
          }
          
          # rename first column to null and order by state column
          names(final.data)[names(final.data)=="state1"] <- ""
          final.data <- final.data[order( final.data$state),] 
          
          return (final.data)
          
}






  
}