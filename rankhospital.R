rankhospital<-function(state, outcome,num){
  #"state" column number
  stateN<-7
  
  #"outcome" parameter check
  #match outcome column to outcome name
  outcomeN<-switch(outcome,"heart attack"=11,"heart failure"=17,"pneumonia"=23)
  #check that proper outcome was submitted! it's actually easier to do it in this order, even though it is less logical :-)
  if(is.null(outcomeN)){ stop("Invalid outcome")}
  
  #read the file into a data frame
  df<-read.csv("outcome-of-care-measures.csv")
  
  #check that proper state was submitted
  if(!any(df[,stateN]==state)){stop("Invalid State")}
  
  #order df according to state,outcome value, hospital name
  #force treatment of outcome as "numeric" instead of "factor"!
  df<-df[order(df[,stateN],as.numeric(as.character(df[,outcomeN])),df[,"Hospital.Name"]),]
  
  #create logical vector for rows where "state" column equals "state" parameter
  st<-df[,stateN]==state 
  
  #"rank" parameter check
  #assign values if "num" is a string 
  rank<-switch(num,"best"=1,"worst"=-1)
  #"num" isn't a string? check if it's a number...if it is, assign it to "rank" 
  if (is.numeric(num)){
    rank<-num
    #is "num" a valid rank for this state? if NOT, return NA
    if (rank<1 || rank>nrow(df[st,])){result<-NA
                                      return(result)}
  } 
  #if it isn't a number, stop and raise an error
  if(is.null(rank)){ stop("Invalid num")}
  
  
  #if rank is not "worst", choose from list of "state" hospitals, the one ranked in "rank" 
  if (rank!= -1){result<-as.character(df[st,][rank,"Hospital.Name"])
                 result}
  else {result<-as.character(df[st,][which.max(na.omit(as.character(df[st,outcomeN]))),"Hospital.Name"])
        result}
  
}