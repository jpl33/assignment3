rankall<-function(outcome,num="best"){
  #"state" column number
  stateN<-7
  
  #"outcome" parameter check
  #match outcome column to outcome name
  outcomeN<-switch(outcome,"heart attack"=11,"heart failure"=17,"pneumonia"=23)
  #check that proper outcome was submitted! it's actually easier to do it in this order, even though it is less logical :-)
  if(is.null(outcomeN)){ stop("Invalid outcome")}
  
  #read the file into a data frame
  df<-read.csv("outcome-of-care-measures.csv")
  
  #order df according to state,outcome value, hospital name
  #force treatment of outcome as "numeric" instead of "factor"!
  df<-df[order(df[,stateN],as.numeric(as.character(df[,outcomeN])),df[,"Hospital.Name"]),]
  
  #get state list
  statelist<-levels(df[,stateN])
 
  #"rank" parameter check
  #assign values if "num" is a string 
  rank<-switch(num,"best"=1,"worst"=-1)
  #"num" isn't a string? check if it's a number...if it is, assign it to "rank" 
  if (is.numeric(num)){rank<-num}
  
  #initialize dataframe
  result<-data.frame(state=character(0),hospital=character(0))
  levels(result[,"state"])<-levels(df[,stateN])
  #levels(result[,"hospital"])<-levels(df[,"Hospital.Name"])
  
  #iterate through statelist, pick each hospital in "rank" for each state 
  for (i in 1:length(statelist)){
    
    #create logical vector for rows where "state" column equals current "state" parameter in "statelist"
    st<-df[,stateN]==statelist[i] 
    
    #what state are we in?
    #result[i,"state"]<-as.character(statelist[i])
    
    #if rank is not "worst", choose from list of "state" hospitals, the one ranked in "rank" 
    if (rank!= -1){
    #  result[,"hospital"]<-as.character(result[,"hospital"])
    #  result[i,"hospital"]<-as.character(df[st,][rank,"Hospital.Name"])
     # result[,"hospital"]<-factor(result[,"hospital"])
    result<-rbind(result,data.frame(state=statelist[i],hospital=as.character(df[st,][rank,"Hospital.Name"])))
                   }
    else {
     # result[,"hospital"]<-as.character(result[,"hospital"])
    #  result[i,"hospital"]<-as.character(df[st,][which.max(na.omit(as.character(df[st,outcomeN]))),"Hospital.Name"])
     # result[,"hospital"]<-factor(result[,"hospital"])
      result<-rbind(result,data.frame(state=statelist[i],hospital=as.character(df[st,][which.max(na.omit(as.character(df[st,outcomeN]))),"Hospital.Name"])))
          }
  
    
}
result
}