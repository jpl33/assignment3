#input :state name (two letters), outcome name
#output: name of best hospital (lowest result, first in alphabetical order in case of tie)
#


best<-function(state, outcome){
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
 
  #get first line of "state" hospitals, get min "outcome" value as benchmark
  min<-as.character(df[st,][1,"Hospital.Name"])
  min  
  
}



# z1<-factor(c(1,2,3,4,5,6,7,1))
# z2<-c("il","il","az","tx","az","tx","tx","il")
# z3<-c("amka","zfat","jones","sloan","morgan","bendict","carter","afula")
# tst<-cbind(z1,z2,z3)
# tst<-tst[order(z2,z1,z3),]
#il<-tst[,"z2"]=="il"
#tst[il,]
#tst[il,][1,]