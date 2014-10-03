#input :state name (two letters), outcome name
#output: name of best hospital (lowest result, first in alphabetical order in case of tie)
#


best<-function(state, outcome){
  #"state" parameter check
  stateN<-7
  #"outcome" parameter check
  if (outcome=="heart attack"){outcomeN<-11}
  #read the file into a data frame
  df<-read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  #order df according to state,outcome value, hospital name
  df<-df[order(df[,stateN],df[,outcomeN],df[,"Hospital.Name"]),]
  #create logical vector for rows where "state" column equals "state" parameter
  st<-df[,stateN]==state 
  #get first line of "state" hospitals, get min "outcome" value as benchmark
  min<-df[st,][1,"Hospital.Name"]
  min
  
}

# z1<-c(1,2,3,4,5,6,7,1)
# z2<-c("il","il","az","tx","az","tx","tx","il")
# z3<-c("amka","zfat","jones","sloan","morgan","bendict","carter","afula")
# tst<-cbind(z1,z2,z3)
# tst<-tst[order(z2,z1,z3),]
#il<-tst[,"z2"]=="il"
#tst[il,]
#tst[il,][1,]