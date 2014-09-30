#input :state name (two letters), outcome name
#output: name of best hospital (lowest result, first in alphabetical order in case of tie)
#
lowest<-function(hospitals){
  #go over the column, start at the top (lowest)
  # add hospital name to a vector, add outcome value to "min"
  hospital<-hospitals[1,1]
  min<-hospitals[1,2]  
  #advance to next line,  once "outcome" column is larger than "min"-  return
  i<-1
  while (hospitals[i+1,2]<min){
    hospital<-c(hospital,hospitals[i+1,i+1])
    i<-i+1
  }
  #sort hospital vector
  sort(hospital)
  #return first (lowest alphabetical) hospital
  hospital[1]
}

best<-function(state, outcome){
  #"state" parameter check
  
  #"outcome" parameter check
  
  #read the file into a data frame
  df<-read.csv("outcome-of-care-measures.csv")
  #order df according to state,outcome, hospital name
  df[order(state,outcome,hospital name]),]
    
  #run "lowest" function which returns the lowest alphabetical name of the lowest ranked (best) hospital
  
  
}

# z1<-c(1,2,3,4,5,6,7,1)
# z2<-c("il","il","az","tx","az","tx","tx","il")
# z3<-c("amka","zfat","jones","sloan","morgan","bendict","carter","afula")
# tst<-cbind(z1,z2,z3)
#tst[order(z2,z1,z3),]