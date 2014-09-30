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
}

best<-function(state, outcome){
  #"state" parameter check
  
  #"outcome" parameter check
  
  #read the file into a data frame
  
  #create a matrix with "hospital name" and the "outcome" columns from the data frame
  
  #order the matrix according to "outcome" column
  
  
}