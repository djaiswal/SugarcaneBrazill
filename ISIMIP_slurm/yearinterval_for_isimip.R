yearinterval_for_isimip<-function(year){
' This function return text string of year interval to read appropsirae INISMIP file
  to read data for a given year
  Argument = year (numeric)
  Output = yearinterval (text string)
'
  remainder<-year%%10
  if(remainder!=0)
    {
     lowerbound<-year-remainder+1
     upperbound<-lowerbound+9
    }
  else
    {
     lowerbound<-year-9
     upperbound<-year
    }
  yearinterval<-paste(lowerbound,"-",upperbound,sep="")
  return(c(lowerbound=lowerbound,upperbound=upperbound,yearinterval=yearinterval))
}
