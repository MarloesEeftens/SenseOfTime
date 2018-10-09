#Fill_missing_dates
fill_missing_dates<-function(x,timevar,fake_date){
  #Select var
  a<-x[[timevar]]
  if(all(gsub(" .*","",as.character(a))==fake_date)){stop("All dates missing, no filling possible")}
  while(any(gsub(" .*","",as.character(a))==fake_date)){
    #Working forwards:
    for(i in 2:length(a)){print(i)
      #Is record i missing with the previous record (i-1) available?
      if(gsub(" .*","",as.character(a[i]))==fake_date&gsub(" .*","",as.character(a[i-1]))!=fake_date){
        a[i]<-as.POSIXct(paste0(gsub(" .*","",as.character(a[i-1]))," ",gsub(".* ","",as.character(a[i]))))
        #Is time i earlier than time i-1? Assume it's the next day!
        if(a[i]<a[i-1]){
          a[i]<-as.POSIXct(paste0(as.character(as.Date(a[i-1])+1)," ",gsub(".* ","",as.character(a[i]))))
        }
      }
    }
    #Working backwards:
    for(i in 1:(length(a)-1)){print(i)
      #Is record i missing with the next record (i+1) available?
      if(gsub(" .*","",as.character(a[i]))==fake_date&gsub(" .*","",as.character(a[i+1]))!=fake_date){
        a[i]<-as.POSIXct(paste0(gsub(" .*","",as.character(a[i+1]))," ",gsub(".* ","",as.character(a[i]))))
        #Is time i later than time i+1? Assume it's the previous day!
        if(a[i]>a[i+1]){
          a[i]<-as.POSIXct(paste0(as.character(as.Date(a[i+1])-1)," ",gsub(".* ","",as.character(a[i]))))
        }
      }
    }
  }
  x[[timevar]]<-a
  return(x)
}
