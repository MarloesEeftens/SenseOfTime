#################################
##### shift_both_ways       #####
##### By: Marloes Eeftens   #####
##### Last edit: 06/04/2018 #####
#################################

#Function "shift_both_ways":
shift_both_ways=function(x,shift){
  if(is.vector(x)){
    return(switch(sign(shift)/2+1.5,data.table::shift(x,abs(shift),fill=NA,type="lag"),data.table::shift(x,abs(shift),fill=NA,type="lead")))
  }
  if(is.data.frame(x)){
    list_data<-switch(sign(shift)/2+1.5,data.table::shift(x,abs(shift),fill=NA,type="lag"),data.table::shift(x,abs(shift),fill=NA,type="lead"))
    x_new<-data.frame()[1:dim(x)[1], ]
    for(i in 1:length(list_data)){
      list_item_i<-list_data[[i]]
      x_new<-data.frame(x_new,list_item_i,stringsAsFactors=FALSE)
    }
    names(x_new)<-names(x)
    rownames(x_new)<-c()
    return(x_new)
  }
}
