#################################
##### synchronize           #####
##### By: Marloes Eeftens   #####
##### Last edit: 07/04/2018 #####
#################################

#Function "synchronize":
synchronize=function(basefile,syncfile,basevar,syncvar,timevar,log_scale=FALSE,plot_stats=FALSE,match_interval=c(-100,100),correlation_req=0.7,irregular=FALSE){

  #0) Check the function arguments:
  if(missing(basefile)){stop("No basefile specified. Define the basefile to which the second should be synched.")}
  if(missing(syncfile)){stop("No syncfile specified. Define the file which should be synched to the basefile.")}
  if(missing(basevar)){stop("No basevar specified. Define the variable the synchronization should be based on.")}
  if(missing(syncvar)){stop("No syncvar specified. Define the variable the synchronization should be based on.")}
  if(missing(timevar)){
    print("No timevar specified...")
    is.POSIXct<-function(x) inherits(x,"POSIXct")
    if(any(sapply(basefile,is.POSIXct)==TRUE)){
      timevar<-names(basefile[sapply(basefile,is.POSIXct)])
      cat("...Assuming timevar = ",'"',timevar,'"',"\n)",sep="")
    }else{
      stop("No timevar specified or detected...")
    }
  }

  #1) Log the variables if needed:
  basefile2<-data.table(subset(basefile,select=c(timevar,basevar)))
  syncfile2<-data.table(subset(syncfile,select=c(timevar,syncvar)))
  if(log_scale==TRUE){
    basefile2[[basevar]]<-log(basefile2[[basevar]])
    syncfile2[[syncvar]]<-log(syncfile2[[syncvar]])
  }

  #2) Save the correlation resulting from each timeshift in a list:
  all_correlations<-list()
  if(irregular==FALSE){
    setkey(basefile2[[timevar]])
    setkey(syncfile2[[timevar]])
    test_merge<-syncfile2[basefile2,roll="nearest",mult="first",rollends=c(FALSE,FALSE)]
    interval_seconds<-as.numeric(median(basefile2[[timevar]]-lag(basefile2[[timevar]],1),na.rm=TRUE),units="secs")
    for (k in seq(from=match_interval[1],to=match_interval[2])){
      test_merge_k<-as.data.frame(cbind(basevar=test_merge[[basevar]],
                                        syncvar=shift_both_ways(test_merge[[syncvar]],k)))
      test_merge_k<-test_merge_k[!is.na(test_merge_k$basevar)&!is.na(test_merge_k$syncvar),]
      if(sd(test_merge_k[["basevar"]])==0|sd(test_merge_k[["syncvar"]])==0|dim(test_merge_k)[1]<2){
        correlation<-NA
      }else{
        correlation<-cor(test_merge[[basevar]],shift_both_ways(test_merge[[syncvar]],k),use="pairwise.complete.obs")
      }
      all_correlations[[k-min(match_interval)+1]]<-data.frame(correlation=correlation,shift=k*interval_seconds)
    }
  }
  if(irregular==TRUE){
    for (k in seq(from=match_interval[1],to=match_interval[2])){
      syncfile2_test<-syncfile2
      syncfile2_test[[timevar]]<-syncfile2_test[[timevar]]+seconds(k)
      setkey(basefile2[[timevar]])
      setkey(syncfile2_test[[timevar]])
      test_merge<-syncfile2_test[basefile2,roll="nearest",mult="first",rollends=c(FALSE,FALSE)]
      correlation<-cor(test_merge[[basevar]],test_merge[[syncvar]],use="pairwise.complete.obs")
      all_correlations[[k-min(match_interval)+1]]<-data.frame(correlation=correlation,shift=k)
    }
  }

  #3) Find out which timeshift gives the optimal correlation:
  all_correlations_new<-rbindlist(lapply(all_correlations,FUN=function(x) x)) #Rbindlist is 10* faster than do.call
  optimal_shift<-all_correlations_new$shift[which.max(all_correlations_new$correlation)]
  if(all(is.na(all_correlations_new$correlation))){
    optimal_shift<-0
    }
  optimal_shift_cor<-max(all_correlations_new$correlation,na.rm=TRUE)

  #4) Evaluate how likely it is that the optimal_shift is truly the optimum:
  if(all(is.na(all_correlations_new$correlation))){
    warning(paste0(
      "The basefile starts at ",min(basefile2[[timevar]])," and finishes at ",max(basefile2[[timevar]]),". \n",
      "The syncfile starts at ",min(syncfile2[[timevar]])," and finishes at ",max(syncfile2[[timevar]]),". \n",
      "There is no overlap between the time series to base the synchronization on. Series will be merged with the original timestamp."))
  }
  if(!all(is.na(all_correlations_new$correlation))){
    if(optimal_shift_cor<correlation_req|optimal_shift %in% head(all_correlations_new$shift)|optimal_shift %in% tail(all_correlations_new$shift)){
      warning("Correlation between the signals suboptimal and/or below predefined requirement...\n
            Suggestions:\n
            - Specify plot_stats=TRUE to examine the plot\n
            - Widen the time interval over which the synchronize function looks for the best match\n
            - If you think the optimal shift was chosen correctly, set a less stringent correlation requirement to avoid this warning (the default is correlation_req=0.7)")
    }
  }

  #5) Generate a plot of the correlation by time shift:
  if(plot_stats==TRUE&!all(is.na(all_correlations_new$correlation))){
    plot1<-ggplot(data=all_correlations_new,aes(x=shift,y=correlation))+
      geom_line(aes(),color=grey(0.4))+
      geom_point(size=1.5)+
      geom_vline(xintercept=optimal_shift)+
      ggtitle(paste0("Correlation by time shift, optimal = ",optimal_shift))
  }

  #6) Do the actual merge, shifting the syncfile by the optimal_shift:
  syncfile_rev<-syncfile
  syncfile_rev[[timevar]]<-syncfile_rev[[timevar]]-seconds(optimal_shift)
  synchronized_data<-merge(basefile,syncfile_rev,by=timevar,all=TRUE)

  #Collapse all the .x and .y variables to their stem variable:
  vars_to_collapse<-names(synchronized_data)[c(grep(".x$",names(synchronized_data)),grep(".y$",names(synchronized_data)))]
  if(length(vars_to_collapse)>0){
    for(m in 1:length(vars_to_collapse)){
      goto_var<-gsub(c(".x$|.y$"),"",vars_to_collapse[m])
      if(!goto_var %in% colnames(synchronized_data)){
        names(synchronized_data)[which(names(synchronized_data)==vars_to_collapse[m])]<-goto_var
      }else{
        synchronized_data[[goto_var]]<-ifelse(is.na(synchronized_data[[goto_var]]),
                                          synchronized_data[[vars_to_collapse[m]]],
                                          synchronized_data[[goto_var]])
        synchronized_data[[vars_to_collapse[m]]]<-NULL
      }
    }
  }

  print(paste0("Time series syncfile was shifted by ",optimal_shift*-1," seconds, correlation between signals: ",signif(optimal_shift_cor,2)))

  #7) Return the results:
  if(plot_stats==TRUE){return(list(synchronized_data,optimal_shift*-1,plot1))}
  if(plot_stats==FALSE){return(synchronized_data)}
  }

