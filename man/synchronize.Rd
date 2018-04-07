\name{synchronize}
\alias{synchronize}
\title{Synchronize two time series of measurements which are misaligned.}
\description{The function takes two times series of measurements by different devices or from different points, between which you expect a correlation. It seems to be always the case that the timestamp of one device is just a little off and the signals are misaligned when we plot them, preventing an accurate calculation of the correlation between the two. This function fixes the misalignment by synchronizing the df_sync to give the highest correlation with the df_base.}
\usage{synchronize(df_base,df_sync,basevar,syncvar,timevar,log_scale=FALSE,plot_stats=FALSE,match_interval=c(-1000,1000),correlation_req=0.7,irregular=FALSE)}
\arguments{
  \item{df_base}{The dataframe you want to use as a basis to join to. Typically the file whose timestamp you believe to be most accurate, or the timestamp which has the most records.}
  \item{df_sync}{The dataframe you want to synchronize to your df_base.}
  \item{basevar}{The variable from the df_base on whose correlation with syncvar you want to base the join.}
  \item{syncvar}{The variable from the df_sync on whose correlation with basevar you want to base the join.}
  \item{timevar}{The variable containing the datetime information. Should be in POSIXct. Defaults to whatever variable is in POSIXct if not specified.}
  \item{log_scale}{Defaults to FALSE. Set to TRUE if the correlation should be calculated between log(basevar) and log(syncvar).}
  \item{plot_stats}{Returns a plot of the correlations found as a function of the evaluated timeshifts. This plot is accessible as the 2nd list element of the function result.}
  \item{match_interval}{How much time could your df_sync be off from the df_base? Specify the window (in seconds) that you want to evaluate.}
  \item{correlation_req}{Correlation required, defaults to 0.7. Synchronize will join the time series based on the optimal join correlation. However, if that optimal correlation was below 0.7, maybe the found optimum was suboptimal, and you would like to get a warning in that situation. Use correlation_req to get warned about possible suboptimal joins. (e.g. This could happen if the time series is 200 seconds off, but you've only specified c(-100,100) as your match interval window.)}
  \item{irregular}{Defaults to FALSE. Most time series i've dealt with are regular intervals, in which case we use a simple dataframe shift, which is quite efficient. If one of both series are irregular, we need a rolling join for each iteration. This should work too, but is much slower. (And I haven't spent time to speed it up, as i haven't had to use it much myself.)}
}
\details{}
\value{By default, the returned dataset is a dataframe in which the two original dataframes (df_base and df_sync) have been merged according to their optimal correlation. If the option plot_stats=TRUE is specified, the output is a list of which the first item is the merged dataset as above, and the second item is a plot of the correlations evaluated.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{This function was written for a project which used 3 different real-time sensors carried by a single person, measuring the same thing, but all at different intervals, with different start/stop times and naturally, not all the instruments' internal timers were synchronized with each other.}
\seealso{}
\examples{
#This is a real-life example which is available from my other package "EMFtools".
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/EMFtools/data/expom_example2.csv"
write.table(expom_example2,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
expom_file1<-import_expom_RF(filename=my_filename1,prefix="EXPOM1_",suffix="_Vm")
#Let's first create a "similar" file (supposedly from another device) where:
#- The timestamp is misaligned (we add some seconds)
#- The data is a bit different (we introduce some jitter)
expom_file2<-cbind(PosixTime=expom_file1$PosixTime+seconds(123),
                   do.call(cbind,lapply(expom_file1[,2:18],jitter)),
                   expom_file1[,19:27])
#We rename the variables which start with "EXPOM1_" to "EXPOM2_":
names(expom_file2)<-gsub("EXPOM1_","EXPOM2_",names(expom_file1))
#To make sure it also works with time series which are not of the same length, delete some observations:
expom_file2<-expom_file2[43:(dim(expom_file2)[1]-21),]

#So much for creating some demo files. Let's assume we received these files from 2 different devices.
#We cannot use a simple merge because the data have an interval of 4s and are off by 123s:
test_merge<-merge(expom_file1,expom_file2,by="PosixTime",all=TRUE)
#There are zero actual matches, so we cannot find a correlation between the two devices:
cor(log(test_merge$EXPOM1_TOTAL_Vm),log(test_merge$EXPOM2_TOTAL_Vm),use="pairwise.complete.obs")

#We can do a rolling merge based on time alone:
expom_file1<-setkey(data.table(expom_file1),PosixTime)
expom_file2<-setkey(data.table(expom_file2),PosixTime)
test_merge<-expom_file2[expom_file1,roll="nearest",mult="first"]
#But still, the time series will be misaligned:
selection1<-subset(test_merge[1:2000,],select=c("PosixTime","EXPOM1_TOTAL_Vm","EXPOM2_TOTAL_Vm"))
ggplot(data=melt(selection1,id.var="PosixTime"),aes(x=PosixTime,y=value))+geom_line(aes(col=variable))
#And the correlation between the devices is underestimated:
cor(log(test_merge$EXPOM1_TOTAL_Vm),log(test_merge$EXPOM2_TOTAL_Vm),use="pairwise.complete.obs")

#We now use synchronize to re-align the two misaligned time series:
sync_data<-synchronize(basefile=expom_file1,syncfile=expom_file2,timevar="PosixTime",
                       basevar="EXPOM1_TOTAL_Vm",syncvar="EXPOM2_TOTAL_Vm",
                       log_scale=TRUE,plot_stats=TRUE)
#The first item of the list contains
head(sync_data[[1]])
#The second item is a plot of the merge:
sync_data[[2]]
#If we now plot the merged result, the time series are optimally aligned:
selection2<-subset(sync_data[[1]][1:2000,],select=c("PosixTime","EXPOM1_TOTAL_Vm","EXPOM2_TOTAL_Vm"))
ggplot(data=melt(selection2,id.var="PosixTime"),aes(x=PosixTime,y=value))+geom_line(aes(col=variable))
#And now we have an almost perfect correlation!
cor(log(sync_data[[1]]$EXPOM1_TOTAL_Vm),log(sync_data[[1]]$EXPOM2_TOTAL_Vm),use="pairwise.complete.obs")
}
\keyword{}
