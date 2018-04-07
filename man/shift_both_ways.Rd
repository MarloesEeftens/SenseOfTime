\name{shift_both_ways}
\alias{shift_both_ways}
\title{Shifts data forward or backward, padding with NA's.}
\description{Convenience function that allows to use both lag() and lead() in one function.}
\usage{shift_both_ways(x,shift)}
\arguments{
  \item{x}{A vector or dataframe that needs to be shifted}
  \item{shift}{A negative or positive integer giving period by which the data are to be shifted.}
}
\details{}
\value{A vector or dataframe that is shifted by the specified period.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{I mostly used this for vectors. The function works on dataframes but efficiency can be improved. I called it shift_both_ways because the data.table package (which this package depends on) already has a function called shift, which (unfortunately) only shifts one way depending on whether you speficy type="lag" or "lead". Just thought this was easier.}
\seealso{}
\examples{
#Make up some data
a<-c(1,2,3,NA,5,6,7,NA,9,10)
my_df<-data.frame(A=1:10,B=LETTERS[1:10],C=letters[1:10],stringsAsFactors= F )

#Same as lead(a,1)
shift_both_ways(a,-1)
shift_both_ways(my_df,2)
}
\keyword{}
