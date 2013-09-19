\name{EBVor}
\alias{EBVor}
\alias{EBPoly}
\alias{EBMarks}
\alias{EBPoints}
\alias{insert.EBVor}
\alias{delete.EBVor}
\alias{plot.EBVor}
\alias{identify.EBVor}
\alias{as.ppp.EBVor}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ EBVor class }
\description{
  An EBVor object store dynamically a Voronoi tesselation of some set points.
}
\usage{
EBVor(pts=500,center,size,sizeIn,mark)
EBMarks(\dots)
EBPoints(obj,\dots)
\method{EBPoints}{default}(pts,nbPts=1,mark=NULL,...)
\method{insert}{EBVor}(vor,pts,...)
\method{delete}{EBVor}(vor,pts)
\method{plot}{EBVor}(vor,type,xlab="",ylab="",center,size,dvArgs,vvArgs,deArgs,veArgs,vcArgs,dvCol,vcCol,dvCex,vvCex,dvPch,vvPch,add=FALSE,vc.colors=cm.colors,dv.colors=heat.colors,dv.cex=c(1,.5),dv.pch=c(15:25,1:14),...) 
\method{identify}{EBVor}(vor,func,mode=1,...)
\method{as.ppp}{EBVor}(vor,window)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vor}{an object of class EBVor.}
  \item{pts}{either a vector or a matrix of coordinates for \code{insert} method and a vector of index for }
  \item{\dots}{further arguments depending on the call to perform.}
}
\details{
     
}
\value{
      
}
\author{ R. Drouilhet }

\seealso{ \code{\link{EBFunc}}, \code{\link{EBGibbs}}, \code{\link{EBStat}}}
\examples{
vor<-EBVor(1000000,marks=EBMarks(m=int(1,1:2)))
insert(vor,runif(100,-250,250),m=sample(1:2,100,rep=T))
plot(vor,vcCol=m,type=c("dv","vc","de"))
plot(vor,dvCol=m,type="de")
plot(vor,0) #list of already plot to reproduce again
plot(vor,1) #plot 1 is then redrawn
plot(vor,2) #plot 2 is then redrawn
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
