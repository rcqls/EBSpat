\name{EBGibbs}
\alias{EBGibbs}
\alias{run.EBGibbs}
\alias{EBPoints.EBGibbs}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ EBGibbs class }
\description{
  EBGibbs class.
}
\usage{
EBGibbs(func,m=10000,nbmax=1000000,center=c(0,0),size=700,sizeIn=500,...)
\method{run}{EBGibbs}(gibbs,m,plot=TRUE,update=TRUE,type=c("dv"),...)
\method{EBPoints}{EBGibbs}(gibbs,nbPts=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{  }
  \item{\dots}{further arguments depending on the call to perform.}
}
\details{
     
}
\value{
      
}
\author{ R. Drouilhet }

\seealso{ \code{\link{EBVor}}, \code{\link{EBFunc}}, \code{\link{EBStat}}}
\examples{
gd<-EBGibbs(~ 2 +Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
run(gd)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
