\name{EBFunc}
\alias{EBFunc}
\alias{EBSumCache}
\alias{EBSamplCache}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ EBFunc, EBCache, EBCacheCompFunc classes }
\description{
  EBFunc, EBSamplCache, EBSumCache, EBSamplCacheCompFunc, EBSumCacheCompFunc classes
}
\usage{
EBFunc(formula,mode)
EBSamplCache(func,nbPts,domainSize,poly,mode)
EBSumCache(func,domainSize,poly)
\method{update}{EBSamplCache}(cache,verbose=FALSE)
\method{update}{EBSumCache}(cache,verbose=FALSE)
EBSamplCacheCompFunc(func,nbPts,domainSize,poly,mode)
EBSumCacheCompFunc(func,domainSize,poly)
\method{update}{EBSamplCacheCompFunc}(cache,verbose=FALSE)
\method{update}{EBSumCacheCompFunc}(cache,verbose=FALSE)
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

\seealso{ \code{\link{EBVor}}, \code{\link{EBGibbs}}, \code{\link{EBStat}}}
\examples{
vor <- EBVor()
insert(vor,runif(20,-350,350))  
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
