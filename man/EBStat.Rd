\name{EBStat}
\alias{EBStat}
\alias{EBPseudoExpo}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ EBPseudoExpo class }
\description{
  EBPseudoExpo class.  
}
\usage{
EBPseudoExpo(model,nbPts,domainSize,mark=NULL,mode=c("random","systematic"),weighted=FALSE)
\method{run}{EBContrastOptim}(self,par0,method=NULL,update=FALSE,print=TRUE,fixed,...)
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

\seealso{ \code{\link{EBFunc}}, \code{\link{EBGibbs}}, \code{\link{EBVor}}}
\examples{
gd<-EBGibbs(~ 2 +Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
run(gd)
pld <- EBPseudoExpo(gd~Del2(l<=20,20<l & l<=80),weight=TRUE,mode="systematic")
run(pld,c(0,0,0))
run(gd)
run(pld)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
