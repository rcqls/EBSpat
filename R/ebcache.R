### EBCache
# cache type = 1 for EBSampl 2 for EBSum!
# mode=systematic or random for EBSampl (initialized in EBPseudoExpo and ...)
EBSamplCache <- function(func,nbPts,domainSize,poly,mode=c("random","systematic")) {
  cache <- CqlsObj(EBCache,EBSamplCache)
  cache$extPtr <- .ExternalInEnvir("EBCache_new",func,envir=cache,PACKAGE = "EBSpat")
  cache$mode<-match.arg(mode)
  cache$cacheType <- as.integer(1)
  cache$func <- func
  cache$.nbPts <- nbPts
  if(length(domainSize)==1) domainSize<-c(domainSize,domainSize)
  cache$.domainSize<-domainSize
  #reactivate(poly)
  cache$poly <- poly
  reg.finalizer(cache,free.externalPtr,TRUE)
  ##class(cache) <- c("EBCache","EBSamplCache","GetAttr")
  cache
}

update.EBSamplCache <- function(cache,verbose=FALSE) {
  if(verbose) cat("updating object EBSamplCache (mode=",cache$mode,") ... ",sep="")
  ### cat("cache$poly->");print.default(cache$poly)
  ### cat("cache$domainSize->");print(cache$domainSize)
  ### cat("cache$nbPts->");print(as.integer(cache$nbPts))
  switch(cache$mode,
  systematic=.External("EBCacheSystSampl_init",cache$extPtr,as.integer(sqrt(cache$nbPts)),cache$domainSize,cache$poly,PACKAGE = "EBSpat"),
  random=.External("EBCacheRandSampl_init",cache$extPtr,as.integer(cache$nbPts),cache$domainSize,cache$poly,PACKAGE = "EBSpat")
  )
}


EBSumCache <- function(func,domainSize,poly) {
  cache <- CqlsObj(EBCache,EBSumCache)
  cache$extPtr <- .ExternalInEnvir("EBCache_new",func,envir=cache,PACKAGE = "EBSpat")
  cache$cacheType <- as.integer(2)
  cache$func <- func
  if(length(domainSize)==1) domainSize<-c(domainSize,domainSize)
  cache$.domainSize <- domainSize
  #reactivate(poly)
  cache$poly <- poly
  reg.finalizer(cache,free.externalPtr,TRUE)
  ##class(cache) <- c("EBCache","EBSumCache","GetAttr")
  cache
}

update.EBSumCache <- function(cache,verbose=FALSE) {
  if(verbose) cat("updating object EBSumCache ... ")
  .External("EBCacheSum_init",cache$extPtr,cache$domainSize,cache$poly,PACKAGE = "EBSpat")
}

reactivate.EBCache <- function(cache) reactivate.externalPtr(cache)

show.EBCache <- function(cache) .External("EBCache_print",cache$extPtr,PACKAGE = "EBSpat")

compute.EBCache<-function(cache,init,code) {
  .External("EBCache_compute",cache$extPtr,substitute(init),substitute(code),PACKAGE = "EBSpat")
}

sum.EBCache<-function(cache,code) {
  ## convert code in substitute(code) (.e. a call) except if code is already a call!
  code.is.call <- try(is.call(code),TRUE)
  if(inherits(code.is.call,"try-error") || !code.is.call) code <- substitute(code)
  ##print(code)
  .External("EBCache_sum",cache$extPtr,code,PACKAGE = "EBSpat")
}

as.matrix.EBCache<-function(cache) .External("EBCache_matrix",cache$extPtr,PACKAGE = "EBSpat")

free.EBCache<-function(cache) .External("EBCache_free",cache$extPtr,PACKAGE = "EBSpat")


### EBCacheCompFunc
# cache type = 1 for EBSampl 2 for EBSum!
# mode=systematic or random for EBSampl (initialized in EBPseudoExpo and ...)
EBSamplCacheCompFunc <- function(func,nbPts,domainSize,poly,mode=c("random","systematic")) {
  cache <- CqlsObj(EBCacheCompFunc,EBSamplCacheCompFunc)
  cache$extPtr <- .ExternalInEnvir("EBCacheCompFunc_new",func,envir=cache,PACKAGE = "EBSpat")
  cache$mode<-match.arg(mode)
  cache$cacheType <- as.integer(1)
  cache$func <- func
  cache$.nbPts <- nbPts
  if(length(domainSize)==1) domainSize<-c(domainSize,domainSize)
  cache$.domainSize<-domainSize
  cache$poly <- poly
  reg.finalizer(cache,free.externalPtr,TRUE)
  cache
}

update.EBSamplCacheCompFunc <- function(cache,verbose=FALSE) {
  if(verbose) cat("updating object EBSamplCacheCompFunc (mode=",cache$mode,") ... ",sep="")
  ### cat("cache$poly->");print.default(cache$poly)
  ### cat("cache$domainSize->");print(cache$domainSize)
  ### cat("cache$nbPts->");print(as.integer(cache$nbPts))
  switch(cache$mode,
  systematic=.External("EBCacheCompFunc_initSystSampl",cache$extPtr,as.integer(sqrt(cache$nbPts)),cache$domainSize,cache$poly,PACKAGE = "EBSpat"),
  random=.External("EBCacheCompFunc_initRandSampl",cache$extPtr,as.integer(cache$nbPts),cache$domainSize,cache$poly,PACKAGE = "EBSpat")
  )
}

EBSumCacheCompFunc <- function(func,domainSize,poly) {
  cache <- CqlsObj(EBCacheCompFunc,EBSumCacheCompFunc)
  cache$extPtr <- .ExternalInEnvir("EBCacheCompFunc_new",func,envir=cache,PACKAGE = "EBSpat")
  cache$cacheType <- as.integer(2)
  cache$func <- func
  if(length(domainSize)==1) domainSize<-c(domainSize,domainSize)
  cache$.domainSize<-domainSize
  cache$poly <- poly
  reg.finalizer(cache,free.externalPtr,TRUE)
  cache
}

update.EBSumCacheCompFunc <- function(cache,verbose=FALSE) {
  if(verbose) cat("updating object EBSumCacheCompFunc ... ")
  ### cat("cache$poly->");print.default(cache$poly)
  ### cat("cache$domainSize->");print(cache$domainSize)
   .External("EBCacheCompFunc_initSum",cache$extPtr,cache$domainSize,cache$poly,PACKAGE = "EBSpat")
}

reactivate.EBCacheCompFunc <- function(cache) reactivate.externalPtr(cache,"EBCacheCompFunc")


as.data.frame.EBCacheCompFunc<-function(cache,...) {
  .External("EBCacheCompFunc_as_dataframe",cache$extPtr,PACKAGE = "EBSpat")
}

as.matrix.EBCacheCompFunc <- function(cache,...) as.matrix(as.data.frame(cache),...)

show.EBCacheCompFunc<-function(cache,...) {
  .External("EBCacheCompFunc_print",cache$extPtr,PACKAGE = "EBSpat")
}
