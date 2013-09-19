## the idea: an object residObj of class EBResid depending on a model and formulas

## run(residObj,vor,param,...) -> update caches with vor$pl

## if param is a character containing the estimation method name launch the estimation method 
## but the simplest solution is to provide the estimation as an argument of param.


## Ex: EBResid(gd~Del2(a=l2)+All2(b=c2),del2(l<40),d2(l<60)) 

EBResid <- function(model,...,nbPts=10000,domainSize,mode=c("random","systematic"),weighted=FALSE,compute=c("normal","noparam","inverse")) {
  if(!inherits(model,"formula")) warning("parameter model has to be a formula!")
  resid <- CqlsObj(EBResid)
  resid$call <- match.call()
  resid$compute <- match.arg(compute)
  resid$formMngr <- CompFuncFormulaManager()

  ## build the ebfunc and formulas
  tmp <- as.list(substitute(c(...)))[-1] #the expressions of the formulas
  ## verboseMode: cat("tmp->");print(tmp)
  tmp <- strsplit(sapply(tmp,deparse),"\\|")
  resid$formulas <- list(model=model,fct=as.list(parse(text=paste("c(",paste(sapply(tmp,function(e) e[[1]]),collapse=","),")",sep="")))[[1]])

  ## verboseMode: cat("formulas ->\n");print(resid$formulas)

  tmp <- unlist(sapply(tmp,function(e) if(length(e)>1) e[[2]]))
  tmp <- if(is.null(tmp)) NULL else  as.formula(paste("~",paste(tmp,collapse="+")))
  


  formula(resid$formMngr,as.call(c(as.name("+"),as.name("Single"),(resid$formulas$model)[[3]])),local=TRUE)
  formula(resid$formMngr,resid$formulas$fct)
  
  ## print(formula(formMngr))
  resid$func <- EBFunc(resid$formMngr$func,mode="default")

  resid$mode <- match.arg(mode)
  resid$weighted <- weighted

  resid$response <- eval(resid$formulas$model[[2]],parent.frame())
  if(missing(domainSize) && !is.null(resid$response)) domainSize <- resid$response$sizeIn
  if(length(domainSize)==1) domainSize <- c(domainSize,domainSize)
  resid$.domainSize <- domainSize
  if(weighted) {SumCache<-EBSumCacheCompFunc;SamplCache<-EBSamplCacheCompFunc}
  else {SumCache<-EBSumCache;SamplCache<-EBSamplCache}
  ## verboseMode: cat("sumCache->")
  ## poly may be NULL if response not initialized yet
  poly <- if(is.null(resid$response)) NULL else resid$response$pl
  resid$sumCache <- SumCache(resid$func,domainSize,poly) 
  ## verboseMode: cat("Done\n");cat("samplCache->")
  resid$samplCache <- SamplCache(resid$func,nbPts,domainSize,poly,mode) 
  resid$response_runs <- 0L
  resid
}

## OBSOLETE SOON!
# EBResid.OLD <- function(model,...,nbPts=10000,domainSize,mode=c("random","systematic"),weighted=FALSE) {
#   if(!inherits(model,"formula")) warning("parameter model has to be a formula!")
#   resid <- CqlsObj(EBResid)

#   ## build the ebfunc and formulas
#   tmp <- as.list(substitute(c(...)))[-1] #the expressions of the formulas
#   tmp <- strsplit(sapply(tmp,deparse),"\\|")
#   resid$formulas <- as.list(parse(text=paste("c(",paste(sapply(tmp,function(e) e[[1]]),collapse=","),")",sep="")))[[1]]

#   ## verboseMode: cat("formulas ->\n");print(resid$formulas)

#   tmp <- unlist(sapply(tmp,function(e) if(length(e)>1) e[[2]]))
#   tmp <- if(is.null(tmp)) NULL else  as.formula(paste("~",paste(tmp,collapse="+")))

#   model <- EBFunc(model,mode="default")
#   ## automatic conversion mode!!!
#   if(is.null(tmp)) {
#     tmp<- autoCompFuncFormula(resid$formulas)
#     if(!is.null(tmp$func)) {
#       resid$formulas <- tmp$form
#       tmp <- tmp$func
#     }
#   }
#   form<- if(is.null(tmp)) formula(model) else formula(model, EBFunc(tmp,mode="default")) 
#   resid$func <- EBFunc(form,mode="default")
#   resid$response <- eval(model$response)
#   if(missing(domainSize) && !is.null(resid$response)) domainSize <- resid$response$sizeIn
#   if(length(domainSize)==1) domainSize <- c(domainSize,domainSize)
#   resid$domainSize <- domainSize
#   resid$mode <- match.arg(mode)
#   resid$weighted <- weighted
#   if(weighted) {SumCache<-EBSumCacheCompFunc;SamplCache<-EBSamplCacheCompFunc}
#   else {SumCache<-EBSumCache;SamplCache<-EBSamplCache}
#   ## verboseMode: cat("sumCache->")
#   ## poly may be NULL if response not initialized yet
#   poly <- if(is.null(resid$response)) NULL else resid$response$pl
#   resid$sumCache <- SumCache(resid$func,domainSize,poly) 
#   ## verboseMode: cat("Done\n");cat("samplCache->")
#   resid$samplCache <- SamplCache(resid$func,nbPts,domainSize,poly,mode) 
#   resid$response_runs <- 0L
#   resid
# }

reactivate.EBResid <- function(resid) { 
  reactivate(resid$func)
  reactivate(resid$sumCache)
  reactivate(resid$samplCache)
  reactivate(eval(resid$response,parent.frame()))
}


## do not forget to launch resid$response <- vor (i.e. EBVor object) if not already provided in the formula call

update.EBResid <- function(resid,verbose=TRUE,cacheOf=NULL) {
  if(verbose) cat("Please wait: updating object EBResid ...")
  if(!is.null(resid$response)) { 
    resid$response <- eval(resid$formulas$model[[2]],parent.frame())
    resid$sumCache$poly <- resid$response$pl
    resid$samplCache$poly <- resid$response$pl
    update(resid$sumCache)
    update(resid$samplCache)
  }
  if(!is.null(cacheOf)) { #example: cacheOf=pseudo after a call to pseudo <- EBPseudoExpo(....)
    resid$sumCache <- cacheOf$sumCache
    resid$samplCache <- cacheOf$samplCache
    ## No Update or comparison!!!
  }
  .funcEnv$pas2 <- as.integer(sqrt(resid$samplCache$nbPts))^2
  resid$leftMat <- as.matrix(resid$samplCache)
  resid$rightMat <- as.matrix(resid$sumCache)
  prepare.formula.EBResid(resid)
  if(verbose) cat(" -> Done!\n")
}

prepare.formula.EBResid <- function(resid) {
  tmp <- resid$func$fct[[2]]$term$compFuncLoc
  tmp <- gsub("\\.c([[:digit:]]+)",".c[\\1]",tmp)
  tmp <- lapply(tmp,function(expr) as.list(parse(text=expr))[[1]])
  tmp2 <- resid$formMngr$formulas
  tmp2 <- lapply(tmp2,expression.replace,dict=tmp)
  resid$leftForm<-substitute(.form*exp(-(.V)),list(.V=tmp2[[1]],.form=tmp2[[2]]))
  resid$rightForm<-tmp2[[2]]
}

run.EBResid<-function(resid,...) {
  leftForm <- update(resid$leftForm,dict=list(...))
  left <- apply(apply(resid$leftMat,1,function(.c) eval(leftForm)),1,sum)/.funcEnv$pas2
  if(resid$compute!="noparam" || !exists("right",envir=resid)) resid$right <- apply(apply(resid$rightMat,1,function(.c) eval(resid$rightForm)),1,sum)/resid$domainSize[1]/resid$domainSize[2]
  left-resid$right
}

## copy of run with paramList as a list
runWithParamList.EBResid<-function(resid,paramList) {
  leftForm <- update(resid$leftForm,dict=paramList)
  left <- apply(apply(resid$leftMat,1,function(.c) eval(leftForm)),1,sum)/.funcEnv$pas2
  if(resid$compute!="noparam" || !exists("right",envir=resid)) resid$right <- apply(apply(resid$rightMat,1,function(.c) eval(resid$rightForm)),1,sum)/resid$domainSize[1]/resid$domainSize[2]
  left-resid$right
}


## OLD STUFF: TO SLOW because of Sum!!!!!
# update.EBResid <- function(resid) { 
#   if(!is.null(resid$response)) { 
#     resid$response <- eval(resid$formulas$model[[2]])
#     resid$sumCache$poly <- resid$response$pl
#     resid$samplCache$poly <- resid$response$pl
#     update(resid$sumCache)
#     update(resid$samplCache)
#   }
#   invisible()
# }

# run.EBResid <- function(resid,...) {
#   reactivate(resid)
#   update <- FALSE
#   if(exists("runs",envir=resid$response) && resid$response$runs != resid$response_runs) update<-TRUE
#   if(update) {
#     update(resid)
#     if(exists("runs",envir=resid$response)) resid$response_runs <- resid$response$runs
#   }

#   #assignment of variables!
#   vars <- list(...)
#   # particular case where param is a list of named parameters. Useful as the result of a estimation procedure.
#   if(length(vars)==1 && is.list(vars[[1]]) && !is.null(names(vars)) && all(names(vars[[1]])!="")) vars <- vars[[1]]
#   for(nm in names(vars)) assign(nm,vars[[nm]],envir=.funcEnv)

#   .funcEnv$pas2 <- as.integer(sqrt(resid$samplCache$nbPts))^2

#   resid$samplForm <- substitute(.form*exp(-(.V))/pas2,list(.V=resid$formMngr$formulas[[1]],.form=resid$formMngr$formulas[[2]]))
#   ##cat("samplForm->\n");print(samplForm)
#   resid$left<-Sum(resid$samplCache,resid$samplForm)
#   if(resid$compute!="noparam" || !exists("right",envir=resid)) resid$right <- Sum(resid$sumCache,resid$formMngr$formulas[[2]])/resid$domainSize[1]/resid$domainSize[2]
#   resid$left-resid$right
# }



## same as before but update is considered to be done via a first beforeRun and then nextRun for loop => used in Takacs-Fiksel
## param is supposed to be initialized
# beforeRun.EBResid <- function(resid) {
#   .funcEnv$pas2 <- as.integer(sqrt(resid$samplCache$nbPts))^2
#   resid$samplForm <- substitute(.form*exp(-.V)/pas2,list(.V=resid$formMngr$formulas[[1]],.form=resid$formMngr$formulas[[2]]))
# }

## OLD STUFF!
# nextRun.EBResid <- function(resid) {
#   resid$left<-Sum(resid$samplCache,resid$samplForm)
#   if(resid$compute!="noparam" || !exists("right",envir=resid)) resid$right <- Sum(resid$sumCache,resid$formMngr$formulas[[2]])/resid$domainSize[1]/resid$domainSize[2]
#   resid$left-resid$right
# }


residuals.EBVor <- function(vor, resid, ...) {
  if(is.null(resid$response) || !identical(resid$response,vor)) {
    resid$response <- vor
    resid$response_runs <- 0L #force the update!
  }
  run(resid, ...)
}

