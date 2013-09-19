## The only interface to optimize a contrast (see EBPseudoExpo)
# add the class EBContrastOptim 
# object has to respond to the method update, contrast.optim (and gradient.optim)


run.EBContrastOptim<-function(self,par0,method=NULL,update=FALSE,verbose=TRUE,fixed,...) {
  ####  selfname<-object.name() #save the name of the object
  ## update stuff (autodetection + action)!
  if(!("updated" %in% names(self))) update<-TRUE
  else {
    ###tmp<-.funcEnv$eventMngr[[parent=as.character(self$func$response),child=selfname]]
    ###if(!is.null(tmp) && length(tmp) && tmp) update<-TRUE
    if(exists("runs",envir=self$response) && self$response$runs != self$response_runs) update<-TRUE
  }
  if(update) {
    update(self,verbose)
    ###tmp<-.funcEnv$eventMngr[[parent=as.character(self$func$response),child=selfname]]
    ###if(is.null(tmp) || length(tmp)==0) .funcEnv$eventMngr[[parent=as.character(self$func$response),child=selfname]]
    ###.funcEnv$eventMngr[[parent=as.character(self$func$response),child=selfname]]<-FALSE #now updated
    self$response_runs <- self$response$runs
  }

  ## parameters stuff!
  if(missing(par0))  {
    if("par" %in% names(self)) param <- self$par #not the first run 
    else param<-self$par0 #first run
  } else if(is.null(par0)) param<-self$par0
  else param<-par0
  ## cat("param->");print(param)
  if(!is.null(self$par0) && length(param)!=length(self$par0)) param<-self$par0
  ## Check: print(param)
  ## fixed and functions stuff!
  if(missing(fixed)) fixed<-rep(FALSE,length(param))
  else if(is.numeric(fixed)) {
    fixedInd<-fixed
    fixed<-rep(FALSE,length(param))
    fixed[fixedInd]<-TRUE
  }

  fn<-function(par) {
    ##print(par);print(param[!fixed])
    param[!fixed]<-par
    ##cat("param->");print(param)
    contrast.optim(self,param)
  }

  gr <- NULL #gradient to appproximate except
  if(has.gradient.optim(self)) { 
    gr <- function(par) {
        param[!fixed]<-par
        gradient.optim(self,param)[!fixed]
    }
  }
  
  ## optim stuff!
  if(is.null(method) || method=="fast") {
    if(length(param[!fixed])>1) param[!fixed]<-(res <- optim(param[!fixed],fn,gr,method="Ne",...))$par
    if(is.null(method)) res<-optim(param[!fixed],fn,gr,method="CG",...)
  } else res<-optim(param[!fixed],fn,gr,method=method,...)
  
  #fixed tips
  param[!fixed]<-res$par
  res$par<-param
  
  if(verbose) print(res)

  ## save stuff
  self$optim<-res
  self$par<-res$par
  param(self) <- res$par
  res$par
}

## to be called first to specify the structure of the 
## maybe investigate a default param.converter behavior param[1] -> Single, param[2] -> ???, .... 
##  Rmk: not used in EBPseudoExpo but in EBTackacsFiksel (and EBPseudo I guess)
param.EBContrastOptim <- function(self,...,lengths) {
  if(missing(lengths)) {
    vars <- list(...)
    if(length(vars)==0) return(self$par)
    if(length(vars)==1 && is.list(vars[[1]])) vars <- vars[[1]]
    lengths <- sapply(vars,length)
    names(lengths) <- names(vars)
    self$par0 <- self$par <- unlist(vars) 
  }
  self$param.converter <- Vector2ListConverter(lengths)
  return(invisible())
}

summary.EBContrastOptim<-function(self) {
  self$optim
}