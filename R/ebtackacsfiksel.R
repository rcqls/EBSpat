#require(getattr)
 
## TODO: 
## 1) run(pseudo,...,gridSize=?,...) 
## C part: set nbPtsGrille and a reallocation of the statex  
## 2) run(pseudo,...,domainSize=?,...) 
## C part: set tailleDomaine
## 3) statex à changer pour calculer la matrice de covariance des estimateurs!

## How this works?
## 1) libebpseudo: used here to calculate the statExG and statExND needed to compute 
## the pseudo and the gradient in the exponential case
## 2) there are two ways to compute of the log-pseudo and its gradient:
## a) inside R from the statex provided by  libebpseudo
## b) inside libebpseudo: no longer available since not in the R spirit! 


####################################################################################################################
##class EBTakacsFiksel
EBTakacsFiksel<-function(model,...,nbPts,domainSize,marks=NULL,mode=c("random","systematic"),weighted=FALSE,compute=c("normal","noparam","inverse")) {
  if(missing(nbPts)) nbPts<-10000L
  tk <- CqlsObj(EBTakacsFiksel,EBContrastOptim)
  tk$call <- match.call()
  tk$resid <- EBResid(model,...,nbPts=nbPts,domainSize=domainSize,mode=mode,weighted=weighted,compute=compute)
  tk$response <- tk$resid$response
  response <- eval(tk$response,parent.frame())
  #added for communicating with EBFunc.new in order to declare the marks names
  .funcEnv$.marks.names <- NULL
  if(is.marked(eval(response))) .funcEnv$.marks.names <- response$del.marks.name
  else if(!is.null(marks)) .funcEnv$.marks.names <- if(is.character(marks)) marks else marks$name
   
  tk$par0 <- NULL
  
  tk$response_runs <- 0L #Normally update has to be done! 
  tk
}

reactivate.EBTakacsFiksel<-function(tk) {
  reactivate(tk$resid)
}

print.EBTakacsFiksel<-function(tk) {
  print(names(tk))
  return(tk)
}

# #update 
# prepare.formula.EBTakacsFiksel <- function(tk) {
#   tmp <- tk$resid$func$fct[[2]]$term$compFuncLoc
#   tmp <- gsub("\\.c([[:digit:]]+)",".c[\\1]",tmp)
#   tmp <- lapply(tmp,function(expr) as.list(parse(text=expr))[[1]])
#   tmp2 <- tk$resid$formMngr$formulas
#   tmp2 <- lapply(tmp2,expression.replace,dict=tmp)
#   tk$leftForm<-substitute(.form*exp(-(.V)),list(.V=tmp2[[1]],.form=tmp2[[2]]))
#   tk$rightForm<-tmp2[[2]]
# }

update.EBTakacsFiksel <- function(tk,verbose=TRUE) {
  if(verbose) cat("Please wait: updating object EBTakacsFiksel ...")
  update(tk$resid)
  tk$updated<-TRUE
  if(verbose) cat(" -> Done!\n")
}

contrast.optim.EBTakacsFiksel<-function(tk,param) {
  #convert param to list
  ## if(!exists("param.converter",envir=tk)) stop("Do not know how to convert param!")
  ## THIS ONE IS TOO SLOW!
  #by(tk$param.converter,param,envir=.funcEnv) #transfer param to .funcEnv
  #sum(nextRun.EBResid(tk$resid)^2)
  ## THIS ONE SEEMS TO REASONABLE
  #left <- apply(apply(tk$leftMat,1,function(l) exp(-(param[1]+sum(param[-1]*l)))*c(1,l)),1,sum)/.funcEnv$pas2
  #right <- c(nrow(tk$rightMat),apply(tk$rightMat,2,sum))/tk$resid$domainSize[1]/tk$resid$domainSize[2]
  ## MODIF
  # leftForm <- update(tk$leftForm,dict=by(tk$param.converter,param))
  # left <- apply(apply(tk$leftMat,1,function(.c) eval(leftForm)),1,sum)/.funcEnv$pas2
  # if(tk$resid$compute!="noparam" || !exists("right",envir=tk)) tk$right <- apply(apply(tk$rightMat,1,function(.c) eval(tk$rightForm)),1,sum)/tk$resid$domainSize[1]/tk$resid$domainSize[2]
  ## Slight change from the previous idea 
  ## PB: je ne sais pas où on définit tk$param.converter (s'inspirer de EBPseudoExpo qui a un cv2l)
  sum(runWithParamList.EBResid(tk$resid,by(tk$param.converter,param))^2)
}

has.gradient.optim.EBTakacsFiksel  <- function(obj,...) FALSE # => no need to define: gradient.optim.EBTakacsFiksel

func.EBTakacsFiksel<-function(pseudo,...) pseudo$func
terms.EBTakacsFiksel<-function(pseudo,...) terms(func(pseudo))
summary.EBTakacsFiksel<-function(pseudo,...) summary(func(pseudo),...)

residuals.EBTakacsFiksel <- function(tk,param) {

}


##class EBTakacsFikselExpo
EBTakacsFikselExpo<-function(model,...,nbPts,domainSize,marks=NULL,mode=c("random","systematic"),weighted=FALSE,compute=c("normal","noparam","inverse")) {
  if(missing(nbPts)) nbPts<-10000L
  tk <- CqlsObj(EBTakacsFikselExpo,EBContrastOptim)
  tk$call <- match.call()
  tk$resid <- EBResid(model,...,nbPts=nbPts,domainSize=domainSize,mode=mode,weighted=weighted,compute=compute)
  tk$response <- tk$resid$response
  response <- eval(tk$response)
  #added for communicating with EBFunc.new in order to declare the marks names
  .funcEnv$.marks.names <- NULL
  if(is.marked(eval(response))) .funcEnv$.marks.names <- response$del.marks.name
  else if(!is.null(marks)) .funcEnv$.marks.names <- if(is.character(marks)) marks else marks$name
   
  tk$par0 <- NULL
  
  tk$response_runs <- 0L #Normally update has to be done! 
  tk
}
