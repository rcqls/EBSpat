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
##class EBPseudoExpo
EBPseudoExpo<-function(model,nbPts,domainSize,marks=NULL,mode=c("random","systematic"),weighted=FALSE) {
  EBFunc.mode("PseudoExpo") #used for conversion of formula
  ### debugMode: cat("formula -> EBFunc")
  if(inherits(model,"formula")) model<-EBFunc(model,"PseudoExpo")
  ### debugMode: cat("model->");print(model)
  pseudo <- if(weighted)
           CqlsObj(EBPseudoExpoWeightedMatrix,EBPseudoExpo,EBContrastOptim,EBReplicableGibbs)
         else 
           CqlsObj(EBPseudoExpoMatrix,EBPseudoExpo,EBContrastOptim,EBReplicableGibbs)
  pseudo$call <- match.call()
  pseudo$mode<-match.arg(mode)
  pseudo$weighted<-weighted
  if(missing(nbPts)) nbPts<-10000L
  pseudo$func<-model
  pseudo$response <- eval(pseudo$func$response,parent.frame()) # thanks to Ege!
  #added for communicating with EBFunc.new in order to declare the marks names
  .funcEnv$.marks.names <- NULL
  if(is.marked(pseudo$response)) .funcEnv$.marks.names <- pseudo$response$del.marks.name
  else if(!is.null(marks)) .funcEnv$.marks.names <- if(is.character(marks)) marks else marks$name
   
  if(missing(domainSize)) domainSize<-pseudo$response$pl$vor$sizeIn
  if(length(domainSize)==1) domainSize<-c(domainSize,domainSize)
  pseudo$.domainSize<-domainSize
  pseudo$.nbPts<-as.integer(nbPts)
  if(pseudo$weighted) {SumCache<-EBSumCacheCompFunc;SamplCache<-EBSamplCacheCompFunc}
  else {SumCache<-EBSumCache;SamplCache<-EBSamplCache}
  ## verboseMode: cat("sumCache->")
  pseudo$sumCache <- SumCache(pseudo$func,pseudo$domainSize,pseudo$response$pl)
  ## verboseMode: cat("Done\n");cat("samplCache->")
  pseudo$samplCache <- SamplCache(pseudo$func,pseudo$nbPts,pseudo$domainSize,pseudo$response$pl,pseudo$mode)
  ## verboseMode: cat("Done\n")
  #pseudo$samplCache$domainSize<-pseudo$domainSize
  #pseudo$par0<-c(Single=model$Single,unlist(sapply(model$nrg,function(e) unlist(e$term$params))))
  #attr(pseudo,"par")<-attr(pseudo,"par0")
  #TODO: if(!is.null(marks)) {
    #pseudo$del.marks.gen<-deparse(marks$gen)
#print(deparse(marks$gen))
    #.External("ebpseudoExpo_genMarks_set",pseudo, PACKAGE = "EBSpat")
  #}
  #class(pseudo)<-c("EBPseudoExpo","EBContrastOptim")

  init.cv2l.EBPseudoExpo(pseudo)
  pseudo$nbParam <- sum(unlist(pseudo$cv2l))  
  pseudo$par0 <- rep(0,pseudo$nbParam)
  pseudo$response_runs <- 0L #Normally update has to be done! 
  pseudo
}

init.cv2l.EBPseudoExpo <- function(pseudo) { 
  st<-list(Single=1)
  for(i in 2:length(pseudo$func$fct)) {
    st <- c(st,list(par=as.vector(pseudo$func$fct[[i]]$term$caracLoc.size)))
  }
  pseudo$cv2l <- Vector2ListConverter(st)
}

"param<-.EBPseudoExpo" <- function(pseudo,value) {
  pars <- by(pseudo$cv2l,value)
  pseudo$func$fct[[1]]$term$vars$Single <- pars$Single
  for(i in 2:length(pars)) pseudo$func$fct[[i]]$term$vars$par <- pars[[i]]
}

reactivate.EBPseudoExpo<-function(pseudo) { 
  #EBFunc.mode("PseudoExpo")
  reactivate(pseudo$func)
  reactivate(pseudo$sumCache)
  reactivate(pseudo$samplCache)
  reactivate(eval(pseudo$response))
}

print.EBPseudoExpo<-function(pseudo) {
  print(names(pseudo))
  return(pseudo)
}

#update 
update.EBPseudoExpoMatrix<-function(pseudo,verbose=TRUE) {
  #reactivate(pseudo)
  if(verbose) cat("Please wait: updating object EBPseudoExpo(Matrix) ...")
  update(pseudo$samplCache,verbose)
  pseudo$left <- cbind(1,as.matrix(pseudo$samplCache)) #<- as.list(pseudo$samplCache) 
  update(pseudo$sumCache,verbose)
  pseudo$right <-as.matrix(pseudo$sumCache)
  pseudo$rightTerm <- apply(cbind(1,pseudo$right),2,sum)/pseudo$domainSize[1]/pseudo$domainSize[2] 
  pseudo$updated<-TRUE
  if(verbose) cat(" -> Done!\n")
}

update.EBPseudoExpoWeightedMatrix<-function(pseudo,verbose=TRUE) {
  #reactivate(pseudo)
  if(verbose) cat("Please wait: updating object EBPseudoExpo(WeightedMatrix) ...")
  update(pseudo$samplCache,verbose)
  tmp <- as.data.frame(pseudo$samplCache)
  pseudo$leftWeight<-(tmp$weight)/sum(tmp$weight)
  pseudo$left<-cbind(1,as.matrix(tmp[-1]))
  #number of point
  #pseudo$leftNb <-  sum(pseudo$left[[1]]$weight)
  #list of indices
  #tmp<- sapply(pseudo$left,function(df) length(df)-1)
  #pseudo$leftInd <- lapply(1:length(tmp),function(i) seq((cumsum(c(0,tmp))+1)[i]+1,length=tmp[i]))
  update(pseudo$sumCache,verbose)
  pseudo$right <- as.data.frame(pseudo$sumCache)
  pseudo$rightTerm<-c(sum(pseudo$right$weight),t(pseudo$right$weight)%*%as.matrix(pseudo$right[-1]))/pseudo$domainSize[1]/pseudo$domainSize[2]
  #pseudo$right<- c(sum(tmp[[1]]$weight),sapply(tmp,function(df) as.vector(t(cbind(df[-1]) ) %*%df[[1]])))/pseudo$domainSize/pseudo$domainSize
  pseudo$updated<-TRUE
  if(verbose) cat(" -> Done!\n")
}

#pseudo et gradient en R
contrast.optim.EBPseudoExpoMatrix<-function(pseudo,param) {
  sum(pseudo$rightTerm*param)+mean(exp(-pseudo$left%*%param))
}

gradient.optim.EBPseudoExpoMatrix<-function(pseudo,param) {
  pseudo$rightTerm-apply(pseudo$left*as.vector(exp(-pseudo$left%*%param)),2,mean)
}

contrast.optim.EBPseudoExpoWeightedMatrix<-function(pseudo,param) {
  sum(pseudo$rightTerm*param)+(t(pseudo$leftWeight)%*%exp(-pseudo$left%*%param))
}

gradient.optim.EBPseudoExpoWeightedMatrix<-function(pseudo,param) {
  pseudo$rightTerm-apply(pseudo$left*as.vector(t(pseudo$leftWeight)%*%exp(-pseudo$left%*%param)),2,mean)
}

func.EBPseudoExpo<-function(pseudo,...) pseudo$func
terms.EBPseudoExpo<-function(pseudo,...) terms(func(pseudo))
summary.EBPseudoExpo<-function(pseudo,...) summary(func(pseudo),...)


identify.EBPseudoExpo<-function(pseudo,mode=1,...) {
  poly<-pseudo$response$pl
  plot(poly$vor,...)
  if(mode==1) pt<-unlist(locator(1))
  else pt<-identify(x=t(poly$vor$delVertex),label=poly$vor$delId,n=1,plot=FALSE)
  ##.External("ebpseudoExpo_statex_dv",pseudo,pt,poly,mode,PACKAGE = "EBSpat")
  pseudo$func[poly,pt]
}
