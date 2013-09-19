##class EBSim (just used to wrap ebsim) -> used in EBGibbs
EBSim<-function(func,poly,m=10000) {
  if(inherits(func,"formula")) func<-EBFunc(func,mode="Gibbs")
  sim <- CqlsObj(EBSim)
  sim$extPtr<-.ExternalInEnvir("EBSim_new",func,poly,m,envir=sim, PACKAGE = "EBSpat")
  sim$pl<-poly
  sim$func<-func
  sim$m<-m
  sim$inside<-FALSE
  reg.finalizer(sim,free.externalPtr,TRUE)
  ##attr(sim,"del.marks.gen") set in EBGibbs!!!
  sim
}

print.EBSim <- function(sim,...) cat("EBSim\n")

reactivate.EBSim<-function(sim) reactivate.externalPtr(sim)

run.EBSim<-function(sim,m) {
  if(!missing(m)) sim$m<-m
  .External("EBSim_run",sim$extPtr, PACKAGE = "EBSpat")
  invisible()
}

"$<-.EBSim"<-function(sim,key,value) {
  key<-match.arg(key,c("inside","func","m","pl","extPtr"))
  switch(key,
  inside={
    .External("EBSim_domainIn_set",sim$extPtr,value, PACKAGE = "EBSpat")
  },
  func={
    .External("EBSim_func_set",sim$extPtr,value, PACKAGE = "EBSpat")
  },
  m={
    .External("EBSim_m_set",sim$extPtr,value, PACKAGE = "EBSpat")
  },
  pl={
    .External("EBSim_poly_set",sim$extPtr,value, PACKAGE = "EBSpat")
  }
  )

  "$<-.Binding"(sim,key,value)
  sim
}

##class EBGibbs
EBGibbs<-function(func,m=10000,nbmax=1000000,center=NULL,centerIn=NULL,size=NULL,sizeIn=NULL,...) {
  call <- match.call()
  EBFunc.mode("Gibbs")
  marks<-list(...)$marks

  #added for communicating with EBFunc.new
  .funcEnv$.marks.names <- if(is.null(marks)) NULL else marks$name 


  if(inherits(func,"EBReplicableGibbs")) {
    rep <- func 
    func <- rep$func
    if(!inherits(rep,"EBGibbs")) rep <- rep$response
    center<-rep$center
    centerIn<-rep$centerIn
    size<-rep$size
    sizeIn<-rep$sizeIn
  }

  if(is.null(center)) center<-c(0,0)
  if(is.null(centerIn)) centerIn <- center
  if(is.null(size)) size <- 700
  if(is.null(sizeIn)) sizeIn <- 500

  gibbs<-EBVor(pts=nbmax,center=center,centerIn=centerIn,size=size,sizeIn=sizeIn,marks=marks) #automatically generated
  #insert attr "sim" in order to simulate it
  if(inherits(func,"formula")) func<-EBFunc(func,mode="Gibbs")
  sim<-EBSim(func,gibbs$pl,m)
  #if(!is.null(marks)) {
  #  attr(sim,"del.marks.gen")<-deparse(marks$gen)
#print(deparse(marks$gen))
  #  .External("EBSim_genMark_set",sim, PACKAGE = "EBSpat") 
  #}

  gibbs$func <- func # to make possible the replicate this object
  gibbs$sim<-sim
  #notice that it inherits of EBVor methods
  gibbs$call <- call
  class(gibbs)<-c("EBGibbs",class(gibbs),"EBReplicableGibbs")
  gibbs
}

print.EBGibbs <- function(sim,...) cat("EBGibbs\n")

reactivate.EBGibbs<-function(gibbs) {
  EBFunc.mode("Gibbs")
  reactivate.externalPtr(gibbs,"EBVor")
  reactivate.externalPtr(gibbs$sim)
}


"param.EBGibbs"<-function(gibbs,...,names=FALSE) param(gibbs$sim$func,callR=match.call(),names=names,args=NULL) 

EBPoints.EBGibbs<-function(gibbs,nbPts=1) {
  if(is.null(gibbs$del.marks.type)) {
    EBPoints(gibbs$delVertex[,-(1:3)],center=gibbs$center,size=gibbs$size,sizeIn=gibbs$sizeIn,nbPts=1)
  } else {
    marks<-list(length=gibbs$del.marks.length,name=gibbs$del.marks.name,type=gibbs$del.marks.type,gen=gibbs$sim$del.marks.gen)
    EBPoints(gibbs$delVertex[,-(1:3)],center=gibbs$center,size=gibbs$size,sizeIn=gibbs$sizeIn,marks=marks,nbPts=1)
  }
}
 
run.EBGibbs<-function(gibbs,m,plot=TRUE,update=TRUE,type=c("dv"),...) {
  #reactivate(gibbs)
  EBFunc.mode("Gibbs")
  params<-param(gibbs)
  #maybe some parameters to update
  args<-list(...)
  isParams=names(args) %in% names(params)
  args<-args[isParams]
  if(length(args)>0) param(gibbs$sim$func,args=args) 
  #initialisation of parameters for the simulation
  params<-param(gibbs)
  paramsNeedInit<-sapply(params,is.character)
  if(any(paramsNeedInit)) 
    warning(paste(paste(names(params)[paramsNeedInit],collapse=" "),if(sum(paramsNeedInit)>1) "need" else "needs","to be initialized first via param method!"))
  else {
    ##cat("run.EBGibbs: params->");print(params)
    for(p in names(params)) assign(p,params[[p]],envir=.funcEnv)
    if(missing(m)) run(gibbs$sim) else run(gibbs$sim,m)
    .External("EBSim_nb_dv", gibbs$sim$extPtr, PACKAGE = "EBSpat")
    if(plot) {
      update(gibbs$sim$pl)
      plot(gibbs)
      #do.call("plot",c(list(gibbs,type=type,args[!isParams])))
    } else {
      if(update) update(gibbs$sim$pl) else warning("Execute update(gibbs$sim$pl) to manually update the Voronoï graph!")
    }
    # number of runs updated!
    gibbs$runs <-  gibbs$runs + 1
  }
  invisible()
}

func.EBGibbs<-function(gibbs,...) gibbs$sim$func
terms.EBGibbs<-function(gibbs,...) terms(func(gibbs))
summary.EBGibbs<-function(gibbs,...) summary(func(gibbs),...)
