#require(getattr)

##class EBVor
EBVor <- function(pts,center=NULL,size=NULL,centerIn=NULL,sizeIn=NULL,marks) {
  call <- match.call()
  if(missing(pts)) pts <- 500
  if(is.numeric(pts) && length(pts)==1) {
    nbmax <- pts
    pts <- NULL
  } else if(inherits(pts,"ppp")) {
    nbmax <- pts$n+1
    if(is.null(center) && pts$win$type=="rectangle") {
      center<-c(mean(pts$win$xrange),mean(pts$win$yrange))
    } 
    if(is.null(size)) {
      if(pts$win$type!="rectangle") stop("Object of class 'ppp' is not a rectangle!")
      size<-c(diff(pts$win$xrange),diff(pts$win$yrange))
    }
    pts <- rbind(pts$x,pts$y)
  } else if((isMat <- is.matrix(pts)) || is.numeric(pts)) {
    if(!isMat) pts <- matrix(pts,nrow=2)
    if(NROW(pts)>2 && NCOL(pts)==2) pts <- t(pts)
    nbmax <- NCOL(pts)+1
    if(is.null(center) || is.null(size)) {
      tmp <- apply(pts,1,range)
      if(is.null(center)) center <- apply(tmp,2,mean)
      if(is.null(size)) size <- apply(tmp,2,diff)
    }
  }

  if(is.null(size)) size <- 700 
  if(is.null(sizeIn)) sizeIn <- size
  if(is.null(center)) center <- c(0,0) 
  if(is.null(centerIn)) centerIn <- center

  vor <- CqlsObj(EBVor)
  vor$call <- call
  if(length(size)==1) size<-c(size,size)
  if(length(center)==1) center<-c(center,center)
  sizeMax <- max(c(abs(center[1]+c(-1,1)*size[1]),abs(center[2]+c(-1,1)*size[2])))*2*1.1
  ## Check: print(center);print(size);print(sizeMax)
  vor$extPtr <- .ExternalInEnvir("EBVor_new", nbmax ,envir=vor, PACKAGE = "EBSpat")
  .External("EBVor_init",vor$extPtr,PACKAGE="EBSpat")
  vor$nbmax <- nbmax
  vor$sizeMax <- sizeMax
  vor$pl <- EBPoly(vor)
  if(!is.null(center)) {
    vor$center <- center ###.External("EBVor_center_set", vor,center, PACKAGE = "EBSpat")
    if(is.null(centerIn)) centerIn <- center
  }
  if(!is.null(centerIn)) vor$centerIn <- centerIn ##.External("EBVor_centerIn_set", vor,centerIn, PACKAGE = "EBSpat")
  if(!is.null(size)) {
    if(length(size)==1) size <- c(size,size)
    vor$size <- size ##.External("EBVor_size_set", vor,size, PACKAGE = "EBSpat")
    if(is.null(sizeIn)) sizeIn <- size
  }
  if(!is.null(sizeIn)) {
    if(length(sizeIn)==1) sizeIn <- c(sizeIn,sizeIn)
    vor$sizeIn <- sizeIn ###.External("EBVor_sizeIn_set", vor,sizeIn, PACKAGE = "EBSpat")
  }
  ## Check: print(sizeMax)
  #
  .External("EBVor_initExt",vor$extPtr,PACKAGE="EBSpat")
  if(!missing(marks)) EBMarks.set(vor,marks) else {
    vor$delMarks <- NULL
    vor$del.marks.name <- NULL
  }
  #print("ici")
  #{ 
    #attr(obj,"del.marks.length")<-marks$length
    #attr(obj,"del.marks.name")<-marks$name
    #attr(obj,"del.marks.type")<-marks$type
    #attr(obj,"del.marks.gen")<-deparse(marks$gen)
    #.External("EBVor_marks_set", obj, PACKAGE = "EBSpat")
  #}
  ### vor$dyn <- new.env()  #to save dynamic stuff
  vor$plot_args <- list("initial default plot (only delaunay vertices, i.e type='dv')"=list(type="dv")) #for instance, the list of arguments for a plot
  vor$plot_current <- 1L #no selection
  vor$plot_runMode <- NULL
  reg.finalizer(vor,free.externalPtr,TRUE)
  update(vor)
  ## Todo: consider marks if pts is a data.frame
  if(!is.null(pts)) {
    insert(vor,pts)
  }
  #print("ici2")
  #runs used in ebstat (for example, EBPseudo) to know about the evolution of "gibbs" 
  vor$runs <- 0
  vor
}

is.marked.EBVor <- function(vor) !is.null(vor$delMarks)

reactivate.EBVor<-function(vor) reactivate.externalPtr(vor)

#TODO: modifier nbmax => recréer un nouveau EBVor et insérer les points de l'ancien puis supprimer l'ancien!

#length is a primitive but also a generic function
length.EBVor <- function(vor) NCOL(vor$delVertex)-3
seq.EBVor <- function(vor) 1:length(vor)


#"size<-.EBVor"<-function(obj,value) {
#  .External("EBVor_size_set", obj,value, PACKAGE = "EBSpat")
#  value
#}

"$<-.EBVor"<-function(vor,key,value) {
  switch(key,
    center={
      if(!is.null(.External("EBVor_center_set", vor$extPtr,value, PACKAGE = "EBSpat"))) "$<-.Binding"(vor,key,value)
    },
    centerIn={
      if(!is.null(.External("EBVor_centerIn_set", vor$extPtr,value, PACKAGE = "EBSpat"))) "$<-.Binding"(vor,key,value)
    },
    size={ #TODO: scaleExt can be updated (dangerously) if needed!
      ## Check: print(value)
      if(length(value)==1) value <- c(value,value)
      if(!is.null(.External("EBVor_size_set", vor$extPtr,value, PACKAGE = "EBSpat"))) "$<-.Binding"(vor,key,value)
    },
    sizeIn={
      ## Check: print(value)
      if(length(value)==1) value <- c(value,value)
      if(!is.null(.External("EBVor_sizeIn_set", vor$extPtr,value, PACKAGE = "EBSpat"))) "$<-.Binding"(vor,key,value)
    },
    {
      "$<-.Binding"(vor,key,value)
    })
  vor
}

notEmbeddedDomain <-function(center,size,centerIn,sizeIn) {
  all(c(abs(center - (centerIn-sizeIn/2)) < size/2,abs(center - (centerIn+sizeIn/2)) < size/2))
} 

#as in the spirit of R, this returns an EBVor object!
"[.EBVor" <- function(vor,pts,center,size,centerIn,sizeIn,addPts=1) {
  mode <- "domain"
  if(!missing(pts)) {
    if(isTRUE(all.equal(pts,as.integer(pts)))) mode <- "pts"
    else if(is.numeric(pts) && length(pts)==2) center <- pts 
  }
  if(missing(center)) center <- vor$center
  if(missing(size)) size <- vor$size
  if(length(size)==1) size <- c(size,size)
  if(missing(centerIn)) centerIn <- vor$centerIn
  if(missing(sizeIn)) sizeIn <- vor$sizeIn
  if(length(sizeIn)==1) sizeIn <- c(sizeIn,sizeIn)
  if(mode=="domain") {
    if(notEmbeddedDomain(center,size,centerIn,sizeIn)) {} #sizeIn <- 10 #TODO
    #print(center);print(size)
    pts <- which.inside(vor,center,size)
  }
  if(is.marked(vor)) {
    newVor <- EBVor(nb=length(pts)+1,center,size,centerIn,sizeIn,marks=EBMarks.get(vor))
#print(pts+3);print(vor$delVertex[,pts+3]);print(vor$delMarks[pts+3,])
    insert(newVor,vor$delVertex[,pts+3],vor$delMarks[pts+3,])
  } else {
    newVor <- EBVor(nb=length(pts)+1,center,size,centerIn,sizeIn)
#print(pts+3);print(vor$delVertex[,pts+3]);print(vor$delMarks[pts+3,])
    insert(newVor,vor$delVertex[,pts+3])
  }
  newVor
}

"[[.EBVor"<-function(vor,pt,func,types) {
  newFunc <- FALSE # to fasten the cleaning of external pointer!
  if(missing(pt)) pt<- NULL
  if(inherits(pt,"formula")) {
    func<-pt;pt<- NULL
  }
  if(missing(func) & inherits(vor,"EBGibbs")) func<-infosFormulaFuncFromFunc(vor$sim$func)
  if(inherits(func,"formula")) {
    #added for communicating with EBFunc.new in order to consider v$m for example!
    .funcEnv$.marks.names <- if(is.marked(vor)) vor$del.marks.name else NULL 
    func<-EBFunc(func,mode="default") ;newFunc <- TRUE
  }
  ###debugMode: cat("pt ->");print(pt)
  res <- func[[vor,pt,types]]
  if(newFunc) free.externalPtr(func) ## cleaning the newly created EBFunc!
  res
}

insideDomain <- function(pt,center,size) all(abs(pt-center)<size/2)

split.EBVor <- function(vor,center,size) {
  if(missing(center)) center <- vor$centerIn
  if(missing(size)) size <- vor$sizeIn
  if(length(size)==1) size <- c(size,size)
  inside<-apply(vor$delVertex[,4:NCOL(vor$delVertex)],2,insideDomain,center=center,size=size)
  list(interior=which(inside),exterior=which(!inside))
}

which.inside <- function(vor,center,size) split(vor,center,size)$interior
which.outside <- function(vor,center,size) split(vor,center,size)$exterior
  
## pts =  missing -> global
## pts is integer -> local with points at index pts
## pts is numeric -> local with points with coordinate pts         
energy.EBVor <- function(vor,pts=NULL,model,centerIn,sizeIn,...) { #model is a formula to declare a model
  newFunc <- FALSE
  func <- NULL
  if(inherits(pts,"formula")) {
    if(!missing(model) && (is.numeric(model) || is.character(model))) tmp <- model else tmp <- NULL
      model <- pts; pts <- tmp
  } 
  if(!missing(model)) func <- model
  if(inherits(vor,"EBGibbs")) func<-vor$sim$func
  if(inherits(func,"formula")) {func<-EBFunc(func,mode="default");newFunc <- TRUE}
  if(is.null(func)) {
    stop("No model specified!")
  }
  if(!is.null(pts) && is.logical(pts)) sizeIn <- pts #delegate so sizeIn=TRUE or FALSE
  if(!missing(sizeIn) || !missing(centerIn)) {
    if(missing(centerIn)) centerIn <- vor$centerIn
    if(missing(sizeIn)) sizeIn <- vor$sizeIn
    if(is.logical(sizeIn) && sizeIn) sizeIn <- vor$sizeIn
    if(is.logical(centerIn) && centerIn) centerIn <- vor$centerIn
    ##debugMode: cat("centerIn,sizeIn -> ");print(centerIn);print(sizeIn)
    if(is.numeric(centerIn) && is.numeric(sizeIn))
      pts <- which.inside(vor,centerIn,sizeIn)
    else stop("No way to decide what you want!")
  }
  if(is.null(pts)) local<- -1 #global
  else if(is.numeric(pts)) local<-1
  else local<- -1 #global
  #First, initialize params in .funcEnv 
  params <- param(func)
  for(p in names(params)) assign(p,params[[p]],envir=.funcEnv)
  #Energy calculation
  if(local>0) {#local
    if(isTRUE(all.equal(pts,as.integer(pts)))) {
      local <- if(length(pts)>1) 10 else 1
      if(any(pts<=0 | pts>ncol(vor$pl$vor$delVertex)-3)) {
        cat("Attention: pts is not a suitable index \n",sep="")
        return(invisible())
      }
       
    } else {
      local<-2
      marks <- multi.data.frame(...)
      if(length(pts)>2) return("Not considered yet")
    }
    
  } 

  res<-NULL
  if(local<0) {#cat("Global mode!\n")
    res <- .External("EBFunc_globalEnergy",func$extPtr,vor$pl$extPtr,package="EBSpat")
  } else if (local<10) {#cat("Local mode!\n")
    if(local==1) makeSup(vor$pl,as.integer(pts))
    if(local==2) makeIns(vor$pl,pts)  
    applyMake(vor$pl) 
    res <- .External("EBFunc_localEnergy",func$extPtr,vor$pl$extPtr,package="EBSpat")
    cancelMake(vor$pl)
    finalMake(vor$pl)
  } else if(local==10) {#real deletion and reinsertion have to be done!
    # save the points and marks!
    pts <- pts[order(vor$delId[pts+3])] #ordered such that normally after deletion and reinsertion the structure has to be the same! An update would be done for security!
    xyPts <- vor$delVertex[,pts+3]
    marksPts <- if(!is.null(vor$delMarks)) vor$delMarks[pts+3,] else multi.data.frame()
    ##debugMode: cat("pts ->");print(pts);print(xyPts);print(marksPts)
    ## delete the points
    delete(vor,as.integer(pts))
    #reinsertion by incrementing the local energy
    res <- 0
    for(i in seq(pts)) {
      ###debugMode: cat("Point",i," inséré->",sep="");print(xyPts[,i])
      makeIns(vor$pl,xyPts[,i],marks=marksPts[i,])
      ###debugMode: cat("Current Point ->");print(currentPoint(vor$pl))
      applyMake(vor$pl)
      res <- res + .External("EBFunc_localEnergy",func$extPtr,vor$pl$extPtr,package="EBSpat")
      ###debugMode: cat("Current Point ->");print(currentPoint(vor$pl))
      applyMake(vor$pl) #VERY IMPORTANT since cancelMake is done automatically when calling "EBFunc_localEnergy".
      finalMake(vor$pl)
    }
    update(vor)
  }
  if(newFunc) free.externalPtr(func) ## cleaning the newly created EBFunc
  res
}

insert.EBVor<-function(vor,pts,...) insert(vor$pl,pts,...)

delete.EBVor<-function(vor,pts) delete(vor$pl,pts)

empty.EBVor<-function(vor,inside=FALSE,plot=TRUE,type=c("dv"),...) {
  empty(vor$pl,inside)
  if(plot) plot(vor,type=type,...)
  invisible()
}

updateVertex.EBVor<-function(vor) {
  .External("EBVor_vertex", vor$extPtr, PACKAGE="EBSpat")
  invisible()
}

updateVorEdge.EBVor<-function(vor) {
  .External("EBVor_vorEdge", vor$extPtr, PACKAGE="EBSpat")
  invisible()
}

updateDelEdge.EBVor<-function(vor) {
  .External("EBVor_delEdge", vor$extPtr, PACKAGE="EBSpat")
  invisible()
}

updateEdge.EBVor<-function(vor) {
  updateVorEdge.EBVor(vor)
  updateDelEdge.EBVor(vor)
  invisible()
}

update.EBVor<-function(vor,force=TRUE) {
  if(!force) force<-is.null(vor$vorVertex) #check if vorVertex is NULL before update!
  if(force) {
    updateVertex.EBVor(vor)
    updateEdge.EBVor(vor)
  }
  invisible()
}

vorGraph.EBVor<-function(vor,...) .External("EBVor_vorGraph",vor$extPtr,PACKAGE="EBSpat")

print.EBVor<-function(obj,...) cat("EBVor\n")


plotSquareDomain <- function(center,size,...) {
  if(length(size)==1) size <- c(size,size)
  lines(center[1]+c(-1,1,1,-1,-1)*size[1]/2,center[2]+c(-1,-1,1,1,-1)*size[2]/2,...)
}

#type= "dv" or "DV"=Delaunay Vertex,"vv" or "VV"=Voronoi Vertex ,
#      "de" or "DE"=Delaunay Edge,"ve" or "VE"=Voronoi Vertex,
#      "vc" or "VC"=Voronoi Cell

plot.EBVor<-function(vor,type,xlab="",ylab="",main="",center,size,dvArgs,vvArgs,deArgs,veArgs,vcArgs,nngArgs,dvCol,vcCol,nngCol,dvCex,vvCex,dvPch,vvPch,domain=list(),domainIn=NULL,add=FALSE,vc.colors=cm.colors,dv.colors=heat.colors,dv.cex=c(1,.5),dv.pch=c(15:25,1:14),windowSize.inches,new.plot=FALSE,saveArgsPlot=TRUE,...) {
  ##### convenient param stuff!
  replot <- NULL
  callPlot <- deparse(sys.call(sys.parent()))
  ###debugMode: cat("callPlot ->");print(callPlot)
  argsPlot <- as.list(match.call())[-(1:2)]
  ###debugMode: cat("argsPlot ->");print(argsPlot)
  if(length(argsPlot)==0 && length(vor$plot_args)>0) {
    if(vor$plot_current==0L) vor$plot_current <- length(vor$plot_args) #last created
    replot <- vor$plot_current
  }
  if(!missing(type) && is.numeric(type) && length(type)==1) replot <- type

  if(!is.null(replot)) {
    if(replot<=0 || replot>length(vor$plot_args)) {
      if(replot<0) {
        replot <- -replot
        vor$plot_args[[replot]]<-NULL
        if(replot == vor$plot_current) vor$plot_current <- length(vor$plot_args)
      }
      if(length(vor$plot_args)>0) {
        cat("Available choice",ifelse(length(vor$plot_args)>1,"s",""),": (=> stands for the current)\n",sep="")
        for(i in 1:length(vor$plot_args)) {
          cat(i,":",if(vor$plot_current==i) "=>" else "  " ,names(vor$plot_args)[i], "\n",sep="")
        }
      } else cat("No choice available for plot!\n")
      return(invisible(vor$plot_args))
    } else {
      do.call("plot",c(list(vor),vor$plot_args[[replot]]))
      vor$plot_current <- replot
      return(invisible())
    }
  }
  ## save argsPlot if not the same
  if(saveArgsPlot && substr(callPlot,1,4)=="plot" &&  (missing(type) || is.character(type))) {
    if(length(argsPlot)>0 && all(sapply(vor$plot_args,function(args) !identical(argsPlot,args)))) 
      vor$plot_args[[callPlot]] <- argsPlot #save the last plot arguments!
      vor$plot_current <- length(vor$plot_args)
      if(length(vor$plot_args)>100) warning("Maybe, you should delete some plot arguments by calling first plot(vor,0)!")
  }

  ##### ploting stuff!
  if(is.null(vor$vorVertex)) update(vor)
  if(missing(type)) {
    type2<-NULL
    if(!missing(vcCol)) type2<-c(type2,"vc")
    if(!missing(dvCol)) type2<-c(type2,"dv")
    if(!missing(dvCex)) type2<-c(type2,"dv")
    if(!missing(dvPch)) type2<-c(type2,"dv")
    if(!missing(deArgs)) type2<-c(type2,"de")
    if(!missing(veArgs)) type2<-c(type2,"ve")
    if(!is.null(type2)) type<-unique(type2) else type<-c("dv","vv","de","ve")
  } else {
    type<-tolower(type)
    if(!missing(vcCol)) type<-c(type,"vc")
    if(!missing(dvCol)) type<-c(type,"dv")
    if(!missing(dvCex)) type<-c(type,"dv")
    if(!missing(dvPch)) type<-c(type,"dv")
    type<-unique(type)
  }
  if(missing(size)) size<-vor$size
  if(missing(center)) center<-vor$center
  if(!add) {
    if(size[1]!=size[2]) {
      if(missing(windowSize.inches))  windowSize.inches <- 7
      if(length(windowSize.inches)==1) {
        windowSize.inches <- if(size[1] > size[2]) c(1,size[2]/size[1])*windowSize.inches
                             else                  c(size[1]/size[2],1)*windowSize.inches
      }
      #cat("Avant");print(dev.list());print(dev.cur())
      #if(substr(names(dev.cur()),1,3)=="X11") {
      #  dev.off()
        #cat("Apres dev.off");print(dev.list());print(dev.cur())
      #}
      if(is.null(dev.list()) || new.plot) dev.new(width=windowSize.inches[1],height=windowSize.inches[2])
      #cat("Apres x11");print(dev.list());print(dev.cur())
    }
    if(!is.null(vor$plot_runMode)) main=paste(vor$plot_runMode,"mode (right click to change)")
    plot(center[1]+c(-1,1)*size[1]/2,center[2]+c(-1,1)*size[2]/2,type="n",xlab=xlab,ylab=ylab,...,main=main,asp=1)
  }

  types <- type ## to take into account of the order!!!
  for(type in types) {
    if(("vc" %in% type) && ncol(vor$delVertex)>3) {
      indCell<-.External("EBVor_polygon",vor$extPtr, PACKAGE = "EBSpat")
      update(vor,force=FALSE)
      args<-list() #vc.colors(sum(indCell==-5)))
      if(!missing(vcCol)) {
        if(inherits(try(is.character(vcCol),silent=T),"try-error")) vcCol<-deparse(substitute(vcCol))
        vcCol<-vor$delMarks[[vcCol]][-(1:3)]
        args$col<-vc.colors(length(unique(vcCol)))[vcCol]
      }
      if(!missing(vcArgs)) lapply(seq(vcArgs),function(i) args[[names(vcArgs)[i]]]<<-vcArgs[[i]])
  #print(args)
      do.call("polygon",c(list(t(vor$vorVertex[,match(indCell,vor$vorId)])),args))
    }
    if("dv" %in% type) {
      args<-list(col="blue",pch=16)
       if(!missing(dvCol)) {
        if(inherits(try(is.character(dvCol),silent=T),"try-error")) dvCol<-deparse(substitute(dvCol))
        dvCol<-vor$delMarks[[dvCol]][-(1:3)]
        args$col<-dv.colors(length(unique(dvCol)))[dvCol]
      }
      if(!missing(dvCex)) {
        if(inherits(try(is.numeric(dvCex),silent=T),"try-error")) {
          dvCex<-deparse(substitute(dvCex))
          dvCex<-vor$delMarks[[dvCex]][-(1:3)]
          dvCex<-seq(dv.cex[1],l=length(dvCex),by=dv.cex[2])[as.integer(factor(dvCex))]
        }
        args$cex<-dvCex
      }
      if(!missing(dvPch)) {
        if(inherits(try(is.numeric(dvPch) | is.character(dvPch),silent=T),"try-error")) {
          dvPch<-deparse(substitute(dvPch))
          dvPch<-vor$delMarks[[dvPch]][-(1:3)]
          dvPch<-dv.pch[as.integer(factor(dvPch))]
        }
        args$pch<-dvPch
      }
      if(!missing(dvArgs))   lapply(seq(dvArgs),function(i) args[[names(dvArgs)[i]]]<<-dvArgs[[i]])
      do.call("points",c(list(vor$delVertex[1,-(1:3)],vor$delVertex[2,-(1:3)]),args))
    }
    if("vv" %in% type) {
      args<-list(col="green",pch=16)
      if(!missing(vvCex)) args$cex<-vvCex
      if(!missing(vvPch)) args$pch<-vvPch
      if(!missing(vvArgs))   lapply(seq(vvArgs),function(i) args[[names(vvArgs)[i]]]<<-vvArgs[[i]])
      do.call("points",c(list(vor$vorVertex[1,],vor$vorVertex[2,]),args))
    }
    if("de" %in% type) {
      args<-list(col="blue")
      if(!missing(deArgs))  lapply(seq(deArgs),function(i) args[[names(deArgs)[i]]]<<-deArgs[[i]])
      delEdge<-matrix(vor$delVertex[,match(vor$delEdge,vor$delId)],nr=4)
      do.call("segments",c(list(delEdge[1,],delEdge[2,],delEdge[3,],delEdge[4,]),args))
    }
    if("ve" %in% type) {
      args<-list(col="green")
      if(!missing(veArgs))  lapply(seq(veArgs),function(i) args[[names(veArgs)[i]]]<<-veArgs[[i]])
      vorEdge<-matrix(vor$vorVertex[,match(vor$vorEdge,vor$vorId) ],nr=4)
      do.call("segments",c(list(vorEdge[1,],vorEdge[2,],vorEdge[3,],vorEdge[4,]),args))
    }
    if(length(nng<-grep("nng$",type,value=TRUE))>0) {#only the first (k-)nng is considered!
      tmp <- strsplit(nng[1],"-")[[1]]
      if(length(tmp)==1) order <- 1 else order <- as.integer(tmp[1])
      xy <- coords(nng <- nearestNeighbours(vor,order=order))
      ##print(xy)
      xyA <- c()
      for(i in seq(xy)) {
        xy0 <- vor$delVertex[,-(1:3)][,nng[[i]]$vertex]
        for(j in 1:NCOL(xy[[i]])) xyA <- cbind(xyA,c(xy0,xy[[i]][,j]))
      }
      args<-list(length=.08,col=1)
      if(!missing(nngCol)) args$col <- nngCol
      if(!missing(nngArgs))   lapply(seq(nngArgs),function(i) args[[names(nngArgs)[i]]]<<-nngArgs[[i]])
      ##print(xyA)
      do.call("arrows",c(list(xyA[1,],xyA[2,],xyA[3,],xyA[4,]),args))
    }
  }
  if(!is.null(domainIn) && !add) {
    if(is.logical(domainIn) && domainIn) domainIn <- list()
      do.call("plotSquareDomain",c(list(vor$centerIn,vor$sizeIn),domainIn))
  }
  if(!is.null(domain) && !add ) {
    do.call("plotSquareDomain",c(list(vor$center,vor$size),domain))
  }
}

labels.EBVor <- function(vor,pts,type=c("delRank","delId","vorRank","vorId"),...) {
  type <- match.arg(type)
  if(type %in% c("delRank","delId")) {
    if(missing(pts)) pts <- (1:(ncol(vor$delVertex)-3))
    text(vor$delVertex[1,pts+3],vor$delVertex[2,pts+3],switch(type,delRank=pts,delId=vor$delId[pts+3]),...) 
  } else {
    if(missing(pts)) pts <- (1:(ncol(vor$vorVertex)-3))
    text(vor$vorVertex[1,pts+3],vor$vorVertex[2,pts+3],switch(type,vorRank=pts,vorId=vor$vorId[pts+3]),...)
  }
  
}

circles.EBVor <- function(vor,pts,poly.col="red",add=FALSE,...) {
  require(plotrix)
  vg <- vorGraph(vor) 
  if(!add) plot(vor,c("dv","de"),deArgs=list(lwd=2),...,saveArgsPlot=FALSE,domain=NULL) #$
  if(!is.null(poly.col)) for(i in pts+3) polygon(vor$delVertex[1,vg[1:3,i]+4],vor$delVertex[2,vg[1:3,i]+4],col=poly.col)
  for(i in pts+3) {
    radius<-sqrt(sum((vor$vorVertex[,i+3]-vor$delVertex[,vg[1,i]+4])^2))
    draw.circle(vor$vorVertex[1,i+3],vor$vorVertex[2,i+3],radius,lwd=2) #$
  }
  #if(!add) plot(vor,c("dv","de"),deArgs=list(lwd=2),add=TRUE)
}


getInfosFormulaFunc <- function(type,infos,vor,...,argsType) { #vor needed for marks!
  nms <- NULL;values <- NULL
  if(missing(argsType)) argsType <- list(...)
  for(info in infos) {
    switch(paste(type,info,sep="."),
      Del1.id={nm <- info;value <- info},
      Del1.x={nm <- paste(info,1:2,sep="");value <- paste(info,"[",1:2,"]",sep="")},
      Del1.v={
        nm <- vor$del.marks.name
        if(!is.null(nm)) {
          value <- paste(info,"$",nm,sep="");nm <- paste(info,"_",nm,sep="")
        }
      },
      Del1.a={nm <- info;value <-info},
      Del2.id={nm <- paste(info,1:2,sep="");value <- paste(info,"[",1:2,"]",sep="")},
      Del2.x={nm <- paste(info,c(11,12,21,22),sep="");value <- paste(info,"[[",c(1,1,2,2),"]]","[",c(1,2,1,2),"]",sep="")},
      Del2.v=,All2.v=,NNG.v={
        nm <- vor$del.marks.name
        if(!is.null(nm)) {
          value <- paste(info,"[[",c(rep(1,length(nm)),rep(2,length(nm))),"]]","$",rep(nm,2),sep="");nm <- paste(info,c(rep(1,length(nm)),rep(2,length(nm))),"_",nm,sep="")
        }
      },
      Del2.a={nm <- paste(info,1:2,sep="");value <- paste(info,"[",1:2,"]",sep="")},
      Del2.l2={nm <- info;value <- info},
      Del2.l={nm <- info;value <- info},
      Del2.ol2={nm <- info;value <- info},
      Del2.ol={nm <- info;value <- info},
      Del2.da={nm <- info;value <- info},
      Del3.id={nm <- paste(info,1:3,sep="");value <- paste(info,"[",1:3,"]",sep="")},
      Del3.x={nm <- paste(info,c(11,12,21,22,31,32),sep="");value <- paste(info,"[[",c(1,1,2,2,3,3),"]]","[",c(1,2,1,2,1,2),"]",sep="")},
      Del3.v={
        nm <- vor$del.marks.name
        if(!is.null(nm)) {
          value <- paste(info,"[[",c(rep(1,length(nm)),rep(2,length(nm)),rep(3,length(nm))),"]]","$",rep(nm,3),sep="");nm <- paste(info,c(rep(1,length(nm)),rep(2,length(nm)),rep(3,length(nm))),"_",nm,sep="")
        }
      },
      Del3.a={nm <- paste(info,1:3,sep="");value <- paste(info,"[",1:3,"]",sep="")},
      Del3.ta={nm <- info;value <- info},
      Del3.tp={nm <- info;value <- info},
      Del3.c={nm <- paste(info,1:2,sep="");value <- paste(info,"[",1:2,"]",sep="")},
      Del3.r2={nm <- info;value <- info},
      Del3.r={nm <- info;value <- info},
      Del3.sa={nm <- info;value <- info},
      Del3.ga={nm <- info;value <- info},
      All2.id={nm <- paste(info,1:2,sep="");value <- paste(info,"[",1:2,"]",sep="")},
      All2.x={nm <- paste(info,c(11,12,21,22),sep="");value <- paste(info,"[[",c(1,1,2,2),"]]","[",c(1,2,1,2),"]",sep="")},
      #All2.v= see Del2.v
      All2.l2={nm <- info;value <- info},
      All2.l={nm <- info;value <- info},
      NNG.id={nm <- paste(info,1:2,sep="");value <- paste(info,"[",1:2,"]",sep="")},
      NNG.x={nm <- paste(info,c(11,12,21,22),sep="");value <- paste(info,"[[",c(1,1,2,2),"]]","[",c(1,2,1,2),"]",sep="")},
      #NNG.v= see Del2.v
      NNG.l2={nm <- info;value <- info},
      NNG.l={nm <- info;value <- info}
    )
    if(!is.null(nm)) {nms <- c(nms,nm);values <- c(values,value)}
  }
  argsType <- if(length(argsType)==0) "" else  paste(c("",paste(names(argsType),argsType,sep="=")),collapse=",")
  ### debugMode: print(argsType)
  cmd <- paste("~",type,"(",paste(paste(nms,values,sep="="),collapse=","),argsType,")",sep="")
  ### debugMode: print(cmd)
  eval(parse(text=cmd))
}


#OLD:  Del2.EBVor<-function(vor,fun,...) {
#  res<-matrix(vor$delVertex[,match(vor$delEdge,vor$delId)],nr=4)
#  if(missing(fun)) res
#  else apply(res,2,fun,...)
#}

Del1.EBVor <- Del2.EBVor <- Del3.EBVor <- All2.EBVor <- NNG.EBVor <- function(vor,pt,...) {#remove in tmp the usual arguments
  tmp <- as.list(match.call())
  pt2<- deparse(substitute(pt))
  type <- strsplit(as.character(tmp[[1]]),"\\.")[[1]][1] #just in case when the call is Del1.EBVor

  infos <- tmp[-(1:2)]
  ### debugMode: print(type)
  argsType <- infos[-1]
   
  argsType <- argsType[names(argsType) %in% EBFunction.args[[type]]]
  ### debugMode: print(argsType);print(infos)
  indArgsType<-which(names(infos) %in% names(argsType))
  ### debugMode: print(indArgsType);print(length(indArgsType))
  if(length(indArgsType)>0) infos <- infos[-indArgsType]
  ### debugMode: print(tmp);print(pt);print(type);print("ici2");print(infos)
  if(length(grep("^[0-9]+",pt2))>0) { #is it an integer?
    pt <- as.integer(pt2)
    infos <- as.character(infos[-1])
  } else if(pt2 %in% EBFunction.infos[[type]]) {
    pt <- NULL 
    infos <- as.character(c(pt2,infos))
  } else if( !missing(pt) && is.numeric(pt) && round(pt)==pt) {#pt exists and is an integer!
    pt <- as.integer(pt)
    infos <- as.character(infos[-1])
  }
  ### debugMode: cat("debug2->");print(type);print(infos);print(argsType);print(getInfosFormulaFunc(type,infos,vor,argsType=argsType))
  if(length(infos)==0) infos <- EBFunction.infos[[type]]
  res <- vor[[pt,getInfosFormulaFunc(type,infos,vor,argsType=argsType)]][[1]]$comp
  if(type=="All2" && !is.null(pt)) res <- res$new
  res
}

#TODO!!!
image.EBVor<-function(vor) {
  obj<-list(size=vor$size,center=vor$center,delVertex=vor$delVertex,delId=vor$delId,delEdge=vor$delEdge,vorVertex=vor$vorVertex,vorId=vor$vorId,vorEdge=vor$vorEdge)
  class(obj)<-"EBVorGraph"
  obj
}
 
makeIns.EBVor <- function(vor,pts,...) makeIns.EBPoly(vor$pl,pts,...)
applyMake.EBVor <- function(vor) applyMake.EBPoly(vor$pl) 
cancelMake.EBVor <-  function(vor) cancelMake.EBPoly(vor$pl) 
finalMake.EBVor <-  function(vor) finalMake.EBPoly(vor$pl)
makeSup.EBVor <- function(vor,pts) makeSup.EBPoly(vor$pl,pts)

##class EBVorGraph
#Only Generated by EBVor with method image just above!!!
plot.EBVorGraph<-plot.EBVor


identify.EBVor<-function(vor,func,mode=1,...) {
  newFunc <- FALSE
  if(inherits(func,"formula")) {newFunc <- TRUE;func<-EBFunc(func,mode="default")}
  plot(vor,...)
  repeat {
    pt<-identifyStep(vor,mode)
    if(is.null(pt)) break
    res<-func[[vor,pt]]
    cat("With the point:\n")
    cat("--------------\n")
    print(t(res$comp$new))
    cat("Without the point:\n")
    cat("-----------------\n")
    print(t(res$comp$old))
    cat("Cummulatives:\n")
    cat("------------\n")
    print(res$cumCom)
    cat("\n")
  }
  if(newFunc) free.externalPtr(func)
  invisible(res)
}

run.EBVor<-function(vor,mode=1,...,pdf=NULL) {
  vor$plot_runMode <- if(mode==1) "Ins" else "Del" 
  lastPlot <- vor$plot_current
  nbArgsPlot <- length(vor$plot_args)
  plot(vor,...)
  if(!is.null(pdf)) {
    plot(vor)
    cptPdf <- 0
    dev.copy2pdf(file=paste(pdf,"-",cptPdf,".pdf",sep=""))
  }
  if(length(vor$plot_args)>nbArgsPlot) vor$plot_current <- length(vor$plot_args)
  cptExit <- 0
  repeat {
    pt<-identifyStep(vor,mode)
    if(is.null(pt)) {
      mode <- -mode
      vor$plot_runMode <- if(mode==1) "Ins" else "Del"
      plot(vor)
      cptExit <- cptExit + 1
      if(cptExit>5) break
    } else {
      cptExit <- 0
      if(!is.null(pdf)) {
        plot(vor)
        if(mode>0) points(pt[1],pt[2],cex=1.5,pch="+") 
        else points(vor$delVertex[1,pt+3],vor$delVertex[2,pt+3],cex=1.5,pch="+")
        cptPdf <- cptPdf + 1
        dev.copy2pdf(file=paste(pdf,"-",cptPdf,".pdf",sep=""))
      }
      if(mode>0) { #insertion
        if(abs(pt[1]-vor$center[1])*2>vor$size[1] || abs(pt[2]-vor$center[2])*2>vor$size[2]) break
        if(!is.null(vor$del.marks.gen)) {
          marks <- eval(parse(text=vor$del.marks.gen))
          class(marks) <- "multi.data.frame"
          insert(vor,pt,marks=marks)
        } else insert(vor,pt) 
      } else { #deletion
        delete(vor,pt)
      }
      update(vor)
      plot(vor)
      if(!is.null(pdf)) {
        cptPdf <- cptPdf + 1
        dev.copy2pdf(file=paste(pdf,"-",cptPdf,".pdf",sep=""))
      }
    } 
  }
  vor$plot_runMode <- NULL
  plot(vor)
  if(!is.null(pdf)) {
    cptPdf <- cptPdf + 1
    dev.copy2pdf(file=paste(pdf,"-",cptPdf,".pdf",sep=""))
  }
  if(length(vor$plot_args)>nbArgsPlot) { 
    vor$plot_args[[length(vor$plot_args)]] <- NULL
    vor$plot_current <- lastPlot
  }
  invisible()
}


##class EBPoly
EBPoly<-function(vor) {
  poly <- CqlsObj(EBPoly)
  poly$extPtr <-.ExternalInEnvir("EBPoly_new", vor$extPtr, envir=poly, PACKAGE = "EBSpat")
  poly$vor <- vor
  poly
}

reactivate.EBPoly<-function(poly) reactivate.externalPtr(poly)


insert.EBPoly <- function(poly,pts,...,marks) {
  if(is.matrix(pts) && ncol(pts)==2 ) pts <- t(pts)
  if(missing(marks)) marks<-multi.data.frame(...) #list with same number of elements (vector) or rows (matrix)! 
  if(length(marks)) {
    marks<-marks[1:NROW(pts),]
  } else marks<-NULL
  .External("EBPoly_ins",poly$extPtr,pts,marks, PACKAGE = "EBSpat")
  update(poly)
}
##################################################
## Rmk: insert(poly,pts,...,marks) equivalent to
# makeIns(poly,pts,...,marks)
# applyMake(poly)
# finalMake(poly)
# update(poly)
##################################################

#The wrapper of the insertion commands!!!
makeIns.EBPoly <- function(poly,pts,...,marks) {
  if(missing(marks)) marks<-multi.data.frame(...) #list with same number of elements (vector) or rows (matrix)! 
  if(length(marks)) {
    marks<-marks[1:NROW(pts),]
  } else marks<-NULL
  .External("EBPoly_make_ins",poly$extPtr,pts,marks, PACKAGE = "EBSpat")
}
#The wrapper of the suppression commands!!!
makeSup.EBPoly <- function(poly,pts) {
  .External("EBPoly_make_sup",poly$extPtr,pts, PACKAGE = "EBSpat")
}
#The wrappers for apply, cancel or final of preliminary makeIns or makeSup!
applyMake.EBPoly <- function(poly) .External("EBPoly_apply",poly$extPtr, PACKAGE = "EBSpat")
cancelMake.EBPoly <- function(poly) .External("EBPoly_cancel",poly$extPtr, PACKAGE = "EBSpat")
finalMake.EBPoly <- function(poly) .External("EBPoly_final",poly$extPtr, PACKAGE = "EBSpat")


closestIndexesPoints <- function(poly,pts) {
  if(is.matrix(pts) && ncol(pts)==2 ) pts <- t(pts)
  pts <- matrix(pts,nrow=2) #if pts is a vector!
  if(ncol(poly$vor$delVertex)==3) return(c())
  if(ncol(pts) > ncol(poly$vor$delVertex)-3) return(1:(ncol(poly$vor$delVertex)-3))
  ## now the non trivial stuff!
  allPts <- poly$vor$delVertex[,-(1:3)]
  allInd <- 1:ncol(allPts) #save the original index!
  ind <- c()
  for(i in 1:ncol(pts)) {
    closestInd <- which.min(apply((allPts-pts[,i])^2,2,sum))
    ind <- c(ind,allInd[closestInd])
    allInd <- allInd[-closestInd]
    allPts <- allPts[,-closestInd]
  }
  ind
}

# pts can be:
#   a vector of index considered as distinct integers 
#   a vector of coordinates
#   a matrix of coordinates (nrow=2 or ncol=2)
delete.EBPoly <- function(poly,pts) {
  # convert any structure different from a vector of considered integer to a vector of index!
  if(!is.vector(pts) || !isTRUE(all.equal(pts,as.integer(pts))) || (length(pts)%%2==0 && isTRUE(all.equal(min(diff(sort(pts))),0))) ) pts <- closestIndexesPoints(poly,pts)
  if(length(pts)==0) return(invisible())
  pts <- rev(sort(as.integer(pts))) #Change: no longer pts-1 since it is done in ebpoly_delet_at -> C indexes in the reverse order in order to remove all the desired points!
  for(pt in pts) .External("EBPoly_delete_at",poly$extPtr,pt, PACKAGE = "EBSpat")
  update(poly)
}

currentPoint<-function(poly) {
  if(inherits(poly,"EBVor")) poly<-poly$pl
  .External("EBPoly_current_dv",poly$extPtr, PACKAGE = "EBSpat")
}

empty.EBPoly<-function(poly,inside=FALSE) {
 .External("EBPoly_empty",poly$extPtr,inside , PACKAGE = "EBSpat")
  update(poly)
}

update.EBPoly<-function(poly) {update(poly$vor)}

vorGraph.EBPoly<-function(poly) vorGraph(poly$vor)

print.EBPoly<-function(poly,...) cat("EBPoly\n")

image.EBPoly<-function(poly) image(poly$vor)


##class EBPoints : method static used in pseudo!!!
EBPoints.default<-function(pts,nbPts=1,marks=NULL,...) {#nbPts=nbre de points supplémentaires!
  # put pts as a matrix 2xN with N the number of points!
  obj<-EBVor(pts=NCOL(pts)+nbPts,marks=marks,...) #marks doit être rajouter lorsqu'il y a une marque
  insert(obj,pts,...) # ... sont les marques
  class(obj)<-c("EBPoints","GetAttr") #ne permet plus d'insertion puisque la classe a changée!!
  obj
}


## NO method insert!!!!
reactivate.EBPoints<-function(pts) {
  invisible(.External("EBVor_reactivate", pts$extPtr, PACKAGE = "EBSpat"))
  update.EBVor(pts)
  invisible()
}

plot.EBPoints<-function(pts,...) plot.EBVor(pts,...)


# spatstat conversion
as.ppp.EBVor<-function(vor,window) {
  xy<-vor$delVertex[,-(1:3)]
  if(missing(window)) window<-owin(vor$center[1]+c(-1,1)*vor$size[1]/2,vor$center[2]+c(-1,1)*vor$size[2]/2)
  ppp(xy[1,],xy[2,],window=window,marks=as.data.frame(vor$delMarks[-(1:3),]))  
}

####################################################################################
## Additional stuff to extract information on some set of Delaunay vertices
####################################################################################
## IMPORTANT: for any point, id corresponds to internal C index when rank corresponds to user R index
## NB: outside points are not considered!
rankFromId.EBVor <- function(vor,ptIds) match(ptIds,vor$delId)-3
idFromRank.EBVor <- function(vor,ptRanks) vor$delId[ptRanks+3]


# output is rank indices
delEdges.EBVor <- function(vor)  {
  res <- apply(vor$delEdge,1:2,function(id) rankFromId.EBVor(vor,id))
  class(res) <- "EBEdgeSet"
  res
}

# Pb: depend on delEdge and above all no way to deal with order greater than 2
delNeighbours.EBVorOLD <- function(vor,ptRanks) {
  if(missing(ptRanks)) ptRanks <- seq(vor)
  if(length(ptRanks)==1) c(rankFromId.EBVor(vor,vor$delEdge[2,vor$delEdge[1,]==idFromRank.EBVor(vor,ptRanks)]),rankFromId.EBVor(vor,vor$delEdge[1,vor$delEdge[2,]==idFromRank.EBVor(vor,ptRanks)]))
  else sapply(ptRanks,function(pt) delNeighbours(vor,pt))
}


## IMPORTANT: unfortunately, ... is required here because of the call Del2(vor,ptRanks,...) inside the body!!!!
delNeighbours.EBVor <- function(vor,ptRanks,...) {#order=??? or range=??? in ...
  if(missing(ptRanks)) ptRanks <- seq(vor)
  ptRanks <- as.integer(ptRanks)
  update(vor)
  if(length(ptRanks)==1) {
    res <- list()
    res$neighbours <- rankFromId.EBVor(vor,setdiff(unique(sort(as.matrix(Del2(vor,ptRanks,...)$new[,1:2]))),idFromRank.EBVor(vor,ptRanks)))
    res$vertex <- ptRanks
    res$vor <- vor # an environment
    res$graph <- "Delaunay" 
    class(res) <- "EBNeighbours"
  } else {
    res <- lapply(ptRanks,function(ptRank) delNeighbours(vor,ptRank,...))
    names(res) <- sapply(res,function(l) l$vertex)
    class(res) <- "EBNeighboursList"
  }
  res
}

print.EBNeighbours <- function(obj,...) cat("Neighbours (",obj$graph,") of ",obj$vertex,": ",paste(obj$neighbours,collapse=","),"\n",sep="")

print.EBNeighboursList <- function(obj,...) for(n in obj) print(n)

lengths.EBNeighbours <- function(obj) {
  # sapply(obj$neighbours,function(neighbour) 
  #   sqrt(sum((obj$vor$delVertex[,-(1:3)][,obj$vertex]-obj$vor$delVertex[,-(1:3)][,neighbour])^2))
  # )
  apply((coords(obj)-obj$vor$delVertex[,-(1:3)][,obj$vertex])^2,2,function(e) sqrt(sum(e)))
}

coords.EBNeighbours <- function(obj) {
  obj$vor$delVertex[,-(1:3)][,obj$neighbours,drop=FALSE] -> res
  colnames(res) <- obj$neighbours
  res
}

summary.EBNeighbours <- function(obj) {
  lengths <- lengths(obj)
  coords <- coords(obj)
  vertex <- delVertices(obj$vor,obj$vertex)
  for(i in 1:length(obj$neighbours)) 
  cat(obj$vertex," [",vertex[1,],",",vertex[2,],"] <--(",lengths[i],")--> ",obj$neighbours[i]," [",coords[1,i],",",coords[2,i],"]\n",sep="")
  return(invisible())
}

summary.EBDirectedNeighbours <- function(obj) {
  lengths <- lengths(obj)
  coords <- coords(obj)
  vertex <- delVertices(obj$vor,obj$vertex)
  for(i in 1:length(obj$neighbours)) 
  cat(obj$vertex," [",vertex[1,],",",vertex[2,],"] --(",lengths[i],")--> ",obj$neighbours[i]," [",coords[1,i],",",coords[2,i],"]\n",sep="")
  return(invisible())
}

plot.EBNeighbours <- function(obj,...) {
  xy <- coords(obj)
  xy0 <- delVertices(obj$vor,obj$vertex)
  xyA <- c()
  for(j in 1:NCOL(xy)) xyA <- cbind(xyA,c(xy0,xy[,j]))
  segments(xyA[1,],xyA[2,],xyA[3,],xyA[4,],...)
}

plot.EBDirectedNeighbours <- function(obj,length=.08,...) {
  xy <- coords(obj)
  xy0 <- delVertices(obj$vor,obj$vertex)
  xyA <- c()
  for(j in 1:NCOL(xy)) xyA <- cbind(xyA,c(xy0,xy[,j]))
  arrows(xyA[1,],xyA[2,],xyA[3,],xyA[4,],length=length,...)
}



lengths.EBNeighboursList <- function(obj) lapply(obj,lengths)
coords.EBNeighboursList <- function(obj) lapply(obj,coords)
summary.EBNeighboursList<- function(obj) {lapply(obj,summary);return(invisible())}
plot.EBNeighboursList <- function(obj,...) {lapply(obj,plot,...);return(invisible())}

# nearestNeighbours.EBVor <- function(vor,ptRanks,...) {
#   order <- list(...)$order
#   if(missing(ptRanks)) ptRanks <- seq(vor)
#   if(length(ptRanks)==1) {
#     if(is.null(order)) order <- 1 #default is 1 just in case ... is empty!
#     #print("ici");print(match.call())
#     res <- delNeighbours(vor,ptRanks,...)
#     res$neighbours <- res$neighbours[order(lengths(res))][1:order]
#     res$graph <- paste(order,"-NNG",sep="")  
#   } else {
#     res <- lapply(ptRanks,function(ptRank) nearestNeighbours(vor,ptRank,...))
#     names(res) <- sapply(res,function(l) l$vertex)
#     class(res) <- "EBNeighboursList"
#   }
#   res
# }

nearestNeighbours.EBVor <- function(vor,ptRanks,order=1) {
  if(missing(ptRanks)) ptRanks <- seq(vor)
  if(length(ptRanks)==1) {
    if(is.null(order)) order <- 1 #default is 1 just in case ... is empty!
    #res <- eval(parse(text=paste("delNeighbours(vor,ptRanks,order=",order,")",sep=""))) #lazzy evaluation
    res <- eval(substitute(delNeighbours(vor,ptRanks,order=.order),list(.order=order))) #lazzy evaluation
    res$neighbours <- res$neighbours[order(lengths(res))][1:order]
    res$graph <- paste(order,"-NNG",sep="") 
    class(res) <- c("EBDirectedNeighbours",class(res))
  } else {
    res <- lapply(ptRanks,function(ptRank) eval(parse(text=paste("nearestNeighbours(vor,ptRank,order=",order,")",sep="")))) 
    names(res) <- sapply(res,function(l) l$vertex)
    class(res) <- "EBNeighboursList"
  }
  res
}

nearestPoint.EBVor <- function(vor,ptCoords) {## maybe it is better to use nearestNeighbours instead!
  closestIndexesPoints(vor$pl,ptCoords)
}



# output is coordinates from point ranks
delVertices.EBVor <- function(vor,ptRanks) {
  res <- vor$delVertex[,-(1:3)]
  if(!missing(ptRanks)) res <-res[,ptRanks,drop=FALSE]
  res
}