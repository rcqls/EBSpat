##S3Methods
insert <- function(obj,...) UseMethod("insert")
delete <- function(obj,...) UseMethod("delete")
reactivate <- function(obj,...) UseMethod("reactivate")
##already declared inside spatstat
if(!exists("is.marked",envir=globalenv())) is.marked <- function(obj, ...) UseMethod("is.marked")
##(already exists in stat!!!) update<-function(obj,...) UseMethod("update")
run <- function(obj,...) UseMethod("run")
empty <- function(obj,...) UseMethod("empty")
Del1 <- function(obj,...) UseMethod("Del1")
Del2 <- function(obj,...) UseMethod("Del2")
Del3 <- function(obj,...) UseMethod("Del3")
All2 <- function(obj,...) UseMethod("All2")
NNG <- function(obj,...) UseMethod("NNG")
vorGraph <- function(obj,...) UseMethod("vorGraph")
circles <- function(obj,...) UseMethod("circles")
Vor1 <- function(obj,...) UseMethod("Del3")
Vor2 <- function(obj,...) UseMethod("Vor2")
delVertices <- function(obj,...) UseMethod("delVertices")
delEdges <- function(obj,...) UseMethod("delEdges")
delNeighbours <- function(obj,...) UseMethod("delNeighbours")
nearestNeighbours <- function(obj,...) UseMethod("nearestNeighbours")
nearestPoint <- function(obj,...) UseMethod("nearestPoint")
lengths <- function(obj,...) UseMethod("lengths")
coords <- function(obj,...) UseMethod("coords")

##the default methods create the class EBPoints
EBPoints<-function(obj,...) UseMethod("EBPoints")


energy <- function(obj,...) UseMethod("energy")
makeIns <- function(obj,...) UseMethod("makeIns")
makeSup <- function(obj,...) UseMethod("makeSup")
applyMake <- function(obj,...) UseMethod("applyMake")
cancelMake <- function(obj,...) UseMethod("cancelMake")
finalMake <- function(obj,...) UseMethod("finalMake")

## func
func<-function(obj,...) UseMethod("func")

param <- function(obj,...) UseMethod("param")
"param<-" <- function(obj,...) UseMethod("param<-")

contrast.optim <- function(obj,...) UseMethod("contrast.optim")
gradient.optim <- function(obj,...) UseMethod("gradient.optim")
has.gradient.optim <- function(obj,...) UseMethod("has.gradient.optim")
has.gradient.optim.default <- function(obj,...) TRUE

Sum <- function(obj,...) UseMethod("sum") #because sum is already a primitive

## tools
# are the parameters (of a list or vector) named?
is.named<-function(l) {nl<-names(l);if(length(l)>0 & length(nl)==0) nl<-rep("",length(l));sapply(nl,nchar)>0}

# adaptation of match.arg avoiding an error
matchChoices <- function (arg, choices, several.ok = FALSE) 
{
    if (missing(choices)) {
        formal.args <- formals(sys.function(sys.parent()))
        choices <- eval(formal.args[[deparse(substitute(arg))]])
    }
    if (is.null(arg)) 
        return(choices[1L])
    else if (!is.character(arg)) 
        stop("'arg' must be NULL or a character vector")
    if (!several.ok) {
        if (identical(arg, choices)) 
            return(arg[1L])
        if (length(arg) > 1L) 
            stop("'arg' must be of length 1")
    }
    else if (length(arg) == 0L) 
        stop("'arg' must be of length >= 1")
    i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
    if (!all(i == 0L)) {
    #    stop(gettextf("'arg' should be one of %s", paste(dQuote(choices), 
    #        collapse = ", ")), domain = NA)
    i <- i[i > 0L]
    if (!several.ok && length(i) > 1) 
        stop("there is more than one match in 'matchChoices'")
    choices[i]
    }
}

#is.in.rectangle<-function(pt,center=c(0,0),dim=c(1,1)) {
#  all(c(pt>=center-dim/2,pt<=center+dim/2))
#}

#used in any identify function in order to provide loop!
identifyStep<-function(vor,mode=1,...) {
  if(mode==1) {
    pt<-unlist(locator(1))
    if(!is.null(pt)) cat("Selected point at (",pt[1],",",pt[2],")\n",sep="")
  } else {
    pt<-identify(x=t(vor$delVertex),label=seq(vor$delId),n=1,plot=FALSE)-3
    if(length(pt)==0) pt<-NULL
    else cat("Selected point delVertex[",pt,"] at (",vor$delVertex[1,pt+3],",",vor$delVertex[2,pt+3],")\n",sep="")
  }
  pt
}

EBSpat.debug <- function(mode) {
  if(!missing(mode)) .funcEnv$debugMode <- mode
  return(.funcEnv$debugMode)
} 


methodName.externalPtr <- function(cqlsobj,methodname,classname) {
  if(missing(classname)) classname  <- class(cqlsobj)[[1]]
  paste(classname,"_",methodname,sep="")
}

free.externalPtr <- function(cqlsobj) {
  if(.funcEnv$debugMode) cat(class(cqlsobj)[[1]],"object at",output.address.externalptr(cqlsobj),"freed!\n")
  .External(methodName.externalPtr(cqlsobj,"free"),cqlsobj$extPtr,PACKAGE = "EBSpat")
}

reactivate.externalPtr<-function(cqlsobj,classname) {
  if(missing(classname)) classname  <- class(cqlsobj)[[1]]
  invisible(.External( methodName.externalPtr(cqlsobj,"reactivate",classname), cqlsobj$extPtr, PACKAGE = "EBSpat"))
}

output.address.externalptr <- function(externalptr) {
  tmp <- CqlsRObj:::externalPtrOutput.CqlsObj(externalptr)[[1]]
  sub("^<pointer: (.*)>","\\1",tmp,perl=TRUE)
}

## interesting stuff to replace expression inside call with dictionnary.
expression.replace <- function(expr,dict) { #dict is here a list
  if(length(expr)==1) {
    if(deparse(expr) %in%  names(dict)) return(dict[[deparse(expr)]]) else return(expr)
  } else {
    for(j in 1:length(as.list(expr))) expr[[j]] <- expression.replace(expr[[j]],dict=dict)
  }
  return(expr)
}

## like substitute but could be used with call saved in expression!
update.call <- function(call,dict) expression.replace(call,dict)

