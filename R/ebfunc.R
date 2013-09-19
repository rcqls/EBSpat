
## mode use for EBFunc parsing
EBFunc.mode<-function(mode) {
  if(!missing(mode)) .funcEnv$.mode<-mode
  return(.funcEnv$.mode)
}

####################################################################################################################
##class EBFunc
EBFunc<-function(formula,mode) {
  ##.funcEnv$.mode specified by used method by calling EBFunc.mode(...)!
  func <- CqlsObj(EBFunc)
  func$extPtr<-.ExternalInEnvir("EBFunc_new", envir=func, PACKAGE = "EBSpat")
  
  if(missing(mode)) mode<-"default"
  func$mode<-mode
  EBFunc.mode(mode)

  ##formule du func : "~ 1 + Del2(...)"
  callTerms<-parseFunc(formula)
  #print(callTerms)
  ##add the config as last parameter
  if(length(attr(callTerms,"response"))) {
    func$response<-callTerms[[1]] #register only the response as a R call which have $pl argument!!
    callTerms<-callTerms[-1]
  }
  ###debugMode: print(callTerms)
  func$fct<-list(list(term=list(vars=new.env(),varsList="Single"))) #first element is Single 
  func$fct[[1]]$term$vars$Single<-0 #default value fixed to 0
  func$terms <- character(0) #initialized first in order to be manipulated inside C code!
  insert(func,funcTerms<-lapply(callTerms,eval))
## prefer update attr(func,"terms") in C code related to insert since it is updated inside function!
##attr(func,"terms")<-unlist(as.list(sapply(funcTerms,terms))) #-> this does not work in insert!!!
  reg.finalizer(func,free.externalPtr,TRUE)
  func
}

#free.externalPtr.EBFunc <- function(func) .External("EBFunc_free",func$extPtr,PACKAGE = "EBSpat")

insert.EBFunc<-function(func,fcts) {
#print(fcts)
  func$newTerms<-c() #used to reactivate EBFunc in C!
  for(fct in fcts) {
    ###debugMode: cat("insert fct");print(fct)
    if(is.numeric(fct))  {
    # this is attached to the R object then there is no need to be reactivated in C code!
    assign("Single",fct,envir=func$fct[[1]]$term$vars)
    #}
    #if(inherits(fct,"EBFunc"))  {
    #  .External("EBFunc_append_func",func,fct,PACKAGE = "EBSpat")
    }   
    if(inherits(fct,"EBFunction")) {
      ###debugMode: cat("Fct ->");print(fct)
      ###debugMode: cat("Fct$term ->");print(fct$term)
      ###debugMode: cat("Fct$term$args->");print(fct$term$args)
      newTerm<-as.integer(length(fct$term$args)>0)
      ###debugMode: cat("NEW term ->");print(newTerm)
      func$newTerms<-c(func$newTerms,newTerm) #to reactivate EBFunc!
      .funcEnv$term <- fct$term ## VERY IMPORTANT: needed for order or range assignment!  
      .External("EBFunc_add_function",func,fct,newTerm,PACKAGE = "EBSpat")
    }
    ###debugMode: cat("fin insert\n")
  }
}

#ex: EBFunc(~Del2(G,a2=l))

compute.EBFunc<-function(func,init,code,type=c("global","local"),point) {
  type<-match.arg(type)
  switch(type,
  global={
     
  },
  local={
    if(!missing("point")) { 
    }
  }
  )
}


"param.EBFunc"<-function(func,...,callR,names=FALSE,args=NULL) {
  #TODO ->DONE : use elt$term$varsList instead of ls(elt$term$vars) which is more complete and allows us to apply some lazzy stuff in the declaration of EBFunc.
  varNames<-lapply(func$fct,function(elt) elt$term$varsList) 
  if(names) return(unlist(varNames))
  if(is.null(args)) {
    if(missing("callR")) callR<-match.call()
    args<-as.list(callR)[-c(1:2)]
    args<-args[!(names(args) %in% c("names","args"))]
  }
  if(length(args)==0) {
    argsAssign<-list()
    keysGet<-unlist(varNames)
  } else {
    argsAssign<-args[is.named(args)]
    keysGet<-unique(unlist(c(sapply(args[!is.named(args)],deparse),names(argsAssign))))
  }
  ## assign  
  if(length(argsAssign)>0) {
    keysAssign<-matchChoices(pkeysAssign<-names(argsAssign),unlist(varNames),TRUE)
    for(i in seq(keysAssign)) {
      key<-keysAssign[[i]];pkey<-pkeysAssign[[i]]
      indVar<-which(sapply(varNames,function(e) key %in% e))
      if(length(indVar)>0) assign(key,argsAssign[[pkey]],envir=func$fct[[indVar[1]]]$term$vars)
    }
  }
  ## get
  res<-list()
  if(length(keysGet)>0) {
    keysGet<-matchChoices(keysGet,unlist(varNames),TRUE)
    for(key in sort(keysGet)) {
      indVar<-which(sapply(varNames,function(e) key %in% e))
      if(length(indVar)>0) res[[key]]<-if(key %in% ls(envir=func$fct[[indVar[1]]]$term$vars)) get(key,envir=func$fct[[indVar[1]]]$term$vars) else "Need to be initialized!"
    }
  }
  ## cat("res->");print(res)
  res
  
}

"param<-.EBFunc" <- function(func,value) {


}


terms.EBFunc<-function(func,...) formula(func,character=TRUE)

## convert one or several EBFunc objects to a formula
formula.EBFunc <- function(func,...,Single=TRUE,character=FALSE,vector=FALSE)  {
  ## select and gather the terms
  terms <- list()
  for(i in seq(EBFunction.type)) terms[[i]] <- character(0)
  termTypes <- lapply(seq(EBFunction.type)-1,function(e) list(e))
  for(term in func$terms) {
   for(i in rev(seq(EBFunction.type))) {
    if(length(grep(paste("EBFunction\\(",i-1,",",sep=""),term))) {
      terms[[i]] <- c( terms[[i]],term) 
      break
    } 
   }
  }
  ## split terms if args!
  for(i in seq(EBFunction.type)) {
    if(length(terms[[i]])>2 && length(EBFunction.args[[i]])>1) {
      ind <- regexpr(paste(EBFunction.args[[i]][2],"=[0-9]*",sep=""),terms[[i]],perl=T)
##print(paste(EBFunction.args[[i]][2],"=[0-9]*",sep=""))
      term.types <- substr(terms[[i]],ind,ind+attr(ind,"match.length")-1)
      ## sort terms[[i]] and term.types
      terms[[i]] <- terms[[i]][order(term.types)]
      term.types <- sort(term.types)
##print(terms[[i]]);print(term.types) 
     ## only if several types
      if(length(unique(term.types))>1) {
        type <- "NULL"
        for(term in rev(seq(terms[[i]]))) {
##cat(term.types[term],"=?",type,"\n",sep="")
          if(nchar(term.types[term])==0) break
          if(term.types[term]==type) {
            terms[[length(terms)]] <- c(terms[[i]][term],terms[[length(terms)]])
            terms[[i]] <- terms[[i]][-term]
          } else {
            #new type
            type <- term.types[term]
            termTypes[[length(termTypes)+1]] <- list(i-1,term.types[term])
            ## append to terms
            terms[[length(terms)+1]] <- terms[[i]][term]
            terms[[i]] <- terms[[i]][-term]
          }
        }
      }
    }
  }
  
  ## clean terms
  for(i in rev(seq(EBFunction.type))) if(length(terms[[i]])==0) {
    terms[[i]] <- NULL
    termTypes[[i]] <- NULL
  }
  ## simplify by terms
  for(i in 1:length(terms)) {  
    term <- terms[[i]]
    ## first term
    terms[[i]] <- sub(paste("EBFunction\\(",termTypes[[i]][[1]],",",sep=""),paste(EBFunction.type[termTypes[[i]][[1]]+1],"(",sep=""),term[1])
    terms[[i]] <- substr(terms[[i]],1,nchar(terms[[i]])-1)
    if(length(term)>1) for(term2 in term[-1]) {
      if(length(termTypes[[i]])>1) tmp <- paste(",",termTypes[[i]][[2]],sep="") else tmp <- ""
      tmp <- sub(paste("EBFunction\\(",termTypes[[i]][[1]],tmp,",",sep=""),",",term2)      
      terms[[i]] <- paste(terms[[i]],tmp,sep="")
      terms[[i]] <- substr(terms[[i]],1,nchar(terms[[i]])-1) #remove the last parenthesis
    }
    ## add final parenthesis
    terms[[i]] <- paste(terms[[i]],")",sep="")
  }

  terms <- unlist(terms)
  if(vector) return(terms)

  ## make the formula
  form <- paste(terms,collapse=" + ")
  if(Single) form <- paste( param(func)$Single,"+",form)
  for(i in seq(EBFunction.type)) form <- gsub(paste("EBFunction\\(",i-1,",",sep=""),paste(EBFunction.type[i],"(",sep=""),form)
  ebfuncs <- list(...)
  if(length(ebfuncs)>0) {
    for(ebfunc in ebfuncs) 
      if(inherits(ebfunc,"EBFunc")) 
        form <- paste(form,"+",formula(ebfunc,Single=FALSE,character=TRUE))
    print(form)
    form <- formula(EBFunc(as.formula(paste("~",form))),Single=TRUE,character=TRUE)
  }
  if(character) return(form)
  as.formula(paste("~",form))
}



summary.EBFunc<-function(func,...) {
  res<-c(param(func)$Single,lapply(func$fct,function(e) e$term))
  names(res)<-c("Single",func$terms)
  res
}

print.EBFunc<-function(func,...) {
  EBFunc.mode(func$mode)
  .External("EBFunc_show_functionList",func$extPtr,PACKAGE = "EBSpat")
  cat("EBFunc\n")
}

reactivate.EBFunc<-function(func) {
  if(!is.null(func$mode)) EBFunc.mode(func$mode)
  reactivate.externalPtr(func)
}

# several tools!
parseFunc<-function(e) {
## print(e);print(class(e));print(as.list(e))
  if(length(e)>1) {
    if(e[[1]]==as.name("+")) return(unlist(c(parseFunc(e[[2]]),parseFunc(e[[3]]))))
    if(e[[1]]==as.name("~")) {
      if(length(e)==3) {
        res <- unlist(c(e[[2]],parseFunc(e[[3]]))) 
        attr(res,"response") <- TRUE
      } else res <- unlist(c(parseFunc(e[[2]])))  
      ##res<-unlist(c(e[-1],parseFunc)) #correction 08/02/09 (version 2.8) lapply instead of sapply because BUG for parseFunc(~Del2(...)) 
      return(res) 
    }
    if(e[[1]]==as.name("(")) return(parseFunc(e[[2]]))

    if(inherits(e[[1]],"name") && (type<-as.character(e[[1]])) %in% names(EBFunction.alias)) {
      e[[1]]<-as.name(paste("EBFunction",sep=""))
      ##cat("e=");print(e);print(as.list(e));print(as.call(as.list(e)));print(length(e));print(class(e))
      ee<-as.list(e)
      e<-as.call(c(ee[1],EBFunction.id[[ EBFunction.alias[[type]] ]],ee[-1]))
      ##cat("e2=");print(e)
    }
#    if(inherits(e[[1]],"name") && as.character(e[[1]]) %in% .Del2) {
#      e[[1]]<-as.name(paste("EBDel2",sep=""))
#      ##e<-as.call(unlist(c(list(as.name("EBFuncDelEdge"),e[-1]))))
#    }
    return(e)
  } else {
    if(is.name(e)) {
      e2 <- eval(e)
      if(inherits(e2,"EBGibbs")) return(parseFunc(formula(e2$sim$func)))
      else if(inherits(e2,"EBFunc")) return(parseFunc(formula(e2)))
      else return(e)
    }
    if(is.numeric(e)) return(e)
    if(inherits(e,"formula")) return(parseFunc(e))
    if(e==as.name("+")) return(c()) else return(e)
  }
}


CompFuncFormulaManager <- function() {
  formMngr <- new.env()
  formMngr$formulas <- list()
  formMngr$origFormulas <- list()
  formMngr$compFuncList <- list()
  formMngr$compFuncCpt <- 0
  ## init carac environment for autoCaracFormula use!
  formMngr$caracEnv <- list()
  class(formMngr) <- "CompFuncFormulaManager"
  formMngr
}

## convert a form to a form with its related EBFunc object
## useful for EBResid objects!

formula.CompFuncFormulaManager <- function(formMngr,form=NULL,vor=NULL,local=NULL) {
  ## convert code in substitute(code) (.e. a call) except if code is already a call!
  form.is.call <- try(is.call(form),TRUE)
  if(inherits(form.is.call,"try-error") || !form.is.call) form <- substitute(form)

  if(is.null(form)) {
    ## return the formula

    return(list(formulas=formMngr$formulas,func=formMngr$func))

  } else { 

    compFuncTypes <- c("A2","All2","ALL2","a2","all2",
      "D1","Del1","DEL1","d1","del1",
      "D2","Del2","DEL2","d2","del2",
      "D3","Del3","DEL3","d3","del3",
      "NN","Nn","nn")

    formMngr$origFormulas[[length(formMngr$origFormulas)+1]] <- form


    get.caracEnv <- function(type) { #type="compFunc"
      if(is.null(formMngr$caracEnv[[type]])) { 
        formMngr$caracEnv[[type]] <<- new.env()
        formMngr$caracEnv[[type]]$List <<- list()
        formMngr$caracEnv[[type]]$Cpt <<- 0
      } 
      return(formMngr$caracEnv[[type]])
    }

    convertCompFunc<-function(e) {
      if(length(e)>1) {
        if(as.character(e[[1]])[1] %in% compFuncTypes ) {
          key <- paste(substring(e[[1]],1,1),tolower(substring(e[[1]],nchar(e[[1]]))),sep="")
          e[[1]] <- as.name(key)
          formMngr$compFuncCpt <- formMngr$compFuncCpt + 1
          attr(e,"name") <- paste(".f",formMngr$compFuncCpt,sep="")
          tmp <- convertEBFunc(e)
          if(is.null(formMngr$compFuncList[[tmp$key]])) formMngr$compFuncList[[tmp$key]] <<- tmp$call 
          else formMngr$compFuncList[[tmp$key]] <<- as.call(unlist(c(as.list(formMngr$compFuncList[[tmp$key]]),tmp$call)))
          ##cat(tmp$key,"->");print(compFuncList[[tmp$key]])
          return(as.name(attr(e,"name")))
        }
        return(as.call(lapply(e,convertCompFunc)))
      } else return(e) 
    }

    convertEBFunc <- function(e) {
      ## print("convertEBFunc");print(e)
      funcName <- attr(e,"name")
      key <- as.character(e[[1]])
      if(is.null(local)) local <- tolower(substr(key,1,1)->tmp)==tmp
      key <- switch(toupper(key),A2="All2",D1="Del1",D2="Del2",D3="Del3",NN="NNG")
      opt <- switch(key,All2="range",Del2=,Del3=,NNG="order",NULL)
      
      funcName2 <- paste(funcName,if(local) "l" else "g" ,sep=".")

      ## if we need to automatically determine the length! 
      EBFunction.infosTest(key,vor) ## vor only required for marks length!

      e2 <- list(as.name(key))
      optVal <- ""
      if(length(e[[2]])>1 && e[[2]][[1]]=="|" && length(e[[2]])==3) {
        if(is.null(opt)) stop("In compFunc formula, type ",key," has no optional argument!")
        optVal <- e2[[2]] <- e[[2]][[2]] #value of opt
        names(e2)[2] <- opt #name of opt
        e[[2]] <- e[[2]][[3]]      
      }
      key2 <- if(nchar(optVal)) paste(key,".",optVal,sep="") else key 
      ## expression
      compFuncForm <- autoCaracFormula(e[[2]],key,local,get.caracEnv(key2),TRUE)
      ## print(compFuncForm)
      e2[[(length(e2)+1)->ii]] <- compFuncForm$form
      if(length(e)==3) {
        funcName2 <- paste(funcName2,e[[3]],sep=".")
        e <- e[-3]
      } else if(!inherits(tmp <- try(EBFunction.length(e[[2]]),TRUE),"try-error") && tmp>1) {
        funcName2 <- paste(funcName2,tmp,sep=".")
      }
      names(e2)[ii] <- funcName2
      ##e2[[(length(e2)+1)->ii]] <- as.name(funcName)
      ##names(e2)[ii] <- funcName2

      ## finalization
      res <- as.call(e2)
      if(!is.null(formMngr$compFuncList[[key2]])) {
        res <- as.list(res)[-1]
        if(nchar(optVal)) res <- as.list(res)[-1]
      }

      return(list(call=res,key=key2 ))
    }
    
    formMngr$formulas[[length(formMngr$formulas)+1]] <-  convertCompFunc(form)

    ## Final ebfunc formula!

    ## Merge the caracs just after the compFuncs 

    makeEBFuncFormula <- function(rest,term=NULL) { ## term=NULL correspond to initial rest
      if(length(rest)==0) return(if(length(term)==0) NULL else as.call(c(as.name("~"),term)))
      if(is.null(term)) {
        if(length(rest)>1) makeEBFuncFormula(rest[-(1:2)],as.call(c(as.name("+"),rest[[1]],rest[[2]])))
        else return(as.call(c(as.name("~"),rest[[1]])))
      }
      else makeEBFuncFormula(rest[-1],as.call(c(as.name("+"),term,rest[[1]])))
    }

    ## Merge the caracs just after the compFuncs 
    compFuncList <- list()
    for(type in names(formMngr$compFuncList)) {
      compFuncList[[type]] <- as.call(unlist(c(as.list(formMngr$compFuncList[[type]]),formMngr$caracEnv[[type]]$List)))
    }

    formMngr$func <- makeEBFuncFormula(compFuncList)

    return(invisible())
  }
}


## OBSOLETE! REPLACED BY CompFuncFormulaManager
# autoCompFuncFormula <- function(form,formMngr=NULL,vor=NULL) {
#   ## convert code in substitute(code) (.e. a call) except if code is already a call!
#   form.is.call <- try(is.call(form),TRUE)
#   if(inherits(form.is.call,"try-error") || !form.is.call) form <- substitute(form)

#   compFuncTypes <- c("A2","All2","ALL2","a2","all2",
#     "D1","Del1","DEL1","d1","del1",
#     "D2","Del2","DEL2","d2","del2",
#     "D3","Del3","DEL3","d3","del3",
#     "NN","Nn","nn")


#   if(is.null(formMngr)) {
#     formMngr <- new.env()
#     formMngr$compFuncList <- list()
#     formMngr$compFuncCpt <- 0
#     ## init carac environment for autoCaracFormula use!
#     formMngr$caracEnv <- list() 
#   }


#   get.caracEnv <- function(type) { #type="compFunc"
#     if(is.null(formMngr$caracEnv[[type]])) { 
#       formMngr$caracEnv[[type]] <<- new.env()
#       formMngr$caracEnv[[type]]$List <<- list()
#       formMngr$caracEnv[[type]]$Cpt <<- 0
#     } 
#     return(formMngr$caracEnv[[type]])
#   }

#   convertCompFunc<-function(e) {
#     if(length(e)>1) {
#       if(as.character(e[[1]])[1] %in% compFuncTypes ) {
#         key <- paste(substring(e[[1]],1,1),tolower(substring(e[[1]],nchar(e[[1]]))),sep="")
#         e[[1]] <- as.name(key)
#         formMngr$compFuncCpt <- formMngr$compFuncCpt + 1
#         attr(e,"name") <- paste(".f",formMngr$compFuncCpt,sep="")
#         tmp <- convertEBFunc(e)
#         if(is.null(formMngr$compFuncList[[tmp$key]])) formMngr$compFuncList[[tmp$key]] <<- tmp$call 
#         else formMngr$compFuncList[[tmp$key]] <<- as.call(unlist(c(as.list(formMngr$compFuncList[[tmp$key]]),tmp$call)))
#         ##cat(tmp$key,"->");print(compFuncList[[tmp$key]])
#         return(as.name(attr(e,"name")))
#       }
#       return(as.call(lapply(e,convertCompFunc)))
#     } else return(e) 
#   }

#   convertEBFunc <- function(e) {
#     funcName <- attr(e,"name")
#     key <- as.character(e[[1]])
#     local <- tolower(substr(key,1,1)->tmp)==tmp
#     key <- switch(toupper(key),A2="All2",D1="Del1",D2="Del2",D3="Del3",NN="NNG")
#     opt <- switch(key,All2="range",Del2=,Del3=,NNG="order",NULL)
    
#     funcName2 <- paste(funcName,if(local) "l" else "g" ,sep=".")

#     ## if we need to automatically determine the length! 
#     EBFunction.infosTest(key,vor) ## vor only required for marks length!

#     e2 <- list(as.name(key))
#     optVal <- ""
#     if(length(e[[2]])>1 && e[[2]][[1]]=="|" && length(e[[2]])==3) {
#       if(is.null(opt)) stop("In compFunc formula, type ",key," has no optional argument!")
#       optVal <- e2[[2]] <- e[[2]][[2]] #value of opt
#       names(e2)[2] <- opt #name of opt
#       e[[2]] <- e[[2]][[3]]      
#     }
#     key2 <- if(nchar(optVal)) paste(key,".",optVal,sep="") else key 
#     ## expression
#     compFuncForm <- autoCaracFormula(e[[2]],key,local,get.caracEnv(key2),TRUE)
#     e2[[(length(e2)+1)->ii]] <- compFuncForm$form

#     if(length(e)==3) {
#       funcName2 <- paste(funcName2,e[[3]],sep=".")
#       e <- e[-3]
#     } else if((tmp <- EBFunction.length(e[[2]]))>1) {
#       funcName2 <- paste(funcName2,tmp,sep=".")
#     }
#     names(e2)[ii] <- funcName2
#     ##e2[[(length(e2)+1)->ii]] <- as.name(funcName)
#     ##names(e2)[ii] <- funcName2

#     ## finalization
#     res <- as.call(e2)
#     if(!is.null(formMngr$compFuncList[[key2]])) {
#       res <- as.list(res)[-1]
#       if(nchar(optVal)) res <- as.list(res)[-1]
#     }
#     return(list(call=res,key=key2 ))
#   }

#   makeEBFuncFormula <- function(rest,term=NULL) { ## term=NULL correspond to initial rest
#     if(length(rest)==0) return(if(length(term)==0) NULL else as.call(c(as.name("~"),term)))
#     if(is.null(term)) {
#       if(length(rest)>1) makeEBFuncFormula(rest[-(1:2)],as.call(c(as.name("+"),rest[[1]],rest[[2]])))
#       else return(as.call(c(as.name("~"),rest[[1]])))
#     }
#     else makeEBFuncFormula(rest[-1],as.call(c(as.name("+"),term,rest[[1]])))
#   }
  
#   form <-  convertCompFunc(form)
  
#   ## Merge the caracs just after the compFuncs 
#   for(type in names(formMngr$compFuncList)) {
#     formMngr$compFuncList[[type]] <- as.call(unlist(c(as.list(formMngr$compFuncList[[type]]),formMngr$caracEnv[[type]]$List)))
#   }

#   return(list(form=form,func=makeEBFuncFormula(formMngr$compFuncList),formMngr=formMngr))

# }


## DO NOT REMOVE! THIS ONE IS USED IN CompFuncFormulaManager
autoCaracFormula <- function(form,type="Del2",local=NULL,carac=new.env(),autoLength=FALSE) {

  ##parameter is a call of length 1, a name and with uppercase first letter 
  ##constant is the same but with lowercase first letter
  ##carac does not contain parameter in its expression
  ##compFunc is anything else

  if(is.numeric(type)) type <- EBFunction.type[[type]]  

  contains.parameter <- function(e) {
    if(length(e)>1) {
      return(any(unlist(sapply(seq(e)[-1],function(i) contains.parameter(e[[i]])))))
    } else if(substr(e,1,1) %in% LETTERS)  return(TRUE) else return(FALSE)
  }

  contains.info <- function(e) {
    if(length(e)>1) {
      return(any(unlist(sapply(seq(e)[-1],function(i) contains.info(e[[i]])))))
    } else if(as.character(e) %in% EBFunction.infos[[type]])  return(TRUE) else return(FALSE)
  }

  simplified.expr <- function(e) {
    if(length(e)>1) {
      if(e[[1]]==">") {e[[1]] <- as.name('<'); tmp <- e[[2]]; e[[2]]<- e[[3]]; e[[3]] <- tmp}
      if(e[[1]]==">=") {e[[1]] <- as.name('<='); tmp <- e[[2]]; e[[2]]<- e[[3]]; e[[3]] <- tmp}
      ee <- list(e[[1]])
      for(i in seq(e)[-1]) ee <- c(ee,simplified.expr(e[[i]]))
      return(as.call(ee))
    } else return(e)
  }

  #print(simplified.expr(~exp(l<40)^th[1]+th[2]*(l2>2000)+th3*(2000 < l2)))

  ## starting by finding parameters names
  
  if(!exists("List",envir=carac)) carac$List <- list()
  if(!exists("Cpt",envir=carac)) carac$Cpt <- 0

  parseExpr <- function(e) {
    if(contains.parameter(e)) {
      if(length(e)>1) return(as.call(unlist(c(e[[1]],lapply(seq(e)[-1],function(i) parseExpr(e[[i]]))))))
      else return(e)
    } else if(contains.info(e)) {
              ow <- options(warn=-1) #disable warning because length of carac$List and e are not multiple but e is not considered of length 1.
              if(length(tmp <- carac$List[carac$List == e]) > 0L) {
                tmp <- strsplit(names(tmp)[1],"\\.")[[1]] #strsplit to extract only the name of the carac!
                caracName <- if(tmp[1]=="") paste(tmp[1:2],collapse=".") else tmp[1]  
                return(as.name(caracName)) 
              } else {
                carac$Cpt <- carac$Cpt + 1
                caracName <- caracName2 <- paste(".c",carac$Cpt,sep="")
                if(!is.null(local)) caracName2 <- paste(caracName2,ifelse(local,"l","g"),sep=".")
                if(autoLength && ((tmp <- EBFunction.length(e))>1)) caracName2 <- paste(caracName2,tmp,sep=".")
                carac$List[[caracName2]] <- if(e[[1]]=="(") e[[-1]] else e #no need of parenthesis!
                return(as.name(caracName))
              }
              options(warn=ow) #recover initial warning system
    } else return(e) 
    
  }

  return(list(form=parseExpr(simplified.expr(form)),caracList=carac$List))

}


#"$<-.EBFunc"<-function(func,key,value) {
#  if(key=="vor") {
#    if(inherits(value,"EBVor")) .External("EBFunc_setVor",func,value, PACKAGE = "EBSpat")
#    else if(inherits(value,"EBPoly")) .External("EBFunc_setVor",func,value$vg, PACKAGE = "EBSpat")
#     else if(inherits(value,"EBSim")) .External("EBFunc_setVor",func,value$pl$vg, PACKAGE = "EBSpat")
#   }
#   func
# }

infosFormulaFuncFromFunc<-function(func) {
  cmd<-c()
  ###debugMode: cat("func$fct->");print(func$fct)
  for(fct in func$fct[-1]) {
    cmdTmp<-c(EBFunction.type[fct$term$type+1],"(")
    cmdTmp<-c(cmdTmp,paste(sapply(fct$term$infos,function(e) paste(e,e,sep="=")),collapse=","))
    args<-paste(unlist(sapply(seq(fct$term$args),function(i) paste(names(fct$term$args)[i],fct$term$args[i],sep="="))),collapse=",")
    if(nchar(args)!=0) cmdTmp<-c(cmdTmp,",",args)
    cmdTmp<-c(cmdTmp,")")
    cmd<-c(cmd,paste(cmdTmp,collapse=""))  
  }
  cmd<-paste("~",paste(cmd,collapse="+"),sep="")
  eval(parse(text=cmd)) #a formula
}

## point =  missing -> global
##          length(1) -> local with point at index as.integer(point)
##          length(2) -> local with point with coordinate point         
"[[.EBFunc"<-function(func,pl,point,types) {
  typesReg<-.External("EBFunc_idTerms",func$extPtr,package="EBSpat")
  if(is.numeric(pl) && inherits(point,c("EBPoly","EBVor"))) {tmp<-pl;pl<-point;point<-tmp} 
  if(inherits(pl,"EBVor")) pl<-pl$pl
  if(missing(types)) types<-typesReg
  if(missing(point) || is.null(point)) local<- -1 #global
  else {
    if(is.numeric(point)) local<-1
    else local<- -1 #global
  }

  if(local>0) {#local
    if(is.numeric(point) && length(point)==1) {
      local<-1
      point<-as.integer(point) #converted in C array!
      if(point<=0 || point>ncol(pl$vor$delVertex)-3) {
        cat("Attention: point=",point," is not a suitable index (lower than ",ncol(pl$vor$delVertex)-3,")!\n",sep="")
        return(invisible())
      }
    } else if(is.numeric(point) && length(point)==2) local<-2
    else {
            cat("Attention: point=",point," is not a suitable input !\n",sep="")
            return(invisible()) 
    } 
  } #else {#global
    #local<-0
    #v<-unlist(strsplit(deparse(substitute(type)),"\\.",perl=TRUE))
    #  choice<-which(c("g","glob","global","l","loc","local") %in% v)
    #  if(length(choice)>0) {
    #    choice<-c("g","glob","global","l","loc","local")[choice[1]] 
    #    local<-(choice %in% c("l","loc","local"))
    #  }
    #  if(any(mask<-(v %in% as.character(unlist(EBFunction.id))))) {
    #    type<-as.integer(v[mask])[1]
    #  }
    #} 
  ###debugMode: cat("local->");print(local)
  res<-list()
  if(local<0) {#cat("Global mode!\n")
    for(type in types) {
      tmp <- .External("EBFunc_componentGet",func$extPtr,pl$extPtr,as.integer(type),as.integer(local),package="EBSpat")
      tmp <- list(type=EBFunction.type[type+1],comp=tmp$comp$new,compFunc=tmp$compFunc$new)
      res <- c(res,list(tmp))
    }
    #names(res) <- 
  } else {#cat("Local mode!\n")
    if(local==1) makeSup(pl,point)
    if(local==2) makeIns(pl,point)  
    #applyMake(pl) 
    for(type in types) {
      tmp<-.External("EBFunc_componentGet",func$extPtr,pl$extPtr,as.integer(type),as.integer(local),package="EBSpat")
      res <- c(res,list(list(type=EBFunction.type[type+1],comp=tmp$comp,compFunc=tmp$compFunc)))
    }
    cancelMake(pl)
    finalMake(pl)
  }
  res
}

terms.numeric<-function(obj,...) obj


############################################### Function
## Declaration of Function here
## Id in C code
EBFunction.id<-list(
  ##clique type (i<i+1 allows us to easily insert new id!)
  Del1=(i<-0),Del2=(i<-i+1),Del3=(i<-i+1),All2=(i<-i+1),NNG=(i<-i+1)
)
rm(i)
## in R,
EBFunction.type<-names(EBFunction.id)
## alias in R
EBFunction.alias<-list(
  EBDel1="Del1",Del1="Del1",D1="Del1",
  EBDel2="Del2",Del2="Del2",D2="Del2",
  EBDel3="Del3",Del3="Del3",D3="Del3",
  EBAll2="All2",All2="All2",A2="All2",
  EBNNG="NNG",NNG="NNG"
)

EBFunction.termTypeAlias<-list(
G="G",Glob="G",Global="G"
)

EBFunction.termType<-names(EBFunction.termTypeAlias)

EBFunction.termTypeId<-unique(unlist(EBFunction.termTypeAlias))

EBFunction.args<-list(
  Del1=c("new"),Del2=c("new","order"),Del3=c("new"),All2=c("new","range"),NNG=c("new","order")
)

EBFunction.infos<-list(
Del1=c("id","x","v","a"),
Del2=c("id","x","v","a","l2","l","ol2","ol","da"),
Del3=c("id","x","v","a","ta","tp","c","r2","r","sa","ga"),
All2=c("id","x","v","l2","l"),
NNG=c("id","x","v","l2","l")
)

## to determine the size of carac and compFunc!
EBFunction.infosTest<- function(type,vor=NULL) {
  ok <- (!is.null(vor) && inherits(vor,"EBVor") && is.marked(vor))
  switch(type,
    Del1={.funcEnv$id<-1L;.funcEnv$x<-c(0,0);if(ok) .funcEnv$v <- eval(parse(text=vor$del.marks.gen));.funcEnv$a<-0},
    Del2={.funcEnv$id <- c(1L,2L);.funcEnv$x<-list(c(0,0),c(1,1));if(ok) .funcEnv$v<-lapply(1:2,function(i) eval(parse(text=vor$del.marks.gen)));.funcEnv$a<-c(0,0);.funcEnv$l2<-0;.funcEnv$l<-0;.funcEnv$ol2<-0;.funcEnv$ol<-0;.funcEnv$da<-0},
    Del3={.funcEnv$id<-c(1L,2L,3L);.funcEnv$x<-list(c(0,0),c(0,1),c(1,0));if(ok) .funcEnv$v<-lapply(1:3,function(i) eval(parse(text=vor$del.marks.gen)));.funcEnv$a<-c(0,0,0);.funcEnv$ta<-0;.funcEnv$tp<-0;.funcEnv$c<-c(0,0);.funcEnv$r2<-0;.funcEnv$r<-0;.funcEnv$sa<-0;.funcEnv$ga<-0},
    All2={.funcEnv$id<-c(1L,2L);.funcEnv$x<-list(c(0,0),c(1,1));if(ok) .funcEnv$v<-lapply(1:2,function(i) eval(parse(text=vor$del.marks.gen)));.funcEnv$l2<-0;.funcEnv$l<-0},
  NNG={.funcEnv$id<-c(1L,2L);.funcEnv$x<-list(c(0,0),c(1,1));if(ok) .funcEnv$v<-lapply(1:2,function(i) eval(parse(text=vor$del.marks.gen)));.funcEnv$l2<-0;.funcEnv$l<-0}
  )

  return(invisible())
}

EBFunction.length<- function(expr) return(length(eval(expr,envir=.funcEnv)))



##class EBFunction
EBFunction<-function( type , ... ) {
  callR<-match.call()
  .funcEnv$term <- getFunctionComponents(type,callR)
  callChar<-makeInfoCall(callR)
  ### debugMode: cat("EBFunction callR=");print(callR)
  fct <- CqlsObj(EBFunction)
  ## ATTENTION: read everything from .funcEnv$term !!!
  fct$extPtr<-.ExternalInEnvir("EBFunc_function_new",type,envir=fct,PACKAGE = "EBSpat")
  fct$func.type<- type
  fct$call<-paste("EBFunction(",type,",",callChar,")",sep="") #save the call in order to reactivate it when released!
  fct$term<-.funcEnv$term #get("term",env=.funcEnv)
  fct
}

append.EBFunction<-function(self,ind , ... ) {
  callR<-match.call()
  fct<-self$fct[[ind]]
  .funcEnv$term <- getFunctionComponents(fct$func.type,callR,skip=3)
  .External("EBFunc_function_append",fct$extPtr,self$extPtr,PACKAGE = "EBSpat")
}


# get EBFunction components with further information:
# -> size of components
# -> arguments of EBFunction declaration
# RMK (IMPORTANT): in order to consider expression v$m (m being the name of the mark) 
#                  as a caracteristic .func$.marks.name has to initialized before 
#                  the creation of EBFunc!!!! 
#                  However, v[["m"]] or v[[1]] is always useable
getFunctionComponents<-function(fId,callR,skip=2) {
  type<-EBFunction.type[[fId+1]]
  #extract the EBFunction components and the arguments (graph and components) 
  comps<-as.list(callR)[-(1:skip)]
  #form first containing the unnamed values of comps 
  form<-comps[!is.named(comps)]
  opts<-list() #for additional
  #components
  comps<-comps[ is.named(comps) ]
#cat("AV:comps->");print(comps)
  comps2<-comps[ !(names(comps) %in% c("size",EBFunction.args[[type]])) ] #it is a list!
#cat("AV:comps2->");print(comps2)
  size<-eval(comps[["size"]])
  if(is.null(size)) size<-list()
  #preliminary conversion depending on the mode
  comps1<-list() #automatic named values
  opts$mode<-EBFunc.mode()
  if(is.null(opts$mode)) opts$mode<-"default"
  #TODO: penser peut-être à externaliser cette partie qui pourrait être étendue par d'autre utilisateur sans besoin de rentrer ici!!!
  getfunc4mode<-paste("getFunctionComponents",opts$mode,sep=".")
  if(exists(getfunc4mode) && is.function(eval(parse(text=getfunc4mode)))) 
    tmp<-do.call(getfunc4mode,list(form=form)) #return list(form=...,comps=...,size=..)
  else {#no external declaration only internal declaration
    tmp<-list()
    switch(opts$mode,
    P=,Pseudo={
      #TODO: testing whether length(form) > 1 
      tmp$nbParam<-length(form)-1
      tmp$comps<-form
      names(tmp$comps)<-c(".V",paste(".dV",1:tmp$nbParam,sep=""))
      tmp$form<-NULL
    },
    PE=,PseudoExpo={
      tmp$nbParam<-length(form)
      tmp$comps<-form
      tmp$comps<-list(.vc=as.call(c(as.name("c"),tmp$comps)),.vf=as.list(parse(text=".vc"))[[1]])
      #names(tmp$comps)<-".vc"
      tmp$size<-list(.vc=tmp$nbParam,.vf=tmp$nbParam)
      tmp$form<- as.call(c(as.name("sum"),as.call(c(as.name("*"),as.name("par"),tmp$comps$.vc))))
    },
    TK=,Takacs=,TakacsFiksel={
            
    },
    Gibbs=,default={#EBResid is default mode
      if(length(form)>0) {
        ##if(length(form)>1) {
          tmp$comps<-form##[-1] and ".V" introduced below! 
          if(length(tmp$comps)>1)  names(tmp$comps)<-c(".V",paste(".f",1:(length(tmp$comps)-1),sep="")) else names(tmp$comps) <- ".V"
          tmp$form<-form[[1]]
        ##} else tmp$form<-form[[1]]
      } else tmp$form<-NULL
    })
  }

  #update tmp fields!
  form<-tmp$form
  if(length(tmp$comps)>0) comps2<-c(tmp$comps,comps2)  
  if(length(tmp$size)>0) size<-c(size,tmp$size)
  ## debugMode: cat("comps2->");print(comps2);print(size)
  #update the components size (default is 1!)
  sizeComp<-rep(1,length(comps2)) #it is a vector
  names(sizeComp)<-names(comps2)
  if(!is.null(size) && all(is.named(size))) 
    for(nm in names(size)) {
      sizeComp[[nm]]<-size[[nm]]
    }

  #further
  args<-list()
  if(any(EBFunction.args[[type]] %in% names(comps))) {
    args<-comps[ EBFunction.args[[type]] ] #it is a list!
    args<-args[!is.na(names(args))] #some names were NA values!
  }
  #infos and varsList
  varsList<-lapply(comps2,function(c) findVars(c))
  unique(unlist(varsList,use.names=FALSE))->infos
  ### debugMode: cat("varsList->");print(varsList);print(infos)
  if(!is.null(form)) infos<-unique(c(infos,findVars(form)))
  infos<-intersect(infos,EBFunction.infos[[type]]) 
  ### debugMode: cat("infos(after intersect)->");print(infos)
  ### => infos are definitely determined! ##cat("infos ->");print(infos)
  #named formulas => not an info no more marks
  varsList<-sapply(varsList,function(vars) setdiff(vars,c(infos,.funcEnv$.marks.names)))
  ### debugMode: cat("varsList->");print(varsList);print(infos)
  isFunc<-sapply(varsList,function(vars) length(vars)>0)
  ### => isFunc definitely determined! ##cat("isFunc ->");print(isFunc)
  varsList<-unique(unlist(varsList,use.names=FALSE)) 
  if(!is.null(form)) varsList<-unique(c(varsList,setdiff(findVars(form),infos))) 
  ### => varsList definitely determined! ##cat("varsList ->");print(varsList)
  varsList<-setdiff(varsList,sapply(comps2,function(e) names(comps2)[!is.null(findVars(e))]))
  varsList<-setdiff(varsList,.funcEnv$.marks.names)
  ### cat("varsList (last) ->");print(varsList)
  isVar<- names(comps2) %in% varsList 
  #OLD: isVar <- sapply(comps2,is.numeric) 
  #     does not work for c(2,4) which is a call and not a numeric
  ### cat("isVar->");print(isVar) ;print(comps2);print(varsList)
  ### print(sizeComp)
  if(length(comps2)==0) { #no component, only a formula!
    compFunc<-list()
    compFunc.size<-integer(0)
    comps<-list()
    comps.size<-integer(0)
    varsEnv<-new.env()
  } else {
###cat("comps2!!");print(comps2);print(isFunc)
    compFunc<-comps2[isFunc]
###print(compFunc)
    compFunc.size<-as.integer(sizeComp[isFunc])
    names(compFunc.size)<-names(compFunc)
    vars<-comps2[isVar & !isFunc]
    varsEnv<-new.env()
#cat("assign vars->");print(vars)
    for(e in names(vars)) assign(e,eval(vars[[e]]),env=varsEnv) 
#cat("varsEnv->"); for(e in names(vars)) {cat("$",e,"\n",sep="");print(get(e,env=varsEnv))}
    # RMK: eval used since vars[[e]] can be of class call (ex: th=c(2,4) as argument)!
#cat("comps2 TOTO");print(comps2)
#print(isVar & !isFunc)
#cat("vars->");print(vars)
    comps<-comps2[!isVar & !isFunc]
#print(comps)
#print(!isVar & !isFunc)
#print(sizeComp[!isVar & !isFunc])
    comps.size<-as.integer(sizeComp[!isVar & !isFunc])
#print(comps.size)
    names(comps.size)<-names(comps)
    #type
    #if(!is.null(opt) && (opt %in% EBFunction.termType)) { #change the type
    #  type=paste(type,opt,sep="")
    #}
  }
  type<-EBFunction.id[[type]]
  #parse variable names: local or global? and maybe size!
  compsLoc<-list();compsLoc.size<-c()
  compsGlob<-list();compsGlob.size<-c()
  iL<-0;iG<-0
  for( i in seq(comps) ) {
    v<-unlist(strsplit(names(comps)[i],"\\.",perl=TRUE))
    if(v[1]=="") {v<-v[-1];v[1]<-paste(".",v[1],sep="")} # to correct names starting with "."
    sizeTmp<-NULL
    if( any(sizeMask<-(v[-1] %in% as.character(1:20))) ) {
      sizeTmp<-as.integer(v[-1][sizeMask][1])
    }
    choice<-which(c("g","b","l") %in% v[-1])
    if(length(choice)==0) choice<-"b" else choice<-c("g","b","l")[choice[1]]
    if(choice %in% c("g","b")) {
      iG<-iG+1
      compsGlob[iG]<-comps[i]
      names(compsGlob)[iG]<-v[1]
      compsGlob.size[iG]<-if(is.null(sizeTmp)) comps.size[i] else sizeTmp
      names(compsGlob.size)[iG]<-v[1]
    }
    if(choice %in% c("l","b")) {
      iL<-iL+1
      compsLoc[iL]<-comps[i]
      names(compsLoc)[iL]<-v[1]
      compsLoc.size[iL]<-if(is.null(sizeTmp)) comps.size[i] else sizeTmp
      names(compsLoc.size)[iL]<-v[1]
    }
  }
  compFuncLoc<-list();compFuncLoc.size<-c()
  compFuncGlob<-list();compFuncGlob.size<-c()
  iL<-0;iG<-0
  for( i in  seq(compFunc) ) {
    v<-unlist(strsplit(names(compFunc)[i],"\\.",perl=TRUE))
    if(v[1]=="") {v<-v[-1];v[1]<-paste(".",v[1],sep="")} # to correct names starting with "."
    sizeTmp<-NULL
    if( any(sizeMask<-(v[-1] %in% as.character(1:20))) ) {
      sizeTmp<-as.integer(v[-1][sizeMask][1])
    }
    choice<-which(c("g","b","l") %in% v[-1])
    if(length(choice)==0) choice<-"b" else choice<-c("g","b","l")[choice[1]]
    if(choice %in% c("g","b")) {
      iG<-iG+1
      compFuncGlob[iG]<-compFunc[i]
      names(compFuncGlob)[iG]<-v[1]
      compFuncGlob.size[iG]<-if(is.null(sizeTmp)) compFunc.size[i] else sizeTmp
      names(compFuncGlob.size)[iG]<-v[1]
    }
    if(choice %in% c("l","b")) {
      iL<-iL+1
      compFuncLoc[iL]<-compFunc[i]
      names(compFuncLoc)[iL]<-v[1]
      compFuncLoc.size[iL]<-if(is.null(sizeTmp)) compFunc.size[i] else sizeTmp
      names(compFuncLoc.size)[iL]<-v[1]
    }
  }
  res<-list(
    form=paste(deparse(form),collapse=""), #the first string without name is the formula
    type=type, #id (integer) of the interaction kind
    vars=varsEnv, #envir (for dynamic trick) containing variables which are numeric named parameters (see param.EBGibbs and run.EBGibbs for the use),
    varsList=varsList, #used in param.EBFunc: a priori does not match with ls(env=varsEnv) since maybe some variables not yet initialized or some variables in varsEnv are useless!
    caracLoc=sapply(compsLoc,deparse),caracLoc.size=compsLoc.size, #named parameters with nothing else than infos in the formula 
    compFuncLoc=sapply(compFuncLoc,deparse),compFuncLoc.size=compFuncLoc.size, #named parameters with other quantities in the formula 
    caracGlob=sapply(compsGlob,deparse),caracGlob.size=compsGlob.size, #same as caracLoc
    compFuncGlob=sapply(compFuncGlob,deparse),compFuncGlob.size=compFuncGlob.size, #same as compFuncLoc
    args=args, #arguments of the interaction function (ex: order=2)
    opts=opts, #additional arguments (ex: nbParam=3 providing the number of param)
    infos=infos #list of the names of the infos
  )
  #print(res)
  res
}

testGetFunctionComponents<-function(fId,...) {
  callR<-match.call()
  getFunctionComponents(fId,callR)
}

terms.EBFunction<-function(fct,...) fct$call

################################################################ Tools

makeInfoCall<-function(callR) {
  #cat("callR->");print(callR)
  #print(as.list(callR))
  res<-as.list(callR)[-(1:2)]
  paste(sapply(1:length(res),function(i,ll) if(is.named(ll[i])) paste(names(ll[i]),ll[i],sep="=") else ll[i],res),collapse=",")
}

getInfoFunc<-function(funcId,...) {
  #cat("params=");print(params)
 
  #cat("call=");print(class(call));print(call)
  infos<-unique(findVars(call))
  #cat("infos=");print(infos)

  #params of the func (function)
  #print(names(params))
  #print(funcId)
  modpar<-pmatch(names(params),EBFunction.args[[EBFunction.type[funcId+1]]],nomatch=0)
  #cat("modpar=");print(modpar)
  funcParams<-character(0)
  
  if(sum(modpar)) {#there is func params!
    funcParams<-params[modpar!=0]
    names(funcParams)<-EBFunction.params[[EBFunction.type[funcId+1]]][modpar]
    params<-params[modpar==0]
  }

  nexpo<-(length(calls)==2) & length(npar<-intersect(names(params),infos))
  #cat("expo?->");print(!nexpo)
  if(nexpo) {
    infos<-setdiff(infos,npar)
    #cat("infos2");print(infos)
    params<-params[npar]
    call<-calls[[2]]
  }
  list(funcId=funcId,funcParams=funcParams,expo=!nexpo,infos=infos,params=params,funcString=paste(deparse(call),collapse=""))
}

# ex: testInfoFunc(1, order = 2, l2 <= 400, 400 < l2 & l2 <= 6400, theta = c(2,4))
testInfoFunc<-function(type,...) {
  callR<-makeInfoCall(match.call())
  eval(parse(text=paste("getInfoFunc(",callR,")",sep="")))
}

## find variables names (related to characteristics) in the expression!!!
findVars<-function(e) {
  if(length(e)>1) return(as.vector(unlist(sapply(2:length(e),function(i) findVars(e[[i]])))))
  if(inherits(e,"name")) return(as.character(e))
}


###########################################################################
## TO CONSERVE THE FIRST VERSION AND TEST IT AGAINST THE NEW ONES!!!!
## DO NOT DELETE WHENEVER I AM NOT SURE THINGS ARE WORKING WELL!
###########################################################################
OLDgetFunctionComponents<-function(fId,callR,skip=2) {
  type<-EBFunction.type[[fId+1]]
  #extract the EBFunction components and the arguments (graph and components) 
  comps<-as.list(callR)[-(1:skip)]
  #formula and option
  form<-comps[!is.named(comps)]
  opt<-NULL
  if(length(form)>0) {
    if(length(form)>1) {
      opt<-deparse(form[[1]])
      form<-form[[2]] #the rest is ignored!
    } else {
      if(deparse(form[[1]])%in% EBFunction.termType) {
        opt<-deparse(form[[1]])
        form<-NULL
      } else form<-form[[1]]
    }
  } else form<-NULL
  #components
  comps<-comps[ is.named(comps) ]
  comps2<-comps[ !(names(comps) %in% c("size",EBFunction.args[[type]])) ] #it is a list!
  #update the components size (default is 1!)
  size<-eval(comps[["size"]])
  sizeComp<-rep(1,length(comps2)) #it is a vector
  names(sizeComp)<-names(comps2)
  if(!is.null(size) && all(is.named(size))) 
    for(nm in names(size)) {
      sizeComp[[nm]]<-size[[nm]]
    }
  #further
  args<-list()
  if(any(EBFunction.args[[type]] %in% names(comps))) args<-comps[ EBFunction.args[[type]] ] #it is a list!
  #infos
  unlist(varsList<-lapply(comps2,function(c) findVars(c)))->infos
  if(!is.null(form)) infos<-c(infos,findVars(form))
  infos<-intersect(infos,EBFunction.infos[[type]])
  #named formulas
  isFunc<-sapply(varsList,function(vars) length(setdiff(vars,infos))>0)
  isVar<-sapply(comps2,is.numeric)
#print(comps2)
#print(isFunc)
#print(sapply(comps2,is.numeric))
#print(sizeComp)
  if(length(comps2)==0) { #no component, only a formula!
    compFunc<-list()
    compFunc.size<-integer(0)
    comps<-list()
    comps.size<-integer(0)
    varsEnv<-new.env()
  } else {
    compFunc<-comps2[isFunc]
    compFunc.size<-as.integer(sizeComp[isFunc])
    names(compFunc.size)<-names(compFunc)
    vars<-comps2[isVar & !isFunc]
    varsEnv<-new.env()
    for(e in names(vars)) assign(e,vars[[e]],env=varsEnv)
#print(comps2)
#print(isVar & !isFunc)
#print(vars)
    comps<-comps2[!isVar & !isFunc]
    comps.size<-as.integer(sizeComp[!isVar && !isFunc])
    names(comps.size)<-names(comps)
    #type
    #if(!is.null(opt) && (opt %in% EBFunction.termType)) { #change the type
    #  type=paste(type,opt,sep="")
    #}
  }
  type<-EBFunction.id[[type]]
  #parse variable names: local or global? and maybe size!
  compsLoc<-list();compsLoc.size<-c()
  compsGlob<-list();compsGlob.size<-c()
  iL<-0;iG<-0
  for( i in seq(comps) ) {
    v<-unlist(strsplit(names(comps)[i],"\\.",perl=TRUE))
    sizeTmp<-NULL
    if( any(sizeMask<-(v[-1] %in% as.character(1:20))) ) {
      sizeTmp<-as.integer(v[-1][sizeMask][1])
    }
    choice<-which(c("g","b","l") %in% v[-1])
    if(length(choice)==0) choice<-"b" else choice<-c("g","b","l")[choice[1]]
    if(choice %in% c("g","b")) {
      iG<-iG+1
      compsGlob[iG]<-comps[i]
      names(compsGlob)[iG]<-v[1]
      compsGlob.size[iG]<-if(is.null(sizeTmp)) comps.size[i] else sizeTmp
      names(compsGlob.size)[iG]<-v[1]
    }
    if(choice %in% c("l","b")) {
      iL<-iL+1
      compsLoc[iL]<-comps[i]
      names(compsLoc)[iL]<-v[1]
      compsLoc.size[iL]<-if(is.null(sizeTmp)) comps.size[i] else sizeTmp
      names(compsLoc.size)[iL]<-v[1]
    }
  }
  compFuncLoc<-list();compFuncLoc.size<-c()
  compFuncGlob<-list();compFuncGlob.size<-c()
  iL<-0;iG<-0
  for( i in  seq(compFunc) ) {
    v<-unlist(strsplit(names(compFunc)[i],"\\.",perl=TRUE))
    sizeTmp<-NULL
    if( any(sizeMask<-(v[-1] %in% as.character(1:20))) ) {
      sizeTmp<-as.integer(v[-1][sizeMask][1])
    }
    choice<-which(c("g","b","l") %in% v[-1])
    if(length(choice)==0) choice<-"b" else choice<-c("g","b","l")[choice[1]]
    if(choice %in% c("g","b")) {
      iG<-iG+1
      compFuncGlob[iG]<-compFunc[i]
      names(compFuncGlob)[iG]<-v[1]
      compFuncGlob.size[iG]<-if(is.null(sizeTmp)) compFunc.size[i] else sizeTmp
      names(compFuncGlob.size)[iG]<-v[1]
    }
    if(choice %in% c("l","b")) {
      iL<-iL+1
      compFuncLoc[iL]<-compFunc[i]
      names(compFuncLoc)[iL]<-v[1]
      compFuncLoc.size[iL]<-if(is.null(sizeTmp)) compFunc.size[i] else sizeTmp
      names(compFuncLoc.size)[iL]<-v[1]
    }
  }
  res<-list(
    form=deparse(form), #the first string without name is the formula
    type=type, #id (integer) of the interaction kind
    vars=varsEnv, #envir (for dynamic trick) containing variables which are numeric named parameters 
    caracLoc=sapply(compsLoc,deparse),caracLoc.size=compsLoc.size, #named parameters with nothing else than infos in the formula 
    compFuncLoc=sapply(compFuncLoc,deparse),compFuncLoc.size=compFuncLoc.size, #named parameters with other quantities in the formula 
    caracGlob=sapply(compsGlob,deparse),caracGlob.size=compsGlob.size, #same as caracLoc
    compFuncGlob=sapply(compFuncGlob,deparse),compFuncGlob.size=compFuncGlob.size, #same as compFuncLoc
    args=args, #arguments of the interaction function (ex: order=2)
    infos=infos #list of the names of the infos
  )
  #print(res)
  res
}

OLDtestGetFunctionComponents<-function(fId,...) {
  callR<-match.call()
  OLDgetFunctionComponents(fId,callR)
}
