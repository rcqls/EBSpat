#.First.lib 
.onLoad<- function(lib,pkg) {
  #require(getattr)
  local({ ##IF PROBLEM SEE CqlsEAP/R/zzz.R
    if(!exists(".funcEnv")) .funcEnv<-new.env(parent=.GlobalEnv)
    if(is.null(.funcEnv$debugMode)) .funcEnv$debugMode <- FALSE
  },.GlobalEnv)
  library.dynam("EBSpat", pkg, lib,local=FALSE)
}
