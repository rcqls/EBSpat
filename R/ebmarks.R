EBMarks<-function(...) {
  tmp<-as.list(match.call())[-1]

  Int<-Integer<-int<-integer<-function(len,gen) {
    if(!missing(gen)) {
      if(inherits(try(is.numeric(gen),silent=TRUE),"try-error")) gen<-substitute(gen)
      else gen<-substitute(sample(x,n,rep=TRUE),list(x=gen,n=len))
    }
    obj<-list(type=1,length=len,gen=gen)
  }

  Dbl<-Double<-Real<-dbl<-double<-function(len,gen) {
    if(!missing(gen)) {
      if(inherits(try(is.numeric(gen),silent=TRUE),"try-error")) gen<-substitute(gen)
      else gen<-substitute(runif(n,fr,to),list(n=len,fr=gen[1],to=gen[2]))
    }
    obj<-list(type=0,length=len,gen=gen)
  }

  marks<-lapply(tmp,eval.parent)
  gen<-as.call(c(as.name("list"),lapply(marks,function(m) m$gen)))
  list(name=names(marks),length=sapply(marks,function(m) m$length),type=sapply(marks,function(m) m$type),gen=gen)
}

EBMarks.set<-function(obj,marks) {  
  obj$del.marks.length <- marks$length
  obj$del.marks.name <- marks$name
  obj$del.marks.type <- marks$type
  obj$del.marks.gen <- deparse(marks$gen)
  .External("EBVor_marks_set", obj$extPtr, PACKAGE = "EBSpat")
}

EBMarks.get <- function(obj) {
  list(length=obj$del.marks.length,name=obj$del.marks.name,type=obj$del.marks.type,gen=obj$del.marks.gen)
}
