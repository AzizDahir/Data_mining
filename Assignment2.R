setwd("C:/Users/aziz/Desktop/5703")
boston<-read.table("data2.r")
ncol<-read.table("col2.r")
colnames(boston)<-t(ncol)
InterpNewton <- function(knot.x, knot.y){
   n <- length(knot.x)
   for (k in 1:(n-1)){
      knot.y[(k+1):n] <- (knot.y[(k+1):n] - knot.y[k])/(knot.x[(k+1):n] - knot.x[k])
   }
   knot.y
}
HornerN <- function (coef, knot.x, z) {
   n <- length(knot.x)
   polyv <- coef[n]*rep(1,length(z))    
   for (k in (n-1):1){
      polyv <- (z - knot.x[k])*polyv +coef[k]
   }
   polyv
}
