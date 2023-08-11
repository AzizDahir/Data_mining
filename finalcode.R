setwd("C:/Users/aziz/Desktop/5703")
diabetes<-read.table("diabetes data.R",sep=',')
ncol<-read.table("diabetes colnames.R")
colnames(diabetes)<-t(ncol)

diabetes.order<-diabetes[order(diabetes[,9]),]
 sum(diabetes.order$diabetes==0)
sum(diabetes.order$diabetes==1)
diabetes.0<-diabetes.order[1:500,]
diabetes.1<-diabetes.order[501:768,]

diabetes.0$insulin[diabetes.0$insulin==0]<-NA
diabetes.0[,5]<-as.numeric(diabetes.0[,5])

diabetes.0$glucose[diabetes.0$glucose==0]<-NA
 diabetes.0[,2]<-as.numeric(diabetes.0[,2])
diabetes.0$pressure[diabetes.0$pressure==0]<-NA
diabetes.0[,3]<-as.numeric(diabetes.0[,3])
 diabetes.0$triceps[diabetes.0$triceps==0]<-NA
 diabetes.0[,4]<-as.numeric(diabetes.0[,4])
 diabetes.0$mass[diabetes.0$mass==0]<-NA
 diabetes.0[,6]<-as.numeric(diabetes.0[,6])

diabetes.1$insulin[diabetes.1$insulin==0]<-NA
diabetes.1[,5]<-as.numeric(diabetes.1[,5])
diabetes.1$glucose[diabetes.1$glucose==0]<-NA
 diabetes.1[,2]<-as.numeric(diabetes.1[,2])
diabetes.1$pressure[diabetes.1$pressure==0]<-NA
diabetes.1[,3]<-as.numeric(diabetes.1[,3])
 diabetes.1$triceps[diabetes.1$triceps==0]<-NA
 diabetes.1[,4]<-as.numeric(diabetes.1[,4])
 diabetes.1$mass[diabetes.1$mass==0]<-NA
 diabetes.1[,6]<-as.numeric(diabetes.1[,6])

na.diabetes.0<-na.omit(diabetes.0)
na.diabetes.1<-na.omit(diabetes.1)
require(mice)
est.0<-mice(diabetes.0, m=5)
esti.0<-complete(est.0,3)
summary(esti.0)
mice(diabetes.1, m=5) #######estimate the missing data
est.1<-mice(diabetes.1, m=5)
esti.1<-complete(est.1,3)
summary(esti.1)

new.diabetes<-rbind(esti.0,esti.1) ##### new data with 768 observations
pairs(new.diabetes)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,use="complete.obs"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)*r
    text(0.5, 0.5, txt, cex = cex.cor)
  }
panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
 }
pairs(new.diabetes, upper.panel=panel.cor, diag.panel=panel.hist)

d.class<-c(rep(0,500),rep(1,268))
pairs(new.diabetes,col=new.diabetes[,9]+1)

new.diabetes.no.class<- new.diabetes[,-9]
(no.pc<-prcomp(new.diabetes.no.class))
summary(no.pc)
plot(no.pc,col=heat.colors(6))

new.diabetes.no.ped<-new.diabetes.no.class[,-7]
(noo.pc<-prcomp(new.diabetes.no.ped))
summary(noo.pc)

indexes<-sample(1:nrow(new.diabetes),size=ceiling(0.2*nrow(new.diabetes)))
test<-new.diabetes[indexes,]
training<-new.diabetes[-indexes,]
training.no.class<- training[,-9]
training.no.pred<-training.no.class[,-7]
(training.pc<-prcomp(training.no.pred))
summary(training.pc)
plot(training.pc,col=heat.colors(6))

test.no.class<- test[,-9]
test.no.pred<-test.no.class[,-7]
(test.pc<-prcomp(test.no.pred))
summary(test.pc)
plot(test.pc,col=heat.colors(6))

fit.training<-glm(training[,9]~training.pc$x[,1]+training.pc$x[,2], quasibinomial(link="logit"))
fit.training
pred.pc<-predict(fit.training, type="response")
confusion(training$diabetes,pred.pc>0.5)

fit.test<-glm(test[,9]~test.pc$x[,1]+test.pc$x[,2], quasibinomial(link="logit"))
fit.test
predd.pc<-predict(fit.test, type="response")
confusion(test$diabetes,predd.pc>0.5)

training.glm<- glm(diabetes ~ .,
data = training,
family = binomial) 
training.glm
summary(training.glm)
cor(training)
n.training <- training[,(-3:5)]
new.training<-n.training[,-5]

new.training.glm<- glm(diabetes ~ .,
data = new.training,
family = binomial) 
new.training.glm
summary(new.training.glm)

confusion = function(pred,obs)
{
    # Build a Confusion matrix

    true = length(unique(obs))

    confusion = table(pred-1,obs)

    rsum = rowSums(confusion)
    csum = c(colSums(confusion),sum(rsum))
    
    cnum = length(unique(pred))+1

    vals = 1:dim(confusion)[1]


    confusion = cbind(confusion,rsum)
    confusion = rbind(confusion,csum)


    right = 0
    prec = NULL
    recall = NULL
    for(i in 1:true)
    {
        right = right + confusion[i,i]
        prec = c(prec,confusion[i,i]/confusion[i,3])
        recall = c(recall,confusion[i,i]/confusion[cnum,i])
    }

    error = sum(rsum) - right
    error = error/sum(rsum)

    OUT = NULL
    OUT$confusion = confusion
    OUT$error = error
    OUT$precision = prec
    OUT$recall = recall



    return(OUT)
}

pred<-predict(new.training.glm, type="response")
confusion(training$diabetes,pred>0.5)

test.glm<-glm(diabetes ~ .,
data = test,
family = binomial) 

n.test <- test[,-(3:5)]
new.test<-n.test[,-5]
new.test.glm<- glm(diabetes ~ .,
data = new.test,
family = binomial) 
new.test.glm
summary(new.test.glm)

predd<-predict(new.test.glm, type="response")
confusion(test$diabetes,predd>0.5)

fmPID <- mob(diabetes ~ pregnant + glucose + mass + pedigree | pressure + triceps +
 insulin + age,
 data = training, model = glinearModel,
 family = binomial())
fmPID
pred.fm<-predict(fmPID, type="response")
confusion(training$diabetes,pred.fm>0.5)

PIDfm <- mob(diabetes ~ pregnant + glucose + mass + pedigree | pressure + triceps +
 insulin + age,
 data = test, model = glinearModel,
 family = binomial())
PIDfm
summary(PIDfm)
plot(PIDfm)
fm.pred<-predict(PIDfm, type="response")
confusion(test$diabetes,fm.pred>0.5)

library(rggobi)
g<-ggobi(new.diabetes)
display(g[1], "Scatterplot Matrix")
display(g[1], "2D Tour")
(old.col<- glyph_colour(g[1])) 
(noquote(rbind(new.diabetes[,9],old.col)))

Davies.Bouldin<-function(A,SS,m){
N <- nrow(A)
S <- sqrt(SS/m)
M <- as.matrix(dist(A))
R <- matrix(0,N,N)
for (i in 1:(N-1)){
for (j in (i+1):N){
R[i,j]<- (S[i] + S[j])/M[i,j]
R[j,i]<-R[i,j]
  }
 }
return(mean(apply(R,1,max)))
}
library(stats)
(k2.diabetes<-kmeans(new.diabetes, 2, 10))
 print(confusion(k2.diabetes$cluster,new.diabetes$diabetes))
graphics.off()
plot(new.diabetes[,2],new.diabetes[,5],xlab="Glucose",ylab="Insulin",type="n",main="k-means - 2 clusters",cex.main=1.5)
text(new.diabetes[,2],new.diabetes[,5],new.diabetes$diabetes, col=k2.diabetes$cluster+1)
mtext(paste("DaviesBouldin Index =",
(floor(1000*Davies.Bouldin(k2.diabetes$centers, k2.diabetes$withinss, k2.diabetes$size) +
0.5))/1000),2,0, cex=1.3)
best.match<- function(table,candidates){
tmp <- {}
for (i in candidates) {
tmp<-c(tmp,length(which(table==i)))
}
candidates[which(tmp == max(tmp))]
}
(cand<- unique(k2.diabetes$cluster))
(error.pos<- c(which(k2.diabetes$cluster[1:384] !=
best.match(k2.diabetes$cluster[1:500], cand)),
which(k2.diabetes$cluster[501:768] !=
best.match(k2.diabetes$cluster[22:43], cand)) +501))

graphics.off()
plot(new.diabetes[,2],new.diabetes[,5],xlab="Glucose",ylab="Insulin",type="n",main="k-means - 2 clusters",asp=1)
text(new.diabetes[,2],new.diabetes[,5],new.diabetes$diabetes, col=(k2.diabetes$cluster*2))
for(i in 1:length(error.pos)){
arrows(80,160,new.diabetes[error.pos[i],2],new.diabetes[error.pos[i],5])
}
text(80,158,"errors")
points(k2.diabetes$enter[,2:1], pch=17, col=2:4,cex=2)


### 24 clusters
wss <- (nrow(x)-1)*sum(apply(new.diabetes,2,var))
      for (i in 2:25) wss[i] <- sum(kmeans(new.diabetes,
                                           centers=i)$withinss)
      plot(1:25, wss, type="b", xlab="Number of Clusters",
           ylab="Within groups sum of squares")   


(k24.diabetes<-kmeans(new.diabetes, 24, 10))
 print(confusion(k24.diabetes$cluster,new.diabetes$diabetes))
graphics.off()
plot(new.diabetes[,2],new.diabetes[,5],xlab="Glucose",ylab="Insulin",type="n",main="k-means - 24 clusters",cex.main=1.5)
text(new.diabetes[,2],new.diabetes[,5],new.diabetes$diabetes, col=k24.diabetes$cluster+1)
mtext(paste("DaviesBouldin Index =",
(floor(1000*Davies.Bouldin(k24.diabetes$centers, k24.diabetes$withinss, k24.diabetes$size) +
0.5))/1000),4,0, cex=1.3)





