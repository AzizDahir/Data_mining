setwd("  ")
diabetes<-read.table("diabetes data.R",sep=',')
ncol<-read.table("diabetes colnames.R")
colnames(diabetes)<-t(ncol)
d.diabetes<-data.matrix(diabetes)
require(mlbench)
data("PimaIndiansDiabetes")
PIDiabetes<-PimaIndiansDiabetes[,1:8]
diabete<-diabetes[,1:8]
chart.Correlation(diabetes, histogram=TRUE, method="spearman")
cor(diabete)
sum(diabetes$A9==0)
sum(diabetes$A9==1)
class<-c(rep(0,500),rep(1,268))
diabetes.A9<-diabetes[order(diabetes[,9]),]
require(PerformanceAnalytics)
chart.Correlation(diabetes.A9, histogram=TRUE, method="spearman",col=class+1)
#####children<-c(rep(5,549),rep(11,196),rep(17,23))
######diabetes.A1<-diabetes[order(diabetes[,1]),]
######pairs(diabetes.A1,col=diabetes[,1]+6)
#######pairs(diabetes.A1,col=children+6)
f.data.std <- function(data) {
  data <- as.matrix(data)
  bar <- apply(data, 2, mean)
  s   <- apply(data, 2, sd)
  t((t(data) - bar)/s)
}
diabete.std<-f.data.std(diabete)
(diabete.std.pc<-prcomp(diabete.std))
summary(diabete.std.pc)
plot(diabete.std.pc, col=heat.colors(6))
library(rggobi)
g<-ggobi(diabetes)
display(g[1], "Scatterplot Matrix")
display(g[1], "2D Tour")
(old.col<- glyph_colour(g[1])) 
(noquote(rbind(diabetes[,9],old.col)))  ###Class
diabete<-data.matrix(diabete)
diabetes.class<-diabetes[,9]
#####lm(diabetes.class~diabete) ### least squares fit on full data
######f.data.std <- function(data) {
  data <- as.matrix(data)
  bar <- apply(data, 2, mean)
  s   <- apply(data, 2, sd)
  t((t(data) - bar)/s)
}
####diabete.std<-f.data.std(diabete)
#####diabetes.class.std<-f.data.std(diabetes.class)
######lm.std<-lm(diabetes.class.std~diabete.std)
###### logistic regression with decision tree
require(party)
fmPID <- mob(diabetes ~ glucose | pregnant + pressure + triceps +
 insulin + mass + pedigree + age,
 data = PimaIndiansDiabetes, model = glinearModel,
 family = binomial())
fmPID
plot(fmPID)
plot(PimaIndiansDiabetes[,4],PimaIndiansDiabetes[,6]) #####missing data
 PimaIndiansDiabetes$glucose[PimaIndiansDiabetes$glucose==0]<-NA
 PimaIndiansDiabetes[,2]<-as.numeric(PimaIndiansDiabetes[,2])
PimaIndiansDiabetes$pressure[PimaIndiansDiabetes$pressure==0]<-NA
PimaIndiansDiabetes[,3]<-as.numeric(PimaIndiansDiabetes[,3])
 PimaIndiansDiabetes$triceps[PimaIndiansDiabetes$triceps==0]<-NA
 PimaIndiansDiabetes[,4]<-as.numeric(PimaIndiansDiabetes[,4])
 PimaIndiansDiabetes$mass[PimaIndiansDiabetes$mass==0]<-NA
 PimaIndiansDiabetes[,6]<-as.numeric(PimaIndiansDiabetes[,6])
Tricep<-na.omit(PimaIndiansDiabetes)
cor(Tricep[,-9])
pairs(Tricep)
#### high correlation with tricep n mass round 0.64 
no.tricep<-PimaIndiansDiabetes[-4]
no.tricep$glucose[no.tricep$glucose==0]<-NA
 no.tricep[,2]<-as.numeric(no.tricep[,2])
no.tricep$pressure[no.tricep$pressure==0]<-NA
no.tricep[,3]<-as.numeric(no.tricep[,3])
 no.tricep$mass[no.tricep$mass==0]<-NA
 no.tricep[,5]<-as.numeric(no.tricep[,5])
no.triceps<-na.omit(no.tricep)
summary(no.triceps)
cor(no.triceps[,-8])
#####require(mice)
#####mice(PimaIndiansDiabetes, m=5)
require(party)
fmPID <- mob(diabetes ~ glucose , pregnant | pressure
 insulin + mass + pedigree + age,
 data = no.triceps, model = glinearModel,
 family = binomial())
plot(PimaIndiansDiabetes[,4],PimaIndiansDiabetes[,6]) #####missing data
 diabetes$glucose[diabetes$glucose==0]<-NA
 diabetes[,2]<-as.numeric(diabetes[,2])
diabetes$pressure[diabetes$pressure==0]<-NA
diabetes[,3]<-as.numeric(diabetes[,3])
 diabetes$triceps[diabetes$triceps==0]<-NA
 diabetes[,4]<-as.numeric(diabetes[,4])
 diabetes$mass[diabetes$mass==0]<-NA
 diabetes[,6]<-as.numeric(diabetes[,6])
diabetes.na.omit<-na.omit(diabetes)
cor(diabetes.na.omit)
require(HH)
vif(diabetes.na.omit)
#########strong correlation btwn tricep and body mass. no multicollinearity
no.tricep<-diabetes[-4]
no.tricep$glucose[no.tricep$glucose==0]<-NA
 no.tricep[,2]<-as.numeric(no.tricep[,2])
no.tricep$pressure[no.tricep$pressure==0]<-NA
no.tricep[,3]<-as.numeric(no.tricep[,3])
 no.tricep$mass[no.tricep$mass==0]<-NA
 no.tricep[,5]<-as.numeric(no.tricep[,5])
no.triceps<-na.omit(no.tricep)
######order according to class
no.triceps<-data.matrix(no.triceps)
###ordered<-no.triceps[order(no.triceps[,8]),]
###no.triceps<-data.frame(no.triceps)
#sum(ordered$diabetes==1)
#sum(ordered$diabetes==2)
library(rggobi)
g<-ggobi(no.triceps)
display(g[1], "Scatterplot Matrix")
display(g[1], "2D Tour")

no.tricep$insulin[no.tricep$insulin==0]<-NA
 no.tricep[,4]<-as.numeric(no.tricep[,4])


#n.ordered<-ordered[-1:234,]

indexes<-sample(1:nrow(no.triceps),size=ceiling(0.2*nrow(no.triceps)))
test<-no.triceps[indexes,]
training<-no.triceps[-indexes,]

require(randomForest)
 dia<-randomforest(diabetes~.,data=no.triceps, ntree=1000, mtry=2,samplesize=c(235,235))
diabetes$insulin[diabetes$insulin==0]<-NA
 diabetes[,5]<-as.numeric(diabetes[,5])
no.insulin<-na.omit(diabetes)

sum(no.insulin$diabetes==0)
sum(no.insulin$diabetes==1)

d.class<-c(rep(0,262),rep(1,130))
diabetes.class<-no.insulin[order(no.insulin[,9]),]
pairs(no.insulin,col=no.insulin[,9]+2)

no.insulin.class<- no.insulin[,-9]
(no.pc<-prcomp(no.insulin.class))
summary(no.pc)
plot(no.pc,col=heat.colors(6))

#####Another route replacing the missing data with mice package of diabetic or not diabetic
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
#######376 rows with missing 
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
####diabetes.class<-new.diabetes[order(new.diabetes[,9]),] already ordered####
pairs(new.diabetes,col=new.diabetes[,9]+1)

new.diabetes.no.class<- new.diabetes[,-9]
(no.pc<-prcomp(new.diabetes.no.class))
summary(no.pc)
plot(no.pc,col=heat.colors(6))

new.diabetes.no.ped<-new.diabetes.no.class[,-7]
(noo.pc<-prcomp(new.diabetes.no.ped))
summary(noo.pc)

##### Comparing pc1 with n w/out pedigree exact same values n summary is the summary so get rid
#### of pedigree only for pca. 
##### from pc1 insulin explains the majority of the data
##### from pc2 glucose explains most of the data.
### logistic regression
coplot(pregnant ~glucose|diabetes, data=new.diabetes)
coplot(glucose ~insulin|diabetes, data=new.diabetes)
coplot(mass ~age|diabetes, data=new.diabetes)

coplot(Number ~Insulin|BMI, data=df.diabetes, overlap=0.1, col=d.diabetes[,9]+2, pch=16)
require(party)
fmPID <- mob(diabetes ~ glucose | pregnant + pressure + triceps +
 insulin + mass + pedigree + age,
 data = new.diabetes, model = glinearModel,
 family = binomial())
fmPID

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
table(training$diabetes,pred.pc>0.5)

fit.test<-glm(test[,9]~test.pc$x[,1]+test.pc$x[,2], quasibinomial(link="logit"))
fit.test
predd.pc<-predict(fit.test, type="response")
confusion(test$diabetes,predd.pc>0.5)

###fitt<-glm(new.diabetes[,9]~noo.pc$x[,1]+noo.pc$x[,2], quasibinomial(link="logit"))
#####fitt

training.glm<- glm(diabetes ~ .,
data = training,
family = binomial) 
training.glm
summary(training.glm)
cor(training)
###n.pred<-predict(training.glm, type="response")
###table(training$diabetes,n.pred>0.5)

###### get rid of insulin cor with glucose
###### get rid of triceps cor with mass
###### get rid of age cor with pregnant
###### get rid of pressure look for another reason other than p-value > alpha

n.training <- training[,(-3:5)]
new.training<-n.training[,-5]

new.training.glm<- glm(diabetes ~ .,
data = new.training,
family = binomial) 
new.training.glm
summary(new.training.glm)

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


####logit n decision tree logit>> pressure,glucose,mass,pedigree d.tree>> pressure,triceps,insulin,age
 
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

write.table(new.diabetes,row.names=FALSE,"C:\\Users\\aziz\\Desktop\\5703\\dia.txt",sep="\t")
confusion(training$diabetes,pred.fm>0.5)
library(rggobi)
g<-ggobi(new.diabetes)
display(g[1], "Scatterplot Matrix")
display(g[1], "2D Tour")
(old.col<- glyph_colour(g[1])) 
(noquote(rbind(new.diabetes[,9],old.col)))
d.class<-c(rep(0,500),rep(1,268))

require(stats)
library(mda)
(k2.diabetes<-kmeans(new.diabetes[,-9], 2, 10))

##X.syn<- clust[[1]]
##for (i in (2:numb.centers)){
##X.syn <- rbind(X.syn, clust[[i]])
##}
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
oldpar <- par(mfrow=c(4,4))
par(mar=c(2,1,2,1))
errs <- rep(0,10)
DBI<- rep(0,10)
for (i in 2:15){
KM<- kmeans(X.syn, i, 15)
plot(X.syn, col=colors[KM$cluster],pch=KM$cluster, main=paste(i,"clusters"))
errs[i-1]<-sum(KM$withinss)
DBI[i-1]<-Davies.Bouldin(KM$centers,KM$withinss)
}
plot(2:15,errs,main="SS")
lines(2:15,errs)
plot(2:15, DBI, main="Davies-Bouldin")
lines(2:15,DBI)
par(oldpar)

library(stats)
print(confusion(k2.diabetes$cluster,new.diabetes$diabetes))
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
graphics.off()
plot(new.diabetes[,2],new.diabetes[,5],xlab="Glucose",ylab="Insulin",type="n",main="k-means - 2 clusters",cex.main=1.5)
text(new.diabetes[,2],new.diabetes[,5],new.diabetes$diabetes, col=k2.diabetes$cluster*2)
mtext(paste("DaviesBouldin Index =",
(floor(10*Davies.Bouldin(k2.diabetes$centers, k2.diabetes$withinss, k2.diabetes$size) +
0.5))/10),2,0, cex=1.3)
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



require(FNN)
cl<-factor(c(rep(0,389),rep(1,225)))
training.knn<-knn(training,test,cl,k=5,prob=TRUE)
pred.knn<-predict(training.knn, type="response")
table(training$diabetes,pred.knn>0.5)
