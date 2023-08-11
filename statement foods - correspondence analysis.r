rm(list=ls())

setwd("D:/DataMining/Databases_for_mining/dataset_for_soft_dev_and_comparison/fact_analysis/afc")
foods <- read.table(file="statements_foods.txt",header=T,sep="\t",row.names=1)
print(foods)

#chi-squared test
print(chisq.test(foods))
##The first table describes the results of the chi-square test of independence. This result is essential.
#Indeed, if the global association between the rows and the columns is too weak, the analysis of the 
#associations between some rows and columns is not really useful. We must be sure that there is
#usable information into the table.
#Here, we obtain c²global = 647.31, with a degree of freedom equal to 91 [= (14 - 1) x (8 – 1)]. The 
#association is statistically significant (p-value < 0.0001).
#In addition, Tanagra provides the f² statistic (Trace), with f² = c²/n = 647.31 / 1760 = 0.3678. It is the 
#total inertia of the cloud of points. The square root of the inertia corresponds to a kind of the 
#correlation coefficient between the rows and the columns7
#The aim of the correspondence analysis is to decompose the f² on a sequence of orthogonal factors
#*********************
#CA using 'ca' package
#*********************
library(ca)
foods.ca <- ca(foods,nd=2)

#eigen values and cumulative proportion of variance explained in percentage
print(cbind(foods.ca$sv^2,(100.0*cumsum(foods.ca$sv^2)/sum(foods.ca$sv^2))))
#Tanagra provides the eigenvalues table (lk). They correspond to the part of the 
total inertia explained by the factors. Because we have orthogonal factors, the sum of all eigenvalues 
(0.193095 + 0.077731 + … + 0.002363) is equal to the total inertia (f² = 0.3678).
We can write the same information in the form of the proportion of the total inertia (e.g. % factor 1 = 
0.193095 / 0.3678 = 52.50 %; % factor 2 = 0.07731 = 21.13 %).

#row analysis
attach(foods.ca)
row.ca <- round(cbind(rowmass,rowdist^2,rowinertia,rowcoord[,1]*sv[1],rowcoord[,2]*sv[2]),5)
colnames(row.ca) <- c("weight","sq.dist","inertia","coord.1","coord.2")
rownames(row.ca) <- rownames
print(row.ca)

#column analysis
col.ca <- round(cbind(colmass,coldist^2,colinertia,colcoord[,1]*sv[1],colcoord[,2]*sv[2]),5)
colnames(col.ca) <- c("weight","sq.dist","inertia","coord.1","coord.2")
rownames(col.ca) <- colnames
print(col.ca)
#


#plotting rows and columns
plot(foods.ca)

#***********************
#CA using 'ade4' package
#***********************
library(ade4)
foods.coa <- dudi.coa(foods,scannf=F,nf=2)

#eigen values and cumulative proportion of variance explained in percentage
print(round(cbind(foods.coa$eig,100.0*cumsum(foods.coa$eig)/sum(foods.coa$eig)),4))

#row analysis - coordinates and contributions
print(cbind(foods.coa$li,inertia.dudi(foods.coa,row.inertia=T)$row.abs))

#column analysis - coordinates and contributions
print(cbind(foods.coa$co,inertia.dudi(foods.coa,col.inertia=T)$col.abs))

#plotting rows and columns
scatter.coa(foods.coa,method=1)

#canonical graph
score.coa(foods.coa,xax=1,dotchart=T)




