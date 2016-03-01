TC.Mary<-curve(dbeta(x,1.3,7.366667)/4.501841,xlab="Proportion Tree Cover/ Acre",ylab="Suitability")

SC.Mary<-curve(dbeta(x,1.5,3.5)/2.108676,xlab="Proportion Shrub Cover/ Acre",ylab="Suitability")

library(rootSolve)
fun<-function(x){(dbeta(x,3,4.5)/2)-1}
uniroot.all(fun,c(0,1))
BG.Mary.fun<-function(x){
  ifelse(x<.2805743,y<-dbeta(x,3,4.5)/2,ifelse(x>.4525119,y<-dbeta(x,3,4.5)/2,y<-1))
}
BG.Mary<-curve(BG.Mary.fun(x),xlab="Proportion Bare Ground/ Acre",ylab="Suitability")

fun2<-function(x){##find multiplier that will make >.15% optimal
  (dbeta(.15,1.1,1)*x)-1
}
uniroot.all(fun2,c(0,5))
FC.Mary.fun<-function(x){
  ifelse(x<.15,y<-dbeta(x,1.1,1)*1.099001,y<-1)
}
FC.Mary<-curve(FC.Mary.fun(x),xlab="Proportion Forb Cover/ Acre",ylab="Suitability")

GC.Mary<-curve(FC.Mary.fun(x),xlab="Proportion Native Grass Cover/ Acre",ylab="Suitability")

fun3<-function(x){(x*dnorm(300,300,100))-1}##find appropriate multiplier for first part of function
uniroot.all(fun3,c(250,350))
fun4<-function(x){(x*dnorm(600,600,200))-1}##find appropriate multiplier for second part of function
uniroot.all(fun4,c(250,800))
Nest.Mary.fun<-function(x){
  ifelse(x<300,y<-dnorm(x,300,100)*250.6628,ifelse(x>600,y<-dnorm(x,600,200)*501.3257,y<-1))
}
Nest.Mary<-curve(Nest.Mary.fun(x),xlab="Number of Appropriate Nest Sites/Acre",ylab="Suitability",xlim=c(0,1000))

GD.Mary<-curve(pgamma(x,10,1.428571),xlim=c(0,20),xlab="Number of Native Grass Species",ylab="Suitability")

FD.Mary<-curve(pgamma(x,20,2),xlim=c(0,20),xlab="Number of Forb Species",ylab="Suitability")

STD.Mary<-curve(pgamma(x,2,.8),xlim=c(0,30),xlab="Number of Tree/Shrub Species",ylab="Suitability")

Edge.Mary.fun<-function(x){ifelse(x==1,y<-1,y<-0)}
binary<-matrix(c(0,1,0,1),2,2)
colnames(binary)<-c("Presence","Suitability")
binary<-as.data.frame(binary)
binary$name<-ifelse(binary$Presence==0,binary$name<-"Absent",binary$name<-"Present")
Edge.Mary<-plot(binary$Suitability~as.factor(binary$name),pch=16,xlab="Presence of Edge habitat",ylab="Suitability")

Layer.Mary.fun<-function(x){ifelse(x==1,y<-1,y<-0)}
Layer.Mary<-plot(binary$Suitability~as.factor(binary$name),pch=16,xlab="Presence of Multilayered Vegetation in Each Acre",ylab="Suitability")

Hopper.Mary<-STD.Mary<-curve(pgamma(x,2,.8),xlim=c(0,10),xlab="Number of Small Grasshoppers During Breeding ",ylab="Suitability",xaxt="n")