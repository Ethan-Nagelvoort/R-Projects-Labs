P = 200000 
r = 0.02817 / 12 
n = 30 * 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02567 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02317 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03067 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03317/ 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
P30<-200000
r30<-0.02817/12
k30 <- 360
MIR30 <- P30+r30*P30
MIP30 <- MIR30 - P30
MIP30
x30 <- (P30*(1+r30)^(k30)*r30)/((1+r30)^(k30)-1)
x30
monthlyloanprincipal30 <- (MIR30 - x30)
IRmat30 <- matrix(NA,nrow=k30,ncol=1)
for( i in 1:nrow(IRmat30) ){
  IRmat30[i,]<- MIP30
  P30 <- P30*(1+r30)-x30
  MIR30 <- P30+r30*P30
  MIP30 <- MIR30 - P30
}
totalprofit30<-colSums(IRmat30)
totalprofit30
plot.new()
plot(1:nrow(IRmat30),IRmat30[,1],type="l",main = "30-year term Monthly interest profit \nmade from a $200,000 Loan at 2.817% APR",
     xlab="Loan Term (in months)",ylab="Interest Profit ($)", lwd=0.8)
text(130,100,"Profit made from interest: $96,495.11",col="red",cex=1)
text(120,50,"Mortgage Total Cost: $296,495.95",col="blue",cex=1)
CFIRmat30<-matrix(NA,nrow=k30,ncol=1)
for( j in 1:nrow(CFIRmat30) ){
  CFIRmat30[j,1]<- sum(IRmat30[1:j,1])
}
plot(1:nrow(CFIRmat30),CFIRmat30[,1],type="l",main = "Cumulative Monthly Interest
Profit Generated from
 a $200,000 Loan at 2.817% APR for 30-Years",
xlab="Month",ylab="Cumulative Monthly Interest Profit",
 lwd=3)
text(240,40000, "Total Cumulative
Interest-Profit: $96,495.11",col = "red",cex = 1.2)
n = 20 * 12
P = 200000 
r = 0.02817 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02567 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02317 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03067 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03317/ 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
n = 15 * 12
P = 200000 
r = 0.02817 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02567 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02317 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03067 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03317/ 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
n = 10 * 12
P = 200000 
r = 0.02817 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02567 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.02317 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03067 / 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
r = 0.03317/ 12 
X = (-P * (1+r)^n * r)/(1 - (1 + r)^n) 
X
P30<-200000
r30<-0.02817/12
k30 <- 360
MIR30 <- P30+r30*P30
MIP30 <- MIR30 - P30
MIP30
x30 <- (P30*(1+r30)^(k30)*r30)/((1+r30)^(k30)-1)
x30
monthlyloanprincipal30 <- (MIR30 - x30)
IRmat30 <- matrix(NA,nrow=k30,ncol=1)
for( i in 1:nrow(IRmat30) ){
  IRmat30[i,]<- MIP30
  P30 <- P30*(1+r30)-x30
  MIR30 <- P30+r30*P30
  MIP30 <- MIR30 - P30
}
totalprofit30<-colSums(IRmat30)
totalprofit30
P20<-200000
r20<-0.02817/12
k20 <- 240
MIR20 <- P20+r20*P20
MIP20 <- MIR20 - P20
MIP20
x20 <- (P20*(1+r20)^(k20)*r20)/((1+r20)^(k20)-1)
x20
monthlyloanprincipal20 <- (MIR20 - x20)
IRmat20 <- matrix(NA,nrow=k20,ncol=1)
for( i in 1:nrow(IRmat20) ){
  IRmat20[i,]<- MIP20
  P20 <- P20*(1+r20)-x20
  MIR20 <- P20+r20*P20
  MIP20 <- MIR20 - P20
}
totalprofit20<-colSums(IRmat20)
totalprofit20
P15<-200000
r15<-0.02817/12
k15 <- 180
MIR15 <- P15+r15*P15
MIP15 <- MIR15 - P15
MIP15
x15 <- (P15*(1+r15)^(k15)*r15)/((1+r15)^(k15)-1)
x15
monthlyloanprincipal15 <- (MIR15 - x15)
IRmat15 <- matrix(NA,nrow=k15,ncol=1)
for( i in 1:nrow(IRmat15) ){
  IRmat15[i,]<- MIP15
  P15 <- P15*(1+r15)-x15
  MIR15 <- P15+r15*P15
  MIP15 <- MIR15 - P15
}
totalprofit15<-colSums(IRmat15)
totalprofit15
P10<-200000
r10<-0.02817/12
k10 <-120
MIR10 <- P10+r10*P10
MIP10 <- MIR10 - P10
MIP10
x10 <- (P10*(1+r10)^(k10)*r10)/((1+r10)^(k10)-1)
x10
monthlyloanprincipal10 <- (MIR10 - x10)
IRmat10 <- matrix(NA,nrow=k10,ncol=1)
for( i in 1:nrow(IRmat10) ){
  IRmat10[i,]<- MIP10
  P10 <- P10*(1+r10)-x10
  MIR10 <- P10+r10*P10
  MIP10 <- MIR10 - P10
}
totalprofit10<-colSums(IRmat10)
totalprofit10
totalprof <- c(totalprofit30, totalprofit20, totalprofit15, totalprofit10)
barplot(totalprof, main = "Total Interest Paid Per Term",
        xlab = "Term", ylab = "Total Interest Paid", 
        names.arg = c("30-year", "20-year", "15-year", "10-year"),
        col = "green")