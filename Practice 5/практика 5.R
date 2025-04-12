
library('MASS')
library('knitr')

data <- read.table(file = "data.csv", header = TRUE, sep = ",")
X<-na.omit(data[,c("insomia.1", "anoreksia.1", "SBP.1", "SV.1", "CO.1", "TPR.1")])
View(X)

pc<-princomp(scale(X))
pc$sdev/sum(pc$sdev)
pc$loading[,seq(3)]

# 67%


op<-par(mfrow=c(2,2))
plot(pc$scores[,1],pc$scores[,2],type="n",xlab="f1", ylab="f2")
text(pc$scores[,1],pc$scores[,2],row.names(X),cex=0.7)
plot(pc$scores[,1],pc$scores[,3],type="n", xlab="f1",ylab="f3")
text(pc$scores[,1],pc$scores[,3],row.names(X),cex=0.7)
plot(pc$scores[,2],pc$scores[,3],type="n",xlab="f2",ylab="f3")
text(pc$scores[,2],pc$scores[,3],row.names(X),cex=0.7)
par(op)


# первая компонента отвечает за SV (ударный объем), CO (сердце), TPR (энергетика) : низкие SV и CO, высокая TPR (сосуды)
# вторая связана с понижением давления (SBP), высокая бессоница (insomia)
# третья с высокие бессоница (insomia) и аноксия (anoreksia)



op<-par(mfrow=c(1,2))
plot(pc$scores[,1],pc$scores[,2],type="n",xlab="f1", ylab="f2")
for(i in c(0,1,2))
  lines(pc$scores[X$anoreksia.1==i,1],pc$scores[X$anoreksia.1==i,2], type="p",pch=20,col=i)
legend('bottomright',c("anoreksia=0","anoreksia=1","anoreksia=2"), cex=0.5,pch=20,col=seq(3))
plot(pc$scores[,1],pc$scores[,2],type="n",xlab="f1", ylab="f2")
for(i in c(0, 1,2))
  lines(pc$scores[ X$insomia.1==i,1],pc$scores[X$insomia.1==i,2],  type="p",pch=20,col=i)
legend('bottomright',c( "insomia=0","insomia=1", "insomia=2"),cex=0.5,pch=20,col=seq(3)+3)
  
  
  