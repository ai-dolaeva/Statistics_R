

# rabdru curwor prcod

DescriptiveStat<-function(X,group)
{
  mm<-tapply(X,group,function(x)mean(x,na.rm=TRUE));mm
  Sd<-tapply(X,group,function(x)sd(x,na.rm=TRUE));Sd
  nn<-tapply(X,group,function(x)length(na.omit(x)));nn
  err<-Sd/sqrt(nn);err
  list(mm=mm,Sd=Sd,nn=nn,err=err)
}
###########################
Fig<-function(x)
{
  hist(x,freq=FALSE)
  f1<-function(x)dnorm(x,mean(x,na.rm=TRUE),sd(x,na.rm=TRUE))
  curve(f1,min(x),max(x),col=2,add=TRUE)
  title(sub=paste("p.Shapiro",format(shapiro.test(x)$p.value,4,2),sep="="))
}
###########################
Sentence<-function(mm,err,nn,p.T)
{A1<-paste(paste(format(mm,digits=3,nsmall=2),format(err,digits=2,nsmall=2),sep="±"),
           nn,sep="/")
A1.<-paste("Средние в группах равны соответственно", paste(A1,collapse=", "))
A2.<-ifelse(p.T>0.05, "различие незначимо", "различие значимо" )
A3.<-paste("p",format(p.T,3,3),sep="=")
paste(c(A1.,A2.,A3.),collapse=", ")
}
 
#select variables 2 градации
table(addicts$curwor)


df<-data.frame(group = addicts$curwor,X= addicts$asi2_emp)
df$group
df$X

p.Sh<-with(df,tapply(X,group,function(x)shapiro.test(x)$p.value));p.Sh


boxplot(X~group,df)
















