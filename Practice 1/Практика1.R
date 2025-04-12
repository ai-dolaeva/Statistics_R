# Вариант 5. Гаммма распределение ??(???? = 1, ???? = 4) 
# 1.  Моделирование распределения
rate <- 1
shape <- 4
N <- 100

X <- rgamma(N, shape, rate);X


# 2. Построение гистограммы 
hist(X,freq=FALSE,ylim=c(0,0.3), main="Гистограмма плотности гамма распределения")

# 2. Нахождение математического ожидания
mu <-mean(X)
mu.<-shape/rate
c(mu.=mu.,mu=mu)
# 2. Дисперсии
sigma<-sd(X)
sigma2.<-shape/(rate^2)
c(sigma2.=sigma2.,sigma2=sigma^2)
# 2. Cтандартного отклонения
sigma.= sqrt(sigma2.)
c(sigma.=sigma.,sigma=sigma)
# 2. Ошибки среднего
c(err.=sigma./sqrt(N),err=sigma/sqrt(N))
# 2. Медианы
c(median.=median(X),median=mu)
# 2. Минимума
min(X)
# 2. Максимума
max(X)
# 2. Размаха
max(X)-min(X)
# 2. Квартилей и интервартильных размахов
q<-quantile(X, probs = seq(0, 1, 0.25));q
# 2. Ассимметирии
library(moments)
c(mean((X-mu.)^3),mean((X-mu)^3),m3<-moment(X, order = 3, central = TRUE))
c(m3/sigma.^(3/2),skewness(X))
# 2.Эксцесса
c(mean((X-mean(X))^4),m4<-moment(X, order = 4, central = TRUE))
m2<-moment(X, order = 2, central = TRUE)
c(kurtosis(X)-3,m4/m2^2-3)

# 3. Оценка параметров распределения 
# по методу моментов
f.<-function(x)dgamma(x,shape/rate)
f<-function(x)dgamma(x,mean(X))
curve(f,0,15,add=TRUE,col=2)
curve(f.,0,15,add=TRUE,col=3)
legend('topright',c("по выборке","по определению"),pch=20,col=c(2,3))

# по методу максимального правдоподобия
Func.prob.log <- function(x) -sum(dnorm(X, mean = x[1], log = TRUE))
res<-optim(c(mu.,sigma.),Func.prob.log)
mu..<-res$par[1];
f..<-function(x)dgamma(x,mu..)
hist(X,freq=FALSE, ylim=c(0,0.25),main="")
curve(f,0,15,add=TRUE,col=2)
curve(f.,0,15,add=TRUE,col=3)
curve(f..,0,15,add=TRUE,col=4,lty=2)
legend('topright',c("по выборке","по определению","оценка правдоподобия"),pch=20,col=c(2,3,4),lty=c(1,1,2))

# 4. Проверка согласия эмпирического и теоретического распределения по критерию хи-квадрат Пирсона
h<-hist(X,plot=FALSE)
# эмпирического распределения по критерию хи-квадрат Пирсона
n.i<-sapply(seq(length(h$breaks)-1)+1,function(i)
length(X[X<h$breaks[i] & X>=h$breaks[i-1]]))
# теоретического распределения по критерию хи-квадрат Пирсона
p.i<-sapply(seq(length(h$breaks)-1)+1,function(i)
pgamma(h$breaks[i],mu..)-pgamma(h$breaks[i-1],mu..))
sum(p.i)



p.i[1]<-pgamma(h$breaks[2],mu..)
p.i[length(p.i)]<-1-pgamma(h$breaks[length(h$breaks)-1],mu..)
sum(p.i)

#проверка условия n*p_i>5
tab<-cbind(h$counts,p.i*length(X));tab
t1<-cumsum(tab[,2]);T1<-min(which(t1>5));T1
t2<-cumsum(tab[seq(nrow(tab),1,-1),2]);t2;T2<-min(which(t1>5));T2<-nrow(tab)-T2;T2
Tab<-apply(tab,2,function(x) c(sum(x[seq(T1)]),x[c((T1+1):T2)],sum(x[c((T2+1):nrow(tab))])))
Tab
#статистика хт-квадрат
chi2<-sum(apply(Tab,1,function(x) (x[1]-x[2])^2/x[2]))
#доверительный уровень вероятности
p.value<-1-pchisq(chi2,nrow(Tab)-3);
print(paste("p.value",round(p.value,4),sep="="))


# 5. Моделирование выборки с нормальным законом распределения и применить критерий согласия Шапиро-Уилка

pp<-sapply(seq(200),function(x)shapiro.test(rnorm(100,4,12))$p.value)

#Критерий Колмогорова-Смирнова
plot(ecdf(pp))
lines(c(0,1),c(0,1),type="l",col=2)
title(sub=paste("p",format(ks.test(pp, "punif")$p.value,3,2),sep="=" ))
