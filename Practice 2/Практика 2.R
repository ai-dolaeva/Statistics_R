# ������ �������������� ��������� ����������
# ������� (�) ��������� � ��������� (depressed.mood.1, hyperemia.1), �������� � ���������� (weakness.1, insomia.1)

# �������������� ���������:

Entropy<-function(x){
  p<-x/sum(x);
  pp<-p[p!=0]
  -sum(p*log(p,2))
}

Uncertain<-function(tab){
  Hxy<-Entropy(as.vector(tab));Hxy
  Hx<-Entropy(rowSums(tab));Hx
  Hy<-Entropy(colSums(tab));Hy
  I<-Hx+Hy-Hxy;I
  c(I/Hx*100, I/Hy*100, 2*I/(Hx+Hy)*100)
}

# 1. ��������� ��� ������� ������������� �� ���� ����� ������������ ���������.
# (depressed.mood.1, hyperemia.1)
X1<-data$depressed.mood.1;X1
Y1<-data$hyperemia.1;Y1
Y1<-ifelse(Y1<2,1,2)
tab1<-table(depressed=X1, hyperemia=Y1);tab1

# (weakness.1, insomia.1)
X2<-data$weakness.1;X2
X2<-ifelse(X2<2,1,2)
Y2<-data$insomia.1;Y2
Y2<-ifelse(Y2<2,1,2)
tab2<-table(weakness=X2, insomia=Y2);tab2

# 2. ��������� �������� ����������� � ������� �� ���������� �� �������� �� �������� ��-������� � �� ������� �������� ������.
# (depressed.mood.1, hyperemia.1)
tab1[,2]/rowSums(tab1)
fisher.test(tab1)$p.value # ������ �������� ������
chisq.test(tab1) # �������� �������

tab1

rowSums(tab1)

tab1[,2]


# (weakness.1, insomia.1)
tab2[,2]/rowSums(tab2)
fisher.test(tab2)$p.value # ������ �������� ������
chisq.test(tab2) # �������� �������

# 3. ��������� ������������ ����������������.
# (depressed.mood.1, hyperemia.1)
Uncertain(tab1)

# (weakness.1, insomia.1)
Uncertain(tab2)

# 4. ��������� �������� ���-������ � ������� ��� �������� �������� ��������� �� �������.
library(nonpar)
library(plyr)
Name<-"depressed.mood"
A<-sapply(colnames(data),function(x)substr(x,1,nchar(Name)))
data.<-apply(data[,names(A[A==Name])],2,function(x)ifelse(x<2,1,2))
tab<-table(data.[,1],data.[,2])
tab
mcnemar.test(data.[,1],data.[,2]) # �������� ���-������
data..<-na.omit(data.[,seq(3)])-1
colnames(data..)
cochrans.q(data.., alpha = 0.05) #��������  �������

Name<-"hyperemia"
A<-sapply(colnames(data),function(x)substr(x,1,nchar(Name)))
data.<-apply(data[,names(A[A==Name])],2,function(x)ifelse(x<1,0,1))
tab<-table(data.[,1],data.[,2])
tab
mcnemar.test(data.[,1],data.[,2]) # �������� ���-������
data..<-na.omit(data.[,seq(3)])-1
colnames(data..)
cochrans.q(data.., alpha = 0.05) #��������  �������

Name<-"weakness"
A<-sapply(colnames(data),function(x)substr(x,1,nchar(Name)))
data.<-apply(data[,names(A[A==Name])],2,function(x)ifelse(x<1,0,1))
tab<-table(data.[,1],data.[,2])
tab
mcnemar.test(data.[,1],data.[,2]) # �������� ���-������
data..<-na.omit(data.[,seq(3)])-1
colnames(data..)
cochrans.q(data.., alpha = 0.05) #��������  �������

Name<-"weakness"
A<-sapply(colnames(data),function(x)substr(x,1,nchar(Name)))
data.<-apply(data[,names(A[A==Name])],2,function(x)ifelse(x<2,1,2))
tab<-table(data.[,1],data.[,2])
tab
mcnemar.test(data.[,1],data.[,2]) # �������� ���-������
data..<-na.omit(data.[,seq(3)])-1
colnames(data..)
cochrans.q(data.., alpha = 0.05) #��������  �������

Name<-"insomia"
A<-sapply(colnames(data),function(x)substr(x,1,nchar(Name)))
data[,names(A[A==Name])]
data.<-apply(data[,names(A[A==Name])],2,function(x)ifelse(x<1,0,1))
tab<-table(data.[,1],data.[,2])
tab
mcnemar.test(data.[,1],data.[,2]) # �������� ���-������
data..<-na.omit(data.[,seq(3)])-1
colnames(data..)
cochrans.q(data.., alpha = 0.05) #��������  �������

Name<-"insomia"
A<-sapply(colnames(data),function(x)substr(x,1,nchar(Name)))
data.<-apply(data[,names(A[A==Name])],2,function(x)ifelse(x<2,1,2))
tab<-table(data.[,1],data.[,2])
tab
mcnemar.test(data.[,1],data.[,2]) # �������� ���-������
data..<-na.omit(data.[,seq(3)])-1
colnames(data..)
cochrans.q(data.., alpha = 0.05) #��������  �������





data <- read.table(file = "data.csv", header = TRUE, sep = ",")
data


