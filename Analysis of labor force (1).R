date<-read.table("tari.txt",header=TRUE)
View(date)
#PART 1
#2.I. Descriptive Statistics

install.packages("mnormt")
library(mnormt)
describe(date)
summary(date)
install.packages("moments")
library(moments)
c2=cbind(date[,3:17])
View(c2)
skewness(c2)
kurtosis(c2)
install.packages("sp")
library(sp)
install.packages("raster")
library(raster)
apply(c2,2,cv)
cv(c2$EMP_VULN_MA)
boxplot(c2$EMP_VULN_MA,col="orange")
boxplot(c2$EMP_VULN_FE,col="PINK")
boxplot(c2$SL_EMP_WORK_MA,col="BLUE")
boxplot(c2$SL_EMP_WORK_FE,col="red")
boxplot(c2$UEM_TOTL_MA,col="YELLOW")
boxplot(c2$UEM_TOTL_FE,col="PURPLE")
boxplot(c2$UEM_1524_MA,col="GRAY")
boxplot(c2$UEM_1524_FE,col="RED")
boxplot(c2$SRV_EMPL_MA,col="BLUE")
boxplot(c2$SRV_EMPL_FE,col="PINK")
boxplot(c2$IND_EMPL_MA,col="YELLOW")
boxplot(c2$IND_EMPL_FE,col="BLUE")
boxplot(c2$AGR_EMPL_MA,col="ORANGE")
boxplot(c2$AGR_EMPL_FE,col="PURPLE")

#3. Graphs,histograms

with(c2,plot(SRV_EMPL_FE,EMP_VULN_FE,main="Vulnerabilitatea femeilor la angajare"))
#cu cat creste nr de femei angajate in domeniul serviciilor, cu atat scade vulnerabilitatea
hist(c2$EMP_VULN_FE,breaks=12,col="pink",main="Vulnerabilitatea la angajare a femeilor")
hist(c2$EMP_VULN_MA,breaks=12,col="blue",main="Vulnerabilitatea la angajare a barbatilor")
hist(c2$SL_EMP_WORK_FE,breaks=12,col="orange",main="Femei salariate")
hist(c2$GDP_EMP,breaks=12,col="yellow",main="GDP pentru angajati")
hist(c2$UEM_TOTL_MA,breaks=12,col="green",main="Barbatii neangajati")
hist(c2$UEM_TOTL_FE,breaks=12,col="purple",main="Femei neangajate")
hist(c2$UEM_1524_MA,breaks=12,col="RED",main="Tineri neangajate")
hist(c2$UEM_1524_FE,breaks=12,col="yellow",main="Tinere neangajate")
hist(c2$SRV_EMPL_MA,breaks=12,col="purple",main="Barbati angajati in sect. serviciilor")
hist(c2$SRV_EMPL_FE,breaks=12,col="pink",main="Femei angajate in sectorul serviciilor")
hist(c2$IND_EMPL_MA,breaks=12,col="gray",main="Barbati angajati in sect. industrial")
hist(c2$IND_EMPL_FE,breaks=12,col="red",main="Femei angajate in sect. industrial")
hist(c2$AGR_EMPL_MA,breaks=12,col="yellow",main="Barbatii care lucreaza in agricultura")
hist(c2$AGR_EMPL_FE,breaks=12,col="green",main="Femei care lucreaza in agricultura")

#plot cu abline
plot(date$EMP_VULN_FE,date$SRV_EMPL_FE,col="red",main="Rep.dep.")
abline(lm(date$EMP_VULN_FE~date$SRV_EMPL_FE))

#plot cu ggplot
install.packages("ggplot2")
library(ggplot2)
ggplot(date,aes(x=SRV_EMPL_FE,y=EMP_VULN_FE))+geom_point(shape=16,size=4,col="red")+geom_text(label=date$Country_Code,vjust=0,hjust=0,size=4)
#Tarile precum BDI,COD,BGR etc (adica cele)din dreapta  jos sunt cele in care nr de femei care lucreaza este foarte mare, iar vulnerabilitatea angajarii acestora este extrem de redusa. se poate observa ca predomina, din tarile alese cele in care femeile din sectorul serviciilor muncesc mai mult, iar acest fapt, o predominanta a populatiei in sectorul serviciilor indica in general un grad de dezvoltare economica mai diciat
#Vulnerabilittea pt aceste taridezvoltate este mai mica->mai stabile din acest punct de vedere

#Performance Analytics

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
c2=cbind(date[,3:17])
chart.Correlation(c2,histogram=TRUE,pch=19)
#pe diag principala sunt histograme cu densit de prob. sub diag princ sunt ploturi intre variabile. liniile rosii sunt dependente una de cealalta.In dreapta sus sunt coef de corelatii (scris mare corel mare scris mic corel mica)***-corel cea mai mare etc..

#4. Correlation matrix and covariance matrix

install.packages("Hmisc")
library(Hmisc)
M=rcorr(as.matrix(c2))
M
install.packages("corrplot")
library(corrplot)
corrplot(M$r,type="upper",p.mat=M$P,sig.level=0.01,insig="blank")
#reprezentarea vizuala a matricei de corelatie

cor(c2)
cov(c2)

#5. Variables grouping

attach(c2)
c2$MC_categorii[GDP_EMP>10000]<-"GDP mai mare de 10000"
c2$MC_categorii[GDP_EMP>5000&GDP_EMP<=10000]<-"GDP intre (5000;10000]"
c2$MC_categorii[GDP_EMP<=5000]<-"GDP mai mic de 5000"
detach(c2)
table(c2$MC_categorii)

#6. Data standardisation

date_std=scale(date[,3:17],scale=TRUE)
apply(date_std,2,sd)
apply(date_std,2,mean)
#media are acm valori foarte mici
cor(date_std)
cov(date_std)
#==================================================================
#II. Principal components analysis

#1. Creation of a PCA model using correlations

pca=princomp(date_std,cor=TRUE)
pca

#2.Proper value extraction and distribution

sdev=pca$sdev
valp=sdev*sdev      #prop 1
procentA=valp*100/15 #p=15 indicators

#procentA cant prel de fiecare comp princ in mod individual din toate var originale
#prima componenta principala preia 54,69 din informatie

procentA
procentC=cumsum(procentA)
procentC
V=zapsmall(data.frame(valp,procentA,procentC))
V
#3. Principal Components Analysis criteria

#Conform criteriului lui Kaiser se observa faptul ca valorile proprii care sunt mai mari decat 1 sunt 3 la numar, corespunzatoare componentei 1,2,3, fapt ce inseamna ca sunt 3 componente principale
#Conform criteriului procentului de acoperire vom pastra in analiza cele 3 componente principale care impreuna au un procent cumulat de 85.606% date.

#Screeplot

scree_plot=prcomp(date_std)
plot(scree_plot,type="l",main="scree_plot")
 #se traseaza linia intre 3 si 4

a=zapsmall(pca$loadings)
write.table(zapsmall(pca$loadings),"Vectori_proprii.txt")
e=eigen(cov(date_std))
e$values#=valp=pca$sdev*pca$sdev #valorile proprii
e$vectors#=loadings #vectorii proprii

#4. Components retention
c=zapsmall(pca$score[,1:3])
c2=cbind(c)
c2 #Am retinut primii vectori proprii pentru cele 3 componente principale

#SCORS
#z1_AGQR=0,402=0.018*(-0.073)-0.051*(-0.757)-0.059*(-0.788)-0.294*(-0.366)-0.214*(-0.363)+0.456*(0.289)-0.416*(-0.685)-0.463*(-0.181)+0.368*(-0.444)+0.383(-0.538)
z1=pca$loadings[,1]%*%date_std[1,]
z1 #4.358837
z2=pca$loadings[,2]%*%date_std[1,]
z2 


cor(c[,1:3])
chart.Correlation(c,histogram=TRUE,pch=19)

 #5. Factorial correlation calculation and Factors Matrix
corFact = zapsmall(cor(date_std,c[,1:3]))
corFact

corrplot(corFact,method="square")
#z1 este corelata puternic cu EMP_VULN_MA,EMP_VULN_FE,SL_EMP_WORK_MA,SL_EMP_WORK_FE,SRV_EMPL_MA,SRV_EMPL_FE,IND_EMPL_MA,AGR_EMPL_MA,AGR_EMPL_FE. Aceasta componenta se va numi Componenta vulnerabilitatii ocuparii fortei de munca
#z2 este corelata puteric cu UEM_TOTL_MA,UEM_TOTL_FE,UEM_1524_MA,UEM_1524_FE. Aceasta componenta se va numi Componenta populatiei somere
#z3 este corelata cu GDP_EMP, IND_EMPL_FE. Aceasta componenta se va numi Componenta contributiei femeilor din sect ind la PIB
#AM ALES SI ACEASTA COMPONENTA, DEOARECE IN MOD NEASTEPTAT NR FEMEILOR CARE LUCREAZA IN INDUSTRIE NU SE CORELEAZA CU componenta 1, ci cu componenta 3, impreuna cu PIB. Acest aspect sugereaza ca acest secotor de activitate, pentru femei nu este foarte dezvoltat fata de cel al barbailor, sau fata de celelalte 3 sectoare, insa contribuie in mod activ la PIB-ul per angajati

#6. General form of principal components 9Mathematical calculation)
#z1=0.944*EMP_VULN_MA+0.971*EMP_VULN_FE-0.938*SL_EMP_WORK_MA-0.969*SL_EMP_WORK_FE+0.241*GDP_EMP-0.314*UEM_TOTL_MA-0.317*UEM_TOTL_FE-0.378*UEM_1524_MA-0.388*UEM_1524_FE-0.876*SRV_EMPL_MA-0.940*SRV_EMPL_FE-0.742*IND_EMPL_MA-0.022*IND_EMPL_FE-0.944*AGR_EMPL_MA+0.947*AGR_EMPL_FE
#z2=0.163*EMP_VULN_MA+0.140*EMP_VULN_FE-0.171*SL_EMP_WORK_MA-0.142*SL_EMP_WORK_FE-0.054*GDP_EMP+0.868*UEM_TOTL_MA+0.893*UEM_TOTL_FE+0.857*UEM_1524_MA+0.859*UEM_1524_FE-0.055*SRV_EMPL_MA-0.084*SRV_EMPL_FE-0.304*IND_EMPL_MA-0.185*IND_EMPL_FE-0.175*AGR_EMPL_MA+0.121*AGR_EMPL_FE
#z2=0.101*EMP_VULN_MA+0.050*EMP_VULN_FE-0.076*SL_EMP_WORK_MA-0.037*SL_EMP_WORK_FE-0.485*GDP_EMP+0.183*UEM_TOTL_MA-0.051*UEM_TOTL_FE+0.147*UEM_1524_MA-0.044*UEM_1524_FE-0.199*SRV_EMPL_MA-0.143*SRV_EMPL_FE-0.384*IND_EMPL_MA+0.895*IND_EMPL_FE-0.032*AGR_EMPL_MA-0.031*AGR_EMPL_FE


#7.Principal scores
#Scorurile principale 1=
0.944*2.0805+0.971*1.7237+0.938*1.9655+0.969*1.6718-0.241*1.0533+0.314*1.4655+0.317*1.1743+0.378*1.5469+0.388*1.3015+0.876*0.0096+0.940*1.5791+0.742*0.8-0.022*1.0751-0.944*0.3652+0.947*1.3721
#11.78694

#8. Data reprezentation in principal plan
c3=data.frame(c2,date[,2])
View(c3)

dev.new()
plot(c3[,1],c3[,2],main="Plot componente Z1 si Z2",xlab="Z1",ylab="Z2")
text(c3[,1],c3[,2],labels=c3[,4],col="red",pos=3,cex=0.7)

#unde se aglomereaza acel nor de puncte si int ce este in afara acelui nor de puncte.
#Norul de puncte se aglomereaza intre valorile z1=-2 si z2=aprox -2. Sugereaza faptul ca pentru ceste tari BLZ,COD,AND,BEL,ARG, BMD vulnerabilitatea este mica, deci si somajul este mic-> rata vulnerabilitatii ocuparii fortei de munca este redusa, populatia isi gaseste locuri de munca si majoritatea lucreaa, de aceea si somajul ia valori f mici, nu sunt foarte multe grupuri vulnerabile.
#un nor mai mic de puncte este CAN, BHS,ATG,ABW,BRN etc. Vulnerabilitatea este mai mare, are valoarea 4, adica in randul populatiei, sunt mai multe persoane predipuse sa nu isi gaseasca un loc de munca, grupuri vulnerabile, insa cu toate acestea rata somajului este mica->dezvoltare economica propice
#Celelalte tari din afara norilor de puncte prceum AFG,BWA,CMR,->vulnerab mare , somaj mare
#BIH-vulnerabilitate mica, somaj mare->dezvoltare a fortei de munca f slabaa. Vuln este aproape de -2, insa somajul este peste 4.


library(ggplot2)
windows()
ggplot(c3,aes(x=c3[,1],y=c3[,2]))+ geom_point(shape=16,size=4,col="red")+geom_text(label=c3[,4],vjust=0,hjust=0,size=4)

#9. Plot for individuals and correlations
dev.new()
df=data.frame(c2)
rownames(df)=date[,1]
biplot(df[,1:2], pca$loadings[,1:2], cex=c(0.7,0.8),xlab="z1",ylab="z2")
# Pe axa ox e z1 si pe axa Oy este z2
#z1 este corelat negativ cu IND_EMPL_MA,SERv_EMP_MA,SERV_EMP_FE,SL_EMP_WORK_MA,SL_EMP_WORK_FE,IND_EMPL_MA si corelat pozitiv cu AGR_EMP_MA,AGR_EMP_FE
#z2 este corelat negativ cu IND_EMPL_FE, GDP_EMP si corelat poziti cu UNEM_TOTAL_FE, UNEM_1524_MA si UNEM_TOT_MA

#Correlation circle
dev.new()
cerc = seq(0,2*pi,length=100)
plot(cos(cerc),sin(cerc),type="l",col="blue",xlab="Z1",ylab="Z2")
text(corFact[,1],corFact[,2],rownames(corFact),col="red",cex=0.7)

#10. Other graphs- Output fct. PCA

install.packages("factoextra")
library(factoextra)
windows()
fviz_pca_ind(pca,col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_pca_var(pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )    
#graficul cu coef de corel colorati in fct de conttrib lor la determinarea comp princ(z)
#cu cat o variablia se apropie mai mult de circum cercului, cu atat este mai important in interpret comp principale
date_std
install.packages("FactoMineR")
library(FactoMineR)
cp1=PCA(date_std)
summary(cp1)

print(pca$loadings[,1:3])
corFact
round(date_std[1:3,],2)

 #PCA output
summary(pca)

#III.Factor analysis

x=rnorm(100,2,0.3)
y=rnorm(100,3,0.5)#medie
z=rnorm(100,3.4,0.7)

#A doua met. pt ind KMO?????????????????????????????????????????????
(cor(x,y)-(cor(x,z)*cor(y,z)))/sqrt((1-cor(x,z)^2)*(1-cor(y,z)^2))
#-0.95

#KMO indicator

R=cor(date_std)
invR=solve(R)
A=matrix(1,nrow(invR), ncol(invR))#gen o matr A cu 1 peste tot cu nr de randuri si col ca inversa matr de corel
for(i in 1:nrow(invR)){
  for(j in (i+1):ncol(invR)){
    A[i,j]=invR[i,j]/sqrt(invR[i,i]*invR[j,j])#deasupra diag
    A[j,i]= A[i,j]
  }
} #construieste coef de corelatie partiali la toate variabilele mele
colnames(A)=colnames(date_std)
rownames(A)=colnames(date_std)
A
kmo.num=sum(R^2)-sum(diag(R^2))
kmo.denom=kmo.num+(sum(A^2)-sum(diag(A^2)))
kmo=kmo.num/kmo.denom
kmo

#0.6734678 ->factorabilitate mediocra,un indicator mediocru

#Testul de sfericitate Bartlett

#ip H0->variabilele sunt ortogonale, ceea ce inseamna ca nu este justificata construirea de factor comun
#ip. H1(alternativa) ->exista cel putin un factor comun.

bart=function(date)
{
  R=cor(date)
  p=ncol(date)
  n=nrow(date)
  chi2=-((n-1)-((2*p)+5)/6)*log(det(R))
  df=(p*(p-1)/2)
  crit=qchisq(.95,df)
  p=qchisq(chi2,df,lower.tail=F)#pvalue
  cat("Bartlett's test of sphericity:x2(",df,")=",chi2,", p=", round(p,6),sep="")
}

bart(date_std)
qchisq(0.05,67.5,lower.tail=F) #67.5=9/2*15 var fi statistic
#87.67925
#fi caculat 2477.283>87.67925=>atunci respingem H0 si acceptam H1->ANALIZA FACTORIALA ARE SENS

# FUNCTIA PCA
install.packages("FactoMineR")
library(FactoMineR)
cp1=PCA(date_std)
summary(cp1)

install.packages("rela")
library(rela)
res=paf(as.matrix(date_std))
res$KMO
res$Bartlett #???

library(GPArotation)

factor1=factanal(date_std,factors=2,rotation="varimax")
print(factor1,digits=2,cutoff=.2,sort=TRUE) #nu e bun

factor2=factanal(date_std,factors=7,rotation="varimax")
print(factor2,digits=2,cutoff=.2,sort=TRUE) #nu e bun

factor3=factanal(date_std,factors=9,rotation="varimax")
print(factor3,digits=2,cutoff=.2,sort=TRUE) #nu e bun

fit.4<-factanal (date_std, factors = 3, rotation ="varimax")
print(fit.4, digits =2, cutoff=.2, sort = TRUE)

fit.5<-factanal (date_std, factors = 11, rotation ="varimax")
print(fit.4, digits =2, cutoff=.2, sort = TRUE)

#=========================================================================================================

date_coresp<-read.table("fem.txt", header=TRUE)
View(date_coresp)
dd=date_coresp[,2:4]
rownames(dd)=date_coresp[,1]
dd
View(dd)
chisq <- chisq.test(dd)
chisq
#x-squared=4060, df=246
#test al independentei, arata daca exista o leg intre linii si coloane
#daca exista, se aplica analiza corespondentelor
#statistica=4060
#df=grade de libertate=(linii-1)*(col-1)=41*6=246

install.packages("FactoMineR")
library(FactoMineR)
res.ca <- CA(dd, graph = FALSE)
res.ca
print(res.ca)
summary(res.ca, nb.dec = 2, ncp = 2)
windows()
plot(res.ca)
#interp in fct de cum se grupeaza liniile in jurul coloanei ex:sotia laundry main meal breakfeast dinner


eig <- get_eigenvalue(res.ca)
eig
#prima col-val proprie--a doua-procentul de varianta --3 col-cumulat

install.packages("factoextra")
library(factoextra)

trace <- sum(eig[,1])#suma val proprii, adica suma landelor
trace
#0.43366 #suma valorilor proprii, adica suma lamdelor
df <- (nrow(date_coresp) - 1) * (ncol(housetasks) - 1) #df=123
df
chi2 <- trace*sum(as.matrix(dd)) #chi2=4063.2
chi2
pval <- pchisq(chi2, df = df, lower.tail = FALSE) #pval=0
pval #0

#descompunerea primului test
eigenvalues <- get_eigenvalue(res.ca)
head(round(eigenvalues, 2))
windows()
fviz_screeplot(res.ca) # lamda reprez grafic si se interpreteaza ca la ACP
row <- get_ca_row(res.ca)
head(row$coord)
head(row$contrib)
#pe linii
col <- get_ca_col(res.ca)
head(col$coord)
head(col$contrib)
#pe coloane
library(corrplot)
windows()
corrplot(row$contrib, is.corr=FALSE)
#contributie fiecarei linii la dimensiuni
fviz_contrib(res.ca, choice = "row", axes = 1:2)
#contributia liniei la dimensiunea 1 si 2 impreuna

fviz_contrib(res.ca, choice = "col", axes = 1:2)
fviz_ca_biplot(res.ca)+ theme_minimal()   #rezultat similar comenzii plot(res.ca)

hist(date$UEM_TOTL_MA,freq=F, xlab="ROE", main="Histograma someri")
lines(density(c2),col="red",lwd=2)

#Part 2

#Cluster Analysis

date<-read.table("tari.txt", header = T, row.names = 1)
View(date)
date_std=scale(date[,2:16],scale=TRUE)
View(date_std)
pca = princomp(date_std,cor=TRUE)
scoruri = pca$scores[,1:3]
colnames(scoruri)<-c("Z1","Z2","Z3")
rownames(scoruri)=rownames(date)
scoruri

#1. Hierarchical classification a
 
#2 metode Ward+una la alegere

d_std = dist(as.matrix(date_std), method="euclidian")#matricea distantelor cu distanta
euclidiana
#method = ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
d_std #matricea distantelor
d_std[1] #4.800501
#Ce interpretare are asta???
#Distanta euclidiana dintre Afganostan si Aruba
date_std[1:2,]
d_euclid_manual=sqrt((date_std[1,1]-date_std[2,1])^2+
(date_std[1,2]-date_std[2,2])^2+
(date_std[1,3]-date_std[2,3])^2+
(date_std[1,4]-date_std[2,4])^2+
(date_std[1,5]-date_std[2,5])^2+
(date_std[1,6]-date_std[2,6])^2+
(date_std[1,7]-date_std[2,7])^2+
(date_std[1,8]-date_std[2,8])^2+
(date_std[1,9]-date_std[2,9])^2+
(date_std[1,10]-date_std[2,10])^2)
d_euclid_manual
clust_std = hclust(d_std, method = "ward.D2")
clust_std
summary(clust_std)###########
cbind(clust_std$merge,clust_std$height)#############

#Peprimele 2 coloane, cu - sunt observatiile, iar cu + clasele.
#A treia coloana reprezinta distanta de agregare= Pasi de agregare/Etape de clasificare
#Componenta 26 si 42 s-au unit intr-o clasa la distanta de agregare de 0.644
#Componenta 6 s-a unit cu compoenta 20 la distanta de agregare de 0.90 la pasul 2
#La pasul 8, clasele 1 si 3 s-au unit la distanta de agregare de 1.67
#La pasul 26, componenta 18 s-a unit la clasa 9 cu distanta euclidian de 3.161

dev.new()
plot(clust_std,labels=rownames(date_std))
rect.hclust(clust_std,k=2,border=2:3)
#dendrograma = arborele clasificarii
#Ajuta sa stabilesti cate clase alegem sa analizam
#te uiti de sus in jos si unde dist de agregare e mai mare, acolo tragi o linie is cate puncte de
intersectie sunt, atatea clustere->3 clustere
clust_std$height[41]-clust_std$height[40] #n=42
clust_std$height[40]-clust_std$height[39]
clust_std$height[39]-clust_std$height[38]
clust_std$height[38]-clust_std$height[37]
clust_std$height[37]-clust_std$height[36]
clust_std$height[36]-clust_std$height[35]
install.packages("NbClust")
library(NbClust)
res<-NbClust(date_std, distance = "euclidean", min.nc=2, max.nc=5,
method = "ward.D2", index = "all")
res$All.index
res$Best.nc
################
#KL zice ca un numar de clustere bun eset 2, iar valoarea indicatorului e de 3.7337
install.packages("cluster")
library(cluster)
si4_std <- silhouette(cutree(clust_std, k = 2), d_std)
dev.new()
plot(si4_std, cex.names = 0.5)
si4_std
#3;39 sunt gresit introduse in clustere, pt ca se afla in partea stanga, au valoarea mai mica decat
0

#Hierarchical classification
#2.1.WARD Method with princial components

d_z = dist(as.matrix(scoruri), method="euclidian")
#method = ("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
d_z #matricea distantelor pentru componentele principale
d_z[1]
scoruri[1:2,]
d_euclid_manual2=sqrt((scoruri[1,1]-scoruri[2,1])^2+
(scoruri[1,2]-scoruri[2,2])^2+
(scoruri[1,3]-scoruri[2,3])^2)
d_euclid_manual2
clust_z = hclust(d_z, method = "ward.D2")
#method poate fi: "ward.D2", "single", "complete", "average", "centroid"
clust_z
summary(clust_z)
cbind(clust_z$merge,clust_z$height) #etapele clasificarii si distanta de agregare
dev.new()
plot(clust_z,labels=rownames(scoruri))
rect.hclust(clust_z,k=2,border=2:3)
clust_z$height[41]-clust_z$height[40] #n=42 -> cea mai mare diferenta => 2 clustere
clust_z$height[40]-clust_z$height[39]
clust_z$height[39]-clust_z$height[38]
clust_z$height[38]-clust_z$height[37]
clust_z$height[37]-clust_z$height[36]
clust_z$height[36]-clust_z$height[35]
res<-NbClust(scoruri, distance = "euclidean", min.nc=2, max.nc=5,
method = "ward.D2", index = "all")
res$All.index
res$Best.nc
dev.new()
si4_z <- silhouette(cutree(clust_z, k = 2), d_z)
plot(si4_z, cex.names =0.5)
#nu sunt valori in partea stanga-> toate valorile sunt cuprinse in 2 clustere si nu ies
si4_z
#Concluzie: K=2 clase atat pentru date_std, cat si pentru CP

# Second Hierarchical classification

#unweighted arithmetic average clustering
#For principal components
#=====================================================
#====================================================
clust_2 = hclust(d_z, method = "average")
clust_2
summary(clust_2)
cbind(clust_2$merge,clust_2$height)
dev.new()
plot(clust_2,labels=rownames(scoruri))
rect.hclust(clust_2,k=4,border=2:3)
clust_2$height[41]-clust_2$height[40] #n=42
clust_2$height[40]-clust_2$height[39]
clust_2$height[39]-clust_2$height[38]
clust_2$height[38]-clust_2$height[37]
clust_2$height[37]-clust_2$height[36]
clust_2$height[36]-clust_2$height[35]
res2<-NbClust(scoruri, distance = "euclidean", min.nc=2, max.nc=5,
method = "average", index = "all")
res2$All.index
res2$Best.nc
dev.new()
si4_z2 <- silhouette(cutree(clust_2, k = 2), d_z)
plot(si4_z2, cex.names =0.5)
si4_z2
#Pentru date standardizate
clust_3= hclust(d_std, method = "average")
clust_3
summary(clust_3)
cbind(clust_3$merge,clust_3$height)
dev.new()
plot(clust_3,labels=rownames(date_std)) #dendrograma = arborele clasificarii
#Ajuta sa stabilesti cate clase alegem sa analizam
#te uiti de sus in jos si unde dist de agregare e mai mare, acolo tragi o linie is cate puncte de
intersectie sunt, atatea clustere->3 clustere
clust_3$height[41]-clust_3$height[40] #n=42 -> cea mai mare diferenta => 2 clustere
clust_3$height[40]-clust_3$height[39]
clust_3$height[39]-clust_3$height[38]
clust_3$height[38]-clust_3$height[37]
clust_3$height[37]-clust_3$height[36]
clust_3$height[36]-clust_3$height[35]
res<-NbClust(date_std, distance = "euclidean", min.nc=2, max.nc=5,method = "average",
index = "all")
res$All.index
res$Best.nc
si4_std2<- silhouette(cutree(clust_3, k = 2), d_std)
dev.new()
plot(si4_std2, cex.names = 0.5)
si4_std2
# CONCLUZIA ??? Cate clustere??
#====================================================
#====================================================

#K-MEANS Algorithm

#Standardisation of variables

k_std=kmeans(date_std,2) #K= clase
k_std
clasa_std=k_std$cluster
c_std=cbind(clasa_std,round(date_std,2))
c_std
m_std=data.frame(c_std)
View(m_std)
dev.new()
plot(m_std[,2], m_std[,11], col=c("red","blue")
[m_std$clasa_std], main="Reprezentarea claselor", xlab=colnames(m_std[2]),
ylab=colnames(m_std[11]))
text(m_std[,2],m_std[,11],labels=rownames(m_std),col="magenta",pos=3,cex=0.7)
spat_std=k_std$totss
spaw_std=k_std$tot.withinss
spaw_std
spab_std=k_std$betweenss
spab_std
r_cls_std=spab_std/spaw_std
variab_std=cbind(spat_std,spaw_std,spab_std,r_cls_std) #descompunerea variabilitatii
variab_std

#Principal components

k_z=kmeans(scoruri,2) #K=4 clase
k_z
clasa_z=k_z$cluster
c_z=cbind(clasa_z,round(scoruri,2))
c_z
m_z=data.frame(c_z)
View(m_z)
dev.new()
plot(m_z[,2], m_z[,3], col=c("red","blue","green","black","magenta","yellow")
[m_z$clasa_z], main="Reprezentarea claselor", xlab=colnames(m_z[2]),
ylab=colnames(m_z[3]))
text(m_z[,2],m_z[,3],labels=rownames(m_z),col="magenta",pos=3,cex=0.7)
spat_z=k_z$totss
spaw_z=k_z$tot.withinss
spab_z=k_z$betweenss
r_cls_z=spab_z/spaw_z
variab_z=cbind(spat_z,spaw_z,spab_z,r_cls_z) #descompunerea variabilitatii
variab_z
#aici criteriul este mai satisfacator pt ca r de aici este mai mare decat celalalt
#aici obt un raport r mai bun pt ca nu sunt corelate 2 cate 2 variabilele
#cu cat r este mai mare, cu atat puterea de discriminare este mai mare
#=================================================================
===========================================================
# Discriminant analysis

date=read.table("tari.txt", header = T, row.names = 1)
date_std <- scale(date[2:16],scale=TRUE)
date_std
pca = princomp(date_std,cor=TRUE)
scoruri = pca$scores[,1:3]
colnames(scoruri)<-c("Z1","Z2","Z3")
rownames(scoruri)=rownames(date)
outlieri=read.table("outlierii.txt",header=T,row.names=1) #outlierii eliminati la inceput
out=cbind(outlieri[,2:16])
outlieri_std <- scale(out,scale=TRUE)
View(outlieri_std)
#Date_standardizate
k_std=kmeans(date_std,2)
clase_std=k_std$cluster
train_std=cbind(date_std,clase_std)
test_std=outlieri_std[,1:15]
test_std
View(train_std)
#Z
k_z=kmeans(scoruri,2)
cls=k_z$cluster
#clasa_z=k_z$cluster
train_z2=cbind(scoruri,cls) #setul de invatare
df=data.frame(train_z2)
attach(df)
df$clase_z[df$cls ==1] <- "cls1"
df$clase_z[df$cls ==2] <- "cls2"
#df$clase_z[df$cls ==3] <- "cls3"
#df$clase_z[df$cls ==4] <- "cls4"
table(df$clase_z)
train_z=cbind(df[,1:3],df[,5])
View(train_z)
vect_pr=pca$loadings[,1:3]
colnames(vect_pr)<-c("A1","A2","A3")
vect_pr
scoruri_test=matrix(,7,3) #7 outlieri si 3 componente principale
rownames(scoruri_test)=rownames(outlieri_std)
colnames(scoruri_test)=colnames(scoruri)
for (k in 1:7) { #6 outlieri
for (i in 1:3){ #3 componente principale
scoruri_test[k,i]=0
for (j in 1:10){ #10 variabile originale
scoruri_test[k,i]=scoruri_test[k,i]+sum(vect_pr[j,i]*outlieri_std[k,j])}}}
test_z=scoruri_test
test_z
#BAYES
install.packages("e1071")
library(e1071)
install.packages("graphics")
library(graphics)
install.packages("processx")
library(processx)
#primele 2 clase:
par(mfrow=c(2,3)) #2 linii (clase) si 3 coloane (Z) => 6 grafice
#Clasa 1 - 3 grafice: cate unul pentru fiecare Z
w1_z1=subset(train_z, train_z[,4]== "cls1")
n1_z1=nrow(w1_z1)
d1_z1 <- density(rnorm(n=n1_z1, mean=mean(train_z[,1]), sd=sd(train_z[,1])))
d2_z1 <- density(train_z[,1])
plot(range(d1_z1$x, d2_z1$x), range(d1_z1$y, d2_z1$y), type = "n", xlab = "x",
ylab = "Density - cls1", main="Z1")
lines(d1_z1, col = "red")
lines(d2_z1, col = "blue")
w1_z2=subset(train_z, train_z[,4]== "cls1")
n1_z2=nrow(w1_z2)
d1_z2 <- density(rnorm(n=n1_z2, mean=mean(train_z[,2]), sd=sd(train_z[,2])))
d2_z2 <- density(train_z[,2])
plot(range(d1_z2$x, d2_z2$x), range(d1_z2$y, d2_z2$y), type = "n", xlab = "x",
ylab = "Density - cls1", main="Z2")
lines(d1_z2, col = "red")
lines(d2_z2, col = "blue")
w1_z3=subset(train_z, train_z[,4]== "cls1")
n1_z3=nrow(w1_z3)
d1_z3 <- density(rnorm(n=n1_z3, mean=mean(train_z[,3]), sd=sd(train_z[,3])))
d2_z3 <- density(train_z[,3])
plot(range(d1_z3$x, d2_z3$x), range(d1_z3$y, d2_z3$y), type = "n", xlab = "x",
ylab = "Density - cls1", main="Z3")
lines(d1_z3, col = "red")
lines(d2_z3, col = "blue")
#.........................................................
w1_z1=subset(train_z, train_z[,4]== "cls2")
n1_z1=nrow(w1_z1)
d1_z1 <- density(rnorm(n=n1_z1, mean=mean(train_z[,1]), sd=sd(train_z[,1])))
d2_z1 <- density(train_z[,1])
plot(range(d1_z1$x, d2_z1$x), range(d1_z1$y, d2_z1$y), type = "n", xlab = "x",
ylab = "Density - cls2", main="Z1")
lines(d1_z1, col = "red")
lines(d2_z1, col = "blue")
w1_z2=subset(train_z, train_z[,4]== "cls2")
n1_z2=nrow(w1_z2)
d1_z2 <- density(rnorm(n=n1_z2, mean=mean(train_z[,2]), sd=sd(train_z[,2])))
d2_z2 <- density(train_z[,2])
plot(range(d1_z2$x, d2_z2$x), range(d1_z2$y, d2_z2$y), type = "n", xlab = "x",
ylab = "Density - cls2", main="Z2")
lines(d1_z2, col = "red")
lines(d2_z2, col = "blue")
w1_z3=subset(train_z, train_z[,4]== "cls2")
n1_z3=nrow(w1_z3)
d1_z3 <- density(rnorm(n=n1_z3, mean=mean(train_z[,3]), sd=sd(train_z[,3])))
d2_z3 <- density(train_z[,3])
plot(range(d1_z3$x, d2_z3$x), range(d1_z3$y, d2_z3$y), type = "n", xlab = "x",
ylab = "Density - cls2", main="Z3")
lines(d1_z3, col = "red") #densitate dintre-o distributie normala cu acelasi nr de linii ca Z din
clasa resp, aceeasi medie si abatere
lines(d2_z3, col = "blue") #densitatea de probabilitate a fiecarui Z in fiecare clasa
#din comparatia celor 2 linii vedem cat de aproape sunt scorurile princip in fiecare clasa de
distributia normala
#rosu distributia normala
#albastru distrib pt z
bayes_z <- naiveBayes(train_z[,-4], train_z[,4])
bayes_z
#A=priori probab - probabilitatea apriorica : nr forme din clasa resp / nr total de forme
#difera de la un ouput la altul
#probabil. conditionate sunt densitatile de probab pentru fiecare Z in parte
table(predict(bayes_z, train_z), train_z[,4])
#se clasifica fiecare obs intr-o clasa #clasa 1 15 ob clasa 2 27 ob
table(predict(bayes_z, test_z))
#clasa predictata, levels: clasele posibile care pot aparea
rownames(test_z)
#LDA
install.packages("MASS")
library(MASS)
test_std=outlieri_std[,1:13]
View(test_std)
ad_std = lda(train_std[,1:13],grouping=clase_std)
ad_std
#pe coloan la LD1 primul coeficient
#Forma generala: LD1(coeficientii)* denumirile indicatorilor=>forma generala a functiilor
discriminante
#group means - centroizii fiecarei clase
#coeficientii functiilor discriminanti - cu ei scriem forma generala a functiilor discriminant
#LD1 = -0.097*RC - 0.306*ROE + 0.166*ROA + ... -1.987*SF => scorurile discriminante (ca
la AcP)
predict_std = predict(ad_std,train_std[,1:13])
table(predict_std$class) #vector de clase : clasele de apartenenta a celor 34 de companii
folosind modelul ad_std
#am reclasificat formele (resubstition)
round(predict_std$posterior,2) #probabilitatea ca forma sa apartina intr-una din clase
round(predict_std$x,2) #scoruri discriminant, cu care se calculeaza probabilitatile de
apartenenta
confMatrix_std = table(Original=train_std[,14],Predicted=predict_std$class)
confMatrix_std #grad de accuratete - necesar la proiect
predict_std = predict(ad_std,test_std[,1:13])
table(predict_std)
#probabilitatea posterioara (posterior) si scorurile discriminant (x)
#Z
ad_z = lda(train_z[,1:3],grouping=cls)
ad_z
predict_z = predict(ad_z,train_z[,1:3])
predict_z$class
table(predict_z$class)
round(predict_z$posterior,4)
round(predict_z$x,2)
confMatrix_z = table(Original=train_z[,4],Predicted=predict_z$class)
confMatrix_z #grad de accuratete
predict_z = predict(ad_z,test_z)
predict_z
