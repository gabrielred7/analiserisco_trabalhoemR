
#========================= Modelo de risco de custo: Custo de construção =====================
construcao<-function(){
library (triangle)
#Constantes  
Ns=1000
  
#Passo 1-Gerar as amostras das variaveis de risco
Escv<-rtriangle(Ns,75,92.5,82.5)
Fund<-rtriangle(Ns,57.5,77.5,67.5)
Estr<-rtriangle(Ns,430,472.5,445)
Telh<-rtriangle(Ns,140,157.5,145)
Acab<-rtriangle(Ns,72.5,107.5,92.5)
#Passo 2-Montar a matriz de cenarios
Cenarios<-cbind(Escv,Fund,Estr,Telh,Acab)

#Passo 3-Avaliar cenarios
Custo<-apply(Cenarios,M=1,sum)

#Passo 4-Mostrar resultados
hist(Custo)
plot(ecdf(Custo))

#Passo 5-Retorna resultado
Cenarios
}

#=======================Algoritmo CPM=======================================
funcCpm<-function(n=9,
                  d=c(0,2,6,4,3,5,4,2,0),
                  Suc=list(c(2,3,4),c(5,7),8,6,8,8,9,9,0),
                  Pre=list(0,1,1,1,2,4,2,c(3,5,6),c(7,8)))
                  
#generates early and late start times of for a cpm network
#Network data:
#n:number of nodes
#d:task duration vector
#Suc:list of successors - zero for none
#Pre:list of predecessors - zero for none
  
#Return vectors early and late 
est<-vector (mode="numeric",length=n)
eft<-vector (mode="numeric",length=n)
lst<-vector (mode="numeric",length=n)
lft<-vector (mode="numeric",length=n)

#Functions
cpmf<-function(s,est){
#find early start and early finish times
  print(s)
  eft[s]<-est[s]+d[s]
	if ((Suc[[s]][1]!=0)){
		for (i in Suc[[s]]){
            if (est[i] < eft[s]){
             est[i]<-eft[s]
            }
		     est<-cpmf(i,est)
		}	
	} 
	est
	}

cpmb<-function(s,lft){
#find late start and finish times
  lst[s]<-lft[s]-d[s]
  	if (Pre[[s]][1]!=0){
		for (i in Pre[[s]]){
            if (lft[i]>lst[s]){
              lft[i]<-lst[s]                
            }                  
			lft<-cpmb(i,lft)
            }
  	}
  lft
}  
  
#Main
    rf<-cpmf(1,est)                         #call forward step
    lft<-rep(rf[n],times=n)                 #initialize late finish times
    rb<-cpmb(n,lft)-d                       #call backward step
    slack<-rb-rf                            #slack times 
    r<-list(est=rf,lst=rb,slack=slack)
    r
}

#===================== Scripts R da Sessão 6 - Correlação ===================

#exemplo 1 da aula
library(triangle)
c1<- rtriangle(1000,10,30,20)
c2<- rtriangle(1000,20,60,40)
ct<-c1+c2
cor1<-cor(c1,c2)
plot(c1,c2,main=paste('Cor=',round(cor1,2)))
hist(ct)

#calcule a cov(c1,c2)
m1<-mean(c1)
m2<-mean(c2)
sp<-(c1-m1)*(c2-m2)
head(sp,30)
mean(sp)
cov(c1,c2)

#exemplo 2
ct<-2*c2+50
plot(ct,c2)
cor2<-cor(ct,c2)
plot(c2,ct,main=paste('Cor=',round(cor2,2)))
hist(ct)

#exemplo 3
u1<-runif(100,-1,1)
hist(u1)
u2<-abs(u1)
hist(u2)
cov1<-cov(u1,u2)
plot(u1,u2,main=paste('Cov=',round(cov1,2)))

#exemplo 4
#depedência linear
e1<-rnorm(1000,0,5)
c3<-2*c1+10+e1
cor2<-cor(e1,c3)
plot(c1,c3,main=paste('Cor=',round(cor2,2)))

#exemplo 5
#depedência linear
c4<- 80 - 2*c1 + e1
plot(c1,c4)

#exemplo 6
#gerando variaveis perfeitamente correlacionadas
library(MASS)
cm<-matrix(c(1,1,1,1),ncol=2)
cm
x<-mvrnorm(500,mu=c(0,0),Sigma=cm)
plot(x[,1],x[,2],main=paste('Cor= 1'))

#exemplo 7
#gerando variaveis perfeitamente não-correlacionadas
cm<-matrix(c(1,0,0,1),ncol=2)
cm
x<-mvrnorm(500,mu=c(0,0),Sigma=cm)
plot(x[,1],x[,2],main=paste('Cor= 0.0'))

#exemplo 8
#gerando variaveis moderadamente correlacionadas
cm<-matrix(c(1,0.5,0.5,1),ncol=2)
cm
x<-mvrnorm(500,mu=c(0,0),Sigma=cm)
plot(x[,1],x[,2],main=paste('Cor= 0.5'))
abline(lm(x[,2]~x[,1]))