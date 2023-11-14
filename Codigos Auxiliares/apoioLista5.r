#Material de apoio a Lista 5
library(triangle)
library(igraph)
source('funcCpm.r')

#1-Par√¢metros
Ns=3000
nivelRisco=0.85
#1.1-parametros do grafo
n=14
d=c(0,6,5,3,1,6,2,1,4,3,2,3,5,0)
Suc=list(c(2,3,4),9,c(5,6,7),8,
         10,12,c(8,11),13,
         14,12,12,13,
         14,0)
Pre=list(0,1,1,1,
         3,3,3,4,
         2,5,7,c(6,10,11),
         c(8,12),c(9,13))
elos=c(1,2,1,3,1,4,2,9,3,5,3,6,3,7,4,8,5,10,6,12,
       7,8,7,11,8,13,9,14,10,12,11,12,12,13,13,14)

#1.2-verificando grafo
g<-make_graph(elos)
tkplot(g,vertex.color='white')

#2-gera√ß√£o simplificada dos cen√°rios
Cenarios<-matrix(ncol=n,nrow=Ns)
Cenarios[,1]=0
Cenarios[,n]=0
#gera valores entre 0.2 d e 2d
for (i in 2:(n-1)){
  #Nota: n„o s„o os parametros da lista 5
  Cenarios[,i]<-ceiling(rtriangle(Ns,0.2*d[i],2*d[i],d[i]))
}
#3-avaliaÁ„o dos cen·rios

#3.1-estruturas de dados
Prazo<-vector(length=Ns)
probCC<-rep(0,n)
Agendas<-matrix(nrow=Ns,ncol=n)

#3.2-iteraÁ„o sobre os cen·rios
for (i in 1:Ns){
  r<-funcCpm(n,Cenarios[i,],Suc,Pre)
  est<-r$est
  slack<-r$slack
  Agendas[i,]<-est
  Prazo[i]<-est[n]+Cenarios[i,n]
  indCritico<-which(slack==0)
  probCC[indCritico]<-probCC[indCritico]+1
  
}

#4-analise dos resultados
#4.1-risco de prazo
hist(Prazo)
#4.2-histogramam da agenda  da atividade 10
hist(Agendas[,10])
#4.3-probabilidades do CC
probCC<-round(probCC/Ns,2)
#4.4-agenda com risco 15%
ordPrazo<-order(Agendas[,n],decreasing = T)
indRisco<-round((1-nivelRisco)*Ns)
Agendas[ordPrazo,5:14]
Agendas[indRisco,]

