##Script para classificar documentos de texto em categorias
#aluno: David Burth Kurka RA:070589
#10/2013

#install.packages("tm.plugin.dc") #instala pacotes, se necessario
#install.packages("cluster")
#install.packages("flexclust")
#install.packages("wordcloud")
#install.packages("RColorBrewer")

library(tm.plugin.dc)
library(cluster)
library(flexclust)
library(wordcloud)
library(RColorBrewer)


####################################################################################
#constroi feature vector conjunto aleatório
##extrai tokens e faz filtros

print("extraindo corpus...")
#extraindo o corpus

#pasta com selecao aleatoria de 1000 documentos. Criada externamente.
corpus <- DistributedCorpus(DirSource("messages-rand/", encoding="UTF-8"), readerControl = list(language="en"))

print("limpando corpus...")
#limpando o corpus
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte")) #trata caracteres indesejados
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower) #passa tudo pra minusculas
#corpus <- tm_map(corpus, removeWords, stopwords()) #corta stopwords
corpus <- tm_map(corpus, stripWhitespace) #corta espaços em branco
corpus <- tm_map(corpus, stemDocument,language="english")

print("montando matriz de frequencias...")

##tira palavras pouco e muito frequentes
dtm.clean <- DocumentTermMatrix(corpus, control=list( minWordLength = 2,
                                                      stopwords=stopwords("en"),
                                                      bounds = list(global = c(5,800))
))

##normaliza
dtm.sparse <- removeSparseTerms(dtm.clean, 0.99)
dtm.norm <- weightTfIdf(dtm.sparse)


###########################################################################################
#definicao de k

#faz agrupamentos usando como distancia: dist.euclideana, cosseno e ejaccard
dtm.dist.euc <- dissimilarity(dtm.norm, method="euclidean")
dtm.dist.cos <- dissimilarity(dtm.norm, method="cosine")
dtm.dist.ejac <- dissimilarity(dtm.norm, method="eJaccard")

ks <- seq(5,95,by=5)
errors.euck <- rep(0,length(ks))
errors.euc <- rep(0, length(ks))
errors.cos <- rep(0, length(ks))
errors.ejac <- rep(0, length(ks))
i <- 1
for (k in ks){
  print(k)
  for (attemp in 1:3){
    ret <- kmeans(dtm.norm, k)
    errors.euck[i] <- errors.euck[i] + ret$tot.withinss
    ret <- pam(dtm.dist.cos, k)
    errors.cos[i] <- errors.cos[i] + sum(ret$clusinfo[,3])/k
    ret <- pam(dtm.dist.ejac, k)
    errors.ejac[i] <- errors.ejac[i] + sum(ret2$clusinfo[,3])/k    
  }
  errors.euck[i] <- errors.euck[i] / 3
  errors.euc[i] <- errors.euc[i] / 3
  errors.cos[i] <- errors.cos[i] / 3
  errors.ejac[i] <- errors.ejac[i] / 3
  i <- i+1
}

##
#calcula hclust
#compara com kmeans (2 histogramas)
hc.ejac <- hclust(dtm.dist.ejac,method="ward")
plot(hc.ejac, labels=F)


##########################################################
#gera corpus para todos os elementos

print("extraindo corpus global...Esse processo pode demorar!")
#extraindo o corpus

#pasta com selecao aleatoria de 1000 documentos. Criada externamente.
corpusg <- DistributedCorpus(DirSource("messages-dist/", encoding="UTF-8"), readerControl = list(language="en"))

#limpando o corpus
corpusg <- tm_map(corpusg, function(x) iconv(enc2utf8(x), sub = "byte")) #trata caracteres indesejados
corpusg <- tm_map(corpusg, removePunctuation)
corpusg <- tm_map(corpusg, removeNumbers)
corpusg <- tm_map(corpusg, tolower) #passa tudo pra minusculas
corpusg <- tm_map(corpusg, stripWhitespace) #corta espaços em branco
corpusg <- tm_map(corpusg, stemDocument,language="english")

print("montando matriz de frequencias global...")
##tira palavras pouco e muito frequentes
dtmg.clean <- DocumentTermMatrix(corpusg, control=list( minWordLength = 2,
                                                        stopwords=stopwords("en"),
                                                        bounds = list(global = c(10,10000))
))
##normaliza
dtmg.sparse <- removeSparseTerms(dtmg.clean, 0.99)
dtmg.norm <- weightTfIdf(dtmg.sparse)
#

########################
#clusteriza todos os documentos
#k definido manualmente, a partir de observacoes: 30
K <- 30

clusters <- kcca(dtmg.norm, K, family=kccaFamily("ejaccard"))
kclusters <- as(clusters, "kmeans") #transforma dados em formato do kmeans
centers <- kclusters$centers



#pega nome dos 4 arquivos mais proximos de cada centro
filename.centroid <- list()
filename.top1 <- list()
filename.top2 <- list()
filename.top3 <- list()

for(c in 1:K){
  #seleciona clusters de uma familia
  grupo <- which(kclusters$cluster == c, arr.ind=T) #todos indices do cluster
  docs <- as.matrix(dtmg.norm[names(grupo)]) #todos os feature vectors do cluster
  topc = distJaccard(docs, kclusters$centers)[,c] #distancia entre docs e centroide
  topc = sort(topc) #ordenacao
  
  
  fileid <- as.integer(names(topc)[1])
  filename.centroid[c] <- names(corpusg)[fileid]
  system(paste("cat messages-dist/", filename.centroid[c], " >> result", c, ".txt", sep=""))
  fileid <- as.integer(names(topc)[2])
  filename.top1[c] <- names(corpusg)[fileid]
  system(paste("cat messages-dist/", filename.top1[c], " >> result", c, ".txt", sep=""))
  fileid <- as.integer(names(topc)[3])
  filename.top2[c] <- names(corpusg)[fileid]
  system(paste("cat messages-dist/", filename.top2[c], " >> result", c, ".txt", sep=""))
  fileid <- as.integer(names(topc)[4])
  filename.top3[c] <- names(corpusg)[fileid]
  system(paste("cat messages-dist/", filename.top3[c], " >> result", c, ".txt", sep=""))
}


##Gera nuvem de frequencia com os documentos do cluster
for(c in 1:K){
  print(c)
  #seleciona clusters de uma familia
  grupo <- which(kclusters$cluster == c, arr.ind=T) #todos indices do cluster
  m <- as.matrix(dtmg.clean[names(grupo)]) #todos os feature vectors do cluster
  v <- sort(colSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:2)]
  png(paste("wordcloud", c, ".png", sep=""), width=1280,height=800)
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  dev.off()
}







##funcao obsoleta, para calcular kmeans em grandes dados
#apos execucao bem sucedida de reducao de feature vector,
#a matriz de termos pode ser trabalhada por funcoes convencionais do R
scalable_kmeans <- function(FV, n, m, k){
  #n - numero de elementos
  #m - numero de features
  #k - numero de clusters
  
  #FV - feature vectors matrix (completa)
  #FV = matrix(rep(c(1:8, 2:9, 3:10, 4:11, 5:12), 20), m, n)
  #FV <- matrix(rnorm(m*n), m, n) #feature vectors matrix
  
  CX <- rep.int(0, n) #cluster associado a cada elemento
  
  #incializa clusters aleatoriamente
  KCl <- as.matrix(FV[,sample(1:n, k)])
  n.iter = 0
  repeat{
    #atribui cluster a cada elemento
    for (f in 1:n){
      dist.diff <- matrix(rep(as.matrix(FV[,f],k)), m, k) - KCl #calcula diferenca do Feature Vector a todos os clusters
      dist.eucl <- sqrt(colSums(dist.diff**)) #distancia euclediana para os vectors
      CX[f] <- which.min(dist.eucl) #guarda em CX o cluster de menor distancia do elemento
    }
    
    #atualiza posicao dos clusters
    KCl.old <- KCl
    KCl <- matrix(0, m, k)
    for (c in 1:k){
      grupo = which(CX == c, arr.ind=T) #seleciona todos os elementos da categoria c
      n.elem = length(grupo)
      for (elem in grupo){
        KCl[,c] <- KCl[,c] + as.matrix(FV[,elem])/n.elem #atualiza posicao do cluster c, com uma média progressiva
      }
    }
    
    n.iter = n.iter + 1
    print(n.iter)
    print(sum(KCl.old-KCl)^2)
    #teste de convergencia
    if(sum(KCl.old-KCl)^2 < 0.001){
      break
    }
  }
  list("CX"=CX, "KCl"=KCl)
}
