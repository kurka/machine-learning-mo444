#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage") 
#install.packages("circular")
#install.packages("geigen")
#install.packages("proxy")
library("circular")
library("EBImage")
library("geigen")
library("proxy")


####funcao: recorta imagem
patch <- function(img, cent.x, cent.y){#, patch.sizex=180, patch.sizey=220){
  #dimensao foto entrada: 180X270
  #dimensao foto saida: 151X196
  
  patch.l <- 75
  patch.r <- 75
  patch.u <- 85
  patch.d <- 110
  
  if((cent.x-patch.l > 0) && (cent.x+patch.r<=nrow(img)) 
     && (cent.y-patch.u>0) && (cent.y+patch.d<=ncol(img))){ 
    x1r  <- (cent.x-patch.l)
    x2r  <- (cent.x+patch.r)
    y1r  <- (cent.y-patch.u)
    y2r  <- (cent.y+patch.d)
    
    foto.patch <- img[x1r:x2r, y1r:y2r]
  }
  else {
    print("eyes in the edge! filling with black pixels")
    mask <- matrix(0, nrow(img)+patch.l+patch.r, ncol(img)+patch.u+patch.d)
    mask[(patch.l+1):(patch.l+nrow(img)),(patch.u+1):(patch.u+ncol(img))] <- img
    
    x1r  <- (patch.l+cent.x-patch.l)
    x2r  <- (patch.l+cent.x+patch.r)
    y1r  <- (patch.u+cent.y-patch.u)
    y2r  <- (patch.u+cent.y+patch.d)
    
    foto.patch <- mask[x1r:x2r, y1r:y2r] 
  }
  
  return(foto.patch)
  
}

###########################################################
#######carrega e processa imagens de treinamento
###########################################################

coord <- read.table("coords.3368mod.txt", row.names=1)
#(arquivo modificado, atribuindo o valor "0" para coordenadas de boca e nariz quando não especificados)


file.list <- list.files(pattern='*.jpg', path="bdr")
m <- length(file.list)

feature.vectors <- matrix(0, nrow=29596, ncol=m)
mean.face <- matrix(0, nrow=151, ncol=196)

i <- 1
groups <- list()
for (filename in file.list){
  print(i)
  
  ##open
  foto <- readImage(paste("bdr", filename, sep="/"))
  #get dimensions
  name <- gsub(".jpg", "", filename)
  x1 <- coord[name,1]
  y1 <- coord[name,2]
  x2 <- coord[name,3]
  y2 <- coord[name,4]
  dx <- x2-x1
  dy <- y1-y2
  dist.eyes <- sqrt((dx)^2 + (dy)^2) 
  
  ##resize
  res.prop <- 70.0/dist.eyes #70 is the mean distance between eyes in all images
  foto.res <- resize(foto, res.prop*256, res.prop*384)
  
  #nova posicao dos olhos:
  x1.res <- res.prop*x1
  y1.res <- res.prop*y1
  
  x2.res <- res.prop*x2
  y2.res <- res.prop*y2
  
  ##rotate
  theta <- atan(dy/dx)
  foto.rot = rotate(foto.res, deg(theta))
  
  #nova posicao dos olhos:
  cx <- nrow(foto.res)/2 + nrow(foto.res) * sqrt(2) * cos(theta - pi/4 - pi/2)/2
  cy <- ncol(foto.res)/2 + ncol(foto.res) * sqrt(2) * sin(theta - pi/4 - pi/2)/2 
  
  x1.rot <- cx + (x1.res)*cos(theta) - (y1.res)*sin(theta)
  y1.rot <- cy + (x1.res)*sin(theta) + (y1.res)*cos(theta)
  x2.rot <- cx + (x2.res)*cos(theta) - (y2.res)*sin(theta)
  y2.rot <- cy + (x2.res)*sin(theta) + (y2.res)*cos(theta)
  
  
  ##patch
  xmed.rot <- round((x1.rot+x2.rot)/2)
  ymed.rot <- round((y1.rot+y2.rot)/2)
  
  #foto.pat <- patch(foto.circ, xmed.rot, ymed.rot)
  foto.pat <- patch(foto.rot, xmed.rot, ymed.rot)
  
  #display(foto)
  #display(foto.res)
  #display(foto.pat)
  #display(foto.rot)
  feature.vectors[,i] <- as.vector(foto.pat)
  
  #save class information
  indiv <- strtrim(name, 5)
  groups[[indiv]] <- c(groups[[indiv]], i)
  
  
  mean.face <-mean.face*(i/(i+1)) + foto.pat/(i+1)
  i <- i + 1
}


###########################################################
#######carrega e processa imagens de teste
###########################################################
test.list <- list.files(pattern='*.jpg', path="bdc")
mt <- length(test.list)

test.vectors <- matrix(0, nrow=29596, ncol=mt)
mean.tface <- matrix(0, nrow=151, ncol=196)

i <- 1
for (filename in test.list){
  print(i)
  
  ##open
  foto <- readImage(paste("bdc", filename, sep="/"))
  #get dimensions
  name <- gsub(".jpg", "", filename)
  x1 <- coord[name,1]
  y1 <- coord[name,2]
  x2 <- coord[name,3]
  y2 <- coord[name,4]
  dx <- x2-x1
  dy <- y1-y2
  dist.eyes <- sqrt((dx)^2 + (dy)^2) 
  
  ##resize
  res.prop <- 70.0/dist.eyes #70 is the mean distance between eyes in all images
  foto.res <- resize(foto, res.prop*256, res.prop*384)
  
  #nova posicao dos olhos:
  x1.res <- res.prop*x1
  y1.res <- res.prop*y1
  
  x2.res <- res.prop*x2
  y2.res <- res.prop*y2
  
  ##rotate
  theta <- atan(dy/dx)
  foto.rot = rotate(foto.res, deg(theta))
  
  
  #nova posicao dos olhos:
  cx <- nrow(foto.res)/2 + nrow(foto.res) * sqrt(2) * cos(theta - pi/4 - pi/2)/2
  cy <- ncol(foto.res)/2 + ncol(foto.res) * sqrt(2) * sin(theta - pi/4 - pi/2)/2
  
  
  x1.rot <- cx + (x1.res)*cos(theta) - (y1.res)*sin(theta)
  y1.rot <- cy + (x1.res)*sin(theta) + (y1.res)*cos(theta)
  
  x2.rot <- cx + (x2.res)*cos(theta) - (y2.res)*sin(theta)
  y2.rot <- cy + (x2.res)*sin(theta) + (y2.res)*cos(theta)
  
  
  ##patch
  xmed.rot <- round((x1.rot+x2.rot)/2)
  ymed.rot <- round((y1.rot+y2.rot)/2)
  
  #foto.pat <- patch(foto.circ, xmed.rot, ymed.rot)
  foto.pat <- patch(foto.rot, xmed.rot, ymed.rot)
  
  
  test.vectors[,i] <- as.vector(foto.pat)
  mean.tface <-mean.tface*(i/(i+1)) + foto.pat/(i+1)
  i <- i + 1
}

###########################################################
#######Experimento 1 - comparacao euclidiana
###########################################################

####Calcula distancias entre imagens de teste e banco. Calcula numero de acertos
teste <- function(X.trein, X.test, files.trein, files.test){
  
  acertos <- matrix(0, ncol(X.test))
  
  for(i in 1:ncol(X.test)){
    print(i)
    dists <- X.trein - X.test[,i] %*% t(rep(1,ncol(X.trein)))
    dists <- dists^2
    dists <- apply(dists,2,sum)
    dists <- sqrt(dists)
    min <- which.min(dists)
    
    if(strtrim(files.trein[min], 5) == strtrim(files.test[i], 5)){
      print("bingo!")
      acertos[i] <- 1
    }
  }
  return(acertos)
}

acertoseucl <- teste(feature.vectors, test.vectors, file.list, test.list)


###########################################################
#######Experimento 2 - PCA
###########################################################
####PCA
pca <- function(Xm, mean, nfeatures){
  #entrada:
  #X          - martiz nXM (n features, M objetos) a ser reduzida a dimensionalidade
  #mean       - vetor nX1 com média de cada feature
  #nfeatures  - numero final de dimensoes apos reducao
  #
  #saida:
  #W           - matriz de transformação (NXnfeatures)
  #autoval     - autovalores da matriz W completa (M autovalores)
  
  #1- subtrai a media de cada imagem
  print(1)
  #Xm <- X - mean %*% t(rep(1,ncol(X)))
  
  #2- calcula matrix de covariancia entre as imagens (M X M) 
  #Aviso: esse metodo so funciona quando M<n!
  print(2)
  cov.xm <- t(Xm) %*% Xm
  
  #3 acha autovetores e autovalores
  print(3)
  eig.xm <- eigen(cov.xm)
  
  eigen.values <- eig.xm[[1]]
  eigen.vectors <- eig.xm[[2]]
  
  #4-transforma para espaco n-dimensional
  print(4)
  W <- Xm %*% eigen.vectors
  
  #5 pega apenas os nfeatures primeiros autovetores (eigenfaces)
  print(5)
  W <- W[,1:nfeatures] 
  
  #6 - normaliza
  #for(i in 1:nfeatures){
  #  W[,i] <- W[,i]/norm(as.matrix(W[,1], 29596, 1))
  #}
  
  return( list(W=W, autoval=eigen.values))
}



##Aplica PCA nos dados
#subtrai media das imagens de treinamento e teste
feature.vectorsn <- feature.vectors - as.vector(mean.face) %*% t(rep(1,ncol(feature.vectors)))
test.vectorsn <- test.vectors - as.vector(mean.face) %*% t(rep(1,ncol(test.vectors)))

res.pca <- pca(feature.vectorsn, as.vector(mean.face), 1000)
W.pca <- res.pca$W

##compara PCA

X.trein <- t(W.pca) %*% feature.vectorsn
X.test <- t(W.pca) %*% test.vectorsn



acertospca <- teste(X.trein, X.test, file.list, test.list)

###########################################################
#######Experimento 3: LDA
###########################################################


###LDA
lda <- function(X, groups, nfeatures){
  #entrada:
  #X          - martiz nXM (n features, M objetos) a ser reduzida a dimensionalidade
  #groups     - lista de listas de elementos por grupo
  #nfeatures  - numero final de dimensoes apos reducao
  #
  #saida:
  #W.lda       - matriz de transformação (NXnfeatures)
  
  #define o num de classes
  n.classes <- length(groups)
  n <- nrow(X)
  mean <- apply(X, 1, mean)
  #inicializa Sw e Sb
  Sw <- matrix(0, n, n)
  Sb <- matrix(0, n, n)
  
  #calcula matriz de espalhamento SW e Sb
  for (i in 1:n.classes){
    print(i) #TODO: apagar esse print
    size.class <- length(groups[[i]])   #tamanho da classe
    X.class <- as.matrix(X[, groups[[i]]], n, size.class)        #seleciona vetores da mesma classe
    
    mean.class <- apply(X.class,1,mean) #vetor medio da classe
    
    X.class <- X.class - mean.class %*% t(rep(1,ncol(X.class)))
    Sw <- Sw + (X.class %*% t(X.class))
    Sb <- Sb + (size.class * ((mean.class - mean)%*% t(mean.class-mean)))
  }
  
  #soluciona problema geral de autovalores
  W.lda <- geigen(Sb, Sw)$vector
  #selecionar numero desejado de autovetores (normalmente C-1)
  tam <- ncol(W.lda) #geigen retorna autovetores em orde descrescente
  W.lda<- W.lda[,(tam-nfeatures+1):tam]
  
  return(W.lda)
}


#####Fisherfaces
#1- reduz dimensionalidade para m-c, utilizando o pca
m <- ncol(feature.vectorsn)
c <- length(groups)
res.pca <- pca(feature.vectorsn, as.vector(mean.face), m-c)
W.pcalda <- res.pca$W
X.pca <- t(W.pcalda) %*% feature.vectorsn 

#2- aplica lda no espaco reduzido
W.ldapca <- lda(X.pca, groups, c-1)

#3 encontra matriz de transformacao para CxN
W.lda <- t(t(W.ldapca)%*%t(W.pcalda))

#####compara LDA

X.trein <- t(W.lda) %*% feature.vectorsn
X.test <- t(W.lda) %*% test.vectorsn


acertoslda <- teste(X.trein, X.test, file.list, test.list)

###########################################################
#######Experimento 4: PCA+LDA
###########################################################

#1 aplica PCA no conjunto original
#(utiliza matriz de transformacao ja calculada anteriormente)
X.trein <- t(W.pca) %*% feature.vectorsn 

#2 aplica LDA no conjunto transformado
W.lda2 <- lda(X.trein, groups, 1000)

#3 matriz de transformacao eh a combinacao das duas matrizes
W.opt <- t(t(W.lda2)%*%t(W.pca))

#####compara PCA+LDA

X.trein <- t(W.opt) %*% feature.vectorsn
X.test <- t(W.opt) %*% test.vectorsn


acertospcalda <- teste(X.trein, X.test, file.list, test.list)

###########################################################
#######Experimento 5: comparacao de metricas
###########################################################

####Utilizacao de diversas metricas de distancia para calculo de similaridade entre as imagens.
testedist <- function(X.trein, X.test, files.trein, files.test, method="eucl"){
  
  acertos <- matrix(0, ncol(X.test))
  
  if(method=="manh"){
    #manhattan distance
    f <- function(x) sum(abs(x-teste))
  }
  else if(method=="inf"){
    #norma infinita
    f <- function(x) norm(x-teste, "M")
  }
  else if(method=="cos"){
    #cos
    f <- function(x) 1-((t(x)%*%teste)/(sqrt((t(x)%*%x)*(t(teste)%*%teste))))
  }
  else if(method=="camberra"){
    #canberra
    f <- function(x) sum(abs(x-teste)/(abs(x)+abs(teste)))
  }
  else if(method=="braycur"){
    #bray-curtis
    f <- function(x) sum(abs(x-teste))/sum(abs(x+teste))
  }
  else if(method=="cor"){
    #cor distance
    f <- function(x) 1-((t(x-mean(x))%*%(teste-mean(teste)))/(sqrt((t(x-mean(x))%*%(x-mean(x))*(t(teste-mean(teste))%*%(teste-mean(teste)))))))
  }
  else{
    #eucl
    f <- function(x) sqrt(sum((x-teste)^2))
  }
  
  
  for(i in 1:ncol(X.test)){
    print(i)
    teste <- as.matrix(X.test[,i])
    dists <- apply(X.trein, 2, f)
    min <- which.min(dists)
    
    if(strtrim(files.trein[min], 5) == strtrim(files.test[i], 5)){
      print("bingo!")
      acertos[i] <- 1
    }
  }
  return(acertos)
}


acertospcalda2 <- sum(testedist(X.trein, X.test, file.list, test.list, method="manh"))
acertospcalda3 <- sum(testedist(X.trein, X.test, file.list, test.list, method="inf"))
acertospcalda4 <- sum(testedist(X.trein, X.test, file.list, test.list, method="cos"))
acertospcalda5 <- sum(testedist(X.trein, X.test, file.list, test.list, method="camberra"))
acertospcalda6 <- sum(testedist(X.trein, X.test, file.list, test.list, method="braycur"))
acertospcalda7 <- sum(testedist(X.trein, X.test, file.list, test.list, method="cor"))

###########################################################
#######Imagens para relatório:
###########################################################
writeImage(mean.face, "meanface.jpeg", quality=85)

for(i in 1:20){
  lena <- matrix(W.pca[,(i)], 151, 196)
  writeImage(lena,  paste("eigenface", i, ".jpeg", sep=""), quality=85)
}

for(i in 1:20){
  lena <- matrix(W.lda[,(1203-i)], 151, 196)
  writeImage(lena,  paste("fisherface", i, ".jpeg", sep=""), quality=85)
}

#analise dos autovalores
eigen.values <- res.pca[[2]]
cvals <- matrix(0, length(eigen.values))
cvals[1] <- eigen.values[1]
for (i in 2:length(eigen.values)){
  cvals[i] = cvals[i-1] + eigen.values[i]
}
cvals = cvals / sum(eigen.values)
plot(cvals, type="l", main="Distribuição Acumulada dos Autovetores", xlab="autovetor", ylab='valor acumulado (%)') #distribuicao acumulada dos autovalores em ordem descrescente
plot(cvals, type="l") #distribuicao acumulada dos autovalores em ordem descrescente

max <- 0
min <- 10
a <- 0
for(i in 1:1203){
  l <- length(groups[[i]])
  a <- a+l
  print(l)
  if(l>max){
    max<-l
  }
  if(l<min){
    min<-l
  }
}
