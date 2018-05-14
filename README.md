# Spectral-Clustering
spectral clustering is to cluster data that is connected 
# Spectral Clustering
#Getting help example
??'standarad deviation'
??'kmeans'
??'pca'
help.start()

setwd("~/your directory Path ")

#The goal of spectral clustering is to cluster data .

# needs a dataset for  clustering:

# install.packages("mlbench") first 
install.packages("mlbench")

# run library(mlbench) firs 
library(mlbench)

# must upload set.seed(111)
set.seed(111)

obj <- mlbench.spirals(100,1,0.025)

#your data must be 4 * obj$x , then run 
my.data <-  4 * obj$x

#ptional to plot, you can save it if you like  
plot(my.data)


# clustering always needs a similarity 
# but Spectral clustering needs a similarity or affinity 

#for exmaple 
#s(x,y) measure determining how close points x and y are from each other. 
# Let'S denote the Similarity Matrix, S, as the matrix that at S_ij = s(x_i, x_j)
  # gives the similarity between observations  x_i and  x_j
  # Common similary measures are: + Euclidean distance: 
    # s(x_i, x_j) = -|| x_i, x_j ||
  
# lets start similarity first  for s, then run 

s <- function(x1, x2, alpha=1) {
  
  exp(- alpha * norm(as.matrix(x1-x2), type="F"))
}

make.similarity <- function(my.data, similarity) {
  
  N <- nrow(my.data)
  S <- matrix(rep(NA,N^2), ncol=N)
  for(i in 1:N) {
    for(j in 1:N) {
      S[i,j] <- similarity(my.data[i,], my.data[j,])
    }
  }
  
  S
}

# Now fro S also you need lets start similarity by  make.similarity(my.data, s) , then run 

S <- make.similarity(my.data, s)


S[1:8,1:8]

# lets start  affinity for s, then run 

make.affinity <- function(S, n.neighboors=2) {
  N <- length(S[,1])
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  A
}

# lets start affinity only for A , then run 

A <- make.affinity(S, 3)  # use 3 neighboors (includes self)
A
A[1:8,1:8]
D <- diag(apply(A, 1, sum)) # sum rows
D[1:8,1:8]
U <- D - A
round(U[1:12,1:12],1)

# L <- diag(nrow(my.data)) - solve(D) %*% A  # simple Laplacian
# round(L[1:12,1:12],1)
# matrix power operator: computes M^power (M must be diagonalizable)

"%^%" <- function(M, power)
  with(eigen(M), vectors %*% (values^power * solve(vectors)))
k   <- 2
evL <- eigen(U, symmetric=TRUE)
Z   <- evL$vectors[,(ncol(evL$vectors)-k+1):ncol(evL$vectors)]
plot(Z, col=obj$classes, pch=20) # notice that all 50 points, of each cluster, are on top of each other

#now you need stats library 
library("stats")
km <- kmeans(Z, centers=k, nstart=5)

plot(my.data, col=km$cluster)

signif(evL$values,2) # eigenvalues are in decreasing order

plot(1:10, rev(evL$values)[1:10], log="y")

abline(v=2.25, col="red", lty=2) # there are just 2 clusters as expected

#now you need kernlab library 
library("kernlab")

# install packages kernlab

install.packages("kernlab")

#now you need kernlab library 
library(kernlab)

sc <- specc(my.data, centers=2)

plot(my.data, col=sc, pch=4)            # estimated classes (x)

points(my.data, col=obj$classes, pch=5) # true classes (<>)

savehistory("~/your directory")

enjoy 

Turki ALjrees
