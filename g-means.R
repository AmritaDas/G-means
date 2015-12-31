data <- read.csv("../data.csv", header=TRUE)

pX <- data[,1]

for(i in 2:dim(data)[2]){
  pX <- cbind(pX, data[,i])
}

library(sp)
library(nortest)
library(stats)

#gmeans function

gmeans <- function(pX, centers, alpha){
  cl <- kmeans(pX, centers, iter.max=50)
  n <- length(cl$centers)/dim(pX)[2]
  o <- 0
  for(j in 1:n){
    #get data of single cluster
    b <- matrix(NA, cl$size[j][1], dim(pX)[2])
    t <- 1
    for(k in 1:dim(pX)[1]){
      if(cl$cluster[k] == j){
        b[t,]  <- pX[k,]
        t <- t+1
      }
    }
    
    #if cluster size > 10, use ad.test
    if(length(b[,1][!is.na(b[,1])]) > 10){
      if(ad.test(b)$p.value < alpha){
        
        if(ad.test(b)$p.value != 0){
          m <- kmeans(b, 1)
          centers <- c()
          kl <- 1
          #delete old center
          for(fg in 1:n){
            x <- (m$centers==cl$centers[fg,])
            f1 <- TRUE
            for(gh in 1:dim(pX)[2]){
              f1 <- f1 && x[gh]
            }
            if(f1 == FALSE){
              centers <- rbind(centers,cl$centers[fg,])
              kl <- kl + 1
            }
          }
          gg <- kmeans(b, 2)
          #add new centers
          centers<- rbind(centers, gg$centers[1,])
          centers<- rbind(centers, gg$centers[2,])
          o <- o+1
        }
      }
    }
    else{     #if cluster size <= 10, use shapiro.test
      if(shapiro.test(b)$p.value < alpha){
        
        if(shapiro.test(b)$p.value != 0){
          m <- kmeans(b, 1)
          centers <- c()
          kl <- 1
          #delete old center
          for(fg in 1:n){
            x <- (m$centers==cl$centers[fg,])
            f1 <- TRUE
            for(gh in 1:dim(pX)[2]){
              f1 <- f1 && x[gh]
            }
            if(f1 == FALSE){
              centers <- rbind(centers,cl$centers[fg,])
              kl <- kl + 1
            }
          }
          gg <- kmeans(b, 2)
          #add new centers
          centers<- rbind(centers, gg$centers[1,])
          centers<- rbind(centers, gg$centers[2,])
          o <- o+1
        }
      }
    }
  }
  if(o == 0){
    return(centers)
  }
  else{
    gmeans(pX, centers, alpha)
  }
}

###########################################Graphs###########

cents <- gmeans(pX, 1, 0.000001)
gm <- kmeans(pX, cents, iter.max=50)
plot(pX,col=gm$cluster)
points(gm$centers, pch=15, col="yellow")

cents1 <- gmeans(pX, 1, 0.00001)
gm1 <- kmeans(pX, cents1, iter.max=50)
plot(pX,col=gm1$cluster)

cents2 <- gmeans(pX, 1, 0.0001)
gm2 <- kmeans(pX, cents2, iter.max=50)
plot(pX,col=gm2$cluster)

