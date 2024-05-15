set.seed(1)
##############
### function f is calculated based on the formula of CDF of V#####
f <- function(t){
  r <- runif(1)
  c <- 2      # c is the length of the monitoring/non-monitoring time period
  M_NM <- floor(t/c)
  p_L <- floor(t/(2*c))
  a_M <- 0
  a_NM <- 0
  a_M <- pexp(t-c*(p_L[1]),rate=delta)  #when t lies in monitoring period
  a_NM <- pexp(c*(p_L[1]) + c,rate=delta)  #when t lies in non-monitoring period
  a_F <- if (M_NM[1]%%2 == 0) a_M else a_NM
  return(a_F - r)
}

###### function v generates random number V  ######
## Support of V : {[0,2),[4,6),[8,10),[12,14),[16,18),[20,22),...}

v<- function(n){
  
  while (TRUE){   
    a1 <- 0
    b1 <- 30
    t <- (a1 + b1)/2
    for (i in 1:n){
      if((f(a1)*f(t)) <= 0){b1 <- t}
      else{a1 <- t}
      t <- a1+(b1-a1)/2
    }
    V <- t
    
    if (floor(V/2)%%2==0) { # this statement confirms that final V
      # is within the domain.
      break
    }
  }
  return(V)
}
##############################

#### ET_m will calculate the mean functional time#####

ET_m <- function(m, lamb, delta){
  T_m <- function(m, lamb, delta){
    sum_of_v = 0
    v  = v(n)
    sum_of_x = sum(rexp(m,rate=lamb))
    while(sum_of_x > v){
      sum_of_v <- sum_of_v + v
      sum_of_x = sum(rexp(m,rate=lamb))
      v = v(n)
    }
    return(sum_of_v + sum_of_x)
  }
  ET <- c()
  for (i in 1:m) {
    T_0 = 0
    iter = 25000
    for (j in 1:iter){
      T_0 = T_0 + T_m(i, lamb, delta)
    }
    ET[i] <- T_0/iter
  }
  return(ET)
} 

####################################

n <- 200
m <- 40
lamb <- 1/0.2
delta <- 1/3
ET_m <- ET_m(m,lamb,delta)


#plot(ET_m,main="Exponential Hacking and Exponential Detecting Times", xlab = "m", ylab=expression("E[T"[m]*"]"),ylim=c(0,25),col="#009999")



## save the ET_m data 
#folder_path <- "/Users/afsersobuj/Desktop/PhD/R code - 1/Data"
#write.csv(ET_m, file = paste0(folder_path, "E_TM_EXP_1.csv"), row.names = FALSE)


library(ggplot2)

m <- seq_along(ET_m)
data <- data.frame(m = m, ET_m = ET_m)


ggplot(data, aes(x = m, y = ET_m)) +
  geom_point(color = "#009999") +
  labs(x = "m", 
       y = expression("E[T"[m]*"]")) +
  ylim(0, 22) +
  xlim(0, 40) +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black", 
                                        fill = NA, 
                                        size = 0.3)) +
  theme(
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize x-axis ticks (color and size)
    axis.ticks.y = element_line(color = "black", size = 0.5))




  
























############################################
#### Continuous vs Periodic #####


ET_m_c <- function(m, lamb, delta){
  T_m <- function(m, lamb, delta){
    sum_of_y = 0
    y = rexp(n=1,rate=delta)
    sum_of_x = sum(rexp(m,rate=lamb))
    while(sum_of_x > y){
      sum_of_y <- sum_of_y + y
      sum_of_x = sum(rexp(m,rate=lamb))
      y = rexp(n=1,rate=delta)
    }
    return(sum_of_y + sum_of_x)
  }
  ET <- c()
  for (i in 1:m) {
    T_0 = 0
    iter = 30000
    for (j in 1:iter){
      T_0 = T_0 + T_m(i, lamb, delta)
    }
    ET[i] <- T_0/iter
  }
  return(ET)
} 


ET_m_c <- ET_m_c(m,lamb,delta)


plot(ET_m,main="Exponential Hacking and Exponential Detecting Times", xlab = "m", ylab=expression("E[T"[m]*"]"),col="#009999")

points(ET_m_c,col="red")
legend(1, 36, legend=c("continuous monitoring", "periodic monitoring"),
       col=c("red", "#009999"), lty="dotted", cex=0.8)






set.seed(1)
##############


f <- function(t){
  r <- runif(1)
  c <- 2
  M_NM <- floor(t/c)
  p_L <- floor(t/(2*c))
  a_M <- 0
  a_NM <- 0
  a_M <- pexp(t-c*(p_L[1]),rate=delta)
  a_NM <- pexp(c*(p_L[1]) + c,rate=delta)
  a_F <- if (M_NM[1]%%2 == 0) a_M else a_NM
  return(a_F - r)
}



v<- function(n){
  
  while (TRUE){   
    a1 <- 0
    b1 <- 30
    t <- (a1 + b1)/2
    for (i in 1:n){
      if((f(a1)*f(t)) <= 0){b1 <- t}
      else{a1 <- t}
      t <- a1+(b1-a1)/2
    }
    V <- t
    
    if (floor(V/2)%%2==0) { # this statement confirms that final V
      # is within the domain.
      break
    }
  }
  return(V)
}
##############################

T_m <- function(m, lamb, delta){
  sum_of_v = 0
  v  = v(n)
  sum_of_x = sum(rexp(m,rate=lamb))
  while(sum_of_x > v){
    sum_of_v <- sum_of_v + v
    sum_of_x = sum(rexp(m,rate=lamb))
    v = v(n)
  }
  return(sum_of_v + sum_of_x)
}

P_mt <- function(t,m,lamb,delta){
  W <- c()
  Prob <- c()
  for (i in 1:m) {
    iter = 25000
    for (j in 1:iter){
      W[j] <-  t < T_m(i, lamb, delta)
    }
    Prob[i] <- sum(W)/iter
  }
  return(Prob)
}

#########################


n <- 200
m <- 40
lamb <- 1/0.2
delta <- 1/3
t_cross_section <- 7
P_mt <- P_mt(t_cross_section,m,lamb,delta)

#plot(P_mt,main="Exponential Hacking and Exponential Detecting Times", xlab = "m", ylab=expression("P"[m]*"(t)"),col="#009999")

## save the P_mt data 
#folder_path <- "/Users/afsersobuj/Desktop/PhD/R code - 1/Data"
#write.csv(P_mt, file = paste0(folder_path, "P_MT_EXP_1.csv"), row.names = FALSE)



library(ggplot2)

m <- seq_along(P_mt)
data <- data.frame(m = m, P_mt = P_mt)


ggplot(data, aes(x = m, y = P_mt)) +
  geom_point(color = "#009999") +
  labs(x = "m", 
       y = expression("P"[m]*"(t)")) +
  ylim(0, 1) +
  xlim(0, 40) +
  theme_minimal()+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.3)) +
  theme(
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize x-axis ticks (color and size)
    axis.ticks.y = element_line(color = "black", size = 0.5))





set.seed(1)
##############


f <- function(t){
  r <- runif(1)
  c <- 2
  M_NM <- floor(t/c)
  p_L <- floor(t/(2*c))
  a_M <- 0
  a_NM <- 0
  a_M <- pexp(t-c*(p_L[1]),rate=delta)
  a_NM <- pexp(c*(p_L[1]) + c,rate=delta)
  a_F <- if (M_NM[1]%%2 == 0) a_M else a_NM
  return(a_F - r)
}



v<- function(n){
  
  while (TRUE){   
    a1 <- 0
    b1 <- 30
    t <- (a1 + b1)/2
    for (i in 1:n){
      if((f(a1)*f(t)) <= 0){b1 <- t}
      else{a1 <- t}
      t <- a1+(b1-a1)/2
    }
    V <- t
    
    if (floor(V/2)%%2==0) { # this statement confirms that final V
      # is within the domain.
      break
    }
  }
  return(V)
}
##############################

T_m <- function(m, lamb, delta){
  sum_of_v = 0
  v  = v(n)
  sum_of_x = sum(rexp(m,rate=lamb))
  while(sum_of_x > v){
    sum_of_v <- sum_of_v + v
    sum_of_x = sum(rexp(m,rate=lamb))
    v = v(n)
  }
  return(sum_of_v + sum_of_x)
}


P_mt <- function(t,m,lamb,delta){
  W <- c()
  iter = 100
  for (j in 1:iter){
      W[j] <-  t < T_m(m, lamb, delta)
  }
  Prob <- sum(W)/iter
  return(Prob)
}
#########################


n <- 200
m <- seq(1,40)
lamb <- 1/0.2
delta <- 1/3
t_cross_section <- seq(0,10,by=0.05)
P_mt1 <- matrix (nrow = length(m),ncol = length(t_cross_section))
output <- c()
D <- data.frame(x= 1,y=1,z=1)
for(i in 1:length(m)){
  for(j in 0:length(t_cross_section)){
    P_mt1[i,j] <- P_mt(t_cross_section[j],m[i],lamb,delta)
    t1 <- m[i]
    t2 <- t_cross_section[j]
    t3 <- P_mt1[i,j]
    output <- c(t1,t2,t3)
    D <- rbind(D,output)
  }
}

#scatterplot3d(D1,xlim=rev(c(0,40)),ylim=c(0,10),zlim=c(0,1))



##scatterplot3d(D3,xlim=rev(c(0,10)),ylim=c(0,40),zlim=c(0,1))


#scatterplot3d(D3,xlim=c(10,0),zlim=c(0,1))

S <- c(1,2,seq(204,8081,by=202))
S1 <- D[-S,]
S2 <- data.frame(S1$x,S1$y,S1$z)
S3 <- S2[,c(2,1,3)]
#scatterplot3d(S3,xlim=rev(range(S3[1])),zlim=c(0,1),angle=55)


S4 <- S3
S4[1] <- max(S3[1]) - S3[1]
library("scatterplot3d")
#scatterplot3d(S4,x.ticklabs=seq(max(S3[1]),min(S3[1]),-2))

a <- scatterplot3d(S4,pch = "", grid=FALSE, xlab = "t",ylab = "m",zlab = expression("P"[m]*"(t)"),box=FALSE,x.ticklabs=seq(max(S3[1]),min(S3[1]),-2))
library("FactoClass")
addgrids3d(S4, grid = c("xy", "xz", "yz"))
a$points3d(S4, pch = 1,col="gray",cex=.3)






