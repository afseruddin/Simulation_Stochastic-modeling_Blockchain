set.seed(1)
##############
### function f is calculated based on the formula of CDF of V#####
f <- function(t){
  c <- 2      # c is the length of the monitoring/non-monitoring time period
  M_NM <- floor(t/c)
  p_L <- floor(t/(2*c))
  a_M <- 0
  a_NM <- 0
  a_M <- pexp(t-c*(p_L[1]),rate=delta)  #when t lies in monitoring period
  a_NM <- pexp(c*(p_L[1]) + c,rate=delta)  #when t lies in non-monitoring period
  a_F <- if (M_NM[1]%%2 == 0) a_M else a_NM
  return(a_F)
}


delta <- 1/3

#curve(Vectorize(f)(x), 0, 30,xlab="v",ylab="P(V < v)", main="CDF of V")
# abline(h=0)









f_vectorized <- Vectorize(f)

intervals <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30) # Adjust intervals as needed
colors <- c("darkgreen", "red", "darkgreen", "red","darkgreen", "red","darkgreen", "red","darkgreen", "red",
            "darkgreen", "red","darkgreen", "red","darkgreen") # Corresponding colors

# Generate a sequence of x values
x_values <- seq(0, 30, by = 0.001)

# Calculate corresponding y values using the vectorized function
y_values <- f_vectorized(x_values)

# Create an empty plot
plot(x_values, y_values, type = "n", xlab = "v", ylab = "P(V<v)", xaxt = "n")

# Loop through each interval
for (i in 1:(length(intervals)-1)) {
  # Find indices of points within the interval
  indices <- which(x_values >= intervals[i] & x_values < intervals[i+1])
  # Plot line segments within the interval with corresponding color
  if (length(indices) > 0) {
    lines(x_values[indices], y_values[indices], col = colors[i])
  }
}

# Add legend
legend("bottomright", legend = c("non-monitoring period", "monitoring period"), col = c("red", "darkgreen"), lty = 1)
abline(h=0)
axis(1, at = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30))




# Random number generates for V

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



# Now we will find the root of (a_F - r) = 0, i.e. value of t for which (a_F-r) is "0"


### First we draw the graph of function f i.e; (a_F - r) to get the idea where
## the root of the function, and choose a and b for bisection method. 

delta <- 1/3

curve(Vectorize(f)(x), 0, 30,xlab="V",ylab="F(v) - U")
abline(h=0)


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


### Histogram of V

V_r <- c()   # V_r = random sample (V)
for (i in 1:1000){
  V_r[i] <- v(200)
}

#hist(V_r,breaks = 100,xlab="v",main="Histogram of V")

ggplot(data.frame(V_r), aes(x = V_r)) +
  geom_histogram(binwidth = 0.4, fill = "lightblue", color = "black") +
  labs(x = "V", y = "Frequency") +
  theme_classic() 




