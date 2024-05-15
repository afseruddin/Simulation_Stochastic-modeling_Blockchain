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

