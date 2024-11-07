rm(list = ls())

# Objective function
objective_function <- function(x, y) {
  return((10 * (x^4) - 15 * (x^2) * y + 2 * (y^4) + 5 * (x^2) + 8 * y^2) / 100)
}

partial_derivative_x <- function(x, y) {
  return((40 * x^3 - 30 * x * y + 10 * x) / 100)
}

partial_derivative_y <- function(x, y) {
  return((-15 * x^2 + 8 * y^3 + 16 * y) / 100)
}

# Gradient descent
gradient_descent_3d <- function(learning_rate, iterations, start_x, start_y) {
  x <- start_x
  y <- start_y
  
  x_values <- numeric(iterations)
  y_values <- numeric(iterations)
  z_values <- numeric(iterations)
  
  for(i in 1:iterations) {
    x_values[i] <- x
    y_values[i] <- y
    z_values[i] <- objective_function(x, y)
    
    grad_x <- partial_derivative_x(x, y)
    grad_y <- partial_derivative_y(x, y)
    
    x <- x - learning_rate * grad_x
    y <- y - learning_rate * grad_y
  }
  
  return(list(
    x_values = x_values,
    y_values = y_values,
    z_values = z_values
  ))
}

# Create surface data
x <- seq(-3, 3, length.out = 50)
y <- seq(-3, 3, length.out = 50)
z <- outer(x, y, objective_function)

# Run gradient descent
gd_result <- gradient_descent_3d(0.05, 50, -2, 2)

# Create the surface plot
persp_output <- persp(x, y, z, 
                      theta = 30, phi = 30,
                      expand = 0.5,
                      col = "lightblue",
                      shade = 0.5,
                      ticktype = "detailed",
                      xlab = "x", ylab = "y", zlab = "z")

# Convert 3D points to 2D plot coordinates
points_2d <- trans3d(gd_result$x_values, 
                     gd_result$y_values, 
                     gd_result$z_values, 
                     persp_output)

# Add the gradient descent path
lines(points_2d, col = "red", lwd = 2)
points(points_2d, col = "red", pch = 16)

