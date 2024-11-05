# Load the required libraries
library(ggplot2)
library(plotly)
library(rgl)

# Objective function
objective_function <- function(x, y) {
  return(x^3 - 3 * x * y + y^2 + 2 * x + y)
}

# Derivatives of the function
partial_derivative_x <- function(x, y) {
  return(3 * x^2 - 3 * y + 2)
}

partial_derivative_y <- function(x, y) {
  return(-3 * x + 2 * y + 1)
}

# Gradient Descent Function
gradient_descent_3d <- function(learning_rate = 0.01, iterations = 100, start_x = 1, start_y = 1) {
  x <- start_x
  y <- start_y
  
  # Store x, y, and z values for plotting
  x_values <- numeric(iterations)
  y_values <- numeric(iterations)
  z_values <- numeric(iterations)
  
  for (i in 1:iterations) {
    grad_x <- partial_derivative_x(x, y)
    grad_y <- partial_derivative_y(x, y)
    
    # Update x and y based on the gradient
    x <- x - learning_rate * grad_x
    y <- y - learning_rate * grad_y
    
    # Store the updated values
    x_values[i] <- x
    y_values[i] <- y
    z_values[i] <- objective_function(x, y)
  }
  
  return(data.frame(Iteration = 1:iterations, x_values, y_values, z_values))
}

# Run the Gradient Descent with specific parameters
learning_rate <- 0.01
iterations <- 100
start_x <- 1
start_y <- 1
gd_result <- gradient_descent_3d(learning_rate, iterations, start_x, start_y)

# Create a sequence for x and y to plot the surface
x_seq <- seq(-3, 3, length.out = 50)
y_seq <- seq(-3, 3, length.out = 50)
z_matrix <- outer(x_seq, y_seq, objective_function)

# Plot the surface using plotly (already existing plot)
p <- plot_ly(x = ~x_seq, y = ~y_seq, z = ~z_matrix) %>%
  add_surface(contours = list(z = list(show = TRUE))) %>%
  layout(scene = list(title = "Gradient Descent Path on Objective Function",
                      xaxis = list(title = "x"),
                      yaxis = list(title = "y"),
                      zaxis = list(title = "f(x, y)")))

# Add the gradient descent path
gd_trace <- list(
  x = gd_result$x_values,
  y = gd_result$y_values,
  z = gd_result$z_values,
  type = "scatter3d",
  mode = "markers+lines",
  marker = list(size = 3, color = 'red'),
  line = list(color = 'red', width = 2)
)
p <- p %>% add_trace(gd_trace)

# Print the plotly plot
p

# Create a 3D perspective plot using rgl
open3d()
persp3d(x_seq, y_seq, z_matrix, col = "lightblue", xlab = "x", ylab = "y", zlab = "f(x, y)")

# Add the gradient descent path points to the rgl plot
points3d(gd_result$x_values, gd_result$y_values, gd_result$z_values, col = "red", size = 5)
lines3d(gd_result$x_values, gd_result$y_values, gd_result$z_values, col = "red", lwd = 2)
