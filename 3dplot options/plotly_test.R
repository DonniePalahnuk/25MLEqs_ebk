# First clear the environment
rm(list = ls())

# Load the required libraries
library(ggplot2)
library(plotly)

# Objective function
objective_function <- function(x, y) {
  return(10 * (x^4) - 15 * (x^2) * y + 2 * (y^4) + 5 * (x^2) + 8 * y^2)
}

# Derivatives
partial_derivative_x <- function(x, y) {
  return(40 * x^3 - 30 * x * y + 10 * x)
}

partial_derivative_y <- function(x, y) {
  return(-15 * x^2 + 8 * y^3 + 16 * y)
}

# Here's the missing function
gradient_descent_3d <- function(learning_rate, iterations, start_x, start_y) {
  x <- start_x
  y <- start_y
  
  x_values <- numeric(iterations)
  y_values <- numeric(iterations)
  z_values <- numeric(iterations)
  
  for (i in 1:iterations) {
    grad_x <- partial_derivative_x(x, y)
    grad_y <- partial_derivative_y(x, y)
    
    x <- x - learning_rate * grad_x
    y <- y - learning_rate * grad_y
    
    x_values[i] <- x
    y_values[i] <- y
    z_values[i] <- objective_function(x, y)
    
    # Print progress every 10 iterations
    if(i %% 10 == 0) {
      cat(sprintf("Iteration %d: x=%.4f, y=%.4f, z=%.4f\n", 
                  i, x, y, z_values[i]))
    }
  }
  
  return(data.frame(Iteration = 1:iterations, 
                    x_values, 
                    y_values, 
                    z_values))
}

# Parameters
learning_rate <- 0.005
iterations <- 50
start_x <- -2
start_y <- 2

# Run gradient descent
gd_result <- gradient_descent_3d(learning_rate, iterations, start_x, start_y)

# Create surface grid
x_seq <- seq(-3, 3, length.out = 200)
y_seq <- seq(-3, 3, length.out = 200)
z_matrix <- outer(x_seq, y_seq, objective_function)

# Print statistics
cat("\nSurface statistics:\n")
cat(sprintf("Surface z-values - Min: %f, Max: %f\n", min(z_matrix), max(z_matrix)))
cat("\nPath statistics:\n")
cat(sprintf("Path z-values - Min: %f, Max: %f\n", min(gd_result$z_values), max(gd_result$z_values)))

# Create the plot
p <- plot_ly() %>%
  add_surface(
    x = x_seq,
    y = y_seq,
    z = z_matrix,
    contours = list(z = list(show = TRUE, color = "black")),
    colorscale = list(
      c(0, "lightyellow"),
      c(0.5, "lightgreen"),
      c(1, "lightblue")
    ),
    opacity = 0.8
  ) %>%
  add_trace(
    x = gd_result$x_values,
    y = gd_result$y_values,
    z = gd_result$z_values,
    type = "scatter3d",
    mode = "markers+lines",
    marker = list(size = 4, color = 'red', opacity = 0.9),
    line = list(color = 'red', width = 3)
  ) %>%
  layout(
    title = "Gradient Descent Path on Pronounced Objective Function",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x, y)")
    )
  )

# Show the plot
p