# Load the required libraries
library(ggplot2)
library(plotly)
library(rgl)

# Further Modified Objective function to make the minimum extremely pronounced
objective_function <- function(x, y) {
  return(10 * (x^4) - 15 * (x^2) * y + 2 * (y^4) + 5 * (x^2) + 8 * y^2)
}

# Derivatives of the further modified function
partial_derivative_x <- function(x, y) {
  return(40 * x^3 - 30 * x * y + 10 * x)
}

partial_derivative_y <- function(x, y) {
  return(-15 * x^2 + 8 * y^3 + 16 * y)
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
start_x <- -2
start_y <- 1
gd_result <- gradient_descent_3d(learning_rate, iterations, start_x, start_y)

# Create a sequence for x and y to plot the surface
x_seq <- seq(-3, 3, length.out = 50)
y_seq <- seq(-3, 3, length.out = 50)
z_matrix <- outer(x_seq, y_seq, objective_function)

# Plot the surface using plotly with modified color scheme
p <- plot_ly(x = ~x_seq, y = ~y_seq, z = ~z_matrix) %>%
  add_surface(
    contours = list(z = list(show = TRUE, color = "black")),
    colorscale = list(
      c(0, "lightyellow"),
      c(0.5, "lightgreen"),
      c(1, "lightblue")
    ),
    opacity = 0.8
  ) %>%
  layout(scene = list(title = "Gradient Descent Path on Pronounced Objective Function",
                      xaxis = list(title = "x"),
                      yaxis = list(title = "y"),
                      zaxis = list(title = "f(x, y)")))

# Add the gradient descent path to the plotly plot
p <- p %>% add_trace(
  x = ~gd_result$x_values,
  y = ~gd_result$y_values,
  z = ~gd_result$z_values,
  type = "scatter3d",
  mode = "markers+lines",
  marker = list(size = 4, color = 'red', opacity = 0.9),
  line = list(color = 'red', width = 3)
)

# Print the plotly plot
p
