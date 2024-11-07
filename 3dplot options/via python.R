rm(list = ls())
library(reticulate)

# Define the objective function and its partial derivatives
objective_function <- function(x, y) {
  return((10 * (x^4) - 15 * (x^2) * y + 2 * (y^4) + 5 * (x^2) + 8 * y^2) / 100)
}

partial_derivative_x <- function(x, y) {
  return((40 * x^3 - 30 * x * y + 10 * x) / 100)
}

partial_derivative_y <- function(x, y) {
  return((-15 * x^2 + 8 * y^3 + 16 * y) / 100)
}

# Gradient descent function
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

# Run gradient descent
gd_result <- gradient_descent_3d(0.05, 50, -2, 2)

# Pass R data to Python
py$x_path <- gd_result$x_values
py$y_path <- gd_result$y_values
py$z_path <- gd_result$z_values

# Plotly visualization
py_run_string("
import plotly.graph_objects as go
import numpy as np

# Create surface data
x = np.linspace(-3, 3, 100)
y = np.linspace(-3, 3, 100)
X, Y = np.meshgrid(x, y)
Z = (10 * X**4 - 15 * X**2 * Y + 2 * Y**4 + 5 * X**2 + 8 * Y**2) / 100

# Create figure
fig = go.Figure()

# Add surface
fig.add_trace(go.Surface(x=x, y=y, z=Z, colorscale='viridis', opacity=0.8))

# Add gradient descent path
fig.add_trace(go.Scatter3d(
    x=x_path, y=y_path, z=z_path,
    mode='lines+markers',
    line=dict(color='red', width=4),
    marker=dict(size=4, color='red'),
    name='Gradient Descent Path'
))

# Update layout
fig.update_layout(
    title='Gradient Descent Optimization',
    scene = dict(
        xaxis_title='x',
        yaxis_title='y',
        zaxis_title='z',
        camera=dict(
            eye=dict(x=1.5, y=1.5, z=1.5)
        ),
        aspectratio=dict(x=1, y=1, z=0.8)
    ),
    showlegend=True,
    width=800,
    height=800
)

fig.show()
")