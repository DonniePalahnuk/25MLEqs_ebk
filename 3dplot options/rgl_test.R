

# install.packages("rgl")
library(rgl)

# Generate unsymmetrical data
x <- runif(100, min = 1, max = 50)  # Range between 1 and 50
y <- rnorm(100, mean = 10, sd = 2)   # Normal distribution centered at 10
z <- rexp(100, rate = 0.2)           # Exponential distribution

# Plot the 3D scatter plot with rgl
plot3d(x, y, z, col = "red", size = 5, xlab = "X Axis", ylab = "Y Axis", zlab = "Z Axis")

bbox3d(xat = c(0, 50), yat = c(0, 20), zat = c(0, 40))

# Extract the current view parameters
current_view <- par3d(c("userMatrix", "zoom", "FOV", "observer"))

# Print out the current view parameters
print("Current View Parameters:")
print(current_view)

rgl.texts(x = 25, y = 15, z = 30, text = "Hello Graph World", color = "blue", cex = 2)


library(plotly)

x <- runif(100, min = 1, max = 50)
y <- rnorm(100, mean = 10, sd = 2)
z <- rexp(100, rate = 0.2)


# Create a 3D scatter plot using plotly
plot <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'markers',
                marker = list(size = 5, color = 'red'))

# Add an annotation "Hello Graph World" at the specified position
plot <- plot %>%
  layout(annotations = list(
    list(
      x = mean(x) - 20,  # Position of annotation in x
      y = min(y) - 2,  # Position in y slightly below the minimum y
      z = mean(z),  # Position in z
      text = "Hello Graph World",
      showarrow = TRUE,
      font = list(
        size = 20,
        color = "blue"
      )
    )
  ))


plot
  
  
)



