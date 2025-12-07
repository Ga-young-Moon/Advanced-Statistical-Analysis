#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

# Data simulation (a)
set.seed(1)
n <- 150
X <- matrix(runif(n, -1, 1), ncol = 1)
ftrue <- function(x) sin(2*pi*x) + 0.5 * cos(8*pi*x)
y <- ftrue(X[, 1]) + rnorm(n, sd = 0.1)
df_points <- data.frame(x = X[, 1], y = y)

# KRR function
gaussian_kernel <- function(x, rho = 1){
  x <- as.numeric(x)
  outer(x, x, function(a, b) exp(-rho * (a - b)^2))
}

fit_krr <- function(x, y, lambda = 1e-3, rho = 1){
  K <- gaussian_kernel(x, rho)
  alpha <- solve(K + lambda * diag(length(x)), y)
  list(alpha = alpha,x_train = as.numeric(x), rho = rho, lambda = lambda)
}

predict_krr <- function(model, x_new){
  k_new <- outer(as.numeric(x_new), model$x_train,
                 function(a, b) exp(-model$rho * (a - b)^2))
  as.vector(k_new%*%model$alpha)
}

# UI
ui <- fluidPage(
    titlePanel("KRR Parameter Exploration"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("rho", "rho (kernel width)",
                        min = 0.1, max = 10, value = 2, step = 0.1),
            sliderInput('lambda', 'lambda (penalty strength)',
                        min = -6, max = 0, value = -2, step = 0.5),
            checkboxGroupInput('show', 'Show layers',
                               choices = c('True f(x)' = 'true', 'Training points' = 'points'),
                               selected = c('true', 'points')),
            sliderInput('grid', 'plot resolution',
                        min = 100, max = 800, value = 400, step = 50),
        ),

        mainPanel(
           plotOutput("krrPlot", height = '520px')
        )
    )
)

# Server
server <- function(input, output, session) {

  lam <- reactive({10^(input$lambda)})
  
  model <- reactive({
    fit_krr(X[, 1], y, lambda = lam(), rho = input$rho)
  })
  
  grid_df <- reactive({
    xg <- seq(-1, 1, length.out = input$grid)
    yhat <- predict_krr(model(), xg)
    data.frame(x = xg, yhat = yhat, f = ftrue(xg))
  })
  
  output$krrPlot <- renderPlot({
    gdf <- grid_df()
    
    p <- ggplot() +
      labs(title = sprintf('KRR fit - rho = %.2f, lambda = %.1e', input$rho, lam()),
           x = 'x', y = 'y') +
      theme_minimal()
    
    if ('points' %in% input$show){
      p <- p + geom_point(data = df_points, aes(x, y),
                          color = 'gray', alpha = 0.7, size = 2)
    }
    if ('true' %in% input$show){
      p <- p + geom_line(data = gdf, aes(x, f),
                         color = 'red', linetype = 'dashed', linewidth = 1)
    }
    p <- p + geom_line(data = gdf, aes(x, yhat),
                       color = 'steelblue', linewidth = 1.2)
    
    p
  })
}

shinyApp(ui = ui, server = server)