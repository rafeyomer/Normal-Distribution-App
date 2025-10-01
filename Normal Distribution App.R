library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

ui <- fluidPage(
  titlePanel("Normal Distribution Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Mean (μ)", min = -5, max = 5, value = 0, step = 0.1),
      sliderInput("sd", "Std. Dev (σ)", min = 0.2, max = 5, value = 1, step = 0.1),
      
      checkboxInput("shade", "Shade probability area", value = TRUE),
      radioButtons("area_type", "Area type",
                   choices = c("Between a and b" = "between",
                               "Left tail (X ≤ a)" = "left",
                               "Right tail (X ≥ b)" = "right"),
                   inline = FALSE),
      
      numericInput("a", "a", value = -1, step = 0.1),
      numericInput("b", "b", value = 1, step = 0.1),
      helpText("Tip: For ‘Between’, choose a < b."),
      
      hr(),
      sliderInput("n", "Sample size (for histogram)", min = 50, max = 2000, value = 300, step = 50)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 uiOutput("selectedProbText"),   # live probability readout
                 plotlyOutput("plot", height = "520px")
        ),
        tabPanel("Probability",
                 uiOutput("selectedProbText2"),  # repeat the readout above the table
                 tableOutput("probTable")
        ),
        tabPanel("About",
                 tags$p("Topic: Normal distribution. Adjust mean and standard deviation to see the curve change."),
                 tags$ul(
                   tags$li("Widgets: sliders (μ, σ, n), numeric inputs (a, b), checkbox (shade), radio buttons (area type), tabs."),
                   tags$li("Interactive plot: ggplotly (hover, zoom, pan)."),
                   tags$li("Probability area shading for left/right/between selections.")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Smooth curve
  curve_df <- reactive({
    x_min <- input$mu - 5*input$sd
    x_max <- input$mu + 5*input$sd
    x <- seq(x_min, x_max, length.out = 600)
    tibble::tibble(x = x, y = dnorm(x, mean = input$mu, sd = input$sd))
  })
  
  # Histogram
  samp <- reactive({
    set.seed(1) # deterministic for grading
    tibble::tibble(x = rnorm(input$n, mean = input$mu, sd = input$sd))
  })
  
  # Probability calculations
  prob_vals <- reactive({
    a <- input$a; b <- input$b
    mu <- input$mu; s <- input$sd
    between <- max(0, pnorm(max(a,b), mu, s) - pnorm(min(a,b), mu, s))
    left    <- pnorm(a, mu, s)
    right   <- 1 - pnorm(b, mu, s)
    list(between = between, left = left, right = right)
  })
  
  # Display the selected probability cleanly
  selected_prob_text <- reactive({
    p <- prob_vals()
    if (input$area_type == "between") {
      if (input$a >= input$b) {
        return("Selected area: P(a < X < b) — please ensure a < b.")
      }
      sprintf("Selected area: P(%.3f < X < %.3f) = %.6f", input$a, input$b, p$between)
    } else if (input$area_type == "left") {
      sprintf("Selected area: P(X ≤ %.3f) = %.6f", input$a, p$left)
    } else {
      sprintf("Selected area: P(X ≥ %.3f) = %.6f", input$b, p$right)
    }
  })
  
  output$selectedProbText <- renderUI({
    tags$p(strong(selected_prob_text()))
  })
  output$selectedProbText2 <- renderUI({
    tags$p(strong(selected_prob_text()))
  })
  
  # Probability table
  output$probTable <- renderTable({
    p <- prob_vals()
    data.frame(
      Event = c("P(a < X < b)", "P(X ≤ a)", "P(X ≥ b)"),
      Probability = signif(c(p$between, p$left, p$right), 6)
    )
  })
  
  output$plot <- renderPlotly({
    cdf <- curve_df()
    smp <- samp()
    
    # Validate "between" a<b
    if (isTRUE(input$shade) && input$area_type == "between" && input$a >= input$b) {
      # Show a minimal blank plot with a message
      p <- ggplot() +
        theme_minimal(base_size = 14) +
        labs(title = "Normal(μ, σ)",
             subtitle = "For ‘Between’ shading, please ensure a < b.",
             x = "x", y = "Density")
      return(ggplotly(p))
    }
    
    # Base ggplot
    subtitle_txt <- selected_prob_text()
    p <- ggplot() +
      geom_histogram(data = smp, aes(x = x, y = after_stat(density)), bins = 40, alpha = 0.4) +
      geom_line(data = cdf, aes(x = x, y = y), linewidth = 1) +
      theme_minimal(base_size = 14) +
      labs(title = "Normal(μ, σ) with Optional Probability Shading",
           subtitle = subtitle_txt,
           x = "x", y = "Density")
    
    # Vertical reference lines at a/b
    if (input$area_type %in% c("between", "left")) {
      p <- p + geom_vline(xintercept = input$a, linetype = 3)
    }
    if (input$area_type %in% c("between", "right")) {
      p <- p + geom_vline(xintercept = input$b, linetype = 3)
    }
    
    # Shading region
    if (isTRUE(input$shade)) {
      mu <- input$mu; s <- input$sd
      a <- input$a; b <- input$b
      
      shade_between <- function(a, b) {
        xx <- seq(min(a,b), max(a,b), length.out = 200)
        yy <- dnorm(xx, mu, s)
        data.frame(xx = xx, yy = yy)
      }
      
      if (input$area_type == "between") {
        sh <- shade_between(a, b)
        p <- p + geom_area(data = sh, aes(x = xx, y = yy), alpha = 0.3)
      } else if (input$area_type == "left") {
        xx <- seq(input$mu - 5*s, a, length.out = 200)
        sh <- data.frame(xx = xx, yy = dnorm(xx, mu, s))
        p <- p + geom_area(data = sh, aes(x = xx, y = yy), alpha = 0.3)
      } else if (input$area_type == "right") {
        xx <- seq(b, input$mu + 5*s, length.out = 200)
        sh <- data.frame(xx = xx, yy = dnorm(xx, mu, s))
        p <- p + geom_area(data = sh, aes(x = xx, y = yy), alpha = 0.3)
      }
    }
    
    ggplotly(p, tooltip = c("x","y"))
  })
}

shinyApp(ui, server)
