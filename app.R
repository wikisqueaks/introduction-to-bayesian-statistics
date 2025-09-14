library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Binomial Distribution and Likelihood"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(
        HTML("This app illustrates how the likelihood function is formed by evaluating
        the probability of observing the fixed data \\((y,n)\\) across many values
        of the binomial parameter \\(\\pi\\). The plot on the right shows the likelihood
        curve, with the blue point marking the likelihood at the chosen \\(\\pi\\).
        Sampling many values of \\(\\pi\\) provides a close approximation to the full
        likelihood curve.")
      ),
      br(),
      tags$h4("Parameter"),
      numericInput("p", "Probability of Success (π):",
        value = 0.25, min = 0, max = 1, step = 0.01, width = "100px"
      ),
      br(),
      tags$h4("The Data"),
      fluidRow(
        column(6, numericInput("y", "Observed (y):", value = 5, min = 0, width = "100px")),
        column(6, numericInput("n", "Trials (n):", value = 20, min = 1, width = "100px"))
      )
    ),
    mainPanel(
      fluidRow(
        column(
          6,
          plotOutput("distPlot"),
          withMathJax(uiOutput("pmfFormula"))
        ),
        column(
          6,
          plotOutput("likPlot"),
          withMathJax(uiOutput("likFormula"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # compute MLE likelihood height + 0.1
  ymaxReactive <- reactive({
    n <- input$n
    y <- input$y
    p_hat <- y / n
    dbinom(y, n, p_hat) + 0.1
  })

  output$distPlot <- renderPlot({
    n <- input$n
    y <- input$y
    p <- input$p
    k <- 0:n
    probs <- dbinom(k, n, p)
    df <- data.frame(k = k, prob = probs, obs = (k == y))

    ggplot(df, aes(x = k, y = prob, fill = obs)) +
      geom_col(color = NA) +
      scale_fill_manual(values = c("FALSE" = "lightgray", "TRUE" = "red"), guide = "none") +
      geom_hline(yintercept = dbinom(y, n, p), linetype = "dotted", color = "blue") +
      labs(
        x = "Number of Successes",
        y = "Probability",
        title = paste0("Binomial Distribution with n=", n, ", π=", sprintf("%.2f", p))
      ) +
      coord_cartesian(ylim = c(0, ymaxReactive())) +
      theme_classic(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$likPlot <- renderPlot({
    n <- input$n
    x <- input$y
    p <- input$p
    p_vals <- seq(0, 1, length.out = 200)
    L <- dbinom(x, n, p_vals)
    df2 <- data.frame(p = p_vals, L = L)

    ggplot(df2, aes(x = p, y = L)) +
      geom_line(size = 1) +
      annotate("point",
        x = p, y = dbinom(x, n, p),
        color = "blue", size = 6
      ) +
      geom_hline(
        yintercept = dbinom(x, n, p),
        linetype = "dotted", color = "blue"
      ) +
      labs(
        x = "π", y = "Likelihood",
        title = paste0("Likelihood Curve: y=", x, ", n=", n)
      ) +
      coord_cartesian(ylim = c(0, ymaxReactive())) +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12)
      )
  })

  output$pmfFormula <- renderUI({
    n <- input$n
    p <- input$p
    isolate({
      withMathJax(
        HTML(
          paste0(
            "Probability mass function: ",
            "\\( P(Y=y) = {n \\choose y} \\pi^{y} (1-\\pi)^{n-y} \\)",
            "<br><br>",
            "With values: ",
            "\\( P(Y=y) = {", n, " \\choose y} ",
            sprintf("%.2f", p), "^{y} (1-", sprintf("%.2f", p), ")^{", n, "-y} \\)"
          )
        )
      )
    })
  })




  output$likFormula <- renderUI({
    n <- input$n
    y <- input$y
    isolate({
      withMathJax(
        helpText(
          paste0(
            "Likelihood function: ",
            "\\( \\mathcal{L}(\\pi) = P(Y=", y, " \\mid n=", n, ", \\pi) \\)"
          )
        )
      )
    })
  })
}

shinyApp(ui = ui, server = server)
