library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .col-sm-6 {
        padding-left: 5px;
        padding-right: 5px;
      }
    "))
  ),
  titlePanel("Probability vs. Likelihood with the Binomial Model"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(
        HTML("
          <div style='text-align: justify;'>
          This app illustrates how the binomial formula is used in two different ways.
          In the <b>probability plot</b> (left), the parameters \\(\\pi\\) and \\(n\\) are held constant,
          and the vertical axis shows how the probability \\(P(Y=y)\\) changes as \\(y\\) varies. The observed
          number of successes is highlighted in red and does not change as the distribution is adjusted.
          In the <b>likelihood plot</b> (right), the data \\((y,n)\\) are held constant,
          and the vertical axis shows how the likelihood function \\(\\mathcal{L}(\\pi)\\) changes
          as \\(\\pi\\) varies. The formulas beneath each plot reflect these two perspectives.
          </div>
        ")
      ),
      br(),
      tags$h4("Parameter"),
      sliderInput("p", "Probability of Success (π):",
        value = 0.25, min = 0, max = 1, step = 0.01, width = "400px"
      ),
      tags$h4("The Data"),
      sliderInput("y", "Observed Successes (y):",
        min = 0, max = 100, value = 5, step = 1, width = "200px"
      ),
      sliderInput("n", "Number of Trials (n):",
        min = 1, max = 200, value = 20, step = 1, width = "200px"
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
          withMathJax(uiOutput("likFormula")),
          withMathJax(uiOutput("likEquation"))
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
      geom_segment(x = y, xend = y, y = 0, yend = ymaxReactive(), color = "red", linetype = "dotted") +
      labs(
        x = "Number of Successes (y)",
        y = "Probability",
        title = bquote("The Binomial Distribution: " ~
          Y %~% Binom(pi == .(sprintf("%.2f", p)), n == .(n))),
        subtitle = "Red line/bar shows the observed successes (\"the data\")."
      ) +
      coord_cartesian(ylim = c(0, ymaxReactive())) +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(color = "red")
      )
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
        x = "Binomial Distribution Parameter (π)", y = "Likelihood",
        title = bquote("Likelihood Function: " ~ cal(L)(pi) ~
          " with " ~ y == .(x) ~ " and " ~ n == .(n)),
        subtitle = "The blue point shows likelihood of observed data given distribution on left."
      ) +
      coord_cartesian(ylim = c(0, ymaxReactive())) +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(size = 12),
        plot.subtitle = element_text(color = "blue")
      )
  })

  output$pmfFormula <- renderUI({
    n <- input$n
    p <- input$p
    isolate({
      withMathJax(
        HTML(
          paste0(
            "<u><b>Binomial probability mass function</b></u>",
            "<br><br>",
            "\\( p(y) = {n \\choose y} \\pi^{y} (1-\\pi)^{n-y} \\)",
            "<br><br>",
            "<b>Keeping \\( \\pi,n \\) constant and varying \\( y \\):</b>",
            "<br><br>",
            "\\( p(y) = {", n, " \\choose y} ",
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
        HTML(
          paste0(
            "<u><b>Likelihood function</b></u>",
            "<br><br>",
            "\\( \\mathcal{L}(\\pi) = P(Y=y \\mid n, \\pi) \\)",
            "<br><br>"
          )
        )
      )
    })
  })

  output$likEquation <- renderUI({
    n <- input$n
    y <- input$y
    isolate({
      withMathJax(
        HTML(
          paste0(
            "<b>Keeping \\( y,n \\) constant and varying \\( \\pi \\):</b>",
            "<br><br>",
            "\\( \\mathcal{L}(\\pi) = {", n, " \\choose ", y, "} ",
            "\\pi^{", y, "} (1-\\pi)^{", n, "-", y, "} \\)"
          )
        )
      )
    })
  })
}



shinyApp(ui = ui, server = server)
