library(shiny)

shinyUI(fluidPage(
  title = 'p-value function plots',
  h2("Plot a P-value Function from a Single Confidence Interval"),
  sidebarLayout(
    sidebarPanel(
      h3("Estimate 1"),
        numericInput("est1.ll", "Lower confidence interval:", 0.95,
                     min = 0.01, max = 100, step = 0.1),
        numericInput("est1.ul", "Upper confidence interval:", 4.00,
                     min = 0.01, max = 100, step = 0.1),
      h3("Estimate type"),
        selectInput("citype", 
                  label = "Confidence interval type",
                  choices = list("95%CI", "90%CI",
                                 "99%CI"),
                  selected = "95%CI"),
      textInput("xlabel", "Measure type:", "Hazard ratio"),
      h3("Format plot"),
      numericInput("labelsize", "Label size:", 12,
                   min = 0.01, max = 25, step = 1),
      numericInput("referencewidth", "Reference line width:", 1,
                   min = 0.01, max = 25, step = 1),
      numericInput("functionwidth", "p-value line width:", 3,
                   min = 0.01, max = 25, step = 1),
      h3("Download plot"),
      radioButtons("filetype", "File type:",
                   choices = c("PDF", "HTML", "Word"),
                   inline = TRUE),
      downloadButton('download')
    ),
    mainPanel(
      helpText("This tool allows you to make a plot of the p-value function, showing the relationship between your data and a range of hypothesised effect sizes"),
      plotOutput('theplot'),
       helpText("The methods are a direct port of the code used in Rothman's excel document, episheet"),
       helpText("Rothman provided the following as a reference within the episheet tool: Poole C. Beyond the confidence interval. American Journal of Public Health. 1987;77(2):195-199."),
       tags$div(class = "header", checked = NA,
                tags$a(
                  href = "https://cran.r-project.org/web/packages/episheet/episheet.pdf", 
                  "pvalue plot function on CRAN")
       )
      )
    )
  )
)