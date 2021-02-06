#' @import shiny
shinyAppUI <- function(.taglist=taglist){fluidPage(

  # Application title
  titlePanel("Process Capability Plots"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Results of retests are shown instead of the first results where a retest has happened."),
      br(),
      dateRangeInput("dates",
                     "Date range",
                     start = "2020-01-01",
                     end = as.character(Sys.Date())),
      uiOutput('gradesinteractive'),
      selectInput("tagvar", h3("Select QC Test:"),
                  choices = .taglist, selected = "SCT CD ~"),
      br(),
      br(),
      checkboxInput("outliervar", "Remove outliers",
                    value = FALSE)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("capabilityPlot")
    )
  )
)
}
