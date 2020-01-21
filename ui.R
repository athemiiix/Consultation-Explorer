library(ggvis)

# Variables that can be put on the x and y axes

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

fluidPage(
  titlePanel("Consultation Explorer"),
  fluidRow(
    column(3,
      wellPanel(
        h4("Filter"),
        sliderInput("ongoing_consult", "Minimum number of ongoing consultations",
          10, 300, 80, step = 10), #ongoing_consult
        sliderInput("StartDate", "Start Date of Consultation", as.Date("2020-01-15", "%Y-%m-%d"), as.Date("2033-10-24", "%Y-%m-%d"), value = as.Date(c("2020-01-15", "2033-10-24")), timeFormat = "%Y-%m-%d"),
          sep = ""), #ymd
        sliderInput("overallimpactlogical", "Overall Impact Level (0 = None, 4 = Severe)",
          0, 4, 0, step = 1), #overall impact
        sliderInput("completed_consult", "Number of Completed Consultations",
          0, 500, c(0, 500), step = 1), #completed consult
        selectInput("projtype", "Project Type (a project can have multiple types)", #project type
          c("Industrial", "Infrastructure", "Natural Resources", "Cultural", "Other")
        ),
        textInput("dept", "Department Name Contains (e.g., Crown-Indigenous...)"), #dept
        textInput("indig", "Indigenous Group name contains (e.g., Algonquins of...)") #indigenous
      ),
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "StartDate"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "ongoing_consult"),
        tags$small(paste0(
          #"Note: 
        ))
      )
    ),
    column(9,
      ggvisOutput("plot1"),
      wellPanel(
        span("Number of Consultations Displayed:",
          textOutput("n_ongoing_consultation")
        )
      )
    )
  )


