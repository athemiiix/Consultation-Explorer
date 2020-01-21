library(ggvis)
library(dplyr)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}
library(shiny)
library(lubridate)

db <- src_sqlite("movies.db")
omdb <- tbl(db, "omdb")
tomatoes <- tbl(db, "tomatoes")
qualtrics <- read.csv("qualtrics.csv", header=TRUE)
myvars <- c("overallimpactlogical","StartDate", "EndDate", "dept", "indig", "ongoing_consult", "planned_consult", "completed_consult", "missed_consult", "indig_decision", "EndDate")
myqualtrics <- qualtrics[myvars]
axisqualtrics <- qualtrics[axis_vars]

parse_date_time(qualtrics$EndDate, c("%Y-%m-%d"))
parse_date_time(qualtrics$StartDate, c("%Y-%m-%d"))
#parse_date_time(qualtrics$EndDate, c("%m/%d/%Y %H:%M"))            
#parse_date_time(qualtrics$StartDate, c("%m/%d/%Y %H:%M"))


#importQualtrics("myapp/PlorkSeptete - Fake Survey Data - for baseline proto.csv", namerow = 1, questionrow = 2, importidrow = 3,
#skip = 3, dropTEXT = FALSE, stringsAsFactors = FALSE)

# Set up handles to database tables on app start
#db <- src_sqlite("movies.db")
#omdb <- tbl(db, "omdb")
#tomatoes <- tbl(db, "tomatoes")

# Join tables, filtering out those with <10 reviews, and select specified columns
#qualtrics <- #inner_join(omdb, tomatoes, by = "ID") %>%
 # filter(qualtrics$ongoing_consult >= 10) %>%
  #select(ID, overallimpactlogical, StartDate, EndDate, 
   #      dept, indig, ongoing_consult, planned_consult, 
    #     completed_consult, missed_consult, Q9, EndDate)

function(input, output, session) {

  # Filter the movies, returning a\ data frame
  consultations <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    Ongoing_consult <- qualtrics$ongoing_consult
    Overallimpactlogical <- qualtrics$overallimpactlogical
    Mindate <- as.Date("2020-01-15", "%Y-%m-%d")
    Maxdate <- as.Date("2033-10-24", "%Y-%m-%d")
    Min_completed_consult <- qualtrics$completed_consult[1]
    Max_completed_consult <- qualtrics$completed_consult[2]

    # Apply filters
    m <- qualtrics %>%
      filter(
        Ongoing_consult <= ongoing_consult,
        qualtrics$overallimpactlogical >= overallimpactlogical,
        qualtrics$StartDate >= Mindate,
        qualtrics$StartDate <= Maxdate,
        qualtrics$completed_consult >= Min_completed_consult,
        qualtrics$completed_consult <= Max_completed_consult
      ) %>%
      arrange(overallimpactlogical)

    # Optional: filter by project type
    if (qualtrics$projtype != "All") {
      projtype <- paste0("%", qualtrics$projtype, "%")
      m <- m %>% filter(projtype %like% projtype)
    }
    # Optional: filter by dept
    if (!is.null(qualtrics$dept) && qualtrics$dept != "") {
      dept <- paste0("%", qualtrics$dept, "%")
      m <- m %>% filter(dept %like% dept)
    }
    # Optional: filter by indig
    if (!is.null(qualtrics$indig) && qualtrics$indig != "") {
      indig <- paste0("%", qualtrics$indig, "%")
      m <- m %>% filter(indig %like% indig)
    }


    m <- as.data.frame(m)

    # Add column which says whether the movie won any Oscars
    # Be a little careful in case we have a zero-row data frame
    m$has_impact <- character(nrow(m))
    m$has_impact[m$overallimpactlogical == 0] <- "No Impact"
    m$has_oscar[m$Oscars >= 1] <- "Has Impact"
    m
  })

  # Function for generating tooltip text
  consultation_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)

    # Pick out the movie with this ID
    qualtrics <- isolate(consultations())
    consultation <- qualtrics[qualtrics$ID == x$ID, ]

    paste0("<b>", ~qualtrics$ID, "</b><br>",
      qualtrics$EndDate, "<br>",
      "$"#, format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
    )
  }

  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == qualtrics$xvar]
    yvar_name <- names(axis_vars)[axis_vars == qualtrics$yvar]

    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    qualtrics %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5,
        stroke = ~qualtrics$consyn, key := ~qualtrics$ID) %>%
      add_tooltip(consultation_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "Currently Consulting", values = c("Yes", "No")) %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
        range = c("orange", "#aaa")) %>%
      set_options(width = 500, height = 500)
  })

  vis %>% bind_shiny("plot1")

  output$n_qualtrics <- renderText({ nrow(qualtrics()) })
}



