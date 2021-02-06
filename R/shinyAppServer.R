#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'





shinyAppServer <- function(input, output,session) {

  appcon <- DBI::dbConnect(odbc::odbc(),
                           "dss-uk-km-db2",
                           UID      = "otandacrystal",
                           PWD      = "crystal",
                           Database = "kereport"
  )

  qctimewindow <- reactive({

    qcoutput <- subsetdf2(qc,as.POSIXct(input$dates[[1]]),as.POSIXct(input$dates[[2]]))
    #qcoutput <- subset(qc,qc$time>as.POSIXct("2020-01-01") & qc$time < as.POSIXct("2021-01-01"))
    #saveRDS(qc,"qctest.rds")
    #qcoutput <- qc %>% filter(DateTurnedUp>as.POSIXct("2020-01-01"))
    qcoutput

  })

  output$gradesinteractive = renderUI({
    mydata <- qctimewindow()
    selectInput('gradevar2', h3('Select Grade:'), choices = sort(unique(mydata$grade[!is.na(mydata$grade)])),selected=sort(unique(mydata$grade[!is.na(mydata$grade)]))[2])
  })

  qcinput <- reactive({

    subsetdf(qc,input$dates[[1]],input$dates[[2]],input$gradevar2,input$tagvar,input$outliervar)

  })

  limits_reactive <- reactive({

    get_limits(as.character(input$gradevar2),as.character(input$tagvar),appcon)
  })

  output$capabilityPlot <- renderPlot({

    plot <- qcinput()
    newname <- paste0(input$tagvar,", grade ",input$gradevar2,", from ",input$dates[1]," to ",input$dates[2])
    colnames(plot)[which(colnames(plot)==input$tagvar)] <- newname
    quick_capability(plot,newname,LSL=limits_reactive()$reject_low[1],USL=limits_reactive()$reject_high[1],target=limits_reactive()$target[1])
    #quick_capability(plot,newname)
  })


  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}
