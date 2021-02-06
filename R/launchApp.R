#' launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function(options=list()) {

  # Connect to database (requires a DSN to be set up) -----------------------


  appcon <- DBI::dbConnect(odbc::odbc(),
                      "dss-uk-km-db2",
                      UID      = "otandacrystal",
                      PWD      = "crystal",
                      Database = "kereport"
  )



  # Get qc data and filter --------------------------------------------------


  qc <- get_data(every_column=TRUE)
  allnacols <- which(sapply(qc,function(x) all(is.na(x))))
  qc <- qc[,-allnacols]



  # Get grades and qc tags --------------------------------------------------

  querystring1 <- paste0("SELECT qsv.grade_spec
                                   FROM quality_spec_criteria_qp_view qsv
                                   group by grade_spec
                                   ")
  gradelist <- DBI::dbGetQuery(appcon, querystring1)
  gradelist <- lapply(gradelist[,1],trimws)
  gradelist <- gradelist[which(gradelist %in% qc$grade)]
  gradelist <- na.omit(gradelist)

  querystring2 <- paste0("SELECT qsv.tag_name
                                   FROM quality_spec_criteria_qp_view qsv
                                   group by tag_name
                                   ")
  taglist <- DBI::dbGetQuery(appcon, querystring2)
  taglist <- lapply(taglist[,1],trimws)
  taglist <- taglist[which(taglist %in% colnames(qc))]

  assign('qc',qc,envir=.GlobalEnv)

  shinyApp(ui = shinyAppUI(taglist), server = shinyAppServer,options = options)
}
