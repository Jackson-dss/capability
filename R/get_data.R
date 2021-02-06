#' @import dplyr
#' @import tidyr
#' @import shiny
#' @importFrom shiny titlePanel

get_data <- function(tibble = TRUE, every_column = FALSE){


  # get qc data from local archive and SQL servers and process --------------


    qc <- import_and_update_qc_data()



    # rename, filter and calculate useful qc columns --------------------------


    qc <- qc %>% select(
      jumbo = jumbo_id
      , DateTurnedUp = DateTurnedUp
      , grade = grade_spec
      , if(every_column==TRUE){everything()}
    )

    qc$grade <- sapply(qc$grade,trimws)


    #get range of qc data for warning message
    qc_start <- first(qc$DateTurnedUp)
    qc_end <- last(qc$DateTurnedUp)



  # If no process data, then just return qc ---------------------------------


    print(paste("Data from",qc_start,"to",qc_end,sep = " "))
    qc <- qc %>% select(time = DateTurnedUp, everything())


    if(tibble==FALSE){
      qc
    }else{
      qc <- tibble::as_tibble(qc)
      qc
    }
}
