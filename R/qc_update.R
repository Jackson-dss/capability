#' @import lubridate

import_and_update_qc_data <- function(){



  # Connect to kemsley SQL servers ------------------------------------------


  #it is necessary to first set up a DSN called 'dss-uk-km-db2' using the "ODBS data sources" app. Use the details below and the plain SQL server driver
  #even though the security details were entered in when setting up the DSN, it seems that they have to be repeated here too, or it doesn't work
  con <- DBI::dbConnect(odbc::odbc(),
                   "dss-uk-km-db2",
                   UID      = "otandacrystal",
                   PWD      = "crystal",
                   Database = "kereport"
  )



  # Read in existing QC data and prep for query -----------------------------


  #archive of QC data stored on local pc which we want to update. Necessary to store locally because the query takes ages.
  #qc <- readRDS("data/qcarchive.rds")
  #qc <- data(qcarchive)
  qc <- as.data.frame(qcarchive)
lasttime <- dplyr::last(qc$DateTurnedUp)
#if the query has just been run (within the last hour), skip running it again, to speed up execution
if(lubridate::time_length(Sys.time() - lasttime)/(60*60)<1){qc}else{

  #Get query parameters
  startdate <- as.Date(dplyr::last(qc$DateTurnedUp)) #this is the query start date not the start of the locally saved data
  enddate <- as.Date(today()+1) #query results are non-inclusive of the second date, so we need to use tomorrow's date in order to return any results from today.
  estimated_time <- as.numeric(enddate - startdate)*15 #seconds, based on a guess that running a day's results of this particular query takes 15 seconds (a bit of an over-estimate)

  #create variables before they are used in lapply (may not be necessary?)
  df <- data.frame()
  outputlist <- list()


  # Run query - Case 1: Long query gets broken down into parts --------------


  if(estimated_time>60*5){ #if query is going to take longer than 5mins (roughly 20 days of results, then split into 20 day intervals)

    print(paste0("Expected run time ",round(estimated_time/60,1)," mins, dividing into 20 day intervals"))
    query_start_time <- Sys.time()
    dateseq <- seq(startdate,enddate,by=1)
    i <- seq(1,length(dateseq),by=20) #only select every 20th day
    if(last(i)==length(dateseq)){}else{i[length(i)+1]<-length(dateseq)} # if length dateseq isnt divisible by 20 then seq will stop short of the end, this adds the end date in

    outputlist <- lapply(i,function(x){
      if(x==1){}else{
        query_enddate <- dateseq[x]
        query_startdate <- if(x==last(i)){dateseq[i[(length(i)-1)]]}else{dateseq[x-20]}
        print(query_startdate)
        # DBgetquery returns the results for an arbitrary query
        # #this stored procedure gets the values on the start date only. no overlap between days, no 0600 timings
        querystring <- paste0("EXEC [dbo].[JUMBO_TEST_RESULTS] '",query_startdate,"', '",query_enddate,"', '03', NULL, 0")
        df <- DBI::dbGetQuery(con, querystring)
        df
      }
    })

    query_end_time <- Sys.time()
    #print(paste0("query took ",round(time_length(query_end_time-query_start_time)/(60),1)," mins to run"))

    qc <- bind_rows(qc,outputlist)



    # Run query - Case 2: Short query just gets run ---------------------------


  }else{

    print(paste0("query should take ",round(estimated_time/60,1)," mins"))
    query_start_time <- Sys.time()

    querystring <- paste0("EXEC [dbo].[JUMBO_TEST_RESULTS] '",startdate,"', '",enddate,"', '03', NULL, 0")
    df <- DBI::dbGetQuery(con, querystring)

    query_end_time <- Sys.time()
    #print(paste0("query took ",round(time_length(query_end_time-query_start_time)/(60),1)," mins to run"))

    qc <- bind_rows(qc,df) # handles missing columns
  }



  # Join locally saved and query data ---------------------------------------


    qc  <- qc %>% distinct(DateTurnedUp, .keep_all = TRUE) #there will be overlap between the last day of the saved results, and the first day of the query results

    qc <- qc[order(qc$DateTurnedUp),]

    #Write to file
    #saveRDS(qc, "data/qcarchive.rds")

    #return the updated dataset
    qc
  }

}
