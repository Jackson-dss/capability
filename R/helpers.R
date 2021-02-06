#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# subsetdf ----------------------------------------------------------------


subsetdf <- function(df,start,end,gradeto,tag,outliers_check){
  if(outliers_check==TRUE){

    df <- df[df$time>start & df$time<end & df$grade==gradeto,]
    df[,which(colnames(df) == tag)] <- lapply(df[,which(colnames(df) == tag)],remove_outliers)
    df

  }else{
    df[df$time>start & df$time<end & df$grade==gradeto,]
  }
}



# subsetdf2 ---------------------------------------------------------------


subsetdf2 <- function(df,start,end){
  df[df$time>start & df$time<end,]
  #df <- subset(df,df$time>start & df$time < end)
  #.df <- subset(df,df$time>as.POSIXct("2020-01-01") & df$time < as.POSIXct("2021-01-01"))
  #.df
}


# get_limits --------------------------------------------------------------


 get_limits <- function(desired_grade,desired_test,appcon){

   querystring3 <- paste0("SELECT ltrim(rtrim(qsv.grade_spec)) as 'grade_spec'
                          ,ltrim(rtrim(qsv.tag_name)) as 'tag_name'
                          ,min(qsv.reject_high) as 'reject_high'
                          ,max(qsv.target) as 'target'
                          ,max(qsv.reject_low) as 'reject_low'
                          FROM quality_spec_criteria_qp_view qsv
                          where ltrim(rtrim(qsv.grade_spec)) = '",desired_grade,"'
                          and tag_name = '",desired_test,"'
                          group by ltrim(rtrim(qsv.grade_spec)), ltrim(rtrim(tag_name))
                          ")
   limits_df <- DBI::dbGetQuery(appcon, querystring3)
   limits_df
 }



# remove_outliers ---------------------------------------------------------


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}



# quick_capability --------------------------------------------------------


#' @import qcc
quick_capability <- function (df, col, LSL="3sigma", USL="3sigma", target = "mean")
{
  df <- as.data.frame(df)
  threesig <- sd(df[, col],na.rm = TRUE) * 3
  meanval <- mean(df[, col],na.rm = TRUE)
  if(is.na(target)){targetval<-NA}else{if(target=="mean"){targetval<- meanval}else{targetval <- target}}
  if(is.na(USL)){USLval<-NA}else{if(USL=="3sigma"){USLval<-meanval+threesig}else{USLval<-USL}}
  if(is.na(LSL)){LSLval<-NA}else{if(LSL=="3sigma"){LSLval<-meanval-threesig}else{LSLval<-LSL}}
  qcc = qcc.groups(df[, col], rep(1, nrow(df)))
  q1 = qcc(qcc, type = "xbar", nsigmas = 3, plot = FALSE)
  q1$data.name <- col
  process.capability(q1, spec.limits = c(LSLval, USLval), target = targetval)
}

