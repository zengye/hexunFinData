############################################################
# Get Assets and Liabilities from hexun.com
#
# Required packages: XML
# Author: ZENG YE
# Mailto: zeno@zengye.name
# Version: demo
# Release date: 2013-09-16
# Commments:
#
# Arguments
#   date: specify the accounting date. Either a character
#         value "latest" which represents for downloading
#         the latest financial index, or an object of class
#         "Date" which represents for a valid accounting 
#         date, including "03-15", "06-30", "09-30" and 
#         "12-31" every year. A valid value for date is 
#         like the string "2013-03-15". 
#   
#   todataframe: A logical value indicating whether the table 
#                of result should be converted to data.frame
#
#   na: Either a logical value indicating whether the default
#       string("--") for missing values should be replaced
#       by blank or a character string that is used to replace
#       the missing values. If na == TRUE, then the missing 
#       values will be replaced with blank
#############################################################


getzcfz <- function(stockid,date="latest",todataframe=F,na=F){
  # attach the package XML if it is not in the search path
  if(!("package:XML" %in% search()))
    library(XML)
  
  url <- paste("http://stockdata.stock.hexun.com/2009_zcfz_",stockid,".shtml",sep="")
  
  if(date == "latest"){
    doc <- htmlParse(url)
    tb <- readHTMLTable(doc,which=3)
  } else{
    # find the oldest valid date for this stock
    urltext <- readLines(url)
    urltext <- iconv(urltext,from="gb2312",to="UTF-8")
    vdate <- grep("dateurl=",urltext,value=T)
    vdate <- strsplit(vdate,",")[[1]]
    vdate <- vdate[length(vdate)-1]
    vdate <- gsub("\\[|'","",vdate,perl=T)
    vdate <- gsub("\\.","-",vdate)
    
    date <- as.Date(date)
    vdate <- as.Date(vdate)
    
    if(date > Sys.Date() || date < vdate ){
      cat("Error: The valid oldest date for stock ", stockid," is ",as.character(vdate),"\n")
      stop()
    } else if(!(as.character(format(date,"%m-%d")) %in% c("03-15","06-30","09-30","12-31"))){
      cat("Error: The valid account dates are ",c("03-15 ","06-30 ","09-30 ","12-31")," for each year\n")
      stop()
    }
    
    url <- paste("http://stockdata.stock.hexun.com/2008/zcfz.aspx?stockid=",stockid,
                 "&accountdate=",gsub("-","\\.",as.character(date)),sep="")
    doc <- htmlParse(url)
    tb <- readHTMLTable(doc,which=3)             
  }
  
  # module: whether replace the default string("--") for missing values on hexun.com
  # with blank
  if(na != F){
    na <- ifelse(na == T,"",na)
    tb <- sapply(tb,gsub,pattern="--",replacement=na)
  }
  
  # module: whether convert "tb" to data.frame
  if(todataframe){
    nam <- tb[,1]
    tb <- as.data.frame(t(as.data.frame(tb[,2:ncol(tb)])))
    names(tb) <- nam
    rownames(tb) <- NULL
  } 
  
  return(tb)
}