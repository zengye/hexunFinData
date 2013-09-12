###############################################################
# Get financial ratios of publicly listed firms from hexun.com
#
# Required Packages: XML
# Author: ZENG YE
# Mailto: zeno@zengye.name
# Version: demo
# Release Date: 2013-09-12
# Comments:
#
# Arguments
#   sep.list: a logical value indicating whether to separate the
#             raw table parsed into seven sub-dataframe and store
#             them in a list. The parts include:
#               zwzc --- 债务状况
#               hlnl --- 获利能力
#               yynl --- 运营能力
#               cwnl --- 财务能力
#               cznl --- 成长能力
#               xjll --- 现金流量
#               dgzb --- 单股指标
#################################################################

getcwbl <- function(stockid,date="latest",todataframe=F,na=F,sep.list=T){
  # attach the package XML if it is not in the search path
  if(!("package:XML" %in% search()))
    library(XML)
  
  url <- paste("http://stockdata.stock.hexun.com/2009_cwbl_",stockid,".shtml",sep="")
  
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
    
    url <- paste("http://stockdata.stock.hexun.com/2008/cwbl.aspx?stockid=",stockid,
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
  
  # module: whether to separate the raw table into seven parts
  if(sep.list){
    subtbnam <- c("债务状况","获利能力","运营能力","财务能力","成长能力","现金流量","单股指标")
    lstnam <- c("cwzk","hlnl","yynl","cwnl","cznl","xjll","dgzb")
    
    if(todataframe){
      loc <- which(colnames(tb) %in% subtbnam)
      
      tmplst <- list()
      for(k in 1:6)
        tmplst[[lstnam[k]]] <- tb[,c(1,(loc[k]+2):(loc[k+1]-2))]
      tmplst[[lstnam[7]]] <- tb[,c(1,(loc[7]+2):ncol(tb))]
      
    } else{
      loc <- which(tb[,1] %in% subtbnam)
      
      tmplst <- list()
      for(k in 1:6)
        tmplst[[lstnam[k]]] <- tb[c(1,(loc[k]+2):(loc[k+1]-2)),]
      tmplst[[lstnam[7]]] <- tb[c(1,(loc[7]+2):nrow(tb)),]
    }
    
    tb <- tmplst
  }
  
  return(tb)
}