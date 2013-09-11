###################################################################
# Get financial data of publicly listed firms from hexun.com
#
# Required additional packages: XML
# Author: ZENG YE
# Mailto: zeno@zengye.name
# Version: demo
# Release date: 2013-09-11
# Comments: 
#
# values for the argument "type": the argument "type" specifies
# the sub-category of financial data. Details are as listed in
# the following:
#   zxcwzb: 最新财务 / Latest Financial Data in English
#   cwbl: 财务比率 / Latest Financial Ratios in English
#   zcfz: 资产负债 / Assets and Liabilities
#   lr: 利润 / Profit
#   xjll: 现金流量 / Cash Flow
#   zysrfb: 主营收入 / Sales Revenue
#   zcjz: 资产减值 / Impairment of Assests
#   yszk: 应收账款 / Account Receivable
#   qtyszk: 其他应收账款 / Other Account Receivable
#   en: Latest Financial Data in English
#
####################################################################

source("/mnt/windows/project/r/hexundata/getzxcwzb.R")

getHexunFin <- function(stockid,type="zxcwzb",...){
  if(!is.vector(stockid) || !is.vector(type) || length(stockid)*length(type) == 0){
    cat("Error: argument stock and type must be a non empty vector!\n")
    stop()
  }
  
  stockid <- as.character(stockid)
  type <- as.character(type)
  
  # all available values for argument "type"
  type_avlb <- c("zxcwzb","cwbl","zcfz","lr","xjll","zysrfb","zcjz","yszk","qtyszk","en")
  if(!all(type %in% type_avlb)){
    cat("Error: arugment type can only have the following values:\n",type_avlb,"\n")    
    stop()
  }
  
  if(any(nchar(stockid) < 6)){
    cat("Error: the following ID has length less than 6:\n",stockid[which(nchar(stockid) < 6)],"\n")
    stop()  
  }
  
  stockfin <- list()
  for (i in 1:length(stockid))
    for(j in 1:length(type)){
      stockfin[[stockid[i]]] <- list()
      stockfin[[stockid[i]]][[type[j]]] <- get(paste("get",type[j],sep=""))(stockid[i],...)
    }
  
  return(stockfin)
}