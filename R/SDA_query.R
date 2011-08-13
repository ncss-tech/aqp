## why doesn't this work ???
# p <- processWSDL('http://sdmdataaccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx?WSDL')

# clean-up results from SDA SOAP query, and return as DF
cleanSDA <- function(i) 
  {
  # remove left-overs from SOAP result
  i$.attrs <- NULL
  # convert NULL in NA
  i[which(sapply(i, is.null))] <- NA
  # convert list to DF
  as.data.frame(i)
  }

SDA_query <- function(q)
  {
  # check for required packages
  if(!require(SSOAP))
    stop('please install the `SSOAP` package')
    
  # setup server, action, and xmlns
  s <- SOAPServer('SDMDataAccess.nrcs.usda.gov', '/Tabular/SDMTabularService.asmx')
  a <- I('http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery')
  x <- c(I("http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx"))
  
  # feedback:
  print('sending SOAP request...')
  
  # submit and process the query
  res <- .SOAP(s, "RunQuery", Query=q, action=a, xmlns=x)
  
  # results are stored in:
  # res$diffgram$NewDataSet
  
  # clean the results, convert to DF
  df <- ldply(res$diffgram$NewDataSet, .fun=cleanSDA, .progress='text')
  df$.id <- NULL
  
  # done
  return(df)
  }





