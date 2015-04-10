writeUtf8 <- function(x, file, bom=F) {
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}


Geocoding <- function (address, output = "json", lang = "zh-TW") {
  # Format can be json or xml
  require(RCurl)
  require(RJSONIO)
  
  # Args:
  #   address: The street address that you want to geocode, in the format used by the 
  #            national postal service of the country concerned. 
  #   output: where output may be either of the following values.
  #     "json" (recommended) indicates output in JavaScript Object Notation (JSON).
  #     "xml" indicates output as XML.
  #   lang: The language in which to return results. 
  #     see https://developers.google.com/maps/faq#languagesupport
  
  #   Returns:
  #     The latitude, longitude and searching address by data frame.
  
  tmp <- iconv(address, "big5", "utf8")
  if(!is.na(tmp)){
    address <- tmp
  }
  
  address <- URLencode(address)
  url <- sprintf("http://maps.googleapis.com/maps/api/geocode/%s?language=%s&address=%s", 
                 output, lang, address)
  
  geoCode <- getURL(url)
  geoCode <- fromJSON(geoCode)
  geoCode <- data.frame(lat = geoCode$results[[1]]$geometry$location[["lat"]],
                        lng = geoCode$results[[1]]$geometry$location[["lng"]],
                        searchAddress = geoCode$results[[1]]$formatted_address)
  return(geoCode)
}