writeUtf8 <- function(x, file, bom=F) {
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}


Geocoding <- function (address = "台灣竹南鎮科北五路二號") {
  require(RCurl)
  require(RJSONIO)
  
  # if the address is big5, translate it to utf8
  tmp <- iconv(address, "big5", "utf8")
  if(!is.na(tmp)){
    address <- tmp
  }
  
  address <- URLencode(address)
  url <- sprintf("http://maps.googleapis.com/maps/api/geocode/json?language=zh-TW&address=%s", address)
  geoCode <- getURL(url)
  geoCode <- fromJSON(geoCode)
  geoCode <- data.frame(lat = geoCode$results[[1]]$geometry$location[["lat"]],
                        lng = geoCode$results[[1]]$geometry$location[["lng"]],
                        searchAddress = geoCode$results[[1]]$formatted_address)
  return(geoCode)
}