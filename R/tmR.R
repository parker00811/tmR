writeUtf8 <- function(x, file, bom=F) {
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}