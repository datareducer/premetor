# Анализ замеров времени выполнения ключевых операций

library(XML)
library(data.table)

files <- list.files(path = "~/1c_perfomance", pattern = ".*\\.xml$", full.names = TRUE, recursive = TRUE)

xml2dt <- function(file) {
  xml <- xmlParse(file)
  nsps = xmlNamespaceDefinitions(xml, simplify = TRUE);
  ns <- getNodeSet(xml, '//prf:KeyOperation', namespaces = nsps)
  
  result <- rbindlist(lapply(ns, function(x) {
    op <- xpathSApply(x, ".", xmlAttrs, namespaces = nsps);
    # Требуется установка кодировки, её указание в параметре xpathSApply не решает проблему:
    Encoding(op) <- "UTF-8"; 
    op <- data.frame(t(op));
    
    me <- xpathSApply(x, ".//prf:measurement", xmlAttrs, namespaces = nsps);
    Encoding(me) <- "UTF-8";
    me <- data.frame(t(me));
    
    cbind(op, me);
  }))
  
  return(result);
}

meas_dt <- rbindlist(
  lapply(files, xml2dt)
)













