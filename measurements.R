# Анализ замеров времени выполнения ключевых операций

library(XML)
library(data.table)

file <- '~/1c_perfomance/2019-08-18 16-18-17-00001.xml'

xml <- xmlParse(file)

nsps = xmlNamespaceDefinitions(xml, simplify = TRUE);

ns <- getNodeSet(xml, '//prf:KeyOperation', namespaces = nsps)

meas_dt <- rbindlist(lapply(ns, function(x) {
  op <- data.frame(t(xpathSApply(x, ".", xmlAttrs, namespaces = nsps )))
  me <- data.frame(t(xpathSApply(x, ".//prf:measurement", xmlAttrs, namespaces = nsps )))
  cbind(op, me)
}))













