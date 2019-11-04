# Анализ замеров времени выполнения ключевых операций

library(XML)
library(data.table)
library(ggplot2)
library(scales)
library(plyr)

files <- list.files(path = "~/1c_perfomance", pattern = ".*\\.xml$", full.names = TRUE, recursive = TRUE)

xml2dt <- function(file) {
  xml <- xmlParse(file)
  nsps = xmlNamespaceDefinitions(xml, simplify = TRUE);
  ns <- getNodeSet(xml, '//prf:KeyOperation', namespaces = nsps)
  
  result <- rbindlist(lapply(ns, function(x) {
    op <- xpathSApply(x, ".", xmlAttrs, namespaces = nsps);
    Encoding(op) <- "UTF-8"; 
    op <- data.frame(t(op), stringsAsFactors = FALSE);
    
    me <- xpathSApply(x, ".//prf:measurement", xmlAttrs, namespaces = nsps);
    Encoding(me) <- "UTF-8";
    me <- data.frame(t(me), stringsAsFactors = FALSE);
    
    cbind(op, me);
  }))
  
  return(result);
}

meas_dt <- rbindlist(
  lapply(files, xml2dt)
)

meas_dt$tSaveUTC <- as.POSIXct(meas_dt$tSaveUTC, tz="GMT", format="%Y-%m-%dT%H:%M:%S")
meas_dt$value <- as.double(meas_dt$value)

# Объединяем однотипные операции
meas_dt[which(name == "Провести. документ. внутреннее потребление товаров. форма. форма документа" |
                name == "Провести и закрыть. документ. внутреннее потребление товаров. форма. форма документа")]$name <- "Проведение документа внутреннее потребление товаров";

meas_dt[which(name == "Провести. документ. заказ поставщику. форма. форма документа" |
                name == "Провести и закрыть. документ. заказ поставщику. форма. форма документа")]$name <- "Проведение документа заказ поставщику";

meas_dt[which(name == "Провести. документ. заявка на расходование денежных средств. форма. форма документа" |
                name == "Провести и закрыть. документ. заявка на расходование денежных средств. форма. форма документа")]$name <- "Проведение документа заявка на расходование денежных средств";

meas_dt[which(name == "Провести. документ. перемещение товаров. форма. форма документа" |
                name == "Провести и закрыть. документ. перемещение товаров. форма. форма документа")]$name <- "Проведение документа перемещение товаров";

meas_dt[which(name == "Провести. документ. приобретение товаров услуг. форма. форма документа" |
                name == "Провести и закрыть. документ. приобретение товаров услуг. форма. форма документа")]$name <- "Проведение документа приобретение товаров услуг";


### Строим диаграмму времени выполнения ключевых операций в течение дня

meas1 <- meas_dt[which(tSaveUTC > as.POSIXct("2019-08-21") & tSaveUTC < as.POSIXct("2019-08-22"))]

# Исключаем операции с малым количеством выполнений
meas1 <- meas1[, ':='( count = .N ), by = name]
meas1 <- meas1[which(count > 50)]

pl1 <- ggplot(meas1, aes(x=tSaveUTC, y=value, group=name, colour=name)) +
       geom_point(size=1) +
       scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H:%M",  tz="Europe/Moscow")) +
       geom_smooth(span = 0.1, method="loess", show.legend=FALSE, method.args=list(degree=1), level=0.6, size=0.8, alpha=0.2) +
       theme(legend.title = element_text(size=11, color = "salmon", face="bold"),
          legend.justification=c(0, 1), 
          legend.position=c(0, 1),  
          legend.background = element_blank()) +
       labs(title = "Время выполения ключевых операций в течение 21.08.2019", x = "", y = "Время выполнения", color = "Ключевые операции") 
      
pl1

### Строим кривые плотности распределения времени выполнения ключевых операций (сравниваем два периода)

meas2 <- meas_dt[which(tSaveUTC > as.POSIXct("2019-08-20") & tSaveUTC < as.POSIXct("2019-08-22"))]

# Исключаем операции с малым количеством выполнений
meas2 <- meas2[, ':='( count = .N ), by = name]
meas2 <- meas2[which(count > 80)]

meas2$Period[meas2$tSaveUTC > as.POSIXct("2019-08-20") & meas2$tSaveUTC < as.POSIXct("2019-08-21")] <- "2019-08-20"
meas2$Period[meas2$tSaveUTC > as.POSIXct("2019-08-21") & meas2$tSaveUTC < as.POSIXct("2019-08-22")] <- "2019-08-21"

means <- ddply(meas2, c("Period", "name"), summarise, grp.mean=mean(value))

pl2 <- ggplot(meas2, aes(x=value, colour=Period, fill=Period)) +
       geom_density(adjust=0.8, alpha=0.2)+
       geom_vline(data=means, aes(xintercept=grp.mean, colour=Period), linetype="dashed") +
       facet_wrap(facets = ~name, labeller = label_wrap_gen(width=40)) + 
       xlim(0,40) +
       labs(title="Сравнение времени выполнения ключевых операций за два периода", x="Время выполнения", y = "Плотность распределения", color="Период") +       
       scale_fill_discrete(guide=FALSE) # иначе легенда дублируется

pl2

