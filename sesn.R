# Построение графика изменения количества сеансов по данным технологического журнала (событие SESN)

library(data.table)
library(stringi)
library(ggplot2)
library(scales)
library(xlsx)

files <- list.files(path = "D:/logs_/sesn", pattern = ".*\\.log$", full.names = TRUE, recursive = TRUE)

sesn_regex <- "^.*SESN.*(Func=Start|Func=Finish).*Appl=1CV8C.*"
func_regex <- "^(\\d{2}:\\d{2}).*((?<=Func=).*?(?=,)).*((?<=IB=).*?(?=,))"

sesn_dt <- rbindlist(
  lapply(files, function(file) {
    lines <- readLines(file, encoding="UTF-8")
    matches <- stri_subset(lines, regex = sesn_regex)
    dt <- data.table(stri_match_first(matches, regex = func_regex)[,c(2,3,4)])
    dt$date <- paste(stri_extract_first(file, regex = "\\d{8}(?=\\.log)"), dt$V1, sep="")
    dt$date <- as.POSIXct(dt$date, tryFormats = c("%y%m%d%H%M:%OS"))
    dt
  }),
  fill=TRUE
)

sesn_dt[, V1:=NULL]
colnames(sesn_dt) <- c("func", "ib", "date")

# Исходные данные по количеству сеансов (на начало периода) можно загружать из файла
# В противном случае предполагается, что на начало периода сеансов нет
#actual_sesn_df <- data.frame(ib=character(), num=integer())
actual_sesn_df <- read.xlsx('D:/actual_sesn_df.xlsx', 1)

total <- sum(actual_sesn_df$num)

sesn_f <- function(ib, func) {
   v <- actual_sesn_df[actual_sesn_df$ib == ib,]
   if (nrow(v) == 0) {
     num = ifelse(func == "Finish", -1, 1)
     ev <- data.frame(ib=ib, num=num)
     actual_sesn_df <<- rbind(actual_sesn_df, ev)
   } else {
     num = v$num + ifelse(func == "Finish", -1, 1)
     set(actual_sesn_df, i = which(actual_sesn_df$ib == ib), j = "num", value = num)
   }
   return(num)
}

sesn_dt <- sesn_dt[which(date >= as.POSIXct("2019-09-30 00:00") & date <= as.POSIXct("2019-09-30 23:59"))]

sesn_dt <- sesn_dt[order(date),] 

sesn_dt[, sesn := 0]
sesn_dt[, total := 0]

for(row in 1:nrow(sesn_dt)) {
  set(sesn_dt, i = row, j = "sesn", value = sesn_f(unlist(sesn_dt[row, "ib"]), unlist(sesn_dt[row, "func"])))

  total <- ifelse(sesn_dt[row, "func"] == "Finish", total - 1, total + 1)
  set(sesn_dt, i = row, j = "total", value = total)
}

p <- ggplot() +
  geom_line(sesn_dt, mapping = aes(x=date, y=sesn, group=ib, colour=ib)) +
  geom_line(sesn_dt, mapping = aes(x=date, y=total), size=1) +
  scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H:%M",  tz="Europe/Moscow")) +
  labs(title = "График изменения количества сеансов за 30.09.2019", y = "Количество сеансов", x = NULL, color="ИБ")
  
print(p)

