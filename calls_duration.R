# Скрипт группирует серверные вызовы по контексту

library(data.table)
library(stringi)
library(xlsx)

files <- list.files(path = "D:/logs_/calls", pattern = ".*\\.log$", full.names = TRUE, recursive = TRUE)

ib_name <- "erp"

call_regex <- paste("^.*CALL.*p:processName=", ib_name, ",.*Context=.*", sep='')
context_regex <- "^.{13}(\\d*(?=,)).*((?<=Context=).*?(?=,)){1}"

calls_dt <- rbindlist(
  lapply(files, function(f) {
    lines <- readLines(f, encoding="UTF-8")
    matches <- stri_subset(lines, regex = call_regex)
    data.table(stri_match_first(matches, regex = context_regex)[,c(2,3)])
  }),
  fill=TRUE
)

colnames(calls_dt) <- c("dur", "context")
calls_dt$dur <- as.numeric(calls_dt$dur)

calls_dt <- calls_dt[,list(count=length(dur), duration_avg=mean(dur), duration_sum=sum(dur)), by=context]

write.xlsx(calls_dt, "D:/calls_duration.xlsx")
