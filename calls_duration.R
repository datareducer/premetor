# Скрипт группирует серверные вызовы по контексту

library(data.table)
library(stringi)

files <- list.files("/home/sull/1c_logs_test", full.names = TRUE, recursive = TRUE)

ib_name <- "erpwork"

calls_dt <- rbindlist(
  lapply(files, function(f) {
    lines <- readLines(f)
    matches <- stri_subset(lines, regex = paste("^.*CALL.*p:processName=", ib_name, ".*Context=.*", sep=''))
    data.table(
      stri_match_first(
        matches,
        regex = "^.{13}(\\d*(?=,)).*((?<=Context=).*?(?=,)){1}"
      )[,c(2,3)]
    )
  }),
  fill=TRUE
)

colnames(calls_dt) <- c("dur", "context")
calls_dt$dur <- as.numeric(calls_dt$dur)

calls_dt <- calls_dt[,list(count=length(dur), duration_avg=mean(dur), duration_sum=sum(dur)), by=context]




