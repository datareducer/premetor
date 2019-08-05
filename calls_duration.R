# Скрипт группирует серверные вызовы по контексту

library(data.table)
library(stringi)

files <- list.files("D:/logs_", full.names = TRUE, recursive = TRUE)

ib_name <- "erpwork"

calls_dt <- rbindlist(
  lapply(files, function(f) {
    lines <- readLines(f, encoding="UTF-8")
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




