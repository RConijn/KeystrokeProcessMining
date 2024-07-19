#ms to hh:mm:ss.SSS format

library(lubridate)
library(tidyverse)

pause <- read_delim(file = "data_in/actionlog_pauses_2sec_nofinal_readingcomb_textsplit_withprofilesandscores.csv",
                  delim = ";") %>% 
  select(-`...1`)

pause2 <- pause %>%
  mutate(time_start = format(as.POSIXct(time_start / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"),
         time_end = format(as.POSIXct(time_end / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"))

write_csv(pause2, "data_out/actionlog_pauses_2sec_nofinal_readingcomb_textsplit_withprofilesandscores.csv")

pause_act <- read_delim(file = "data_in/actionlog_pauses_activity_nofinal_readingcomb_textsplit_withprofilesandscores.csv",
                    delim = ";") %>% 
  select(-`...1`)

pauseact2 <- pause_act %>%
  mutate(time_start = format(as.POSIXct(time_start / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"),
         time_end = format(as.POSIXct(time_end / 1000, "UTC", origin = "1970-01-01"), "%H:%M:%OS3"))

write_csv(pauseact2, "data_out/actionlog_pauses_activity_nofinal_readingcomb_textsplit_withprofilesandscores.csv")


