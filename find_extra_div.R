library(tidyverse)
x <- read_lines("convertedLesson12.Rmd")

tibble(start   = str_count(x, "<div "), 
           end = str_count(x, "</div>")) %>% 
  mutate(cs_start = cumsum(start), 
         cs_end   = cumsum(end), 
         is_extra = cs_start < cs_end) %>% 
  mutate(row = row_number()) %>% 
  filter(is_extra) %>% 
  slice(1) %>% 
  {.$row}
