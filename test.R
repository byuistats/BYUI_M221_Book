

wiki_to_md <- function(wiki_file){
  library(tidyverse)
  library(glue)
  x <- read_lines(wiki_file)
  new <- x %>% 
  str_replace_all("^={5}", "##### ") %>% 
  str_remove_all( "={5}$")          %>% 
  str_replace_all("^={4}", "#### ") %>% 
  str_remove_all( "={4}$")          %>% 
  str_replace_all("^={3}", "### ")  %>% 
  str_remove_all( "={3}$")          %>% 
  str_replace_all("^={2}", "## ")   %>% 
  str_remove_all( "={2}$")          %>% 
  str_replace_all("'{3}", "**")     %>% 
  str_replace_all("'{2}", "*")      %>% 
  str_replace_all("^:{2}", "* ")      %>% 
  str_remove_all( "^:{1}")           %>% 
  str_replace_all("^\\[\\[(File):", '<img src="./Images/') %>%
  str_replace_all("\\|(link).*\\]\\]$", '">') %>%
  str_replace_all("\\<div style.*", glue('<a href="javascript:showhide(', "'Q2')",'"><span style="font-size:8pt;">Show/Hide Solution</span></a>')) %>%
  str_replace_all('\\<div class="mw\\-.*', '<div id="Q2" style="display:none;">')
  new_file_name <- paste0("converted", wiki_file)
  write_lines(new, new_file_name)
  remove(x,new)
}

# files <- list.files(pattern = "Lesson\\d{1,2}(.Rmd)")
# files_list <- map(files, read_lines)
# converted_files <- map(files_list[2:22], wiki_to_md)
# map(2:22, ~ write_lines(converted_files, paste0("Lesson", ., "_converted.Rmd")))
