

wiki_to_md <- function(wiki_file){
  require(tidyverse)
  require(glue)
  x <- read_lines(wiki_file)
  new <- x %>% 
# Step 1: Fix Headers ----------------------------------------------------------
  str_replace_all("^={5}", "##### ") %>% 
  str_remove_all( "={5}$")          %>% 
  str_replace_all("^={4}", "#### ") %>% 
  str_remove_all( "={4}$")          %>% 
  str_replace_all("^={3}", "### ")  %>% 
  str_remove_all( "={3}$")          %>% 
  str_replace_all("^={2}", "## ")   %>% 
  str_remove_all( "={2}$")          %>% 
# Step 2: Fix bullet points ----------------------------------------------------
  str_replace_all("^\\:\\*\\s", "- ") %>% 
  str_replace_all("^\\:\\*{2}\\s", "  + ") %>% 
# Step 3: Fix Bold & Italics ---------------------------------------------------
  str_replace_all("'{3}", "**")     %>% 
  str_replace_all("'{2}", "*")      %>% 
  str_replace_all("^:{2}\*", "- ")  %>% 
  str_replace_all("^:{2}", "- ")    %>% 
  str_remove_all("^:{1}")           %>% 
  str_replace_all("^\\[\\[(File):", '<img src="./Images/') %>%
  str_replace_all("\\|(link).*\\]\\]", '">') %>%
  str_replace_all("\\|\\d*(px)\\|\\w*\\|\\w*\\=\\]\\]", '">') %>% 
  str_replace_all("\\<div style.*", glue::glue('<a href="javascript:showhide(', "'Q2')",
                                         '"><span style="font-size:8pt;">Show/Hide Solution</span></a>')) %>%
  str_replace_all('\\<div class="mw\\-.*', '<div id="Q2" style="display:none;">') %>% 
  str_replace_all("\\<\\w*\\>\\$\\<\\/\\w*\\>", '?') %>% 
  #str_replace_all("\\s\\?", "\\$") %>% 
  str_replace("toc\\_float:\\strue$", "toc_float: false") %>% 
  str_replace_all("\\<(span class)\\=(Link)\\>\\[\\[(Data)\\|(\\w*)\\]\\]\\<\\/(span)\\>",
                  "[\\4](./Data/\\4.xlsx)")
  new_file_name <- paste0("converted", wiki_file)
  write_lines(new, new_file_name)
  
}



# files <- list.files(pattern = "Lesson\\d{1,2}(.Rmd)")
# files_list <- map(files, read_lines)
# converted_files <- map(files_list[2:22], wiki_to_md)
# map(2:22, ~ write_lines(converted_files, paste0("Lesson", ., "_converted.Rmd")))
