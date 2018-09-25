library(tidyverse)



wds <- read_lines("Lesson24.Rmd")

wds_new <- wds %>% 
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
  str_replace_all("^\\[\\[(File):", '<img src="./Images/') %>% 
  str_replace_all("\\|(link).*\\]\\]$", '">') %>% 
  str_replace_all("\\<div style.*", '<a href="javascript:showhide("Q2")"><span style="font-size:8pt;">Show/Hide Solution</span></a>') %>% 
  str_replace_all('\\<div class="mw\\-.*', '<div id="Q2" style="display:none;">') %>% 
  str_replace_all('showhide\\("Q2"',"showhide('Q2'")


write_lines(wds_new, "Lesson24_conv.Rmd")
