#### Compile Math 221 Homework  ######
# installing/loading the package:
#   if(!require(installr)) { install.packages("installr"); require(installr)} #load / install+load installr
# 
# # Installing pandoc
# install.pandoc()
library(reshape2)
library(rmarkdown)
library(tidyverse)
library(tinytex)
library(mosaic)


#### functions for converstion #####
names_table <- data.frame(
  fname_part1 = c("Preparation", "HW"),
  assignment = c("Assignment", "Assignment"),
  key = c("Answer_Key", "Answer_Key"),
  worked = c("Worked_Solutions", "Worked_Solutions")
  , stringsAsFactors = FALSE)

row.names(names_table) <- c("preparation", "homework")


convert_M221 = function(
  paths, 
  output_folder = "current_files",
  key = FALSE,
  prep_hw = NULL,
  mapping_table = names_table) {
  rm(list = ls()[!ls() %in%
                   c("paths", "output_folder", "key", "prep_hw", "mapping_table")])
  
  for (i in 1:length(paths)) {
    pathi <- paths[i]
    path_details <- unlist(strsplit(pathi, "/"))
    
    if (is.null(prep_hw) == TRUE) prep_hw = path_details[i]
    
    fnamei <- path_details[length(path_details)] 
    lessonNumberi <- gsub("L", "", unlist(strsplit(fnamei, "_"))[1])
    outname_keyi <- paste(lessonNumberi,
                          paste(mapping_table[prep_hw,
                                              c("fname_part1", "key")], collapse = "_"), sep = "_")
    outname_assignmenti <- paste(lessonNumberi,
                                 paste(mapping_table[prep_hw,
                                                     c("fname_part1", "assignment")], collapse = "_"), sep = "_")
    
    for (j in c("A", "B", "C")) {
      
      class_out_key <- paste(outname_keyi, j, sep = "_")
      class_out_assignment <- paste(outname_assignmenti, j, sep = "_")
      
      # key:  can't write out a generic output file so it needs the
      # full file name with extension plus the format
      rmarkdown::render(pathi,
                        output_dir = file.path(output_folder,
                                               paste("L", lessonNumberi, sep = "")),
                        output_file = paste(class_out_key, ".html", sep = ""),
                        output_format = c("html_document"),
                        params = list(key = TRUE, plinks = TRUE,
                                      keyname = paste(outname_keyi, j, sep = "_"),
                                      docname = paste(outname_assignmenti, j, sep = "_")))
      
      # rmarkdown::render(pathi, 
      #                   output_dir = file.path(output_folder,
      #                                          paste("L", lessonNumberi, sep = "")),
      #                   output_file = paste(class_out_key, ".pdf", sep = ""),
      #                   output_format = c("pdf_document"),
      #                   params = list(key = TRUE))
      
      rmarkdown::render(pathi, 
                        output_dir = file.path(output_folder,
                                               paste("L", lessonNumberi, sep = "")),
                        output_file = paste(class_out_key, ".docx", sep = ""),
                        output_format = c("word_document"),
                        params = list(key = TRUE))
      
      # assignment
      
      rmarkdown::render(pathi, 
                        output_dir = file.path(output_folder,
                                               paste("L", lessonNumberi, sep = "")),
                        output_file = paste(class_out_assignment, ".html", sep = ""),
                        output_format = c("html_document"),
                        params = list(key = FALSE, plinks = TRUE,
                                      keyname = paste(outname_keyi, j, sep = "_"),
                                      docname = paste(outname_assignmenti, j, sep = "_")))
      
      # rmarkdown::render(pathi,
      #                   output_dir = file.path(output_folder,
      #                                          paste("L", lessonNumberi, sep = "")),
      #                   output_file = paste(class_out_assignment, ".pdf", sep = ""),
      #                   output_format = c("pdf_document"),
      #                   params = list(key = FALSE))
      
      rmarkdown::render(pathi,
                        output_dir = file.path(output_folder,
                                               paste("L", lessonNumberi, sep = "")),
                        output_file = paste(class_out_assignment, ".docx", sep = ""),
                        output_format = c("word_document"),
                        params = list(key = FALSE))
    } # end j
    rm(list = ls()[!ls() %in%
                     c("paths", "output_folder", "key", "prep_hw", "mapping_table")])
  } # end i
} # end function

### The working directory where the homework_prep folder is stored within the repo.
setwd("homework_prep_build")

#### File names need to be updated

prep_files <- list.files(
  file.path("preparation"),
  pattern = "rmd",
  ignore.case = TRUE,
  full.names = TRUE)

hw_files <- list.files(
  file.path("homework"),
  pattern = "rmd",
  ignore.case = TRUE,
  full.names = TRUE)


### January 26, 2021
## Fixed errors in the lessons 1,3, and 4 Homework and Prep-Work

#1
convert_M221(prep_files[1])
convert_M221(hw_files[1])

#2
convert_M221(prep_files[2])
convert_M221(hw_files[2])

#3
convert_M221(prep_files[3])
convert_M221(hw_files[3])

#4
convert_M221(prep_files[4])
convert_M221(hw_files[4])

#5
convert_M221(prep_files[5])
convert_M221(hw_files[5])

#6
convert_M221(prep_files[6])
convert_M221(hw_files[6])

#7
convert_M221(prep_files[7])
convert_M221(hw_files[7])

#There is no L08

#9
convert_M221(prep_files[8])
convert_M221(hw_files[8])

#10
convert_M221(prep_files[9])
convert_M221(hw_files[9])

#11
convert_M221(prep_files[10])
convert_M221(hw_files[10])

#12
convert_M221(prep_files[11])
convert_M221(hw_files[11])

#13
convert_M221(prep_files[12])
convert_M221(hw_files[12])

#14
convert_M221(prep_files[13])
convert_M221(hw_files[13])

#There is something weird about #14
---
  #There is no lesson 15  
  
  #16
  convert_M221(prep_files[14])
convert_M221(hw_files[14])

#17
convert_M221(prep_files[15])
convert_M221(hw_files[15])

#18
convert_M221(prep_files[16])
convert_M221(hw_files[16])

#19
convert_M221(prep_files[17])
convert_M221(hw_files[17])
---
  #There is no Lesson 20
  
  #21
  convert_M221(prep_files[18])
convert_M221(hw_files[18])

#22
convert_M221(prep_files[19])
convert_M221(hw_files[19])

#23
convert_M221(prep_files[20])
convert_M221(hw_files[20])

# requires that your working directory setwd("homework_prep_build").
R.utils::copyDirectory('current_files', '../hp', overwrite = TRUE) 

# rmarkdown::render_site("../") 

# for bug on 10/7/2016 with not reporting numbers for problem 10.
# convert_M221(prep_files[11])
# convert_M221(prep_files[15])
# convert_M221(hw_files[15])
