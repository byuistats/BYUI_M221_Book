# How to Update and Use this Folder

In this folder are six primary folders:
  *current file
  *excel_data_original
  *homework
  *images
  *preparation
  *and scripts

**Current file**
In this folder are all the pdf, doc, and html files for each lesson.  The files
are separated into homework, homework_answer, prepartion, and prepartion_answer.
You DO NOT update the files in the current_files folder, this will be done using a 
different file.

**excel_data_original**
This folder holds excel files are called on.

**homework**
Here is where all the rmd files are for homework are stored.  The questions and 
text are written in the top half, and then in the bottom r-chuck, is where you 
write the solutions.

**images**
Where images are stored.

**preparation**
All preparation rmd files are here and are formated the same way as the homework.

**scripts**
The folder holds important r scripts that aid in the creating files.

###
Steps to Changing/Updating Folders
###

1. Open up the the assignment that needs changed - whether it be in the homework 
or preparation folder.  

2. Inside the rmd file, make the changes.
  A. Make sure that if you add a question, you add a solution in the solution
  chunk at the bottom.
  B. Also beware if you changed the order of the questions (1,2,3,4)
  
3. Save the file

4. Open up the compile_scipts.R file.

5. Run the function and code near top.

6. You will need to change the setwd at the bottom for your computer.

7. Go down to the lesson number that needs changed and the line that applies
  to the changes (prep or homework).
  A. What this is doing is running the rmd files that you changed and then
  placing the changed files in the correct lesson in the current_files.
  
8. After that, you can go check to see of the new files in that lesson are correct.

9. Lastly, push to GitHub.

10.  I would be wise to write down the data and the file changes you made at the
  bottom of the compile_scripts.R file so that future people know what and when
 this file was last used.
 
 
Good luck!
  -Asher Hanson
