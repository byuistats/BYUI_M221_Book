#for some reason I have two getGMT functions, both do the same thing.  I should consilidate.  Update the getGMT function in both createDropbox and CreateContent to deal with DayLightSavings time issues.


fullFile <- function(base=".",name){paste(base,"/",name,sep="")} #Used to set all other directories off the base 

# If the HTML and text entries are not empty, we have to strip some parts of the code. So far I know I have to get rid of <, >, &.  Not sure of what else.  
reformatHTML <- function(html){
  html <- gsub("&","&amp;",html)
  html <- gsub("<","&lt;",html)
  html <- gsub(">","&gt;",html)    
  return(html)
}

maindir <- "/home/bmwdruff/Desktop/StatsCourses2015"
outputdir <- fullFile(maindir,"output")
supportdir <- fullFile(maindir,"support")

unlink(outputdir,recursive=TRUE)
dir.create(outputdir,recursive=TRUE)
setwd(maindir)

source("createQuizzes.R")
source("createGradeBook.R")
source("createContent.R")
source("createDropbox.R")
source("createDiscussions.R")


#' This command grabs a sheet from the web, and then builds the coures from it. 
#' You have to make the google doc sheet public, with at least anyone on web with link. 
#' Then we can use the RCurl package to grab the csv from the web. 


build_imsmanifest_from_CSV<-function(gsheet,outdir=outputdir,startingRow=19,Use_Question_Library=FALSE){
  #The CSV file contains a Type column that sorts bewteen quiz,drop,disc,file,link,start,stop.
  df<-read.csv(filename,colClasses="character")
  build_imsmanifest(df,outdir,startingRow,Use_Question_Library)
#  if(Use_Question_Library==TRUE){file.copy("questiondb.xml",outdir)}
}

build_imsmanifest_from_GoogleDocs<-function(gsheet,outdir=outputdir,startingRow=19,Use_Question_Library=FALSE){
  require(RCurl)
  fileUrl <- paste0("https://docs.google.com/spreadsheets/d/",gsheet,"/export?format=csv")
  fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
  df <-  read.csv(textConnection(fileCSV),colClasses="character")
  build_imsmanifest(df,outdir,startingRow,Use_Question_Library)  
#  if(Use_Question_Library==TRUE){file.copy("questiondb.xml",outdir)}
}

build_imsmanifest<-function(df,outdir=".",startingRow=19,Use_Question_Library=FALSE){
  
  #Remove the top ??? rows of instructions. Subtract 1 from the spreadsheet, as the top row is column names. 
  #Currently this value is 19 in the sheet.  It may change later. 
  startRow<-startingRow-1
  df<-df[seq(startRow,nrow(df)),]
  
  require(XML)
  myroot <- NULL
  if(Use_Question_Library==FALSE){}
  else if( file.exists(fullFile(supportdir,"questiondb.xml"))==TRUE ) {
    xmlfile<- xmlParse(fullFile(supportdir,"questiondb.xml"))
    myroot <- xmlRoot(xmlfile)[[1]]
  } else {
    print("No questiondb.xml file found in this folder.  Cannot load quiz questions.");
  }
  
  
  
  #Content items. List the columns needed, in the right order, then make the content data.frame
  contentCols<-c(
    "Type","Title","Id","File_Link","Target","Is_Visible","Due_Date","Due_Time",
    "Start_Date","Start_Time","End_Date","End_Time","Content_Description")
  contentdf<-df[,contentCols] #Grabs just the content info. Needs to be passed to createContent()
  contentdf
  content<-createContent(contentdf)
  
  #Quiz items.  Same idea as content items.
  quizCols<-c(
    "Title", "Id", "Quiz_Category", "Quiz_Description", "Quiz_Show_Description", "Quiz_Introduction", "Quiz_Show_Introduction", 
    "Quiz_Page_Header", "Quiz_Show_Page_Header", "Quiz_Page_Footer", "Quiz_Show_Page_Footer", "Quiz_Allow_Hints", "Quiz_Disable_Right_Click", 
    "Quiz_Disable_Pager", "Quiz_Is_Active", "Start_Date", "Start_Time", "End_Date", "End_Time", 
    "Quiz_Display_In_Calendar", "Quiz_Password", "Quiz_Time_Limit", "Quiz_Show_Clock", "Quiz_Enforce_Time_Limit", "Quiz_Grace_Period", 
    "Quiz_Late_Limit_Type", "Quiz_Late_Limit_Data_Type", "Quiz_Auto_Grade", "Quiz_Include_In_Gradebook", "Quiz_Allow_Auto_Export", 
    "Quiz_Is_Forward_Only", "Quiz_Attempts_Allowed", "Quiz_Attempt_Restrictions", "Quiz_Mark_Calculation_Type", "Quiz_Feedback_Text", 
    "Quiz_Duration", "Quiz_Show_Questions_Type", "Quiz_Show_Correct_Answers", "Quiz_Show_Class_Average", 
    "Quiz_Show_Score_Distribution", "Quiz_Notify_Email","Quiz_Show_Solutions_After_Ends","Quiz_Library_Folders")
  quizdf<-df[df$Type=="quiz",]; quizdf<-quizdf[,quizCols]
  #quizdf<-df[df$Type=="quiz",]; quizdf<-quizdf[,c(c(3,4),seq(20,32),seq(10,13),seq(33,54))]
  quizresources<-buildQuizzesAndGetResourceXML(quizdf,outdir)
  surveydf<-df[df$Type=="survey",]; surveydf<-surveydf[,quizCols]
  #quizdf<-df[df$Type=="quiz",]; quizdf<-quizdf[,c(c(3,4),seq(20,32),seq(10,13),seq(33,54))]
  surveyresources<-buildSurveyAndGetResourceXML(surveydf,outdir)
  
  quizlibraryresources<-""
  if(Use_Question_Library==FALSE){}else{
    quizlibraryresources<-'<resource identifier="res_question_library" type="webcontent" d2l_2p0:material_type="d2lquestionlibrary" d2l_2p0:link_target="" href="questiondb.xml" title="Question Library" />'
  }
  #Dropbox items. Same idea. 
  dropCols<-c(
    "Title", "Id", "Due_Date", "Due_Time", "Start_Date", "Start_Time", 
    "End_Date", "End_Time", "Drop_Instructions", "Drop_Category", "Drop_Out_Of")
  dropdf<-df[df$Type=="drop",];  dropdf<-dropdf[,dropCols]
  #dropdf<-df[df$Type=="drop",];  dropdf<-dropdf[,c(c(3,4),seq(8,13),seq(16,18))]
  dropresources<-buildDropAndGetXML(dropdf,outdir)[1]
  
  #Gradebook items now. (id,(GB_Items)) and (GB_categories)
  #' This needs some work.  I need to strip the id off the items that are NOT connected to anything in the course.
  #' So I need to pass in the Quiz_Include_In_Gradebook and Dropbox_Out_Of parameters.
  #' Check these parameters, and if they are not set appropriately, then delete the id before passing itemdf to the function.
  #' Also, if it's not a quiz or dropbox type, I should strip the Id period.  Just do this.  
  #' So pass "Type", "Quiz_Include_In_Gradebook", "Dropbox_Out_Of", and then check these.  Then reset itemdf to remove these 3.
  #' Adjust the conditional formatting to show (1) non gradeable items and (2) quiz/drop items that aren't included.
  #' Make highlighting different for these items.
  
  itemCols<-c(
    "Id", "GB_Item_Name", "GB_Item_Short_Name", "GB_Item_Category_Id", "GB_Item_Description", 
    "GB_Item_Show_Description", "GB_Item_Max_Points", "GB_Item_Can_Exceed_Weight", 
    "GB_Item_Show_Average", "GB_Item_Show_Distribution", "GB_Item_Is_Active", "GB_Item_Is_Bonus")
  categoryCols<-c(
    "GB_Category_Name", "GB_Category_Short_Name", "GB_Category_Id", "GB_Category_Description",
    "GB_Category_Show_Description", "GB_Category_Weight", "GB_Category_Can_Exceed_Weight", 
    "GB_Category_Show_Average", "GB_Category_Show_Distribution", "GB_Category_Is_Active")
  itemdf <- df[df$GB_Item_Name!="",]; itemdf <- itemdf[,itemCols];
  categorydf <- df[df$GB_Category_Name!="",];  categorydf<-categorydf[,categoryCols];
  #itemdf <- df[df$GB_Item_Name!="",]; itemdf <- itemdf[,c(4,seq(56,66))];
  #categorydf <- df[df$GB_Category_Name!="",];  categorydf<-categorydf[,c(seq(68,77))];
  GBresources<-buildGradeBookAndGetGradeBookXML(itemdf,categorydf,outdir)[1]
  
  
  #Now we build the discussion boards and get the resource codes.   
  discdf<-df[df$Type=="disc" | df$Forum_Id!="",]
  discussionresources <- buildDiscussionsAndGetResourceXML(discdf,outdir)
  
  #add the syllabus_d2l.xml file, if it exists, and add it to the imsmanifest.   Just place it in the support folder, if you want it included.
  syllabusresources <- ""
  if(file.exists(fullFile(supportdir,"syllabus_d2l.xml"))){
    syllabusresources <- '<resource identifier="res_syllabus" type="webcontent" d2l_2p0:material_type="d2lsyllabus" d2l_2p0:link_target="" href="syllabus_d2l.xml" title="" />'
  }

  imstop <- paste('<?xml version="1.0" encoding="UTF-8"?>','<manifest xmlns:d2l_2p0="http://desire2learn.com/xsd/d2lcp_v2p0" xmlns:scorm_1p2="http://www.adlnet.org/xsd/adlcp_rootv1p2" xmlns="http://www.imsglobal.org/xsd/imscp_v1p1">','<organizations default="d2l_orgs">','<organization identifier="d2l_org">',sep="\n")
  imsmid <- paste('</organization>','</organizations>','<resources>',sep="\n")
  imsend <- paste('</resources>','</manifest>',sep="\n")
  
  
  imsmanifest<-paste(
    imstop,
    content[1],
    imsmid,
    content[2],
    syllabusresources,
    quizlibraryresources,
    paste(quizresources,collapse='\n'),
    paste(surveyresources,collapse='\n'),
    paste(discussionresources,collapse='\n'),
    GBresources,
    dropresources,
    imsend,
    "\n",sep="\n")
  
  xmlfile <- xmlParse(imsmanifest)
  cat(toString.XMLNode(xmlfile),file=fullFile(outdir,"imsmanifest.xml"))
  # cat(imsmanifest,file=fullFile(outputdir,"imsmanifest.xml"))
  return(imsmanifest)
}

#' Things to do
#' Create the overview file syllabus_d2l.xml.  This might be a good idea to include eventually.
#' Add Description to Dropbox items in Content View (it's possible to set it there).
#' Update the 5 types of submission view for quizzes and what each option means.  1= don't show. 2=show all questions.
#' Include Discussion and Forum items. Later though.  Finish 221 first.
#' Include a check to see if html file is there.  If not, use Template file with correct title, banner, etc.
#' Include a checklist item.
#' Include a News item.  I should do this sooner, rather than later, as it will be useful.

createZip <- function(filename,int=1){
  if(int==1){
    setwd(outputdir)
    system(paste0("zip -9 -r -q ",filename," *"))
    setwd(maindir)
  }
} #calls the zip command on linux to create the zip folder. 


require(XML)
myroot <- NULL
if( file.exists(fullFile(supportdir,"questiondb.xml"))==TRUE ) {
  xmlfile<- xmlParse(fullFile(supportdir,"questiondb.xml"))
  myroot <- xmlRoot(xmlfile)[[1]]
} else {
  print("No questiondb.xml file found in this folder.  Cannot load quiz questions.");
}


#Stats Course 
stats_gsheet <- "1xkikStGsnrW5RanQCmEuMsZRnPqm6As2JtpvNSwc7lw"
#General Practice
gsheet <- "1EPLvR3IK2DZ-IpeLnIYgrIZYBLyzxt1E0DuSyj0Ttc8"


gsheet <- '1-ZK8aGsK3D4TZT-o1fGDgDZAhkk4u4el1JmCFHQTqNM'   #Bonnies's 11:30 Class

#ims<-build_imsmanifest_from_GoogleDocs(stats_gsheet,Use_Question_Library=TRUE)
useQL<-TRUE

maindir="/home/bmwdruff/Desktop/StatsCourses2015"
outputdir=fullFile(maindir,"output")

makeCourse<-function(gsheet,teacher,ccode,time,bscode='',mapleta=''){
  setwd(maindir)
  

  unlink(outputdir,recursive=TRUE)
  dir.create(outputdir,recursive=TRUE)
  supportdir <<- fullFile(maindir,"support")
  #Inside each teacher specific folder I'll place a support, supportA, supportB, and supportC folders.
  teacherdir <<- fullFile(maindir,paste0("teacher-specific/",teacher)) 
  fullFile(maindir,"support")
  system(paste0("cp -a ",supportdir,"/* ",outputdir,"/." ))
  supportdir <<- fullFile(maindir,paste0("support",ccode))
  system(paste0("cp -a ",supportdir,"/* ",outputdir,"/." ))

  require(XML)
  myroot <<- NULL
  if( file.exists(fullFile(supportdir,"questiondb.xml"))==TRUE ) {
    xmlfile<- xmlParse(fullFile(supportdir,"questiondb.xml"))
    myroot <<- xmlRoot(xmlfile)[[1]]
  } else {
    print(supportdir)
    print("No questiondb.xml file found in this folder.  Cannot load quiz questions.");
  }
  print('hi')

  ims<-build_imsmanifest_from_GoogleDocs(gsheet,Use_Question_Library=useQL)
  ##Make a script that copies the teachers specific files over the top of the existing files. Copy support first, then supportX.Do this BEFORE you zip the folder. 
  system(paste0("cp -a ",teacherdir,"/support","/* ",outputdir,"/." ))
  system(paste0("cp -a ",teacherdir,"/support",ccode,"/* ",outputdir,"/." ))

  ##Find every instance of BS_CODE and MAPLE_TA_CODE in the HTML files and XML files, and replace it with the bscode given and MapleTA code. Putting these numbers into the spreadsheet online fixes the XML files, but does not take care of the HTML files.  That has to be done manually.
  setwd(paste0(outputdir,"/Course_Files"))
  system(paste0("sed -i 's/BS_CODE/",bscode,"/g' *.html"))  
  system(paste0("sed -i 's/MAPLE_TA_CODE/",mapleta,"/g' *.html"))  
  setwd(maindir)
  
  filename=paste0("D2L-",teacher,ccode,time,".zip")
  createZip(filename)
  system(paste0("mv output/",filename," ","/home/bmwdruff/Dropbox/22X/BSCourseZips/2017Spring/."))
  ##At somet point, move the folder name to a function pass.
  supportdir<<-fullFile(maindir,"support")
}


#For next semester, you'll need to automate the MapleTA course code process.  Scott has all the files already in his A directory.  I just need to figure out what the difference is between the A and other sections in YouTube links and other things. 

# 2017 Spring Classes
makeCourse('1jAbqBHxMOT35HJ3YcG3Vr7JBXtY7EWMxE_u5lZPKLxQ', teacher = 'Harmon', ccode = 'A', time='Sec2_TF',bscode='236546',mapleta='1')
makeCourse('1VZLpNjsJZcr-PWrImXLqkL-li1XHoxT6UJa9eUnUMYE', teacher = 'Harmon', ccode = 'A', time='Sec4_TF',bscode='236624',mapleta='1')
makeCourse('1ZIzbj5TGl5DAscQNjj9bH864a7UzItTYrxDAK4318AI', teacher = 'Harmon', ccode = 'A', time='Sec6_TF',bscode='236627',mapleta='1')


#Winter 2017 Classes
# makeCourse('1RYvWV8S66rV6HrVpTpOTHAgCqlA0qgMeR5hm2eeAEj8', teacher = 'None', ccode = 'B', time='TA_BC_Course')
# makeCourse('1jSzp7TJJjmz_u1WX-iy1Kr6rGzPEG71KuJ6jXcIAbSw', teacher = 'None', ccode = 'A', time='TA_A_Course')
# 
# makeCourse('1NPGkksxTzX8gkruH3kUc0OsbELTjPAxUnKwTXcyHA5w', teacher = 'Harmon', ccode = 'A', time='Sec1_7.45.TF',bscode='210930',mapleta='770158705') #
# makeCourse('1YdyqkXo6XlL2sEUwgEF4-VFTFNuM-hc62g8xKkicYKw', teacher = 'Harmon', ccode = 'A', time='Sec4_9.00.TF',bscode='220497',mapleta='629798740') #
# makeCourse('19qjgR3tADY21rUphaB6rOOhUWl7VisnvlrhKJHpQBzU', teacher = 'Hathaway', ccode = 'A', time='Sec2_9.00.TF')
# makeCourse('1dFwrGEDWuNOCcM45i11cbvzVf1N7JAik7Ie_VXJNRdk', teacher = 'Hathaway', ccode = 'A', time='Sec2_3.15.TF')
# makeCourse('1RjmbOGBhGtH33u9spWuh8ECbXwbUafiqi6RFu7ElN70', teacher = 'Stohel',   ccode = 'A', time='Sec3_11.30.TF')
# makeCourse('16qpoACzF_VEc4cZFjh02r9IY6z8_7Hp81sU5bWSlVxc', teacher = 'Stohel',   ccode = 'C', time='Sec2_10.15.TF')
# makeCourse('1FeGqdmLRbcJG7bsrnH_xaIPdLGP9kBpcUF2RAc_dCio', teacher = 'Saunders', ccode = 'B', time='Sec1_7.45.TF')
# makeCourse('1ABiMer1xwKkJr2VspRWkor31dz0iV0FkyOQBMTmyAxk', teacher = 'Johnson',  ccode = 'B', time='Sec2_10.15.TF')
# makeCourse('1aOZ0IOsxNdO4w7WmS1NxTJSU89vq4lMnwu1V00tj9Fg', teacher = 'Johnson',  ccode = 'B', time='Sec6_9.00.TF')
# makeCourse('1grqD1teFdxnStA1aGzAf1vb6ADP-cE3tT_hirEkIZgE', teacher = 'Woodruff', ccode = 'B', time='Sec3_11.30.TF')
# makeCourse('1kDQdbZjGdTHN5bEWlC0z42vM6tGm7SJROoyB_2x_xYE', teacher = 'Cromar',   ccode = 'B', time='Sec4_12.45.TF')
# makeCourse('1K0oRO_toTHOzN7eVKPLDtJtT1WtEUDfvWtAjj0JWxdU', teacher = 'Cromar',   ccode = 'B', time='Sec5_3.15.TF')
# makeCourse('17s1IY6RAv8zTMtp1IfJRzPSCjPn5Mrk3qzmxdWvbcsk', teacher = 'Moon',     ccode = 'C', time='Sec1_9.00.TF')
# makeCourse('1g_FSuZseqwi9ioJaFnb4WhxPw4pEMOiyO2cLgIWcT6Y', teacher = 'Qumsiyeh', ccode = 'C', time='Sec2_12.45.TF')






#Fall 2016 Classes.
# makeCourse('16K9rMB-vszdhmA4RW6KwSuRq_YdGEf2C0kNl786UPiI', teacher = 'Harmon', ccode = 'A', time='Sec1.7.45.TF')
# makeCourse('11MMhKG78OWIhJxwGLiUj-8gQGnyf-7ajNzRs2nrFPgI', teacher = 'Harmon', ccode = 'A', time='Sec2.11.30.MR')
# makeCourse('1LgnRYwmsFBTZ2I5f703TygfEAQbnndBkGaXqNxkXGFk', teacher = 'Hathaway', ccode = 'A', time='Sec3.12.45.TF')
# makeCourse('1XWDK1gZtH5FJuGgJJ0ilUWvVs0c74CSO20tQ809nltY', teacher = 'Hathaway', ccode = 'A', time='Sec4.7.45.TR')
# makeCourse('1tQf9dvd8cHMdRuKAK_X1anxyNzPkXk5wjiaTkL_kEEw', teacher = 'Saunders', ccode = 'B', time='Sec1.7.45.MR')
# makeCourse('16rmOcPTKYE22ffooBU8UfhLBpZzkmRC4c-iPxXzt2D4', teacher = 'Saunders', ccode = 'B', time='Sec2.9.00.MR')
# makeCourse('1YC7Oh4FT9rlfFCyB2Bn7KJhZkNvD0Bd4CDZQi0yLIUY', teacher = 'Palmer', ccode = 'B', time='Sec3.10.15.MR')
# makeCourse('13d4aiaJeowNp-j9gw6eygN-dSkVqtYWCOjgnBWqwzu4', teacher = 'Cromar', ccode = 'B', time='Sec4.11.30.TF')
# makeCourse('1OXWAIr9YPNNpuXmmqEiVP5D9pk3IzqrZFjk05e9QKb8', teacher = 'Cromar', ccode = 'B', time='Sec5.3.15.MR')
# makeCourse('1brTE17uZQfmAwWjwcQNuv9uEkKVcT8tuAGGvQdBELoU', teacher = 'Stohel', ccode = 'B', time='Sec6.10.15.TR')
# makeCourse('1feeJDD-A5zY8K9zCFm7RRMtZaH-85pIARm7dMchd-5k', teacher = 'Johnson', ccode = 'C', time='Sec1.9.00.TF')
# makeCourse('1cBNQ1i2FzbuIj4Atk4wT3i5CxxAlOGFQkxHkJjWlS1k', teacher = 'Palmer', ccode = 'C', time='Sec2.10.15.TF')
# makeCourse('1XMrFkAek994xPs_uvxDgx6nEfrq9XpFXgENEIpkWt6k', teacher = 'Brown', ccode = 'C', time='Sec3.12.45.MR')
# makeCourse('1CTePv70v-ZDSkWD4SWUsR0uw4OflTN-ApRd9KEs6SDc', teacher = 'Brown', ccode = 'C', time='Sec4.3.15.TF')
# 
# makeCourse('1BecqkPSTwR1y9a9LJgOfLEY8BGWMjU0xOSFhs1bFaNc', teacher = 'TA', ccode = 'A', time='TA')
# makeCourse('1BecqkPSTwR1y9a9LJgOfLEY8BGWMjU0xOSFhs1bFaNc', teacher = 'TA', ccode = 'B', time='TA')
# makeCourse('1BecqkPSTwR1y9a9LJgOfLEY8BGWMjU0xOSFhs1bFaNc', teacher = 'TA', ccode = 'C', time='TA')
# 1BecqkPSTwR1y9a9LJgOfLEY8BGWMjU0xOSFhs1bFaNc
# 
# 
# gsheet<-'1T_KpFKldwdyADkJdDXj8qi2RW9E_1V5lcODcQDzVUcU' #Master
# makeCourse(gsheet, teacher = 'Master-',ccode = 'B',time = '-MR')
# 


#Spring 2016 classes.
# 
# gsheet<-'1zBeXNTE2-zSknBGKN5-nVXuOs2xarcyTvMNg3ZjdwGI' #Garrett A 7:45 TF
# makeCourse(gsheet, teacher = 'Garrett-',ccode = 'A',time = '-TF-7.45')
# gsheet<-'1KcdfyZQNjBrUDahsmBi8XAcmPllr9LydUYduBtyUOPA' #Garrett B 12:45 MR
# makeCourse(gsheet, teacher = 'Garrett-',ccode = 'B',time = '-MR-12.45')
# gsheet<-'1uI2dH3Fd1EqzfKSoak733r7O06_HmPLhCH8GGb51Qag' #Garrett C 9:00 MR
# makeCourse(gsheet, teacher = 'Garrett-',ccode = 'C',time = '-MR-9.00')
# 
# 
# gsheet<-'1HUoirM41JlXVm1YbXk0IiUeHtalymejq725JRV2oQEc' #Alyssa B 12:45 TF
# makeCourse(gsheet, teacher = 'Alyssa-',ccode = 'B',time = '-TF-12.45')
# gsheet<-'1tufkHD7HyHW4wCYmTszHYdAzybYNC_q9RMiIZnUVvhg' #Alyssa C 12:45 MR
# makeCourse(gsheet, teacher = 'Alyssa-',ccode = 'C',time = '-MR-12.45')
# 
# gsheet<-'1p2BpLKGOYzcfS0GyhKDcG68U2mwZCzt2bb4Ne0M0n8I' #Andy Newey A 11:30 MR
# makeCourse(gsheet, teacher = 'Andy-',ccode = 'A',time = '-MR-11.30')
# 
# 
# gsheet <- '1-ZK8aGsK3D4TZT-o1fGDgDZAhkk4u4el1JmCFHQTqNM'   #Bonnies's B MR 7:45 Class
# makeCourse(gsheet, teacher = 'Bonnie-',ccode = 'B',time = '-MR-7.45')
# gsheet <- '1eoTbhQ8NCMtgfaqVqRYdXf7Lw47b60Fi4BpV7jveG5g'   #Bonnies's B TF 11:30 Class
# makeCourse(gsheet, teacher = 'Bonnie-',ccode = 'B',time = '-TF-7.45')
# 
# 
# gsheet <- '1mdLS8tCt_z_G8BVIERdfTB0h9DQ45cWR2NX0SiRWXPU'   #Ryan 10:15 B MR 
# makeCourse(gsheet, teacher = 'Ryan-',ccode = 'B',time = '-MR-10.15')
# gsheet <- '17xVGY5Yr_CFn38xCuscOwBO1x9DYscgvPxle1duwGN0'   #Ryan 3:15 B MR 
# makeCourse(gsheet, teacher = 'Ryan-',ccode = 'B',time = '-MR-3.15')
# 
# 
# gsheet <- '1zw0yGCucEEpXGVvnOzuLHJsbh7GBUYiP8m0j-uqzgn8'   #J. 9:00 A TF
# makeCourse(gsheet, teacher = 'J-',ccode = 'A',time = '-TF-9.00')
# gsheet <- '1VrXmNuEMYfanqzdhWGAlneQWDjNCbb7nN4g4lds9Lv0'   #J. 10:15 C TF
# makeCourse(gsheet, teacher = 'J-',ccode = 'C',time = '-TF-10.15')
# 
# 
# 
# gsheet <- '1geDldf-IgtBwXEQsCgB8xzO35jG3H93FSDlkblmzqtA' #Colin's example.
# 
# gsheet <- '1nnjxsPwLhqmTXWHPjlL3b9NRtLWooDpofSYn3s5t5mY' #Pierce Math Course Offerings.
# makeCourse(gsheet, teacher = 'Pierce',ccode = '',time = '')
# 
