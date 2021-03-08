#Tested and working fine.


#Make discussions. Open a csv file containing topic and forum settings.  Then create an xml file for each forum. This will only return the code needed to put in the imsmanifest, and won't return the xml for the several discussion pages.
#Make Gradebook. Open a csv file with gradebook settings.  Build the gradebook, create the grades_d2l.xml file, and return the imsmanifest code.
#We'll need a similar item for news, for overview (syllabus_d2l.xml file), dropbox items, etc.  

### This function is needed not just here, but anywhere you have HTML as possible input. Descriptions are the main problem 
# If the HTML entries are not empty, we have to strip some parts of the code. So far I know I have to get rid of <, >, &.  Not sure of what else.  
reformatHTML <- function(html){
  html <- gsub("&","&amp;",html)
  html <- gsub("<","&lt;",html)
  html <- gsub(">","&gt;",html)    
  return(html)
}

#' The next three functions are used for including the quiz library and importing questions into quizzes.
#' To use this feature, the questions you want loaded in a quiz must reside in folders.  This code takes entire folders, possibly several, and loads them into a quiz. You just type the folder(s), such as "Self Report/Self Report (F2F),Lesson 02/Homework", in the appropriate column of the CSV sheet, and this code does the rest.   

# library(XML)
# myroot <- NULL
# if( file.exists("questiondb.xml")==TRUE ) {
#   xmlfile<- xmlParse("questiondb.xml")
#   myroot <- xmlRoot(xmlfile)[[1]]
# } else {
#   print("No questiondb.xml file found in this folder.  Cannot load quiz questions.");
# }


# Given the base node to access in the "myroot" global, this traverses through the question library and finds all the questions in a given folder.  It assigns the same values from the question library, and returns the xml needed to include this folder of questions. 

addSectionFromQuizLibrary <- function(basenode){
  
  if(is.null(myroot)){
    print(paste0("No quiz library detected. Cannot get section. ",basenode));return("")
  }
  
  sectionNode <- getNodeSet(myroot,basenode)
  if(is.null(sectionNode)){print(paste0("In Quiz Library, could not find ",basenode));return("")}
  mytitle<-xmlGetAttr(sectionNode[[1]],"title")
  procextensions<-toString.XMLNode(sectionNode[[1]][[1]])
  idNodes <- getNodeSet(myroot,paste0(basenode,'/item'))
  ids <- lapply(idNodes, function(x) xmlAttrs(x)['label'])
  values <- lapply(idNodes, xpathApply, path = './itemmetadata/qtimetadata/qti_metadatafield[fieldlabel="qmd_weighting"]/fieldentry', xmlValue)
  attributes <- lapply(idNodes, xpathApply, path = './itemproc_extension')
  
  num=length(idNodes)
  iteminfo<-vector("character",num)
  for(i in 1:num){
    if(is.null(values[[i]])){
      iteminfo[i]<-paste(
        '<itemref linkrefid="',toString(ids[[i]][1]),'" d2l_2p0:page="1">',"\n",
        '<d2l_2p0:file href="questiondb.xml"/>',"\n",
        '<d2l_2p0:points>',"0.000000000",'</d2l_2p0:points>',"\n",
        '<d2l_2p0:difficulty>0</d2l_2p0:difficulty>',"\n",
        '<d2l_2p0:isbonus>no</d2l_2p0:isbonus>',"\n",
        '<d2l_2p0:ismandatory>no</d2l_2p0:ismandatory>',"\n",
        '</itemref>',sep="")
      
    }
    else{
      iteminfo[i]<-paste(
        '<itemref linkrefid="',toString(ids[[i]][1]),'" d2l_2p0:page="1">',"\n",
        '<d2l_2p0:file href="questiondb.xml"/>',"\n",
        '<d2l_2p0:points>',toString(values[[i]][[1]][1]),'</d2l_2p0:points>',"\n",
        toString.XMLNode(attributes[[i]][[1]][[1]]),"\n",
        toString.XMLNode(attributes[[i]][[1]][[2]]),"\n",
        toString.XMLNode(attributes[[i]][[1]][[3]]),"\n",
        '</itemref>'
        ,sep="")
      
    }
  }
  quizdata<-paste(
    paste0('<section title="',mytitle,'" d2l_2p0:page="1">'),
    procextensions,
    paste(iteminfo,
          collapse = "\n"),"</section>",sep="\n")
  return(quizdata)
}


###This function takes something like "Lesson 2/Group Questions" and returns "//section[@title=\"Lesson 2\"]/section[@title=\"Group Questions\"]".  This is needed so that I can type a folder from the Quiz library that I want to grab questions from, and then get the appropriate Xpath syntax.

getBaseNode<-function(folder){
  myfolders <- strsplit(folder,'/')
  #  myfolders
  #  test<-lapply(myfolders,function(x) paste('/section[@title="',x,'"]',sep=""))
  #  paste(test[[1]],collapse = "")
  #idNodes <- getNodeSet(myroot,'//section[@title="Exam 3 Questions"]/item')
  
  
  basenode<-paste('/',paste(lapply(myfolders[[1]],function(x) paste('/section[@title="',x,'"]',sep ="")),collapse =""),sep="")
  basenode
}


#folders is a comma separated list of folders, such as "Self Report/Self Report (F2F),Lesson 12/Group Questions".  This folder should exist in the quiz library.  If it doesn't, then the function returns ""
quizQuestionXML <- function(folders){
  y<-strsplit(folders,",")
  baseNodes<-lapply(y[[1]],getBaseNode)
  questions<-""
  try(questions<-sapply(baseNodes, function(x) addSectionFromQuizLibrary(x)))
  
  paste('<section ident="CONTAINER_SECTION">',
        paste(questions,collapse = "\n"),
        '</section>',sep="\n")
}
#'
#'  Let's test the above 3 functions
#'  
# Quiz_Library_Folders<-"Lesson 27/Homework,Self Report,Lesson 12/Homework,"
# qqxml<-quizQuestionXML(Quiz_Library_Folders)
# cat(qqxml,'\n',sep="", file="text.xml")




library(chron)  

#' @author Ben Woodruf
#' @return A D2L time stamp for quizzes
#' Requires that you give it a valid chron object. 
makeTimeStampXML <- function(mydate){
  paste(
    '<d2l_2p0:timestamp>','\n',  
    '<d2l_2p0:month>',format(mydate,"%m"),'</d2l_2p0:month>','\n',
    '<d2l_2p0:day>',format(mydate,"%d"),'</d2l_2p0:day>','\n',
    '<d2l_2p0:year>',format(mydate,"%Y"),'</d2l_2p0:year>','\n',
    '<d2l_2p0:hour>',format(mydate,"%H"),'</d2l_2p0:hour>','\n',
    '<d2l_2p0:minutes>',format(mydate,"%M"),'</d2l_2p0:minutes>','\n',
    '<d2l_2p0:seconds>',format(mydate,"%S"),'</d2l_2p0:seconds>','\n',
    '</d2l_2p0:timestamp>','\n',
    sep=""
  )  
}

#'  @author Ben Woodruff
#'  @details This function does great things
#'  @param Name This is a character vector vector
#'  @example 
#'  @export
makeQuizXML <- function(
  Title,        Id,
  Quiz_Category="",        Quiz_Description="",  
  Quiz_Show_Description="no",        Quiz_Introduction="",        
  Quiz_Show_Introduction="no",        Quiz_Page_Header="",        
  Quiz_Show_Page_Header="no",        Quiz_Page_Footer="",        
  Quiz_Show_Page_Footer="no",        Quiz_Allow_Hints="no",        
  Quiz_Disable_Right_Click="no",        Quiz_Disable_Pager="no",        
  Quiz_Is_Active="no",        Start_Date="", Start_Time="",        
  End_Date="", End_Time="",        Quiz_Display_In_Calendar="no",        
  Quiz_Password="",        Quiz_Time_Limit="120",        
  Quiz_Show_Clock="no",        Quiz_Enforce_Time_Limit="no",        
  Quiz_Grace_Period="5",        Quiz_Late_Limit_Type="0",        
  Quiz_Late_Limit_Data_Type="",        Quiz_Auto_Grade="no",        
  Quiz_Include_In_Gradebook="no",        Quiz_Allow_Auto_Export="no",        
  Quiz_Is_Forward_Only="no",        Quiz_Attempts_Allowed="1",        
  Quiz_Attempt_Restrictions="",        Quiz_Mark_Calculation_Type="1",        
  Quiz_Feedback_Text="",        Quiz_Duration="0",          
  Quiz_Show_Questions_Type="1",        Quiz_Show_Correct_Answers="no",
  Quiz_Show_Class_Average="no",        Quiz_Show_Score_Distribution="no",
  Quiz_Notify_Email="", Quiz_Show_Solutions_After_Ends="no",
  Quiz_Library_Folders=""
){
  ### There is NO WAY to check the "Show Question Score and Out Of Score" option.  It is always checked. Another option I can't adjust... UGH!!!!
  
  #If Name or Id is empty, then do nothing and return "".  
  if(Title==""){print("No Name given."); return("")}
  if(Id==""){print("No Id given."); return("")}
  
  #Note that Date_Start will be created from Start_Date and Start_Time, and similar with Date_End. I use the chron package to do the date extraction.
  Date_Start <- paste('<d2l_2p0:date_start/>',"\n",sep="")
  Date_End <- paste('<d2l_2p0:date_end/>',"\n",sep="")
  if(Start_Date!=""){
    try(
      if(mydate <- chron(Start_Date, Start_Time, format = c(dates = "m/d/y", times = "h:m:s"))){
        Date_Start<-paste('<d2l_2p0:date_start>','\n',makeTimeStampXML(mydate),'</d2l_2p0:date_start>','\n',sep="")
      }
    )
  }
  if(End_Date!=""){
    try(
      if(mydate <- chron(End_Date, End_Time, format = c(dates = "m/d/y", times = "h:m:s"))){
        Date_End<-paste('<d2l_2p0:date_end>','\n',makeTimeStampXML(mydate),'</d2l_2p0:date_end>','\n',sep="")
      }
    )
  }
  
  ###Jay's comment
  #This could be better if we read in the 2nd line of the csv file as an OPTIONS variable.  Then this calls the variables, and sets the defaults. That definitely could be nice. Maybe just have a way to set the defaults several times from the quiz csv fie.  
  
  #Some values cannot be blank, so if they were entered in blank, we have to reset them.
  if(Quiz_Show_Description=="")       {Quiz_Show_Description="no"}
  if(Quiz_Show_Introduction=="")      {Quiz_Show_Introduction="no"}
  if(Quiz_Show_Page_Header=="")       {Quiz_Show_Page_Header="no"}
  if(Quiz_Show_Page_Footer=="")       {Quiz_Show_Page_Footer="no"}        
  if(Quiz_Allow_Hints=="")            {Quiz_Allow_Hints="no"}
  if(Quiz_Disable_Right_Click=="")    {Quiz_Disable_Right_Click="no"}
  if(Quiz_Disable_Pager=="")          {Quiz_Disable_Pager="no"}
  if(Quiz_Is_Active=="")              {Quiz_Is_Active="no"}
  if(Quiz_Display_In_Calendar=="")    {Quiz_Quiz_Display_In_Calendar<-"no"}
  if(Quiz_Time_Limit=="")             {Quiz_Time_Limit="120"}
  if(Quiz_Show_Clock=="")             {Quiz_Show_Clock="no"}
  if(Quiz_Grace_Period=="")           {Quiz_Grace_Period<-"5"}
  if(Quiz_Late_Limit_Type=="")        {Quiz_Late_Limit_Type<-"0"}
  if(Quiz_Auto_Grade=="")             {Quiz_Auto_Grade="no"}
  if(Quiz_Allow_Auto_Export=="")      {Quiz_Allow_Auto_Export="no"}
  if(Quiz_Is_Forward_Only=="")        {Quiz_Is_Forward_Only="no"}
  if(Quiz_Attempts_Allowed=="")       {Quiz_Attempts_Allowed="1"}
  if(Quiz_Mark_Calculation_Type=="")  {Quiz_Mark_Calculation_Type<-"1"}
  if(Quiz_Duration=="")               {Quiz_Duration="0"}
  if(Quiz_Show_Questions_Type=="")    {Quiz_Show_Questions_Type="1"}
  if(Quiz_Show_Correct_Answers=="")   {Quiz_Show_Correct_Answers="no"}
  if(Quiz_Show_Class_Average=="")     {Quiz_Show_Class_Average="no"}
  if(Quiz_Show_Score_Distribution==""){Quiz_Show_Score_Distribution="no"}
  if(Quiz_Feedback_Text=="")          {Quiz_Feedback_Text <- '<p>Your quiz has been submitted successfully.</p>'}
  
  #Some items don't show up in the xml code unless they are not blank.  Set these valuse before building the xml code. 
  if(Quiz_Attempt_Restrictions!=""){############################ This Needs Serious Work.  Maybe just tell people to type them in manually.
    #############  Attempt Restrictions - Needs work.
    #
    # Replace attempt restrictions with this if there are any.  This will be tough to address in a CSV. I might just make someone type this in directly, giving them the starting example. That's probably the easiest. 
    #'<d2l_2p0:attempt_restrictions>
    # <d2l_2p0:min_2>20.000000000</d2l_2p0:min_2>
    # <d2l_2p0:max_2>100.000000000</d2l_2p0:max_2>
    # <d2l_2p0:min_3>50.000000000</d2l_2p0:min_3>
    # <d2l_2p0:max_3>80.000000000</d2l_2p0:max_3>
    # </d2l_2p0:attempt_restrictions>
    #'
    
    Quiz_Attempt_Restrictions<-paste('<d2l_2p0:attempt_restrictions/>',"\n",sep="")
  }
  else{Quiz_Attempt_Restrictions<-paste('<d2l_2p0:attempt_restrictions/>',"\n",sep="")}
  if(Quiz_Include_In_Gradebook=="yes"){
    Quiz_Include_In_Gradebook<-paste('<grade_item d2l_2p0:is_autoexport="',Quiz_Allow_Auto_Export,
                                     '" resource_code="grc-',Id,'"></grade_item>',"\n",sep="")}
  else{Quiz_Include_In_Gradebook<-""}
  if(Quiz_Category!=""){Quiz_Category<-paste('<category>',Quiz_Category,'</category>',"\n",sep="")}
  if(Quiz_Notify_Email!=""){Quiz_Notify_Email <- paste('<d2l_2p0:notification_email>',Quiz_Notify_Email,'</d2l_2p0:notification_email>','\n',sep="")}
  if(Quiz_Password!=""){Quiz_Password<-paste('<d2l_2p0:password>',Quiz_Password,'</d2l_2p0:password>',sep="")}
  if(Quiz_Enforce_Time_Limit=="yes"){
    Quiz_Enforce_Time_Limit <- paste(
      '<d2l_2p0:enforce_time_limit>',Quiz_Enforce_Time_Limit,'</d2l_2p0:enforce_time_limit>','\n',
      '<d2l_2p0:grace_period>',Quiz_Grace_Period,'</d2l_2p0:grace_period>','\n',
      '<d2l_2p0:late_limit>',Quiz_Late_Limit_Type,'</d2l_2p0:late_limit>','\n',
      '<d2l_2p0:late_limit_data>',Quiz_Late_Limit_Data_Type,'</d2l_2p0:late_limit_data>','\n',sep="")
  }
  else{
    Quiz_Enforce_Time_Limit <- paste(
      '<d2l_2p0:enforce_time_limit>',Quiz_Enforce_Time_Limit,'</d2l_2p0:enforce_time_limit>','\n',
      '<d2l_2p0:grace_period>',Quiz_Grace_Period,'</d2l_2p0:grace_period>','\n',
      '<d2l_2p0:late_limit>',Quiz_Late_Limit_Type,'</d2l_2p0:late_limit>','\n',sep="")
  }
  
  show_after_end <- ""
  if(Quiz_Show_Solutions_After_Ends=="yes" && End_Date!=""){
    try(
      if(mydate <- chron(End_Date, End_Time, format = c(dates = "m/d/y", times = "h:m:s"))){
        show_after_end <- paste(
          '<assessfeedback title="Show Solutions After Due Date">',
          '<rubric>',
          '<flow_mat>',
          '<material>',
          '<mattext texttype="yes"/>',
          '</material>',
          '</flow_mat>',
          '</rubric>',
          '<release_date>',
          makeTimeStampXML(mydate),
          '</release_date>',
          '<d2l_2p0:duration>0</d2l_2p0:duration>',
          '<d2l_2p0:response_display_type_id>2</d2l_2p0:response_display_type_id>',
          '<d2l_2p0:show_correct_answers>yes</d2l_2p0:show_correct_answers>',
          '<d2l_2p0:submission_restrictip>no</d2l_2p0:submission_restrictip>',
          '<d2l_2p0:show_class_average>',Quiz_Show_Class_Average,'</d2l_2p0:show_class_average>','\n',
          '<d2l_2p0:show_score_distribution>',Quiz_Show_Score_Distribution,'</d2l_2p0:show_score_distribution>','\n',
          '</assessfeedback>',
          sep=""
        )
      }
    )
  }
  
  #' We now add the questions to the quiz, from a given quiz library. We first check to see if the main node of the questiondb.xml file has been set to myroot (a globalish variable)
  containersection<-'<section ident="CONTAINER_SECTION"/>'
  if(Quiz_Library_Folders!=""){
    if(exists("myroot")){
      if(is.null(myroot)){}else{
        try(containersection<-quizQuestionXML(Quiz_Library_Folders))
      }
    }
  }
  
  quizxml <- paste(
    '<?xml version="1.0" encoding="UTF-8"?>','\n',
    '<questestinterop xmlns:d2l_2p0="http://desire2learn.com/xsd/d2lcp_v2p0">','\n',  
    '<assessment title="',reformatHTML(Title),'" ident="res_quiz-',Id,'" d2l_2p0:resource_code="qrc-',Id,'">','\n',
    '<rubric>','\n',
    '<flow_mat>','\n',
    '<material>','\n',
    '<mattext d2l_2p0:isdisplayed="',Quiz_Show_Description,'" texttype="text/html">',reformatHTML(Quiz_Description),'</mattext>','\n',
    '</material>','\n',
    '</flow_mat>','\n',
    '</rubric>','\n',
    '<assessmentcontrol hintswitch="',Quiz_Allow_Hints,'" solutionswitch="',Quiz_Auto_Grade,'" feedbackswitch="no"/>','\n',
    '<presentation_material>','\n',
    '<flow_mat>','\n',
    '<material label="page header">','\n',
    '<mattext d2l_2p0:isdisplayed="',Quiz_Show_Page_Header,'" texttype="text/html">',reformatHTML(Quiz_Page_Header),'</mattext>','\n',
    '</material>','\n',
    '<material label="page footer">','\n',
    '<mattext d2l_2p0:isdisplayed="',Quiz_Show_Page_Footer,'" texttype="text/html">',reformatHTML(Quiz_Page_Footer),'</mattext>','\n',
    '</material>','\n',
    '</flow_mat>','\n',
    '</presentation_material>','\n',
    '<assess_procextension>','\n',
    '<d2l_2p0:intro_message d2l_2p0:isdisplayed="',Quiz_Show_Introduction,'" texttype="text/html">',
    reformatHTML(Quiz_Introduction),'</d2l_2p0:intro_message>','\n',
    Quiz_Category,
    Quiz_Include_In_Gradebook,
    Quiz_Notify_Email,
    '<d2l_2p0:disable_right_click>',Quiz_Disable_Right_Click,'</d2l_2p0:disable_right_click>','\n',
    '<d2l_2p0:disable_pager_access>',Quiz_Disable_Pager,'</d2l_2p0:disable_pager_access>','\n',
    '<is_active>',Quiz_Is_Active,'</is_active>','\n',
    Date_Start,
    Date_End,
    '<d2l_2p0:has_schedule_event>',Quiz_Display_In_Calendar,'</d2l_2p0:has_schedule_event>','\n',
    Quiz_Password,
    '<d2l_2p0:is_attempt_Rldb>no</d2l_2p0:is_attempt_Rldb>','\n',
    '<d2l_2p0:is_subview_Rldb>no</d2l_2p0:is_subview_Rldb>','\n',
    '<d2l_2p0:time_limit>',Quiz_Time_Limit,'</d2l_2p0:time_limit>','\n',
    '<d2l_2p0:show_clock>',Quiz_Show_Clock,'</d2l_2p0:show_clock>','\n',
    Quiz_Enforce_Time_Limit,
    '<d2l_2p0:attempts_allowed>',Quiz_Attempts_Allowed,'</d2l_2p0:attempts_allowed>','\n',
    Quiz_Attempt_Restrictions,
    '<d2l_2p0:mark_calculation_type>',Quiz_Mark_Calculation_Type,'</d2l_2p0:mark_calculation_type>','\n',
    '<d2l_2p0:is_forward_only>',Quiz_Is_Forward_Only,'</d2l_2p0:is_forward_only>','\n',
    '</assess_procextension>','\n',
    '<assessfeedback>','\n',
    '<rubric>','\n',
    '<flow_mat>','\n',
    '<material>','\n',
    '<mattext texttype="yes">',reformatHTML(Quiz_Feedback_Text),'</mattext>','\n',
    '</material>','\n',
    '</flow_mat>','\n',
    '</rubric>','\n',
    '<d2l_2p0:duration>',Quiz_Duration,'</d2l_2p0:duration>','\n',
    '<d2l_2p0:response_display_type_id>',Quiz_Show_Questions_Type,'</d2l_2p0:response_display_type_id>','\n',
    '<d2l_2p0:show_correct_answers>',Quiz_Show_Correct_Answers,'</d2l_2p0:show_correct_answers>','\n',
    '<d2l_2p0:submission_restrictip>no</d2l_2p0:submission_restrictip>','\n',
    '<d2l_2p0:show_class_average>',Quiz_Show_Class_Average,'</d2l_2p0:show_class_average>','\n',
    '<d2l_2p0:show_score_distribution>',Quiz_Show_Score_Distribution,'</d2l_2p0:show_score_distribution>','\n',
    '</assessfeedback>','\n',
    show_after_end,'\n',
    containersection,'\n',
    '</assessment>','\n',
    '</questestinterop>',sep="")
  
#  cat(quizxml,"\n",sep="",file=paste("quiz-",Id,".xml",sep=""))
  
  resourcexml<-paste('<resource identifier="res_quiz-',Id,'" type="webcontent" d2l_2p0:material_type="d2lquiz" d2l_2p0:link_target="" href="quiz-',Id,'.xml" title="',reformatHTML(Title),'" />',sep="")
  
  return(c(resourcexml,quizxml))
}
# print(makeQuizXML("First","q23",Start_Date="3/4/5",Start_Time="6:7:8"))

# ##### Builds quiz files. Returns a vector containing the XML to load quizzes into imsmanifest.xml 
# buildQuizzesAndGetResourceXMLold<-function(filename="quizzes.csv",outdir="."){
#   df<-read.csv(filename,colClasses="character")
#   num <- nrow(df)
#   #Ignore first 5 rows, and also keep only columns 2 through 42.  Shaves off any extra code that someone might have used.
#   df <- df[seq(6,num),seq(2,42)]
#   num <- num-5
#   quizresourcexml<-vector("character",num)
#   for(i in 1:num){
#     quizxml <- do.call(makeQuizXML,as.list(df[i,])) #Passes the entire row to the function.
#     quizresourcexml[i] <- quizxml[1]
#     Id <- df[i,2]
#     if(Id!=""){cat(quizxml[2],"\n",sep="",file=fullFile(outdir,paste("quiz-",Id,".xml",sep="")))}
#   }  
#   return(quizresourcexml)
# }

library(XML)
#' You must pass this a data.frame with the correct columns for use in the functions above.  An example of how to do this is below. the function.
buildQuizzesAndGetResourceXML<-function(quizdf,outdir="."){
  num <- nrow(quizdf)
  if(num==0){return(c(""))}#If there are no quizzes, then don't return anything.
  
  quizresourcexml<-vector("character",num)
  for(i in 1:num){
    quizxml <- do.call(makeQuizXML,as.list(quizdf[i,])) #Passes the entire row to the function, but first removes the column names with as.character. Otherwise, if the column names in Excel don't EXACTLY match here, problems.  
    quizresourcexml[i] <- quizxml[1]
    Id <- quizdf[i,2]
    if(Id!=""){
      xmlfile <- xmlParse(quizxml[2])
      cat(toString.XMLNode(xmlfile),file=fullFile(outdir,paste("quiz-",Id,".xml",sep="")))
      # cat(quizxml[2],"\n",sep="",file=fullFile(outdir,paste("quiz-",Id,".xml",sep="")))
    }
  }  
  return(quizresourcexml)
}


source("createSurvey.R")

# filename="course.csv"
# df<-read.csv(filename,colClasses="character")
# #remove the top ??? rows of instructions 
# df<-df[seq(5,nrow(df)),]
# quizdf<-df[df$Type=="quiz",]; quizdf<-quizdf[,c(3,4,20,21,22,23,24,25,26,27,28,29,30,31,32,10,11,12,13,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54)]
# #Now loop through the items and make the correct XML
# head(quizdf)
# mynewout<-buildQuizzesAndGetResourceXML(quizdf)
# mynewout
