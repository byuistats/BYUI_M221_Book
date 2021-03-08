#For surveys

#Tested and working fine.

#'  @author Ben Woodruff
#'  @details This function does great things
#'  @param Name This is a character vector vector
#'  @example 
#'  @export
makeSurveyXML <- function(
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
  if(Title==""){print("No Survey Name given."); return("")}
  if(Id==""){print("No Survey Id given."); return("")}
  
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
    '<assessment title="',reformatHTML(Title),'" ident="res_survey-',Id,'" d2l_2p0:resource_code="src-',Id,'">','\n',
    '<rubric>','\n',
    '<flow_mat>','\n',
    '<material>','\n',
    '<mattext d2l_2p0:isdisplayed="',Quiz_Show_Description,'" texttype="text/html">',reformatHTML(Quiz_Description),'</mattext>','\n',
    '</material>','\n',
    '</flow_mat>','\n',
    '</rubric>','\n',
    '<assessmentcontrol hintswitch="no" solutionswitch="no" feedbackswitch="no"/>','\n',
    '<presentation_material>','\n',
    '<flow_mat>','\n',
    '<material label="page footer">','\n',
    '<mattext d2l_2p0:isdisplayed="',Quiz_Show_Page_Footer,'" texttype="text/html">',reformatHTML(Quiz_Page_Footer),'</mattext>','\n',
    '</material>','\n',
    '</flow_mat>','\n',
    '</presentation_material>','\n',
    '<assess_procextension>','\n',
    '<d2l_2p0:submission_message texttype="text/html">&lt;p&gt;Thank you for submitting your survey.&lt;/p&gt;</d2l_2p0:submission_message>',
    '<d2l_2p0:anonymous>yes</d2l_2p0:anonymous>',
    '<d2l_2p0:instant_feedback>no</d2l_2p0:instant_feedback>',
    '<is_active>',Quiz_Is_Active,'</is_active>','\n',
    Date_Start,
    Date_End,
    '<d2l_2p0:has_schedule_event>',Quiz_Display_In_Calendar,'</d2l_2p0:has_schedule_event>','\n',
    '<d2l_2p0:attempts_allowed>editable</d2l_2p0:attempts_allowed>',
    '<d2l_2p0:is_forward_only>',Quiz_Is_Forward_Only,'</d2l_2p0:is_forward_only>','\n',
    '</assess_procextension>','\n',
    containersection,'\n',
    '</assessment>','\n',
    '</questestinterop>',sep="")
  
#  cat(quizxml,"\n",sep="",file=paste("quiz-",Id,".xml",sep=""))
  
  resourcexml<-paste('<resource identifier="res_survey-',Id,'" type="webcontent" d2l_2p0:material_type="d2lsurvey" d2l_2p0:link_target="" href="survey-',Id,'.xml" title="',reformatHTML(Title),'" />',sep="")
  
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


#' You must pass this a data.frame with the correct columns for use in the functions above.  An example of how to do this is below. the function.
buildSurveyAndGetResourceXML<-function(quizdf,outdir="."){
  num <- nrow(quizdf)
  if(num==0){return(c(""))}#If there are no quizzes, then don't return anything.
  
  quizresourcexml<-vector("character",num)
  for(i in 1:num){
    quizxml <- do.call(makeSurveyXML,as.list(quizdf[i,])) #Passes the entire row to the function, but first removes the column names with as.character. Otherwise, if the column names in Excel don't EXACTLY match here, problems.  
    quizresourcexml[i] <- quizxml[1]
    Id <- quizdf[i,2]
    if(Id!=""){
      xmlfile <- xmlParse(quizxml[2])
      cat(toString.XMLNode(xmlfile),file=fullFile(outdir,paste("survey-",Id,".xml",sep="")))
      # cat(quizxml[2],"\n",sep="",file=fullFile(outdir,paste("quiz-",Id,".xml",sep="")))
    }
  }  
  return(quizresourcexml)
}

# filename="course.csv"
# df<-read.csv(filename,colClasses="character")
# #remove the top ??? rows of instructions 
# df<-df[seq(5,nrow(df)),]
# quizdf<-df[df$Type=="quiz",]; quizdf<-quizdf[,c(3,4,20,21,22,23,24,25,26,27,28,29,30,31,32,10,11,12,13,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54)]
# #Now loop through the items and make the correct XML
# head(quizdf)
# mynewout<-buildQuizzesAndGetResourceXML(quizdf)
# mynewout
