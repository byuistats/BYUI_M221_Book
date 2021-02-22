#Tested and working fine.



library(chron)  
####
####    PROBlEM!!!  This doesn't take into account daylight savings time.  There may be issues with dates because of DST.
####
#### Problem.  This adds 6 hours, not 7, so works great during spring terms.  Must add 7 hours between Oct and Mar (daylight savings time issue).  This function is not yet complete. 
#' @example getGMT("4/5/20","3:4:5")
#' @return "yyyy-mm-ddThh:mm:ss".  Needed for due dates and other times in Content, Dropbox, and Discussion area.
getGMT <-function(Date="",Time=""){
  if(Date==""){print("No Date provided."); return("")}
  Date_Full <-""
  try(
    if(mydate <- chron(Date, Time, format = c(dates = "m/d/y", times = "h:m:s"))){
      mydate <-mydate+6/24  #Change to 7/24 based on day of year. Or better, if you are past a certain time, then add one hour.
      if     (mydate > chron("3/8/2016","2:0:0",  format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/2/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/9/2016","2:0:0",  format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/3/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/10/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/5/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/12/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/6/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/13/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/7/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/14/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/1/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/8/2016","2:0:0",  format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/3/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/10/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/4/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/11/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/5/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      else if(mydate > chron("3/12/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate}
      else if(mydate > chron("11/6/2016","2:0:0", format = c(dates = "m/d/y", times = "h:m:s"))){mydate <-mydate+1/24}
      Date_Full<-paste(
        format(mydate,"%Y"),"-",
        format(mydate,"%m"),"-",
        format(mydate,"%d"),"T",
        format(mydate,"%H"),":",
        format(mydate,"%M"),":",
        format(mydate,"%S"),
        sep="")
    }
  )
  return(Date_Full)
}


### Here are the column names from content.csv
# (Type, Title, Id, File_Link, Target, Is_Visible, Due_Date, Due_Time, Start_Date, Start_Time, End_Date, End_Time)
# The functions below all create a vector c(content,resource) that keeps the xml needed to load something into BS.

#' @return Gives vector of XML code for including quizzes on content page. [1] is content [2] is resource.
createContentQuiz <- function(Title,Id,Target="_self",Is_Visible="true",Due_Date="",Due_Time=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for quiz content item."); return(c("",""))}
  if(Title==""){print("No Title for quiz content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="" ',OPTION,' ',DATE_DUE,
    '> <title>',reformatHTML(Title),'</title></item>',sep="")
  
  #Now make the resource connection.
  if(Target!="_blank"){Target<-"_self"}
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="contentlink" d2l_2p0:link_target="',Target,
    '" href="/d2l/common/dialogs/quickLink/quickLink.d2l?ou={orgUnitId}&amp;type=quiz&amp;rcode=qrc-',Id,
    '" title="" />',sep="")
  
  #Return a vector with both the above bits. 
  return(c(content,resource))    
}
#' @return Gives vector of XML code for including survey on content page. [1] is content [2] is resource.
createContentSurvey <- function(Title,Id,Target="_self",Is_Visible="true",Due_Date="",Due_Time=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for survey content item."); return(c("",""))}
  if(Title==""){print("No Title for survey content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="" ',OPTION,' ',DATE_DUE,
    '> <title>',reformatHTML(Title),'</title></item>',sep="")
  
  #Now make the resource connection.
  if(Target!="_blank"){Target<-"_self"}
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="contentlink" d2l_2p0:link_target="',Target,
    '" href="/d2l/common/dialogs/quickLink/quickLink.d2l?ou={orgUnitId}&amp;type=survey&amp;rcode=src-',Id,
    '" title="" />',sep="")
  
  #Return a vector with both the above bits. 
  return(c(content,resource))    
}
createContentDrop <- function(Title,Id,Target="_self",Is_Visible="true",Due_Date="",Due_Time=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for dropbox content item."); return(c("",""))}
  if(Title==""){print("No Title for dropbox content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="" ',OPTION,' ',DATE_DUE,
    '> <title>',reformatHTML(Title),'</title></item>',sep="")
  
  #Now make the resource connection.
  if(Target!="_blank"){Target<-"_self"}
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="contentlink" d2l_2p0:link_target="',Target,
    '" href="/d2l/common/dialogs/quickLink/quickLink.d2l?ou={orgUnitId}&amp;type=dropbox&amp;rcode=drc-',Id,
    '" title="" />',sep="")
  
  #Return a vector with both the above bits. 
  return(c(content,resource))    
}
createContentDisc <- function(Title,Id,Target="_self",Is_Visible="true",Due_Date="",Due_Time=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for discussion content item."); return(c("",""))}
  if(Title==""){print("No Title for discussion content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="" ',OPTION,' ',DATE_DUE,
    '> <title>',reformatHTML(Title),'</title></item>',sep="")
  
  #Now make the resource connection.
  if(Target!="_blank"){Target<-"_self"}
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="contentlink" d2l_2p0:link_target="',Target,
    '" href="/d2l/common/dialogs/quickLink/quickLink.d2l?ou={orgUnitId}&amp;type=discuss&amp;rcode=trc-',Id,
    '" title="" />',sep="")

  #Return a vector with both the above bits. 
  return(c(content,resource))    
}
createContentFile <- function(Title,Id,File_Link,Target,Is_Visible="true",Due_Date="",Due_Time="",Start_Date="",Start_Time="",End_Date="",End_Time="",Content_Description=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for File content item."); return(c("",""))}
  if(Title==""){print("No Title for File content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  DATE_START <-""
  DATE_END <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  if(Start_Date!=""){DATE_START<-paste(' date_start="',getGMT(Start_Date,Start_Time),'"',sep="")}
  if(End_Date!=""){DATE_END<-paste(' date_end="',getGMT(End_Date,End_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="',reformatHTML(Content_Description),'" ',OPTION,' ',DATE_DUE,' ',DATE_START,' ',DATE_END,
    '> <title>',reformatHTML(Title),'</title></item>',sep="")
  
  #Now make the resource connection. By default, files will open internally.
  if(Target!="_blank"){Target<-"_self"}
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="content" d2l_2p0:link_target="',Target,
    '" href="Course_Files/',File_Link,
    '" title="" />',sep="")
  
  #Return a vector with both the above bits. 
  return(c(content,resource))
}
createContentLink <- function(Title,Id,File_Link,Target,Is_Visible="true",Due_Date="",Due_Time="",Start_Date="",Start_Time="",End_Date="",End_Time="",Content_Description=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for Link content item."); return(c("",""))}
  if(Title==""){print("No Title for Link content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  DATE_START <-""
  DATE_END <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  if(Start_Date!=""){DATE_START<-paste(' date_start="',getGMT(Start_Date,Start_Time),'"',sep="")}
  if(End_Date!=""){DATE_END<-paste(' date_end="',getGMT(End_Date,End_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="',reformatHTML(Content_Description),'" ',OPTION,' ',DATE_DUE,' ',DATE_START,' ',DATE_END,
    '> <title>',reformatHTML(Title),'</title></item>',sep="")
  
  #Now make the resource connection. By default, links will open externally.
  if(Target!="_self"){Target<-"_blank"}
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="contentlink" d2l_2p0:link_target="',Target,
    '" href="',File_Link,
    '" title="" />',sep="")
  
  #Return a vector with both the above bits. 
  return(c(content,resource))
}
createContentStart <- function(Title,Id,Is_Visible="true",Due_Date="",Due_Time="",Start_Date="",Start_Time="",End_Date="",End_Time="",Content_Description=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for Start content item."); return(c("",""))}
  if(Title==""){print("No Title for Start content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  DATE_START <-""
  DATE_END <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  if(Start_Date!=""){DATE_START<-paste(' date_start="',getGMT(Start_Date,Start_Time),'"',sep="")}
  if(End_Date!=""){DATE_END<-paste(' date_end="',getGMT(End_Date,End_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="',reformatHTML(Content_Description),'" ',OPTION,' ',DATE_DUE,' ',DATE_START,' ',DATE_END,
    '> <title>',reformatHTML(Title),'</title>',sep="") #There is no "</item>" at the end of this. The XML code fails on import if you don't use a Stop item later. 
    
  #Now make the resource connection.
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="webcontent" d2l_2p0:link_target="" href="" title="" />',sep="")
  
  #Return a vector with both the above bits. 
  return(c(content,resource))
}
createContentStop <- function(){
  content<-"</item>"
  resource<-""
  return(c(content,resource))
}

createContentVideo <- function(Title,Id,File_Link,Target="_self",Is_Visible="true",Due_Date="",Due_Time="",Start_Date="",Start_Time="",End_Date="",End_Time="",Content_Description=""){
  #Start by creating the organizations content.
  if(Id==""){print("No ID for Video content item."); return(c("",""))}
  if(Title==""){print("No Title for Video content item."); return(c("",""))}
  OPTION<-""
  if(tolower(Is_Visible)=="false"){OPTION<-'isvisible="false"'}
  DATE_DUE <-""
  DATE_START <-""
  DATE_END <-""
  if(Due_Date!=""){DATE_DUE<-paste(' date_due="',getGMT(Due_Date,Due_Time),'"',sep="")}
  if(Start_Date!=""){DATE_START<-paste(' date_start="',getGMT(Start_Date,Start_Time),'"',sep="")}
  if(End_Date!=""){DATE_END<-paste(' date_end="',getGMT(End_Date,End_Time),'"',sep="")}
  content <-paste(
    '<item identifier="',Id,'" identifierref="RC-',Id,
    '" d2l_2p0:resource_code="crc-',Id,'" description="',reformatHTML(Content_Description),'" ',OPTION,' ',DATE_DUE,' ',DATE_START,' ',DATE_END,
    ' resource_type_key="D2L.Video"> <title>',reformatHTML(Title),'</title></item>',sep="")    
    
  #Now make the resource connection. By default, files will open internally.
  if(Target!="_blank"){Target<-"_self"}
  resource <-paste(
    '<resource identifier="RC-',Id,
    '" type="webcontent" d2l_2p0:material_type="contentlink" d2l_2p0:link_target="',Target,
    '" href="',File_Link,
    '" title="" />',sep="")
  
  
  #Return a vector with both the above bits. 
  return(c(content,resource))
}



createContentItem <- function(Type, Title, Id, File_Link, Target, Is_Visible, Due_Date, Due_Time, Start_Date, Start_Time, End_Date, End_Time, Content_Description){
  if(Type=="quiz"){ return(createContentQuiz(Title, Id, Target, Is_Visible, Due_Date, Due_Time))}
  if(Type=="survey"){ return(createContentSurvey(Title, Id, Target, Is_Visible, Due_Date, Due_Time))}
  if(Type=="drop"){ return(createContentDrop(Title, Id, Target, Is_Visible, Due_Date, Due_Time))}
  if(Type=="disc"){ return(createContentDisc(Title, Id, Target, Is_Visible, Due_Date, Due_Time))}
  if(Type=="file"){ return(createContentFile(Title, Id, File_Link, Target, Is_Visible, Due_Date, Due_Time, Start_Date, Start_Time, End_Date, End_Time, Content_Description))}
  if(Type=="link"){ return(createContentLink(Title, Id, File_Link, Target, Is_Visible, Due_Date, Due_Time, Start_Date, Start_Time, End_Date, End_Time, Content_Description))}
  if(Type=="video"){ return(createContentVideo(Title, Id, File_Link, Target, Is_Visible, Due_Date, Due_Time, Start_Date, Start_Time, End_Date, End_Time, Content_Description))}
  if(Type=="start"){return(createContentStart(Title, Id, Is_Visible, Due_Date, Due_Time, Start_Date, Start_Time, End_Date, End_Time, Content_Description))}
  if(Type=="stop"){ return(createContentStop())}
  print("No Type given for content item.")
  return(c("",""))
}




createContent<-function(contentdf){
  num<-nrow(contentdf)
  Module_Level=0
  contentVec<-vector("character",num)
  resourceVec<-vector("character",num)
  #Now loop through the items and make the correct XML
  i=1
  while (i <= num){
    #print(paste("Module_Level is",Module_Level,sep=""))
    #If Module level ever reaches zero, and you don't start a module, then BS does odd things.  Just print a warning about this, and make the user go back to their file and enter a module. 
    if(contentdf[i,1]=="stop"){Module_Level<-Module_Level-1}
    if(Module_Level>=0){
      itemInfo<-do.call(createContentItem,as.list(contentdf[i,]))
      contentVec[i]<-itemInfo[1]
      resourceVec[i]<-itemInfo[2]
      if(contentdf[i,1]=="start"&&contentVec[i]!=""){Module_Level<-Module_Level+1}
      if(Module_Level==0 && contentdf[i,1]!="stop"){print(paste0("You are creating an item (",contentdf[i,2],") outside of any modules. Did you forget a 'start' item in your spreadsheet? You can do this, but this item will be put in its own module - not what you normally expect."))}
    }
    else{#Skip the "stop" entry.
      print("You've stopped a module that was never started. Ignoring 'stop' line. Check your spreadsheet.")
      Module_Level<-Module_Level+1
    }
    i<-i+1
  }
  extraStops<-c("")
  if(Module_Level>0){
    extraStops <- vector("character",Module_Level)
    print("You haven't stopped every module that you started. Closing all modules. Check your spreadsheet.")
    while(Module_Level>0){
      extraStops[Module_Level] <- createContentStop()[1]
      Module_Level <- Module_Level-1
    }  
  }
  
  fullContent<-paste(c(contentVec,extraStops),collapse="\n")
  fullResource<-paste(resourceVec,collapse = "\n")
  return(c(fullContent,fullResource))
}
# #The CSV file contains a Type column that sorts bewteen quiz,drop,disc,file,link,start,stop.
# #filename="course.csv"
# df<-read.csv("course.csv",colClasses="character")
# #remove the top few rows of instructions, the first column, and any columns after 14. 
# contentdf<-df[seq(5,nrow(df)),seq(2,14)]
# head(contentdf)
# createContent(contentdf)
