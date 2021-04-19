



#Discussion Boards in Online
#Student Info - Replace with GoogleDocsSpreadsheet
#Simulation - done
#Questions and Conversations - done
#Google Activity - 
#Google Hangouts Sign up - replace with GoogleDocsSpreadsheet


#I see three ways to organize it.  I'll do both.
#(1) Simulation, Week 1, Q&C, Group Collab, discussion boards, with topics all placed inside, and linked to the gradebook. Content links are placed in the topics. 
#(2) Each item gets its own discussion board, with a single topic, linked to the gradebook. Links in the content are placed on the topic, not on the forum.
#(3) Each item gets its own discussion board, with NO TOPICS, as the topics are auto generated based on groups (). Setting up the gradebook here is now a MESS!.  Done once, and then we NEVER change the class again, just change the HTML files as time goes on. Here the content links are placed on the forum, not the topic.  



#' Creates the xml needed to create a forum. Topics, is a vector of xml for topics. I just needs to be collapsed to include. 
createDiscussionForum <- function(
  topics,
  Forum_Title,
  Forum_Id,
  Disc_Description="",
  Disc_Allow_Anon="False",
  Disc_Is_Hidden="False",
  Disc_Requires_Approval="False",
  Disc_Is_Locked="False",
  Disc_Must_Post_To_Participate="False"
){
  if(Forum_Id==""){print("No Forum_Id for discussion forum.");return("")}
  if(Forum_Title==""){print("No Forum_Title for discussion forum.");return("")}

  if(tolower(Disc_Allow_Anon)!="true"){Disc_Allow_Anon<-"False"}
  else{Disc_Allow_Anon<-"True"}
  if(tolower(Disc_Is_Hidden)!="true"){Disc_Is_Hidden<-"False"}
  else{Disc_Is_Hidden<-"True"}
  if(tolower(Disc_Requires_Approval)!="true"){Disc_Requires_Approval<-"False"}
  else{Disc_Requires_Approval<-"True"}
  if(tolower(Disc_Is_Locked)!="true"){Disc_Is_Locked<-"False"}
  else{Disc_Is_Locked<-"True"}
  if(tolower(Disc_Must_Post_To_Participate)!="true"){Disc_Must_Post_To_Participate<-"False"}
  else{Disc_Must_Post_To_Participate<-"True"}

  if (Disc_Description!=""){Disc_Description <- paste0('<description>',reformatHTML(Disc_Description),'</description>')}
  
  forumxml<-paste0(
    '<?xml version="1.0"?>','\n',
    '<discussion>','\n',
    '<forum resource_code="frc-',Forum_Id,'">','\n',
    '<properties>','\n',
    '<allow_anon>',Disc_Allow_Anon,'</allow_anon>','\n',
    '<is_hidden>',Disc_Is_Hidden,'</is_hidden>','\n',
    '<requires_approval>',Disc_Requires_Approval,'</requires_approval>','\n',
    '<is_locked>',Disc_Is_Locked,'</is_locked>','\n',
    '<must_post_to_participate>',Disc_Must_Post_To_Participate,'</must_post_to_participate>','\n',
    '</properties>','\n',
    '<content>','\n',
    '<title>',reformatHTML(Forum_Title),'</title>','\n',
    Disc_Description,'\n',
    '</content>','\n',
    '<topics>','\n',
    paste(topics,collapse = "\n"),'\n',
    '</topics>','\n',
    '</forum>','\n',
    '</discussion>')
  return(forumxml)
}

#' Puts the right pieces together to create the xml needed to go inside a forum xml file.
createDiscussionTopic <- function(
  Title,
  Id,
  Disc_Description="",
  Disc_Allow_Anon="False",
  Disc_Is_Hidden="False",
  Disc_Requires_Approval="False",
  Disc_Is_Locked="False",
  Disc_Must_Post_To_Participate="False",
  Disc_Score_Out_Of="",
  Disc_Is_Auto_Score="False",
  Disc_Include_In_GB="False",
  Disc_Include_Nonscored_Values="False",
  Disc_Rating_Type_Id="0"
){
  if(Id==""){print("No Id for discussion topic.");return("")}
  if(Title==""){print("No Title for discussion topic.");return("")}
  if(tolower(Disc_Allow_Anon)!="true"){Disc_Allow_Anon<-"False"}
  else{Disc_Allow_Anon<-"True"}
  if(tolower(Disc_Is_Hidden)!="true"){Disc_Is_Hidden<-"False"}
  else{Disc_Is_Hidden<-"True"}
  if(tolower(Disc_Requires_Approval)!="true"){Disc_Requires_Approval<-"False"}
  else{Disc_Requires_Approval<-"True"}
  if(tolower(Disc_Is_Locked)!="true"){Disc_Is_Locked<-"False"}
  else{Disc_Is_Locked<-"True"}
  if(tolower(Disc_Must_Post_To_Participate)!="true"){Disc_Must_Post_To_Participate<-"False"}
  else{Disc_Must_Post_To_Participate<-"True"}
  
  if(tolower(Disc_Is_Auto_Score)!="true"){Disc_Is_Auto_Score<-"False"}
  else{Disc_Is_Auto_Score<-"True"}
  if(tolower(Disc_Include_In_GB)!="true"){Disc_Include_In_GB<-"False"}
  else{Disc_Include_In_GB<-"True"}
  if(tolower(Disc_Include_Nonscored_Values)!="true"){Disc_Include_Nonscored_Values<-"False"}
  else{Disc_Include_Nonscored_Values<-"True"}
  if(Disc_Rating_Type_Id==""){Disc_Rating_Type_Id<-"0"}
  
  if (Disc_Description!=""){Disc_Description <- paste0('<description>',Disc_Description,'</description>')}
  if (Disc_Score_Out_Of!=""){Disc_Score_Out_Of <-paste0('<score_out_of>',Disc_Score_Out_Of,'</score_out_of>')}
  if (Disc_Include_In_GB=="True"){Disc_Include_In_GB <- paste0('<grade_item_id>',grade_item_id,'</grade_item_id>')}
  else{Disc_Include_In_GB<-""}
  
  topicxml<-paste0(
    '<topic resource_code="trc-',Id,'">','\n',
    '<properties>','\n',
    '<allow_anon>',Disc_Allow_Anon,'</allow_anon>','\n',
    '<is_hidden>',Disc_Is_Hidden,'</is_hidden>','\n',
    '<requires_approval>',Disc_Requires_Approval,'</requires_approval>','\n',
    '<must_post_to_participate>',Disc_Must_Post_To_Participate,'</must_post_to_participate>','\n',
    '<is_locked>',Disc_Is_Locked,'</is_locked>','\n',
    Disc_Score_Out_Of,'\n',
    '<is_auto_score>',Disc_Is_Auto_Score,'</is_auto_score>','\n',
    Disc_Include_In_GB,'\n',
    '<include_nonscored_values>',Disc_Include_Nonscored_Values,'</include_nonscored_values>','\n',
    '<rating_type_id>',Disc_Rating_Type_Id,'</rating_type_id>','\n',
    '</properties>','\n',
    '<content>','\n',
    '<title>',Title,'</title>','\n',
    Disc_Description,'\n',
    '</content>','\n',
    '</topic>')
  return(topicxml)
}







#' Each forum requires a line in the imsmanifest, as well as a file. This command produces that line, and creates the file. 
#' It return a vector with both bits.  
#' Forumxml is one forum.   Topics is a vector of topicxml bits. Forum_Id should come from the CSV file (if blank, use Id.)  
buildForum <- function(forumxml,Forum_Id,outdir="."){
  #We now output the file to outdir/forum_Forum_Id.xml
  require(XML)
  xmlfile <- xmlParse(forumxml)
  cat(toString.XMLNode(xmlfile),file=fullFile(outdir,paste0("forum_",Forum_Id,".xml")))
  #cat(forumxml,file=fullFile(outdir,paste0("forum_",Forum_Id,".xml")))
  #We now make the imsmanifest snippet, using the resource code 'res_discuss-forum_Forum_Id'
  forumims<-paste0('<resource identifier="res_discuss-forum_',Forum_Id,'" type="webcontent" d2l_2p0:material_type="d2ldiscussion" d2l_2p0:link_target="" href="forum_',Forum_Id,'.xml" title=""/>')
  return(forumims)
}




#'  I need to pass in all the rows that either have a disc or forum.  Forums don't show up on content though, so that's tough. 
#'  I could search for disc items, and also for forum items. Forum items can be added on ANY row that's not a disc row.
#'  So I just leave a row for making new forums.
#'  
#'  Now, sometimes we want to group items by forum.  Sometimes we want to just make a new forum for each disc.  This should be easy to do.
#'  Read in the data. Make all the forums that exist, and get a list of their names.
#'  Then go through the disc items, and add the discs to forums, if their names match. If there is no forum that matches an item, then make a new forum first and add the item to that forum.  That should do it. 
#'  
#'  I need to find a nice way to sort the forums.  They should come in order of appearance in the content (my opinion).
#'  
#'  Go down linearly.  If you find a forum, make it.  If you find a disc, make it and add it to a forum.  If that forum doesn't exist yet, make it and then add it.  Each time you add a topic, check to see if it has been added to a forum yet. 
#'  Sometimes we WANT empty forums.  This should be an option.  Let it happen.
#'  
#'  Perhaps the first thing I should do is go through the Forum_ID line, and if it is blank, then fill it in with the ID.  
#'  Second, for each different forum ID, extract the dataframe which matches this ID.  Then build all the topics and load them in. Easy peasy.
#'  If a sub dataframe has only 1 row, then use that row to build both the forum and topic.
#'  If a sub dataframe has more than 1 row, then grab the sub datafram whose Type is not disc. If more than one, ERROR.  Otherwise, use that one row to build the forum and then load the topics. 
#'  That should do it. 

#What needs to pass in the dataframe? Type, Title, Id, Dates, Is_Visible, Then all the Disc_* items. 


buildDiscussionsAndGetResourceXML<-function(df,outdir="."){
  #If there's nothing to do, then don't don anything.
  if(nrow(df)==0){return("")}
  #df should only contain relevant rows, those that have type Type="disc" or have a nonempty Forum$Id.
  #What options do you need to read for each type of object.  Just list that here, then call these later. 
  forum_options<-c("Forum_Title", "Forum_Id", "Disc_Description", "Disc_Allow_Anon", "Disc_Is_Hidden", "Disc_Requires_Approval", "Disc_Is_Locked", "Disc_Must_Post_To_Participate")
  topic_options<-c("Title","Id","Disc_Description", "Disc_Allow_Anon", "Disc_Is_Hidden", "Disc_Requires_Approval", "Disc_Is_Locked", "Disc_Must_Post_To_Participate", "Disc_Score_Out_Of", "Disc_Is_Auto_Score", "Disc_Include_In_GB", "Disc_Include_Nonscored_Values", "Disc_Rating_Type_Id")

  #If there is no Forum_Id present, then use the Id to fill it. Do this for every item.  
  for(i in 1: nrow(df)){
    if(df$Forum_Id[i]==""){df$Forum_Id[i]<-df$Id[i]}
  }
  #Get a list of the Forum_Ids
  Forum_Ids<-levels(factor(df$Forum_Id))
  Forum_Ids
  #Just verify that it works.
  df[,c("Type","Forum_Id","Forum_Title")]
  df[,forum_options]
  df[,topic_options]
  #Count how many forums are needed, and initial the vector to store the xml.
  forumXML<-vector("character",length(Forum_Ids))
  forumXML
  #Intial the vector to store the resource XML.
  forumResources <- vector("character",length(Forum_Ids))
  #This cycles through each forum, and makes the forum xml.
  for(forum in 1:length(Forum_Ids)){ 
    if(Forum_Ids[forum]!=""){
      #reduce the rows to only those with the correct Forum_Id
      forumdf<-df[df$Forum_Id==Forum_Ids[forum],]
      forumdf
      #We have to find the forum settings as our first goal. 
      #Search for those rows that are NOT topics.  Hopefully there is just 1. If there is more than one, then use the first for the options.
      thisForumSettings<-forumdf[forumdf$Type!="disc",forum_options]
      thisForumSettings
      if(nrow(thisForumSettings)==0){#This is what we do if there are no rows. 
        thisForumSettings<-forumdf[1,]
        thisForumSettings$Forum_Title<-thisForumSettings$Title
        thisForumSettings<-thisForumSettings[,forum_options]
      } else{#There was a row with the forum settings on it. 
        thisForumSettings<-thisForumSettings[1,]
      }
      #Time to add topics. We intialize a vector to place the topic xml into.
      topicsXML<-vector("character",nrow(forumdf))
      #Create each topic.
      for(topic in 1:nrow(forumdf)){
        forumdf$Type[topic]
        #It's only a topic if Type="quiz".  This may need to be address later.
        if(forumdf$Type[topic]=="disc"){
          forumdf[topic,topic_options]
          topicsXML[topic]<-do.call(createDiscussionTopic,as.list(forumdf[topic,topic_options]))
        }
      }
      #Now combine the ForumSettings together with the topics, to pass to createDiscussionForum.
      mylist<-as.list(thisForumSettings)
      mylist[["topics"]]<-topicsXML
      #This now makes the forum xml.  
      forumXML[forum]<-do.call(createDiscussionForum,mylist)
      outdir=outputdir
      outdir
      forumResources[forum]<-buildForum(forumXML[forum],Forum_Ids[forum],outdir)
    }
  }
  
  return(forumResources)
}

# #####This section is for testing
# 
# gsheet <- "1EPLvR3IK2DZ-IpeLnIYgrIZYBLyzxt1E0DuSyj0Ttc8"
# require(RCurl)
# fileUrl <- paste0("https://docs.google.com/spreadsheets/d/",gsheet,"/export?format=csv")
# fileCSV <- getURL(fileUrl,.opts=list(ssl.verifypeer=FALSE))
# 
# df <-  read.csv(textConnection(fileCSV),colClasses="character");
# nrow(df)
# df<-df[seq(18,nrow(df)),]
# df
# nrow(df)
# discdf<-df[df$Type=="disc" | df$Forum_Id!="",]
# 
# buildDiscussionsAndGetResourceXML(discdf,outputdif)
