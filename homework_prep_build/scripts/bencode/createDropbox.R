#Let's wrap this in with the content page, and let Dropbox items be created from that page.  There aren't many other options.
#I'll have to add 2 columns to the content page, to take care of all this. 



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



#' This will make the XML for a category, given a name and a vector of folders. 
createDropCategory <- function(Name,folders=c("")){
  if(Name==""){return(paste(folders,collapse = "\n"))}
  paste(
    '<category name="',Name,'">',"\n",
    paste(folders,collapse = "\n"),"\n",
    '</category>',
    sep="")
}

#This will make the XML for a folder.  It returns the xml and the category name. 
createDropFolder <- function(Title,Id,Due_Date="",Due_Time="",Start_Date="",Start_Time="",End_Date="",End_Time="",Drop_Instructions,Drop_Category,Drop_Out_Of=""){
  if(Title==""){print("No Title for Dropbox Folder.");return(c("",""))}
  if(Id==""){print("No Id for Dropbox Folder.");return(c("",""))}
  OUT_OF<-""
  if(Drop_Out_Of!=""){OUT_OF<-paste(' out_of="',Drop_Out_Of,'" grade_item="grc-',Id,'"',sep="")}
  INSTRUCTIONS<-""
  if(Drop_Instructions!=""){INSTRUCTIONS<-paste(
    '<instructions text_type="text/html"><text>',reformatHTML(Drop_Instructions),'</text></instructions>',sep="")}
  DATE_DUE <-""
  DATE_START <-""
  DATE_END <-""
  if(Due_Date!="")  {DATE_DUE  <-paste('<date_due>',  getGMT(Due_Date,Due_Time),    '</date_due>',  sep="")}
  if(Start_Date!=""){DATE_START<-paste('<date_start>',getGMT(Start_Date,Start_Time),'</date_start>',sep="")}
  if(End_Date!="")  {DATE_END  <-paste('<date_end>',  getGMT(End_Date,End_Time),    '</date_end>',  sep="")}
  
  folderxml<-paste(
    '<folder name="',Title,'" folder_type="2"',OUT_OF,
    ' folder_is_retricted="false" files_per_submission="0" submissions="2" resource_code="drc-',Id,'">',
    DATE_START,DATE_DUE,DATE_END,INSTRUCTIONS,
    '</folder>',
    sep=""
    )
  return(c(folderxml,Drop_Category))  
}

#' Import a data.frame that contains precisely the columns given below, in this order.
#' (Title, Id, Due_Date, Due_Time, Start_Date, Start_Time, End_Date, End_Time, Drop_Instructions, Drop_Category, Drop_Out_Of)
#' @return A vector c(dropIMS,dropXML).  Mostly we'll just use the first entry. Als creates the file dropbox_d2l.xml
buildDropAndGetXML <- function(dropdf,outdir="."){
  
  if(nrow(dropdf)==0){return(c("",""))}
  
  catfac<-factor(dropdf$Drop_Category)
  catfac
  category_Names<-levels(catfac)
  num_categories<-length(category_Names)
  categoryXML<-vector("character",num_categories)
  for(j in 1:num_categories){
    cat<-category_Names[j]
    current_Category<-dropdf[dropdf$Drop_Category==cat,]
    num_folders <- nrow(current_Category)
    myfolders<-vector("character",num_folders)
    for(i in 1:num_folders){
      myfolders[i]<-do.call(createDropFolder,as.list(current_Category[i,]))[1]
    }
    categoryXML[j]<-createDropCategory(cat,myfolders)
  }

  dropstart<-"<dropbox>"
  dropend<-"</dropbox>"
  #' I need to now sort dropdf so that it tells me how many of each folder to put in each category. 
  dropXML <- paste(dropstart,paste(categoryXML,collapse = "\n"),dropend,"\n",sep = "\n")

  xmlfile <- xmlParse(dropXML)
  cat(toString.XMLNode(xmlfile),file=fullFile(outdir,"dropbox_d2l.xml"))
  #cat(dropXML,file=fullFile(outdir,"dropbox_d2l.xml"))
  
  dropIMS<-'<resource identifier="res_dropbox" type="webcontent" d2l_2p0:material_type="d2ldropbox" d2l_2p0:link_target="" href="dropbox_d2l.xml" title="" />'
  return(c(dropIMS,dropXML))    
}
### This Tests the function above, and shows the code needed to use this in createAll.
# filename="course.csv"
# df<-read.csv(filename,colClasses="character")
# #remove the top ??? rows of instructions, the first column, and any columns after 14. 
# df<-df[seq(5,nrow(df)),]
# dropdf<-df[df$Type=="drop",];  dropdf<-dropdf[,c(3,4,8,9,10,11,12,13,16,17,18)]
# head(dropdf)
# 
# myout<-buildDropAndGetXML(dropdf,outputdir)
# myout[1]
# myout[2]
