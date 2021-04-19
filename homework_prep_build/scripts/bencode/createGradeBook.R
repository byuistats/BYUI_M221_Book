#Tested and working fine.


#Things to know about the gradebook:
#  (1) identifiers on "items" don't do anthing. If they are not numbers then the item won't import.
#  (2) The WeightDistributionType for each category doesn't do anything.  You have to manually set these.  I got rid of the max grade (which really just set the weight in the category.)
#  (3)After you input your gradebook, change the settings on categories. 

makeGBCategory = function(
  GB_Category_Name,  
  GB_Category_Short_Name, 
  GB_Category_Id, 
  GB_Category_Description="",  
  GB_Category_Show_Description="false",  
  GB_Category_Weight="10",  
  GB_Category_Can_Exceed_Weight="false",  
  GB_Category_Show_Average="false",  
  GB_Category_Show_Distribution="false",  
  GB_Category_Is_Active="true"
){
  if(GB_Category_Show_Description=="") {GB_Category_Show_Description="false"  }
  if(GB_Category_Weight=="")           {GB_Category_Weight="10"}
  if(GB_Category_Can_Exceed_Weight==""){GB_Category_Can_Exceed_Weight="false"}
  if(GB_Category_Show_Average=="")     {GB_Category_Show_Average="false"}
  if(GB_Category_Show_Distribution==""){GB_Category_Show_Distribution="false"}
  if(GB_Category_Is_Active=="")        {GB_Category_Is_Active="true"}

  if(GB_Category_Name==""){print("Grade category missing Name."); return("")}
  bob <- paste(
    '<category identifier="',GB_Category_Id,'">','\n',
    '<name>',reformatHTML(GB_Category_Name),'</name>','\n',
    '<short_name>',reformatHTML(GB_Category_Short_Name),'</short_name>','\n',
    '<show_average>',tolower(GB_Category_Show_Average),'</show_average>','\n',
    '<show_distribution>',tolower(GB_Category_Show_Distribution),'</show_distribution>','\n',
    '<description text_type="text/html" is_displayed="',tolower(GB_Category_Show_Description),'">',reformatHTML(GB_Category_Description),'</description>','\n',
    '<is_active>',tolower(GB_Category_Is_Active),'</is_active>','\n',
    '<scoring>','\n',
    '<weight>',GB_Category_Weight,'</weight>','\n',
    '<can_exceed_weight>',tolower(GB_Category_Can_Exceed_Weight),'</can_exceed_weight>','\n',
    '<is_auto_pointed>false</is_auto_pointed>','\n',
    '<high_non_bonus_drop>0</high_non_bonus_drop>','\n',
    '<low_non_bonus_drop>0</low_non_bonus_drop>','\n',
    '</scoring>','\n',
    '</category>',sep="")
  return(bob)
}

makeGBItem <- function(
  Id,
  GB_Item_Name,
  GB_Item_Short_Name="",
  GB_Item_Category_Id,
  GB_Item_Description="",
  GB_Item_Show_Description="false",
  GB_Item_Max_Points="10",
  GB_Item_Can_Exceed_Weight="false",
  GB_Item_Is_Bonus="false",
  GB_Item_Show_Average="false",
  GB_Item_Show_Distribution="false",
  GB_Item_Is_Active="true"
){
  if(Id==""                        ){ print("Grade item missing ID. Creating item with no Id."); ITEM <- ""}
  else                              {ITEM <-paste('resource_code="grc-',Id,'"',sep="")}
  if(GB_Item_Name==""              ){print("Grade item missing Name. Not making item. "); return("")}
  if(GB_Item_Short_Name!=""        ){GB_Item_Short_Name <- paste('<short_name>',reformatHTML(GB_Item_Short_Name),'</short_name>',"\n",sep="")}
  if(GB_Item_Category_Id!=""       ){GB_Item_Category_Id <- paste('<category_id>',GB_Item_Category_Id,'</category_id>',"\n",sep="")}
  if(GB_Item_Show_Description==""  ){GB_Item_Show_Description <- "false"}
  if(GB_Item_Max_Points==""        ){GB_Item_Max_Points <- "10"}
  if(GB_Item_Can_Exceed_Weight=="" ){GB_Item_Can_Exceed_Weight <- "false"}
  if(GB_Item_Is_Bonus==""          ){GB_Item_Is_Bonus <- "false"}
  if(GB_Item_Show_Average==""      ){GB_Item_Show_Average <- "false"}
  if(GB_Item_Show_Distribution=="" ){GB_Item_Show_Distribution <- "false"}
  if(GB_Item_Is_Active==""         ){GB_Item_Is_Active <- "true"}
  
  bob <- paste(
    '<item ',ITEM,'>',"\n",
    GB_Item_Category_Id,
    '<name>',reformatHTML(GB_Item_Name),'</name>',"\n",
    GB_Item_Short_Name,
    '<show_average>',tolower(GB_Item_Show_Average),'</show_average>',"\n",
    '<show_distribution>',tolower(GB_Item_Show_Distribution),'</show_distribution>',"\n",
    '<description text_type="text/html" is_displayed="',tolower(GB_Item_Show_Description),'">',GB_Item_Description,'</description>',"\n",
    '<type_id>1</type_id>',"\n", #Only allow numeric type problems.
    '<is_active>',tolower(GB_Item_Is_Active),'</is_active>',"\n",
    '<scoring>',"\n",
    '<can_exceed_weight>',tolower(GB_Item_Can_Exceed_Weight),'</can_exceed_weight>',"\n",
    '<out_of>',GB_Item_Max_Points,'</out_of>',"\n",
    '<is_bonus>',tolower(GB_Item_Is_Bonus),'</is_bonus>',"\n",
    '<exclude_from_final_grade_calc>false</exclude_from_final_grade_calc>',"\n",
    '</scoring>',"\n",
    '</item>',"\n",sep="")  
  return(bob);
}


#Now I need to open the right csv file, read in the gradebook, and then cycle through the items. 
#Count the number of categories.
#Count the number of items.
#Make a vector to hold the information.
#Run through all the items, and make them.  Store the information in two vectors. 
#Create the gradebook file, export it.  



#Create the one line snippet of code that includes the gradebook in the imsmanifest.  Return this snippet, or return "" if empty. 
makeGBims <- function(filename="grades_d2l.xml"){  
  return(paste('<resource identifier="res_grades" type="webcontent" d2l_2p0:material_type="d2lgrades" d2l_2p0:link_target="" href="',filename,'" title="" />',sep=""))
}


require(XML)
createGBxml<-function(itemdf,categorydf,outdir="."){
  ncategories <- nrow(categorydf)
  nitems<- nrow(itemdf)
  categoryXMLvec<-vector("character",ncategories)
  itemXMLvec<-vector("character",nitems)
  #Now loop through the items and make the correct XML
  i <- 1
  while (i <= ncategories){
    categoryXML<-do.call(makeGBCategory,as.list(categorydf[i,]))
    categoryXMLvec[i]<-categoryXML
    i <- i+1
  }
  i <- 1
  while (i <= nitems){
    itemXML<-do.call(makeGBItem,as.list(itemdf[i,]))
    itemXMLvec[i] <- itemXML
    i <- i+1
  }
  fullcategoryXML<-""
  if(ncategories!=0){fullcategoryXML<-paste(categoryXMLvec,collapse='\n')}
  fullitemXML<-""
  if(nitems!=0){fullitemXML<-paste(itemXMLvec,collapse='\n')}
  #Now just paste everything together, you're done.
  GBxml <- paste(
    '<?xml version="1.0"?>','\n',
    '<grades>','\n',
    '<configuration>','\n',
    '<auto_update_final_grade>1</auto_update_final_grade>','\n',
    '<grading_system>1</grading_system>','\n',
    '<include_empty_grades_in_final>false</include_empty_grades_in_final>','\n',
    '<show_user_name>0</show_user_name>','\n',
    '<show_user_email>0</show_user_email>','\n',
    '</configuration>','\n',
    '<categories>','\n',
    fullcategoryXML,'\n',
    '</categories>','\n',
    '<items>','\n',
    fullitemXML,'\n',
    '</items>','\n',
    '</grades>','\n',sep="")
  xmlfile <- xmlParse(GBxml)
  cat(toString.XMLNode(xmlfile),file=fullFile(outdir,"grades_d2l.xml"))
  # GBxml 
  # cat(GBxml,"\n",sep="",file=fullFile(outdir,"grades_d2l.xml"))
  return(GBxml)
}























buildGradeBookAndGetGradeBookXML <- function(itemdf,categorydf,outdir=".")
{
  GBxmlfile<-createGBxml(itemdf,categorydf,outdir)
  imsgrades <-makeGBims()
  return(c(imsgrades,GBxmlfile))
}


