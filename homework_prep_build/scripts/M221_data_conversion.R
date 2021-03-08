###  Read in Math 221 Excel Data files and make CSV and RDS files #####
###  Needs to check consistency of names of files ###
### save out csv and r data files  #####


### libraries #####

library(xlsx)


excel_data_fnames = list.files("excel_data_original/")

for (i in excel_data_fnames){try(xlsxConvert(i,rds="RDS_Files",csv="CSV_Files"))}

wfiles = list.files("RDS_files")
#### These have errors in Excel read in
# [1] "Apollo.xlsx"                      "BabyBoom-JSE.xlsx"                "CauseOfDeath-Malnourished.xlsx"  
#  [4] "COPD-Rehab.xlsx"                  "Gratitude.xlsx"                   "IllinoisBirthWeights.xlsx"       
#  [7] "InsulinResistanceDepression.xlsx" "LASaturnWinter06.xlsx"            "MadisonCountyRealEstate.xlsx"    
# [10] "OreoDoubleStuf.xlsx"              "ZungSim.xlsx"                    

xlsxConvert("COPD-Rehab.xlsx",rds="RDS_Files", csv="CSV_Files")


###### Need to strip xlsx files of the data and then write a readme file with the data provenance.  Also need to write files to 
###### common folder with the different file types available in the folder.  We want the following formats.

		# rds, csv, xlsx, sav, (should we have sas files?)
		# going to make it build from csv
		# will have a second function that strips data provenance from xlsx files


#'	@title Export csv files to other file formats as defined 
#' 	@param csv_file is a character string of the csv files (with its path)
#'	@param export_format is a character vector of formats to convert.  Defaults to ".RDS".  Potential file types are .xlsx, .RDS, .sav, .dta, and sas7bda
#'	@param export_folder is the folder where the exported data will be placed.  In this folder the a subfolder will be created for each csv file name and 
#'  selected export_format file types will be created in the folder.   
#'	@param details  If TRUE then information is printed to screen during the process.  Default is TRUE.
#'	@param readme is the data summary that describes the data.  Accepts a text string describing the provenance. 
#'	@notes Requires the use of the xlsx and haven package. https://github.com/hadley/haven/
#'	@example csvExport()
#'	@export

csvExport  = function(csv_file, export_format=NULL, export_folder,details=TRUE,readme=NULL){

	library(haven)
	library(xlsx)
	if (is.null(export_format))  stop("Don't you want to export something....")
	if(details==TRUE) print(csv_file)
    file_name = rev(strsplit(csv_file,"/")[[1]])[1]
	file_folder = gsub(".csv","",file_name)
	# read in the file 
	# looks like the standard format is to have a column with comments.  This column is removed. 
	temp.file = read.csv(csv_file,stringsAsFactors=F)
	colnames(temp.file) <- gsub("\\.", "_", tolower(colnames(temp.file)))
	if(details==TRUE){
	print(head(temp.file))	
	}
	dir.create(file.path(export_folder,file_folder))
	if(!is.null(readme)) cat(readme,file=file.path(export_folder,file_folder,gsub(".csv",".readme",file_name)))
	if(any(export_format%in%".RDS")==TRUE) saveRDS(temp.file,file=file.path(export_folder,file_folder,gsub(".csv",".RDS",file_name)))
	if(any(export_format%in%".xlsx")==TRUE) write.xlsx(temp.file,file=file.path(export_folder,file_folder,gsub(".csv",".xlsx",file_name)), row.names = FALSE)
	if(any(export_format%in%".sav")==TRUE) write_sav(temp.file,path=file.path(export_folder,file_folder,gsub(".csv",".sav",file_name)))
	if(any(export_format%in%".dta")==TRUE) write_dta(temp.file,path=file.path(export_folder,file_folder,gsub(".csv",".dta",file_name)))
	if(any(export_format%in%".csv")==TRUE) write.csv(temp.file,file=file.path(export_folder,file_folder,file_name),row.names=FALSE)
		
	if(details==TRUE) return(temp.file)

}


# csv_files = list.files("C:/git/bitbucket/byuistatistics/data/csv",full.names=TRUE)
# #csvExport(csv_files[1],export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")


# for (i in 1:length(csv_files)){
# 	csvExport(csv_files[i],export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")	
# 	print(i)
# }


#csvExport("C:/Users/hathawayj/OneDrive/Documents/BYUI/M221_IntroStats/StudyTime_221.csv",export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")


# fpath = "C:/Users/hathawayj/OneDrive/Documents/BYUI/Data"
# files = c("HotDog_Health.txt","Music_Height.csv","Dart_Expert_Dow_6month_anova.txt")


# temp = tempdir()
# write.csv(read.table(file.path(fpath,files[1]),header=T),file=file.path(temp,gsub(".txt",".csv",files[1])),row.names=FALSE)
# csvExport(file.path(temp,gsub(".txt",".csv",files[1])),
# 	       export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")

# library(reshape)
# bob = read.csv(file.path(fpath,files[2]))
# write.csv(melt(bob,na.rm=TRUE),file=file.path(fpath,"Music_Height_long.csv")
# csvExport(file.path(fpath,"Music_Height_long.csv"),
# 	       export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")

# bob = read.table(file.path(fpath,files[3]),header=T)

# write.csv(melt(bob,id.vars="CONTEST.PERIOD"),file.path(temp,gsub(".txt",".csv",files[3])),row.names=FALSE)
# csvExport(file.path(temp,gsub(".txt",".csv",files[3])),
# 	       export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")


#csvExport("C:/Users/hathawayj/OneDrive/Documents/BYUI/Data/movies.csv",export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")


library(readr)
draft <- read_csv("C:/Users/hathawayj/odrive/OneDrive_BYUI/data/Draft_vietnam.csv", na = "#N/A")
write_csv(draft, "C:/Users/hathawayj/odrive/OneDrive_BYUI/data/Draft_vietnam.csv")

csvExport("C:/Users/hathawayj/odrive/OneDrive_BYUI/data/Draft_vietnam.csv",export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")


csvExport("C:/Users/hathawayj/Downloads/MathSelfEfficacy.csv",export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")

csvExport("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.csv",export_format=c(".RDS",".xlsx",".sav",".dta",".csv"),export_folder="C:/git/github/byuistats/data")

