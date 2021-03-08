###  Read in Math 221 Excel Data files and make CSV and RDS files #####
###  Needs to check consistency of names of files ###
### save out csv and r data files  #####


### libraries #####

#'	@title Convert Excel Files to CSV files from the M221 Class
#'	@param excel_name is the vector of file names to be converted
#'	@param rds_folder is the folder where the rds data format is to be saved.  
#'  Default is NULL.  If NULL then that file type is not created.  If a folder 
#'  path is listed then the file type will be written to that location.  
#' 	@param csv is the folder where the csv data format is to be saved.  
#'  Default is NULL.  If NULL then that file type is not created.  If a folder 
#'  path is listed then the file type will be written to that location 
#'	@param details  If TRUE then information is printed to screen during the process.  Default is TRUE.
#'	@param excel_dir The directory of the Excel files 
#'	@export

xlsxConvert  = function(excel_name, rds=NULL,csv=NULL,details=TRUE,excel_dir=file.path(getwd(),"Excel_Data_Files")){
	if (is.null(rds) & is.null(csv)) stop("Don't you want to save something....")
	if(details==TRUE) print(excel_name)
	# read in the file 
	# looks like the standard format is to have a column with comments.  This column is removed. 
	temp.file = read.xlsx(file.path(excel_dir,excel_name),sheetIndex=1,stringsAsFactors=F,as.data.frame=F)

	temp.file = temp.file[,!colnames(temp.file)%in%c("Source","source","Sources","sources",
		"Comments","Comment","comments","comment")]

	if(!is.null(rds)) saveRDS(temp.file,file=file.path(rds,gsub(".xls",".RDS",gsub(".xlsx",".RDS",excel_name))))
	if(!is.null(csv)) write.csv(temp.file,file=file.path(csv,gsub(".xls",".csv",gsub(".xlsx",".csv",excel_name))),row.names=FALSE)

	if(details==TRUE){
	print(head(temp.file))	
	}
	
	if(details==TRUE) return(temp.file)

}



