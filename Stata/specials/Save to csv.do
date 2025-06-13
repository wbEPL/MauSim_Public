clear all
macro drop _all
 
*===============================================================================
// Set Up - Parameters
*===============================================================================

if "`c(username)'"=="wb419055" {
	
	*Running R or Stata folder 
	local Software "Stata"
	
	*Project path: where results are going to be saved 
	global path     	"C:\Users\wb419055\OneDrive - WBG\AWCF1\04 MAURITANIA\PROJECTS\01 MRT Fiscal\00-Public_repository\MauSim-1.0\"
	
	* Data Path: where raw data is being saved, It could be the drop box or one drive folder to be sure we are always using latest data but if you have connection problems you can dowload the data to your local computer
	global pathdata     "C:\Users\wb419055\OneDrive - WBG\AWCF1\04 MAURITANIA\PROJECTS\01 MRT Fiscal\00-Public_repository\MauSim-1.0\01-Data"
	
	* DO files path: Ideally will be also saved in the projects path but Github gives the advantage of saving them wherever you want. Important: If you saved them in the folders project you need to add a subfolder within scripts 
	global thedo     	"${path}/02-Scripts/`c(username)'/`Software'"		
	
	}


if "`c(username)'"=="gabriellombomoreno" {
	local Software 		"Stata"
	global path     	"/Users/gabriellombomoreno/Dropbox/WorldBank/MauSim_Tool"
	global pathdata     "/Users/gabriellombomoreno/Dropbox/WorldBank/MauSim_Tool/01-Data/"
	global thedo     	"${path}/02-Scripts/`c(username)'/`Software'"		
}	
	

*----- Do not modify after this line

	*version 18

	* Data subfolders 
	global data_sn 		"${pathdata}/MRT_2019_EPCV/Data/STATA/1_raw"
    global data_other   "${pathdata}/MRT_FIA_OTHER"

	global presim       "${path}/01-Data/2_pre_sim"
	global tempsim      "${path}/01-Data/3_temp_sim"
	global data_out    	"${path}/01-Data/4_sim_output"

	* Output subfolders 
	global tool         "${path}/03-Outputs/`c(username)'/"	// 	 
	
	
	* global tool         "${path}/03-Outputs" 
	* This gives flexibility that the excel of inputs and outputs to be different but is not being used right now. Right now we use the same excel file but kept the flexibility 
	global xls_sn 		"${tool}/MRT_Sim_tool_VI.xlsx"  
	global xls_out    	"${tool}/MRT_Sim_tool_VI.xlsx"	
	
	* Script subfolders	
	global theado       "$thedo/ado"	
	global thedo_pre    "$thedo/_pre_sim"
	


*===============================================================================
		*Read presim data and save 
*===============================================================================
	
	
*local counter = 1
local files : dir "${presim}" files "*"
foreach f of local files{
	
	local f1 = subinstr("`f'", ".dta", "", .)
	di "`f1'"
	
	cap use "${presim}//`f1'.dta", clear	
	
	label drop _all
	
	export delimited "${presim}/csv//`f1'.csv", replace
	
}	
	

	
* End of do-file
	