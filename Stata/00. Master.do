/*============================================================================*\
 Simulation Tool - Mauritania
 Authors: Gabriel Lombo, Madi Mangan, Andrés Gallegos, Daniel Valderrama
 Start Date: January 2024
 Update Date: March 2025
\*============================================================================*/
  
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
	
	scalar t1 = c(current_time)
	
	
	* Activating intermediate steps to test the tool 
	global devmode = 1  		// =1 if the tool is in developers mode
								// In the developers mode all the data is being saved 
								// in .dta files in the subfolders in 3_temp_sim 
	
	global asserts_ref2018 = 0	// indicates the reference scenario is being run and therefore the validation checks are being implemented 
	
	
	* Paramaters to read the parameters of an csv file already produced 
	global csv_file = 0							// Rewrite csv file with parameters scenario 
	global scenario_name_load = "MRT_Ref_2019" 	// Scenario to rewrite the parameters
	global scenario_name_save = "MRT_Ref_2019_v4"		// New name
	
	global save_scenario = 1
	global load_scenario = 0
	global sheetname $scenario_name_save	
	
	
*===============================================================================
// Installing and running packages Isolate Environment
*===============================================================================

sysdir set PLUS "${thedo}/ado"

* Other packages: labutil shp2dta gtools vselect tab_chi ereplace 
local user_commands //Add required user-written commands

foreach command of local user_commands {
	capture which `command'
	if _rc == 111 {
		ssc install `command'
	}
}

*===============================================================================
// Run ado files
*===============================================================================

local files : dir "$theado" files "*.ado"
foreach f of local files{
	 qui: cap run "$theado//`f'"
}


*===============================================================================
// Run pre_simulation files (Only run once)
*===============================================================================

if (0) qui: do "${thedo_pre}/00. Master - Presim.do"

*===============================================================================
// Run simulation files
*===============================================================================

*-------------------------------------
// 00. Set up
*-------------------------------------

if (0) qui: do "${thedo}/00a. Dictionary.do"

if (1)  do "${thedo}/00b. Pullglobals.do"

*-------------------------------------
// 01. P1 - Direct Taxes
*-------------------------------------

if (1) do "${thedo}/01. Direct Taxes - Income Tax.do" 

*-------------------------------------
// 02. P2 - Social Security Contributions
*-------------------------------------

if (1) qui: do "${thedo}/02. Social Security Contributions.do" 

*-------------------------------------
// 03. P3 - Direct Transfers
*-------------------------------------

if (1) qui: do "${thedo}/03. Direct Transfers.do" 

*-------------------------------------
// 04. P4 - Indirect Taxes - Custom Duties
*-------------------------------------

if (1) qui: do "${thedo}/04. Indirect Taxes - Custom Duties.do" 

*-------------------------------------
// 05. P5 - Indirect Subsidies
*-------------------------------------

if (1) do "${thedo}/05. Indirect Subsidies.do" 

*-------------------------------------
// 06. P4 - Indirect Taxes - Excises 
*-------------------------------------

if (1) qui: do "${thedo}/06. Indirect Taxes - Excises.do"
 
*-------------------------------------
// 06. P4 - Indirect Taxes - VAT 
*-------------------------------------
 
if (1) qui: do "${thedo}/07. Indirect Taxes - VAT.do" 

*-------------------------------------
// 05. P6 - In-Kind Transfers
*-------------------------------------

if (1) qui: do "${thedo}/08. In-Kind Transfers.do" 

*-------------------------------------
// 06. Income Aggregates
*-------------------------------------

if (1) qui: do "${thedo}/09. Income Aggregates.do" 

*-------------------------------------
// 07. Process outputs
*-------------------------------------

if (1) qui: do "${thedo}/10. Outputs - Tool.do" 

if (1) qui: do "${thedo}/10. Outputs - R-Shinny.do" 

if (0) qui: do "${thedo}/10. Outputs - Figures.do" 


if "`sce_debug'"=="yes" dis as error  ///
	"You have not turned off the debugging phase in ind tax dofile !!!"


*-------------------------------------
// 08. Cross-validation 
*-------------------------------------

if (0) qui: do "${thedo}/11. Validation.do" 



*===============================================================================
// Launch Excel
*===============================================================================

shell ! "$xls_out"

scalar t2 = c(current_time)

display as error "Running the complete tool took " ///
	(clock(t2, "hms") - clock(t1, "hms")) / 1000 " seconds"


* End of do-file
	