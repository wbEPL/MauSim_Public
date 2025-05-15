/*==============================================================================
	To do:			Read Parameters

	Editted by:		Gabriel Lombo
	Laste Updated:	Aug 27, 2024
===============================================================================*/


*===============================================================================
// Set globals in this do-file
*===============================================================================

*===============================================================================
// Read Parameters
*===============================================================================
*Two formats to read parameters, from csv files not linked to the excel sheet or from the clean excel sheet. The doe below should be run only once to create the csv files for R-Shinny developers


if ($csv_file == 1) { // Scenario from the excel that you are chooseing the parameters
	import excel "$xls_sn", sheet("p_${scenario_name_load}") first clear
	export delimited "$presim/params.csv", replace varnames(1)
}

	import delimited "$presim/params.csv", varnames(1) clear

	* Drop general global
	local dropglobal "sheetname scenario_name_save save_scenario load_scenario sheetname Software path pathdata thedo data_sn data_other presim tempsim data_out tool xls_sn xls_out theado thedo_pre t1 asserts_ref2018 devmode scenario_name_load"
	
	foreach i in globalname `dropglobal'   {
		cap drop if globalname == "`i'"
	}
	
	
	* Policy names
	levelsof globalname, local(params)
	foreach z of local params {
		levelsof globalcontent if globalname == "`z'", local(val)
		global `z' `val'
	}
	
	