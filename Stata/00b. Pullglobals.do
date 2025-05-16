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


// This assumes you have a csv already created. We are using a shortcut to creat it here 
if ($csv_file == 1) { 
	
	import excel "$xls_sn", sheet("p_${scenario_name_load}") first clear
	export delimited "$presim/params.csv", replace 

	import delimited "$presim/params.csv", varnames(1) clear

	* Drop general globals
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
	
}
else {


*===============================================================================
// Set globals in this do-file
*===============================================================================

*----- Sheet names
global sheet1 "Policy" 
global sheet2 "Params_raw"
global sheet3 "Params_region_raw"
global sheet4 "Params_prod_raw"
global sheet5 "Params_tranches_1_raw"
global sheet6 "Params_tranches_2_raw"


*===============================================================================
// Read Parameters
*===============================================================================

/*-------------------------------------------------------/
	1. Policy Names
/-------------------------------------------------------*/

*------ Policy
import excel "$xls_sn", sheet("$sheet1") firstrow clear

keep if varname != "."

* Policy names
levelsof varname, local(params)
foreach z of local params {
	levelsof varlabel if varname=="`z'", local(val)
	global `z'_lab `val'
}
	
* Policy categories
gen order = _n
bysort category (order): gen count = _n

keep category varname count
ren varname v_	
	
reshape wide v_, i(category) j(count)		
		
egen v = concat(v_*), punct(" ")	
gen globalvalue = strltrim(v)
		
levelsof category, local(params)
foreach z of local params {
	levelsof globalvalue if category=="`z'", local(val)
	global `z'_A `val'
}
	
drop v_1 v globalvalue
	
egen v = concat(v_*), punct(" ")	
gen globalvalue = strltrim(v)
		
levelsof category, local(params)
foreach z of local params {
	levelsof globalvalue if category=="`z'", local(val)
	global `z' `val'
}


/*-------------------------------------------------------/
	2. Parameters
/-------------------------------------------------------*/

*------ Settings
import excel "$xls_sn", sheet("$sheet2") first clear

levelsof globalname, local(params)
foreach z of local params {
	levelsof globalvalue if globalname == "`z'", local(val)
	global `z' `val'
}
	
/*-------------------------------------------------------/
	4. Allocation by region
/-------------------------------------------------------*/
	
forvalues i = 1/ $n_progs {

	import excel "$xls_sn", sheet("$sheet3") first clear	
		
	if "${pr_div_`i'}" == "departement"  | "${pr_div_`i'}" == "region"  {
		
		drop if location ==.				
		destring beneficiaires, replace	
		destring montant, replace		
			
		keep if policy == "am_prog_`i'"
		
		keep location beneficiaires montant	
			
		* As parameters
		levelsof location, local(location)
		foreach z of local location {			
			levelsof beneficiaires if location == `z', local(beneficiaires`z')
			global am_prog_`i'_ben_`z' `beneficiaires`z''
		} 						
		ren location ${pr_div_`i'}			
		save "$tempsim/${pr_div_`i'}_`i'.dta", replace 
	}
}

	
/*	levelsof region, local(region)
	global regionPNBSF `region'
	foreach z of local region {
		levelsof Beneficiaires if region==`z', local(PNBSF_Beneficiaires`z')
		global PNBSF_Beneficiaires`z' `PNBSF_Beneficiaires`z''
		levelsof Montant if region==`z', local(PNBSF_montant`z')
		global PNBSF_montant`z' `PNBSF_montant`z''
	} 
*/
	


/*-------------------------------------------------------/
	5. Parameters by product
/-------------------------------------------------------*/
	
import excel "$xls_sn", sheet("${sheet4}") first clear

levelsof codpr, local(products)
global products "`products'"

* Organize table
drop cod_reduit
ren * value_*
ren value_codpr codpr

reshape long value_, i(codpr) j(var_, string)

tostring codpr, replace
gen globalname = var_ + codpr

ren value_ globalvalue

* Store as parameters
keep globalname globalvalue

levelsof globalname, local(params)
foreach z of local params {
	levelsof globalvalue if globalname == "`z'", local(val)
	global `z' `val'
}		
		

/*-------------------------------------------------------/
	6. Parameters by Tranches - Direct Taxes
/-------------------------------------------------------*/
	
import excel "$xls_sn", sheet("$sheet5") first clear
	
drop if rate == "."
destring rate min max plus, replace
		
gen pol_name = name + "_" + regime	
levelsof pol_name, local(types)
	
replace min = 0 if min == .
replace plus = 0 if plus == .
replace max = 10000000000 if max == .

global names_DirTax ""
foreach t of local types {
	global names_DirTax "$names_DirTax `t'"
	levelsof threshold if pol_name=="`t'", local(tholds)
	global tholds`t' "`tholds'"
		
	foreach z of local tholds {
		levelsof max if threshold=="`z'" & pol_name=="`t'", local(Max`z')
		global max`z'_`t' `Max`z''

		levelsof min if threshold=="`z'" & pol_name=="`t'", local(Min`z')
		global min`z'_`t' `Min`z''
			
		levelsof rate if threshold=="`z'" & pol_name=="`t'", local(Rate`z')
		global rate`z'_`t' `Rate`z''
			
		levelsof plus if threshold=="`z'" & pol_name=="`t'", local(Plus`z')
		global plus`z'_`t' `Plus`z''	
	}
}	
	
/*-------------------------------------------------------/
	7. Parameters by Tranches - Electricity Subsidies
/-------------------------------------------------------*/

import excel "$xls_sn", sheet("$sheet6") first clear
	
drop if Tariff=="."

levelsof Type, local(types)
global typesElec "`types'"
foreach t of local types {
	levelsof Threshold if Type=="`t'", local(tholds)
	global tholdsElec`t' "`tholds'"
	foreach z of local tholds {
			
		levelsof Max  if Threshold=="`z'" & Type=="`t'", local(Max`z')
		global Max`z'_`t' `Max`z''
			
		levelsof Subvention  if Threshold=="`z'" & Type=="`t'", local(Subvention`z') 
		global Subvention`z'_`t' `Subvention`z''
			
		levelsof Tariff  if Threshold=="`z'" & Type=="`t'", local(Tariff`z') 
		global Tariff`z'_`t' `Tariff`z''
	}
}


}