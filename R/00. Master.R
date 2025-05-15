# ============================================================================ #
# Simulation Tool - Mauritania (Version R)
# Authors: Aliou Diallo, Abou Diallo, Mouhamed Mahmoud, Abou Dieng
# Start Date: April 2025
# Update Date: April 2025
# ============================================================================ #

# Initialisation
rm(list = ls())
gc()

# ============================================================================ #
# Configuration des chemins
# ============================================================================ #

username <- "alioudiallo"
path <- "C:/Users/Index Informatique/Desktop/World_Bank"
pathdata <- file.path(path, "01-Data")
thedo <- file.path(path, "02-Scripts", username, "R")

# Sous-dossiers
data_sn <- file.path(pathdata, "MRT_2019_EPCV/Data/STATA/1_raw")
data_other <- file.path(pathdata, "MRT_FIA_OTHER")
presim <- file.path(path, "01-Data/2_pre_sim")
tempsim <- file.path(path, "01-Data/3_temp_sim")
data_out <- file.path(path, "01-Data/4_sim_output")

# Outputs
tool <- file.path(path, "03-Outputs", username)
xls_sn <- file.path(tool, "MRT_Sim_tool_VI.xlsx")
xls_out <- file.path(tool, "MRT_Sim_tool_VI.xlsx")

# Paramètres d'exécution
devmode <- TRUE
asserts_ref2018 <- FALSE

# Début du chronométrage
t1 <- Sys.time()

# ============================================================================ #
# Chargement des packages
# ============================================================================ #

required_packages <- c("dplyr", "readxl", "openxlsx", "here", "haven", "tidyr", "purrr", "tibble")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# ============================================================================ #
# Chargement des paramètres globaux
# ============================================================================ #

source(file.path(thedo, "00b. Pullglobals.R"))

# ============================================================================ #
# Exécution des modules
# ============================================================================ #

# 01. Impôts directs
source(file.path(thedo, "01. Direct Taxes - Income Tax.R"))

# [Autres modules à décommenter quand prêts]
# source(file.path(thedo, "02. Social Security Contributions.R"))
# source(file.path(thedo, "03. Direct Transfers.R"))

# ============================================================================ #
# Finalisation
# ============================================================================ #

# Ouvrir le fichier Excel des résultats
if(file.exists(xls_out)) {
  shell.exec(xls_out)
}

# Temps d'exécution
t2 <- Sys.time()
message(sprintf("Durée totale d'exécution : %.2f secondes", difftime(t2, t1, units = "secs")))