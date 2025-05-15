# ============================================================================ #
# Chargement des paramètres depuis Excel
# ============================================================================ #

library(readxl)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)

# Noms des feuilles Excel
sheets <- list(
  policy = "Policy",
  params_raw = "Params_raw",
  params_region = "Params_region_raw",
  params_prod = "Params_prod_raw",
  params_tranches_1 = "Params_tranches_1_raw",
  params_tranches_2 = "Params_tranches_2_raw"
)

# ============================================================================ #
# 1. Noms des politiques
# ============================================================================ #

policy_data <- read_excel(xls_sn, sheet = sheets$policy) %>%
  filter(!is.na(varname))

# Version corrigée pour les labels
policy_labels <- setNames(policy_data$varlabel, policy_data$varname)
list2env(as.list(setNames(policy_labels, paste0(names(policy_labels), "_lab"))), envir = .GlobalEnv)

# Catégories de politiques (version corrigée)
policy_categories <- policy_data %>%
  group_by(category) %>%
  summarise(vars = list(varname), .groups = 'drop')

# Création des variables globales par catégorie
for(cat in policy_categories$category) {
  vars <- unlist(policy_categories$vars[policy_categories$category == cat])
  
  # Version avec nommage correct
  assign(paste0(cat, "_A"), vars, envir = .GlobalEnv)
  assign(cat, paste(vars, collapse = " "), envir = .GlobalEnv)
}

# ============================================================================ #
# 2. Paramètres généraux
# ============================================================================ #

params_raw <- read_excel(xls_sn, sheet = sheets$params_raw) %>%
  filter(!is.na(globalname))

# Conversion en variables globales
for(i in 1:nrow(params_raw)) {
  assign(params_raw$globalname[i], params_raw$globalvalue[i], envir = .GlobalEnv)
}

# ============================================================================ #
# 3. Allocation par région
# ============================================================================ #

if(exists("n_progs")) {
  region_params <- read_excel(xls_sn, sheet = sheets$params_region) %>%
    filter(!is.na(location))
  
  for(i in 1:n_progs) {
    pr_div <- get(paste0("pr_div_", i))
    if(pr_div %in% c("departement", "region")) {
      prog_data <- region_params %>%
        filter(policy == paste0("am_prog_", i)) %>%
        mutate(across(c(beneficiaires, montant), as.numeric))
      
      # Sauvegarde par région
      for(loc in unique(prog_data$location)) {
        assign(paste0("am_prog_", i, "_ben_", loc),
               prog_data$beneficiaires[prog_data$location == loc],
               envir = .GlobalEnv)
      }
      
      # Sauvegarde du fichier complet
      prog_data %>%
        select(location, beneficiaires, montant) %>%
        rename(!!pr_div := location) %>%
        haven::write_dta(file.path(tempsim, paste0(pr_div, "_", i, ".dta")))
    }
  }
}

# ============================================================================ #
# 4. Paramètres par produit
# ============================================================================ #

prod_params <- read_excel(xls_sn, sheet = sheets$params_prod)

# Solution robuste pour supprimer cod_reduit si elle existe
cols_to_remove <- intersect("cod_reduit", colnames(prod_params))
if(length(cols_to_remove) > 0) {
  prod_params <- prod_params %>% select(-all_of(cols_to_remove))
}

prod_params <- prod_params %>%
  pivot_longer(-codpr, names_to = "var", values_to = "value") %>%
  mutate(globalname = paste0(var, codpr))

# Conversion en variables globales
for(i in 1:nrow(prod_params)) {
  assign(prod_params$globalname[i], prod_params$value[i], envir = .GlobalEnv)
}

# Liste des produits
products <- unique(prod_params$codpr)
assign("products", products, envir = .GlobalEnv)

# ============================================================================ #
# 5. Paramètres par tranches (Impôts directs)
# ============================================================================ #

tax_tranches <- read_excel(xls_sn, sheet = sheets$params_tranches_1) %>%
  filter(!is.na(rate)) %>%
  mutate(across(c(rate, min, max, plus), as.numeric),
         pol_name = paste(name, regime, sep = "_"),
         min = ifelse(is.na(min), 0, min),
         plus = ifelse(is.na(plus), 0, plus),
         max = ifelse(is.na(max), 1e10, max))

# Stockage des paramètres par type
tax_types <- unique(tax_tranches$pol_name)
assign("names_DirTax", paste(tax_types, collapse = " "), envir = .GlobalEnv)

for(type in tax_types) {
  type_data <- tax_tranches %>% filter(pol_name == type)
  thresholds <- unique(type_data$threshold)
  
  assign(paste0("tholds", type), thresholds, envir = .GlobalEnv)
  
  for(th in thresholds) {
    th_data <- type_data %>% filter(threshold == th)
    assign(paste0("max", th, "_", type), th_data$max, envir = .GlobalEnv)
    assign(paste0("min", th, "_", type), th_data$min, envir = .GlobalEnv)
    assign(paste0("rate", th, "_", type), th_data$rate, envir = .GlobalEnv)
    assign(paste0("plus", th, "_", type), th_data$plus, envir = .GlobalEnv)
  }
}

# ============================================================================ #
# 6. Paramètres par tranches (Subventions électricité)
# ============================================================================ #

elec_tranches <- read_excel(xls_sn, sheet = sheets$params_tranches_2) %>%
  filter(!is.na(Tariff))

elec_types <- unique(elec_tranches$Type)
assign("typesElec", elec_types, envir = .GlobalEnv)

for(type in elec_types) {
  type_data <- elec_tranches %>% filter(Type == type)
  thresholds <- unique(type_data$Threshold)
  
  assign(paste0("tholdsElec", type), thresholds, envir = .GlobalEnv)
  
  for(th in thresholds) {
    th_data <- type_data %>% filter(Threshold == th)
    assign(paste0("Max", th, "_", type), th_data$Max, envir = .GlobalEnv)
    assign(paste0("Subvention", th, "_", type), th_data$Subvention, envir = .GlobalEnv)
    assign(paste0("Tariff", th, "_", type), th_data$Tariff, envir = .GlobalEnv)
  }
}
