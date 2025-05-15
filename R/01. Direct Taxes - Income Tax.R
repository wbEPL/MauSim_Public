# ============================================================================ #
# Simulation des impôts directs - Version finale corrigée
# ============================================================================ #

library(dplyr)
library(haven)

# 1. Chargement des données ---------------------------------------------------
data <- read_dta(file.path(presim, "02_Income_tax_input.dta")) %>%
  select(hhid, 
         allow1_ind_1, allow2_ind_1,
         an_income_1, an_income_2, an_income_3,
         tax_ind_1, tax_ind_2, tax_ind_3,
         regime_1, regime_2, regime_3) %>%
  mutate(across(c(starts_with("allow"), starts_with("an_income")), as.numeric))

# 2. Conversion des paramètres ------------------------------------------------
i <- 1
DirTax_1_allow1_val <- as.numeric(DirTax_1_allow1_val)
DirTax_1_allow2_val <- as.numeric(DirTax_1_allow2_val) 
DirTax_1_allow3_val <- as.numeric(DirTax_1_allow3_val)

# 3. Calcul des abattements ---------------------------------------------------
data <- data %>%
  mutate(
    aux1 = allow1_ind_1 * DirTax_1_allow1_val,
    aux2 = allow1_ind_1 * DirTax_1_allow2_val,
    aux3 = allow2_ind_1 * DirTax_1_allow3_val * an_income_1,
    allow_1 = -(aux1 + aux2 + aux3)
  )

# 4. Calcul de la base imposable ----------------------------------------------
data <- data %>%
  mutate(
    tax_base_1 = pmax(an_income_1 + allow_1, 0),
    tax_base_2 = an_income_2,
    tax_base_3 = an_income_3
  ) %>%
  select(-aux1, -aux2, -aux3, -allow1_ind_1, -allow2_ind_1, -allow_1)

# 5. Calcul de l'impôt par tranche --------------------------------------------
if(exists("names_DirTax")) {
  tax_types <- unlist(strsplit(names_DirTax, " "))
  
  for(tax_num in 1:3) {
    if(tax_num <= length(tax_types)) {
      tax <- tax_types[tax_num]
      data[[paste0("income_tax_", tax_num)]] <- NA_real_
      
      if(exists(paste0("tholds", tax))) {
        thresholds <- get(paste0("tholds", tax))
        
        for(th in thresholds) {
          min_val <- as.numeric(get(paste0("min", th, "_", tax)))
          max_val <- as.numeric(get(paste0("max", th, "_", tax)))
          rate <- as.numeric(get(paste0("rate", th, "_", tax)))
          plus <- as.numeric(get(paste0("plus", th, "_", tax)))
          
          data <- data %>%
            mutate(
              !!paste0("income_tax_", tax_num) := case_when(
                .data[[paste0("tax_base_", tax_num)]] >= min_val &
                  .data[[paste0("tax_base_", tax_num)]] <= max_val &
                  .data[[paste0("tax_ind_", tax_num)]] == 1 ~
                  (.data[[paste0("tax_base_", tax_num)]] - min_val) * rate + plus,
                TRUE ~ .data[[paste0("income_tax_", tax_num)]]
              )
            )
        }
      }
    }
  }
}

# 6. Sauvegarde des résultats -------------------------------------------------
if(devmode) {
  write_dta(data, file.path(tempsim, "Direct_taxes_complete.dta"))
  
  data %>%
    group_by(hhid) %>%
    summarise(
      IRPP = sum(income_tax_1, na.rm = TRUE),
      IBAPP = sum(income_tax_2, na.rm = TRUE),
      Property_Tax = max(income_tax_3, na.rm = TRUE)
    ) %>%
    write_dta(file.path(tempsim, "income_tax_collapse.dta"))
}

message("Calcul des impôts directs terminé avec succès!")
