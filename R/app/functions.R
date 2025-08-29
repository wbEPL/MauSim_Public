###############################################################################
## Functions Data Visualization
## Author: Gabriel Lombo
## Contributor: 
## Date: 03/05/2022
## Last Update: 
## R Version: 4.1.2
###############################################################################

# require(pacman)
# p_load(rlang)

#' @title organize_table_long
#' @description Organize the table in a long way with several...
#' @param data table of indicator
#' @param decile decile or quintil 
#' @param ... filters
#' @return data.frame with the data sctructure to make the figures: scenario \code{scenario}, 
#' policy \code{names}, fgt, decile or quintil \code{measure}, value \code{value}, 
#' poverty lines \code{reference}
#' @example
#' organize_table_long()

#' @title organize_table_long
#' @description Organize the table in a long way to create policy tables
#' @param data tool_all
#' @param ... filters
#' @return data.frame with the data sctructure to make the figures: scenario \code{scenario}, 
#' policy \code{names}, fgt, decile or quintil \code{measure}, value \code{value}, 
#' poverty lines \code{reference}
#' @example
#' organize_table_long(table_all, )

assign_type <- function(.data, .names) {
  
  as.data.frame(.data) %>%
    mutate(yd_deciles_pc = as.numeric(yd_deciles_pc)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(deciles_pc = as.numeric(deciles_pc)) %>%
    mutate(decile = as.character(deciles_pc)) %>%
    mutate(quintil = case_when(inRange(deciles_pc, 1, 2) ~ 1,
                               inRange(deciles_pc, 3, 4) ~ 2,
                               inRange(deciles_pc, 5, 6) ~ 3,
                               inRange(deciles_pc, 7, 8) ~ 4,
                               inRange(deciles_pc, 9, 10) ~ 5)) %>%
    merge(.names, by = "variable", all.x = T) %>%
    select(c("n", "scenario", "variable", "names", "deciles_pc", "yd_deciles_pc", "quintil", "decile",
             "measure", "reference", "value"))
  
}

organize_table_long <- function(data, measure, policy, dist){
  .measure <- rlang::as_name(enquo(measure))
  .dist <- enquo(dist)
  
  data %>%
    filter(measure==!!.measure) %>%
    filter(variable %in% policy) %>%
    filter(!is.na(deciles_pc)) %>%
    filter(deciles_pc != 0) %>%
    group_by(n, scenario, !!.dist, names) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    arrange(scenario, names, as.numeric(!!.dist))
}





#' @title read_scope
#' @description Read parameters from metadata
#' @param data scope sheet of metadata
#' @param name filter of Name
#' @param category filter of Caegory
#' @return parameter value as character with or without names
#' @example
#' read_scope(scope, fgt)

read_scope <- function(data, name, category){
  .name <- rlang::as_name(enquo(name))
  .category <- rlang::as_name(enquo(category))
  
  a <- data %>%
    filter(Name == !!.name) %>%
    filter(Category == !!.category)
  
  if (is.na(a$Value2)) a <- a[, c("Category", "Value")]
  if (is.na(a$Value)) a <- a[, c("Category")]
  
  a <- as.character(a %>% select("Value"))
  
  return(a)
}

read_scope2 <- function(data, name){
  .name <- rlang::as_name(enquo(name))
  
  a <- data %>%
    filter(Name == !!.name)
  
  if (all(is.na(a$Value3))) a <- a[, c("Category", "Value", "Value2")]
  if (all(is.na(a$Value2))) a <- a[, c("Category", "Value")]
  if (all(is.na(a$Value))) a <- a[, c("Category")]
  if (length(a) == 5) a <- a[, c("Category", "Value","Value2", "Value3")]
  
  if (length(a) == 1) {
    a <- as.list(a %>% select("Category"))
    a<- a[[1]]
    # a <- strsplit(paste(a$Category, collapse = " "), " ")[[1]]
  }
  if (length(a) == 2) {
    b <- a$Category
    a <- as.list(a %>% select("Value"))
    a <- strsplit(paste(a$Value, collapse = " "), " ")[[1]]
    names(a) <- b
  }
  return(a)
}


#' organize_table_wide()
#' @seealso
#' * [organize_table_long()] other function
#' 
#' 
#' 
#' filter_policy()
#' read_params()
#' fix_scope()




