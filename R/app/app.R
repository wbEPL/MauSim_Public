###############################################################################
## Public Dashboard
## Autor: Gabriel Lombo
## Contribuyentes: 
## Fecha: 26/06/2025
## Ultima fecha de Modificacion: 
## Version de R: 4.1.2
###############################################################################

# rm(list = ls())
# setwd("/Users/gabriellombomoreno/Documents/WorldBank/Data_Visuzalization/app")

require(pacman)
p_load(argparse, here, shinydashboard, shiny, shinyjs, dplyr, tidyr, ggplot2, DT, 
       readxl, lubridate, stringr, shinyTime, purrr, shinyWidgets, SpaDES, xlsx, writexl,
       rlang)

#####   Directory    #####
parser  <- ArgumentParser()

parser$add_argument("--tool", default = "AFW_Sim_tool_VI.xlsx")
parser$add_argument("--metadata", default = "Metadata.xlsx")
parser$add_argument("--functions", default = "functions.R")

args <- parser$parse_args()

source(args$functions)


########### CHANGE FROM HERE TO DEPLOY APP  ##########
{
  
  ####    Parameters    #####
  # Read auxiliar data
  policy_scope <- read_excel(args$tool, sheet = "Policy_scope")
  scope <- read_excel(args$metadata, sheet = "Scope")
  label_graph <- read_excel(args$metadata, sheet = "Label_graph") %>%
    mutate(label_en = ifelse(is.na(label_en), "", label_en))
  
  # Default parameters
  n_scenario <- NULL
  t1 <- NULL
  a1 <- NULL
  download_figure_def <- NULL
  defaultData <- as.numeric(read_scope(scope, other, defaultData))
  adm_param2 <- as.numeric(read_scope(scope, other, adm_param2))
  adm_GDP2 <- as.numeric(read_scope(scope, other, adm_GDP2))
  sel_income <- read_scope(scope, other, sel_income)
  sel_pov_line <- read_scope(scope, other, sel_pov_line)
  all_scenarios <- read_scope2(scope, all_scenarios)
  fgt <- read_scope2(scope, fgt)
  income <- read_scope2(scope, income)
  pov_lines <- read_scope2(scope, pov_lines)
  indicator <- read_scope2(scope, indicator)
  sheets <- c("all", "p_")
  
  # Policy Tree
  all_policy_tree <- read_scope2(policy_scope, all_policy_tree)
  tree <- create_tree(all_policy_tree)
  tree_policy_id <- data.frame(
    input = all_policy_tree$Value3,
    return = ifelse(!is.na(all_policy_tree$Value2), all_policy_tree$Value2, all_policy_tree$Value),
    id = c(
      tree[[1]]$children[[1]]$id,
      tree[[1]]$children[[2]]$id,
      tree[[1]]$children[[3]]$id, 
      
      tree[[2]]$children[[1]]$id,
      tree[[2]]$children[[2]]$id,
      tree[[2]]$children[[3]]$id,
      tree[[2]]$children[[4]]$id,
      tree[[2]]$children[[5]]$id,
      
      tree[[3]]$children[[1]]$id,
      tree[[3]]$children[[2]]$id,
      tree[[3]]$children[[3]]$children[[1]]$id,
      tree[[3]]$children[[3]]$children[[2]]$id,
      tree[[3]]$children[[3]]$children[[3]]$id,
      
      tree[[4]]$children[[1]]$children[[1]]$id,
      tree[[4]]$children[[1]]$children[[2]]$id,
      tree[[4]]$children[[1]]$children[[3]]$id,
      
      tree[[4]]$children[[2]]$children[[1]]$id,
      tree[[4]]$children[[2]]$children[[2]]$id,
      tree[[4]]$children[[2]]$children[[3]]$id
      
    ), stringsAsFactors = FALSE)
  
  # Policy Labels
  aux_names <- tree_policy_id %>%
    select(c("input", "return")) %>%
    rename("names" = "return") %>%
    rename("variable" = "input")
  
  ####    Ui        ####
  ui <- fluidPage(
    useShinyjs(),
    
    dashboardPage(
      dashboardHeader(title = "CEQ"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Report", tabName = "report", icon = icon("bar-chart-o"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "report",
                  fluidRow(
                    
                    ##### Select Input  #####
                    tabBox(
                      width = 4, #title = span(tagList(tags$i(class="fa-solid fa-filter"), ""), style = "font-size: 28px"),
                      tabPanel("Options",
                               # h3("Instructions"),
                               # tags$ol(
                               #   tags$li("Add your tool."),
                               #   tags$li("Select one or multiple scenarios, the results will be in the order of your selection."),
                               #   tags$li("Select the indicator and other parameters."),
                               #   tags$li("Press to add or delete a table you want to download, you can previsualize it in the tab Data") ,
                               #   tags$li("Press to download the tables in an excel file, each sheet is a table."),
                               # ),
                               # hr(),
                               fileInput("file", "Select your excel file", width = 250,
                                         placeholder = "AFW_Sim_tool_VI.xlsx",
                                         accept = ".xlsx"),
                               virtualSelectInput('op_scenario', label = "Select scenario (multiple choice):",
                                                  choices = all_scenarios, 
                                                  selected = c("test1_GMB", "test2_MRT", "test1_SEN"),         
                                                  width = 300,
                                                  onServerSearch = "searchLabel", 
                                                  multiple = TRUE),
                               prettyRadioButtons("op_comparison", "Select module:", width = 200,
                                                  choices = c("Compare by policy (only one scenario)" = 1, "Compare by scenario (only one policy)" = 2, 
                                                              "Compare by scenario and policy" = 3),
                                                  selected = 2),
                               prettyRadioButtons("op_indicator", "Select indicator:", width = 200,
                                                  choices = indicator, selected = indicator[2]),
                               treeInput(
                                 inputId = "op_policy_tree",
                                 label = "Select policy (multiple choice):",
                                 choices = tree,
                                 selected = c(tree[[2]]$children[[1]]$id),
                                 returnValue = "id",
                                 closeDepth = 0
                               ),
                               selectizeInput("op_income", "Select income level:", width = 200,
                                              choices = income, selected = "yd"),
                               selectizeInput("op_povline", "Select poverty line:", width = 200,
                                              choices = pov_lines, selected = 1),
                               textInput("op_GDP", "Select GDP (in millions):", width = 200,
                                         placeholder = adm_GDP2),
                               textInput("op_param", "Select Policy admin value (in millions):", width = 200,
                                         placeholder = adm_param2),
                               actionButton("accept_txt", "Accept"),
                      )
                    ),
                    ##### Display  #####
                    tabBox(
                      width = 8, # title = span(tagList(tags$i(class="fa-solid fa-chart-simple"), "Stats"), style = "font-size: 28px"),
                      tabPanel("Figure",
                               plotOutput("figure"),
                               hr(color="gray"),
                               actionButton("add_figure", "Add Figure", icon = tags$i(class="fa-solid fa-cart-shopping")),
                               actionButton("delete_figure", "Delete Figure", icon = tags$i(class="fa-solid fa-trash")),
                               # downloadButton('download', 'Download'),
                               downloadButton("report", "Download Report"),
                               
                               br(), br(),
                               textOutput("download_txt"),
                      )
                      # tabPanel("Data",
                      #          DT::dataTableOutput("table")
                      # )
                      # tabPanel("Params",
                      #          DT::dataTableOutput("params")
                      # ),
                      # tabPanel("Test",
                      #          # DT::dataTableOutput("table_test")
                      #          tableOutput("table_test")
                      # )
                    )
                  )
          )
        )
      )
    )
  )
  
  
  ####     Server        ####
  server <- function(input, output, session) {
    
    options(shiny.maxRequestSize=30*1024^2)
    
    #####   0. Default        --------------------------------------------------
    #####   Reactive Values   --------------------------------------------------
    rV <- reactiveValues(download_figure = download_figure_def)
    rV <- reactiveValues(download_figure_txt = defaultData)
    rV <- reactiveValues(scenarios = all_scenarios)
    rV <- reactiveValues(adm_GDP = adm_GDP2)
    rV <- reactiveValues(adm_param = adm_param2)
    
    #####   Default Values    --------------------------------------------------
    observe({
      rV$scenarios <- scenario_names()
      rV$download_figure <- NULL
      rV$download_figure_txt <- 0
      rV$adm_GDP <- adm_GDP2
      rV$adm_param <- adm_param2
    })
    
    #####   Admin Data input    ------------------------------------------------
    observeEvent(input$accept_txt, {
      rV$adm_GDP <- as.numeric(input$op_GDP)
      rV$adm_param <- as.numeric(input$op_param)
    })
    
    #####   1. Read tool and download tables       -----------------------------
    #####   Upload File                            -----------------------------
    table <- reactive({
      inFile <- input$file
      tab <- lapply(1:length(rV$scenarios), function(i) {
        tab2 <- sapply(sheets, function(j) {
          if (is.null(inFile))  {
            readxl::read_excel(args$tool, sheet = paste0(j, rV$scenarios[i]), 
                               col_types = "text")
          }
          else {
            readxl::read_excel(inFile$datapath, sheet = paste0(j, rV$scenarios[i]), 
                               col_types = "text")
          }
        })
      })
    })
    
    #####   Append scenarios                        ----------------------------
    append <- reactive({
      lapply(1:length(sheets), function(j) {
        for (i in 1:length(input$op_scenario)) {
          n_scenario <- which(input$op_scenario[i]==rV$scenarios)
          
          t2 <- table()[[n_scenario]][[j]] %>% 
            mutate(n = i) %>%
            mutate(scenario = input$op_scenario[i])
          
          t1 <- rbind(t1, t2)
        }
        return(t1)
      })
    })
    
    #####   Download Tables                         ----------------------------
    output$download <- downloadHandler(
      
      filename = function() {
        paste0("Figures_CEQ", ".xlsx")
      },
      content = function(file) {
        write_xlsx(rV$download_figure, path = file)
      }
    )
    
    #####   Add figure                              ----------------------------
    observeEvent(input$add_figure, {
      rV$download_figure_txt = rV$download_figure_txt + 1
      rV$download_figure[[rV$download_figure_txt]] <- table_print()
    })
    
    #####   Delete Figure                           ----------------------------
    observeEvent(input$delete_figure, {
      if (rV$download_figure_txt == 0) {
        return(NULL)
      } else {
        rV$download_figure[[rV$download_figure_txt]] <- NULL
        rV$download_figure_txt = rV$download_figure_txt - 1
      }
    })
    
    
    
    # Report 
    output$report <- downloadHandler(
      filename = "report.pdf",
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- list(n = rV$download_figure_txt, 
                       tab = table_order(), 
                       tab_all = indicator2(), 
                       lab = label_graph, 
                       indicator = input$op_indicator,
                       comparison = input$op_comparison
                       # scenario = input$op_scenario
        ) 
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )    
    
    
    
    #####   2. Scenarios names as input   --------------------------------------
    #####   Upload senarios names         --------------------------------------
    scenario_names <- reactive({
      inFile <- input$file
      if (is.null(inFile))  {
        sheetnames <- as.data.frame(excel_sheets(path = args$tool))
      } else {
        sheetnames <- as.data.frame(excel_sheets(path = inFile$datapath))
      }
      colnames(sheetnames) <- "sheet"
      
      sheetnames <- sheetnames %>% 
        filter(grepl("all", sheet)) %>% 
        filter(sheet!="allUser_def_sce")
      
      as.character(substring(sheetnames$sheet, 4))
    })
    
    #####   Assign scenarios names to input  -----------------------------------
    observeEvent(input$file, {
      rV$scenarios <- scenario_names()
      updateVirtualSelect('op_scenario', 'Select scenario:', 
                          choices = rV$scenarios)
    })
    
    ####  3. Create Policy Tables ####
    indicator2 <- reactive({
      
      # Parameters
      inc_level <- input$op_income
      gdp = rV$adm_GDP
      param = rV$adm_param
      
      keep2 <- as.list(strsplit(paste(input$op_policy_tree, collapse = " "), " ")[[1]])
      sel_policy <- tree_policy_id %>% filter(id %in% keep2)
      keep <- as.list(strsplit(paste(sel_policy$input, collapse = " "), " ")[[1]])
      keep_pov <- as.list(paste0("yc_inc_", substring(keep, 1, nchar(keep)-3)))
      
      table_all <- as.data.frame(append()[[1]]) %>%
        mutate(yd_deciles_pc = as.numeric(yd_deciles_pc)) %>%
        mutate(value = as.numeric(value)) %>%
        mutate(deciles_pc = as.numeric(deciles_pc)) %>%
        arrange(n)
      
      #####   Revenue             ----------------------------------------------
      revenue_all <- organize_table_long(assign_type(table_all, aux_names), benefits, keep, quintil) %>%
        mutate(value = round(value/1000000000, 2))
      
      first <- revenue_all[revenue_all$n == 1, c("names", "quintil", "value")] %>%
        rename("value_tot" = "value") 
      
      revenue <- revenue_all[, c("scenario", "names", "quintil", "value")] %>%
        pivot_wider(id_cols = c(names, quintil), names_from = scenario, values_from = value) %>%
        merge(first, by = c("names", "quintil"), all = T) %>%
        group_by(names) %>%
        mutate_at(vars(-c("names", "quintil")), list(~round(./sum(value_tot)*param, 0)))
      
      #####   Revenue GDP         ----------------------------------------------
      revenue_gdp <- revenue %>%
        mutate_at(vars(-c("names", "quintil", "value_tot")), list(~round(./gdp*100, 2))) %>%
        select(-value_tot) %>%
        gather(scenario, value, -names, -quintil) %>%
        select(c("scenario", "names", "quintil", "value")) %>%
        rename("measure" = "quintil")
      
      #####   Revenue Dif         ----------------------------------------------
      revenue_gdp_dif <- revenue %>%
        mutate_at(vars(-c("names", "quintil", "value_tot")), 
                  list(~(round(100*(.+(-value_tot))/(sum(., na.rm = T)+(-sum(value_tot, na.rm = T))), 2)))) %>%
        select(-value_tot) %>%
        gather(scenario, value, -names, -quintil) %>%
        select(c("scenario", "names", "quintil", "value")) %>%
        rename("measure" = "quintil")
      
      #####   Poverty Dif         ----------------------------------------------
      poverty_gl2 <- table_all %>%
        filter(variable %in% paste0(sel_income, "_pc")) %>%
        filter(reference %in% sel_pov_line  | is.na(reference)) %>%
        filter(measure %in% fgt) %>%
        mutate(value = round(100*value, 2))
      
      first <- poverty_gl2[poverty_gl2$n == 1, c("variable", "measure", "value")] %>%
        rename("value_tot" = "value") 
      
      poverty_dif <- poverty_gl2[, c("scenario", "variable", "measure", "value")] %>%
        pivot_wider(id_cols = c(variable, measure), names_from = scenario, values_from = value) %>%
        merge(first, by = c("variable", "measure"), all = T) %>%
        group_by(variable) %>%
        mutate_at(vars(-c("variable", "measure")), list(~round(.+(-value_tot), 3))) %>%
        select(-value_tot) %>%
        gather(scenario, value, -variable, -measure) %>% 
        mutate(names = "All")
      
      #####   Marginal Contribution        -------------------------------------
      poverty_gl <- table_all %>%
        filter(variable %in% paste0(sel_income, "_pc")) %>%
        filter(measure %in% fgt) %>%
        select(c("scenario", "measure", "reference", "value")) %>%
        rename("value_gl" = "value")
      
      marginal <- table_all %>%
        filter(variable %in% keep_pov) %>%
        filter(reference %in% sel_pov_line  | is.na(reference) ) %>%
        filter(measure %in% fgt) %>%
        select(c("scenario", "variable", "measure", "reference", "value")) %>%
        merge(poverty_gl, by = c("scenario", "measure", "reference"), all.x = T) %>%
        mutate(marginal = round(-100*(value_gl-value), 2)) %>%
        mutate(variable = paste0(substring(variable, 8, nchar(variable)), "_pc" )) %>%
        merge(aux_names, by = "variable", all.x = T) %>%
        select(c("scenario", "names", "measure", "marginal")) %>%
        rename("value" = "marginal") 
      
      #####   Absolute Incidence           -------------------------------------
      incidence_ab <- table_all %>%
        filter(measure=="benefits") %>%
        filter(variable %in% keep) %>%
        mutate(variable = ifelse(is.na(deciles_pc), paste0(variable, "_yd"), paste0(variable, "_ymp"))) %>%
        mutate(decile = ifelse(is.na(deciles_pc), yd_deciles_pc , deciles_pc)) %>% 
        filter(decile!=0) %>%
        filter(grepl("_yd$", variable)) %>%
        select(c("scenario", "decile", "variable", "value")) %>%
        group_by(scenario, variable) %>%
        mutate_at(vars(-c("scenario", "variable", "decile")), list(~(100*./sum(., na.rm = T)))) %>%
        mutate(value = round(value, digits = 2)) %>%
        mutate(variable = substring(variable, 1, nchar(variable)-3)) %>%
        merge(aux_names, by = "variable", all.x = T) %>%
        arrange(scenario, variable, decile) %>%
        select(c("scenario", "names", "decile", "value")) %>%
        rename("measure" = "decile")
      
      #####   Relative Incidence           -------------------------------------
      incidence_rel <- table_all %>%
        filter(measure=="netcash") %>%
        filter(variable %in% keep) %>%
        mutate(variable = ifelse(is.na(deciles_pc), paste0(variable, "_", inc_level), paste0(variable, "_ymp"))) %>%
        mutate(decile = ifelse(is.na(deciles_pc), yd_deciles_pc , deciles_pc)) %>%
        select(c("scenario", "decile", "variable", "value")) %>%
        mutate(value = ifelse(value < 0, value*(-100), value*(100))) %>%
        filter(decile!=0) %>%
        filter(grepl(paste0("_", inc_level), variable)) %>%
        mutate(variable = substr(variable, 1, nchar(variable)-3)) %>%
        mutate(value = round(value, digits = 2)) %>%
        # mutate(decile = as.factor(round(decile))) %>%
        mutate(variable = as.factor(variable)) %>%
        merge(aux_names, by = "variable", all.x = T) %>%
        mutate(decile = as.numeric(decile)) %>%
        arrange(scenario, variable, decile) %>%
        select(c("scenario", "names", "decile", "value")) %>%
        rename("measure" = "decile")
      
      # List of indicator, important to keep the same order as in the input$op_indicator
      list(marginal, incidence_rel, incidence_ab, revenue_gdp, revenue_gdp_dif, poverty_dif)
    })
    
    #### Test ####
    output$table_test <- renderTable({
      table_order()
    })
    
    #####   Table order                             ----------------------------
    table_order <- reactive({
      
      n_indicator <- which(indicator == input$op_indicator)
      
      order_scenarios <- as.data.frame(append()[[1]]) %>%
        select(c("scenario", "n")) %>%
        distinct()
      
      indicator2()[[n_indicator]] %>%
        merge(order_scenarios, by = 'scenario', all = T) %>%
        arrange(n) %>%
        select(-n)
    })
    
    
    #####   Table print                             ----------------------------
    table_print <- reactive({
      
      table_order() %>%
        pivot_wider(id_cols = c(names, measure), names_from = scenario, values_from = value, 
                    values_fill = list(n = 0))
    })
    
    
    
    ####  4. Display  ####
    #####   Text number of figures      ----------------------------------------
    output$download_txt = renderText({
      paste("You added ", rV$download_figure_txt, " figures to the report")
    })
    
    #####   Parameters                  ----------------------------------------
    # output$params <- DT::renderDataTable({
    #   req(input$op_scenario)
    #   
    #   n_sheet <- which(sheets == "all")
    #   
    #   params <- data.frame(append()[[2]]) %>%
    #     pivot_wider(id_cols = globalname, names_from = scenario, values_from = globalcontent)
    #   
    #   if (length(input$op_scenario)==1) {
    #     tab <-params
    #   }
    #   else {
    #     tab <- params %>%
    #       rowwise() %>%
    #       filter(n_distinct(c_across(-globalname)) > 1) %>%
    #       ungroup()
    #   }
    #   datatable(tab,
    #             options = list(scrollX = TRUE),
    #             escape = FALSE,
    #             rownames = FALSE)
    # })
    
    #####   Table                       ----------------------------------------
    output$table <- DT::renderDataTable({
      req(input$op_scenario, input$op_indicator)
      
      datatable(table_print(),
                options = list(scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE)
    })
    
    #####   Figure                      ----------------------------------------
    output$figure <- renderPlot({
      
      req(input$op_scenario, input$op_policy_tree, input$op_comparison)
      
      # Labels
      title = as.character(label_graph %>%
                             filter(indicator == input$op_indicator) %>%
                             filter(variable == "title") %>%
                             select("label_en"))
      subtitle = as.character(label_graph %>%
                                filter(indicator == input$op_indicator) %>%
                                filter(variable == "subtitle") %>%
                                select("label_en"))
      ylab = as.character(label_graph %>%
                            filter(indicator == input$op_indicator) %>%
                            filter(variable == "ylab") %>%
                            select("label_en"))
      xlab = as.character(label_graph %>%
                            filter(indicator == input$op_indicator) %>%
                            filter(variable == "xlab") %>%
                            select("label_en"))
      caption = as.character(label_graph %>%
                               filter(indicator == input$op_indicator) %>%
                               filter(variable == "caption") %>%
                               select("label_en"))
      
      if (input$op_comparison == 1 & length(input$op_scenario) == 1) {
        
        table_order() %>%
          ggplot(aes(x = as.factor(measure), y = value, fill = names)) + 
          geom_bar(stat = "identity", position = position_dodge()) + 
          theme_bw() + scale_fill_brewer(palette = "Paired") +
          labs(title = title, subtitle = subtitle,
               caption = caption) + xlab(xlab) + ylab(ylab) +
          theme(
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.caption = element_text(size = 12)
          )
      }
      
      else if (input$op_comparison == 2 & length(input$op_policy_tree) <= 2) {
        
        table_order() %>%
          ggplot(aes(x = as.factor(measure), y = value, fill = scenario)) + 
          geom_bar(stat = "identity", position = position_dodge()) + 
          theme_bw() + scale_fill_brewer(palette = "Paired") +
          labs(title = title, subtitle = subtitle,
               caption = caption) + xlab(xlab) + ylab(ylab) +
          theme(
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.caption = element_text(size = 12)
          )
      }
      
      else if (input$op_comparison == 3) {
        
        table_order() %>%
          ggplot(aes(x = as.factor(measure), y = value, fill = scenario)) + 
          geom_bar(stat = "identity", position = position_dodge()) + 
          facet_wrap(~names, ncol = 1) +
          theme_bw() + scale_fill_brewer(palette = "Paired") +
          labs(title = title, subtitle = subtitle,
               caption = caption) + xlab(xlab) + ylab(ylab) +
          theme(
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.caption = element_text(size = 12)
          )
      }
      
      else {
        message("Choose correctly the parameters")
      }
      # return(p)
      
    }) 
    
    
    #####   View options                ----------------------------------------
    # by Indicator
    observeEvent(input$op_indicator, {
      if (input$op_indicator %in% indicator[4:5]) {
        shinyjs::hide("op_indicator2"); shinyjs::show("op_policy_tree"); shinyjs::hide("op_income");
        shinyjs::hide("op_povline"); shinyjs::show("op_GDP"); shinyjs::show("op_param");
        shinyjs::show("accept_txt")
      }
      if (input$op_indicator %in% indicator[1:3]) {
        shinyjs::hide("op_indicator2"); shinyjs::show("op_policy_tree"); shinyjs::hide("op_income");
        shinyjs::hide("op_povline"); shinyjs::hide("op_GDP"); shinyjs::hide("op_param");
        shinyjs::hide("accept_txt")
      }
      if (input$op_indicator %in% indicator[6]) {
        shinyjs::hide("op_indicator2"); shinyjs::hide("op_policy_tree"); shinyjs::hide("op_income");
        shinyjs::hide("op_povline"); shinyjs::hide("op_GDP"); shinyjs::hide("op_param");
        shinyjs::hide("accept_txt")
      }
    })
    
  }  
  
  shinyApp(ui = ui, server = server)
}