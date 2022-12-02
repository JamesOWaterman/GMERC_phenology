
### GMERC Phenology Dashboard

# packages ----

library(shiny)
library(shinyWidgets)
library(shinyauthr)
library(tidyverse)
library(rsconnect)
library(magrittr)
library(janitor)
library(splitstackshape)
library(hrbrthemes)
library(stringdist)
library(synthesisr)
library(snakecase)
library(data.table)
library(bslib)
library(waiter)
library(DT)
library(data.table)

# memory ----
memory.limit(size=56000)

# static elements ----

# gmerc_pheno_master <- read.csv("gmerc_pheno_master.csv")
gmerc_pheno_master <- readRDS("gmerc_pheno_master.rds")
tag_key <- read.csv("gmerc_tag_key.csv") 

tag_unique_list <- unique(sort(gmerc_pheno_master$tag_unique))
genus_list <- unique(sort(gmerc_pheno_master$genus))
species_list <- unique(sort(gmerc_pheno_master$species))
common_name_list <- unique(sort(gmerc_pheno_master$common_name))
location_list <- unique(sort(gmerc_pheno_master$location))
observer_list <- unique(sort(gmerc_pheno_master$observer))
pheno_moto_list <- unique(sort(gmerc_pheno_master$pheno_moto))
moto_day_list <- unique(sort(gmerc_pheno_master$moto_day))
moto_plot_list <- unique(sort(gmerc_pheno_master$moto_plot))
item_list <- unique(sort(gmerc_pheno_master$item))
vegetation_list <- unique(sort(gmerc_pheno_master$vegetation))

# define ui ----

shinyApp(                                                                         # shinyApp start (SINGLE)
  ui = fluidPage(                                                                 # fluidPage start (SINGLE)
    
    waiter::use_waiter(),                                                         # progress spinner
    theme = bs_theme(version = 4, bootswatch = "minty"),                          # set theme
    titlePanel(                                                                   # titlePanel start
      title=div(                                                                  # title start
        img(src="gmerc_logo.png", height=198*0.9, width=203*0.9, align="left",
            style="margin-left:5px; margin-right:25px; margin-bottom:20px"),      # logo image (top left)
        "Greater Mahale Ecosystem Research and Conservation: Phenology Data Portal",
        p(),
      )                                                                         # title end
    )                                                                           # titlePanel end
    ,
    
    tabsetPanel(                                                    # tabset panel start (SINGLE)
      tabPanel(                                                     # tabpanel start (1)
        "Data Input", fluid = TRUE,
        sidebarLayout(                                              # sidebar Layout start (1)
          sidebarPanel(                                             # sidebar Panel start (1)
            helpText(                                               # helptext start (1)
              style = "text-align: justify;",
              p(),
              "Enter password, upload new GMERC phenology data and hit the",
              strong("Process"),
              "button below.",
              p(),
              "This will combine the new and existing data.",
              p(),
              "This may take several minutes; please do not exit the page or hit 'back' in browser",
              p(),
            )                                                     # helptext end (1)
            ,
            passwordInput("upload_password", "Password", value = ""),
            
            fileInput(inputId = "new_data",
                      label = "Upload new data (.csv format)",
                      multiple = FALSE,
                      accept = ".csv",
                      buttonLabel = "Browse...",
                      placeholder = "No file selected")
            ,
            
            actionButton("process", "Process")
            ,
          )                                                         # sidebar Panel end (1)
          ,
          
          mainPanel(                                                  # mainPanel start (1)
            div(tableOutput("contents"), style="font-size:65%"),
          )                                                         # mainPanel end (1)
        )                                                           # sidebar Layout end (1)
      )                                                           # tabpanel end (1)
      ,
      
      tabPanel(                                                     # tabpanel start (2)
        "Data Query", fluid = TRUE,
        
        h5("Specify data you wish to access, enter password, and hit the",
           strong("Download Data"),
           "button. By default, all data are selected."),
        h6("Large data queries may take several minutes to compile and download; please do not exit the page or hit 'back' in browser."),
        p(),
        p(),
        fluidRow(                                                               # fluidRow start (1)
          column(width=4, offset=0,                                               # column & style start (2a)
                 div(style = "padding-top:20px; padding-right:0px;                
                         margin-right:-240px; margin-left:100px",
                     
                     dateRangeInput(inputId = "query_date", label = "Date Range",
                                    start = "2009-01-01", end = Sys.Date(),
                                    format = "yyyy-mm-dd", weekstart = 1),
                     
                     pickerInput(inputId = "tag_unique",
                                 label = "Select tag(s):",
                                 choices = tag_unique_list,
                                 selected = tag_unique_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "item",
                                 label = "Select item(s):",
                                 choices = item_list,
                                 selected = item_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "vegetation",
                                 label = "Select vegetation type(s):",
                                 choices = vegetation_list,
                                 selected = vegetation_list,
                                 options = list(
                                   `actions-box` = TRUE,
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ),
                                 multiple = TRUE),
                     
                     passwordInput("download_password", "Password", value = ""),

                     downloadButton("download_data", "Download Data"),

                 )),                                                                # column & style end (2a)
          
          column(width=4, offset=0,                                                 # column & style start (2b)
                 div(style = "padding-top:20px; padding-right:0px;
                       margin-right:-240px; margin-left:50px",
                     
                     pickerInput(inputId = "genus",
                                 label = "Select plant genus('):",
                                 choices = genus_list,
                                 selected = genus_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "species",
                                 label = "Select plant species('):",
                                 choices =species_list,
                                 selected =species_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "common_name",
                                 label = "Select plant(s) (local name):",
                                 choices = common_name_list,
                                 selected = common_name_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "location",
                                 label = "Select location(s):",
                                 choices = location_list,
                                 selected = location_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                 )),                                                            # column & style end (2b)
          
          column(width=4, offset=0,                                             # column & style start (2c)
                 div(style = "padding-top:15px; padding-left:0px;
                       margin-right:-240px; margin-left:30px", 
                     
                     pickerInput(inputId = "observer",
                                 label = "Select observer(s):",
                                 choices = observer_list,
                                 selected = observer_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "pheno_moto",
                                 label = "Select Phenology and/or Burn:",
                                 choices = pheno_moto_list,
                                 selected = pheno_moto_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "moto_day",
                                 label = "Select Burn day(s):",
                                 choices = moto_day_list,
                                 selected = moto_day_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                     pickerInput(inputId = "moto_plot",
                                 label = "Select Burn plot(s):",
                                 choices = moto_plot_list,
                                 selected = moto_plot_list,
                                 options = list(
                                   `actions-box` = TRUE, 
                                   size = 10,
                                   `selected-text-format` = "count > 3"
                                 ), 
                                 multiple = TRUE),
                     
                 )),                                                # column & style end (2c)
        )                                                         # fluidRow end (1)
      )                                                               # tabpanel end (2)
      ,
      
      tabPanel(                                                       # tabpanel start (3)
        "Data Edit", fluid = TRUE,
        fluidRow(
          column(width=12,
                 h5("GMERC Master Data Set: Double-click to edit table cells.
                    Confirm admin password to overwrite master data set."),
                 h6("This may take a few moments; please do not exit the page or hit 'back' in browser until the 'Overwrite' pop-up vanishes.")),
          hr(),
          column(width=4,
                 div(style = "margin-right:0px",
                     passwordInput("edit_password", "Password", value = ""))),
          column(width=4,
                 div(style = "padding-top:33px; margin-left:-100px",
                     actionButton("save", "Overwrite data"))),
          hr(),
          DTOutput("table")
        )
      )                                                               # tabpanel end (3)
      
    )                                                               # tabset panel end (SINGLE)
  )                                                                 # fluidPage end (SINGLE)
  ,
  
  # define server ----
  
  server <- function(input, output, session) {                        # server start (SINGLE)
    
    # import new data ----
    df_upload <- reactive({ 
      readr::read_csv(input$new_data$datapath)
    })
    
    # display head of input data ----
    output$contents <- renderTable({                              
      df <- data.frame(df_upload())
      return(head(df))
    })
    
    # process uploaded data and add to master df ----
    observeEvent(input$process, {                                       # process input data - observeEvent start
                                                                        # end progress spinner when processing complete
      showModal(modalDialog("Processing new data...", footer=NULL))
      
      req(input$upload_password=="alex_piel")
      
      # drop any rows where all values = NA ----
      df <- df_upload() %>%
        dplyr::filter(if_any(everything(), ~ !is.na(.)))            
      
      # clean column/var names ----
      names(df) <- names(df) %>%                                    
        stringr::str_replace("\\s", "_") %>%
        stringr::str_replace("\n", "_") %>%
        stringr::str_replace("-", "_") %>%
        stringr::str_replace("__", "_") %>%
        stringr::str_replace(":", "_")
      names(df) <- tolower(names(df))
      
      # rename columns/vars ----
      df <- df %>%
        dplyr::rename(date_time=start,                              
                      lat_y=geopoint_latitude,
                      lon_x=geopoint_longitude,
                      alt_z=geopoint_altitude,
                      geo_accuracy=geopoint_accuracy,
                      observer=nani,
                      observer_other=mtu_tofauti,
                      pheno_moto=siku,
                      location=eneo,
                      moto_plot=plot,
                      tag_raw_pheno=metali,
                      tag_pheno_other=metali_tofauti,
                      tag_raw_moto=metali2,
                      tag_moto_other=metali_tofauti2,
                      moto_plot_burned_last=moto,
                      moto_plot_burned_evidence=evidence,
                      moto_plot_burned=yesno,
                      vital_status=mzikuf,
                      dbh=mtu_dbh,
                      chimp_nest_50m=kiota_kiota_sokwe,
                      chimp_nest_age1=kiota_kiota_age,
                      chimp_nest_age2=kiota_kiota_age2,
                      chimp_nest_age3=kiota_kiota_age3,
                      chimp_nest_age4=kiota_kiota_age4,
                      new_leaves=mti_data_majani_mapya,
                      mature_leaves=mti_data_majani_kawaida,
                      old_leaves=mti_data_majani_njano,
                      budding_flowers=mti_data_maua_mapya,
                      mature_flowers=mti_data_maua_kawaida,
                      unripe_fruit=mti_data_matunda_mapya,
                      ripe_fruit=mti_data_matunda_kuiva,
                      fallen_fruit=mti_data_matunda_chini,
                      note1_gmerc=vitu_vingine,
                      meta_instanceid=meta_instanceid)
      
      # unpack date_time var ----
      df <- df %>%                                                  
        dplyr::select(-date) %>% 
        dplyr::mutate(date_time=stringr::str_sub(date_time, 5, 50),
                      year=stringr::str_sub(date_time, -4),
                      month=stringr::str_sub(date_time, 1,3),
                      month_num=match(month, month.abb),
                      month=tolower(month),
                      day=stringr::str_sub(date_time, 5,6),
                      time=stringr::str_sub(date_time, 8,15),
                      date=lubridate::make_date(year, month_num, day),
                      date_time=lubridate::ymd_hms(paste(date, time))) %>% 
        dplyr::select(-day)
      
      # check if tag_raw_pheno & tag_raw_moto overlap ----
      df <- df %>%                                                  
        dplyr::mutate(tag_check=dplyr::case_when(                   
          !is.na(tag_raw_pheno) & !is.na(tag_raw_moto) ~ "overlap",
          !is.na(tag_raw_pheno) & is.na(tag_raw_moto) ~ "pheno_only",
          is.na(tag_raw_pheno) & !is.na(tag_raw_moto) ~ "moto_only",
          TRUE ~ ""))
      
      # create single tag_raw var (combine pheno and moto) ----
      df <- df %>%                                                   
        dplyr::mutate(tag_raw=dplyr::case_when(
          is.na(tag_raw_pheno) & !is.na(tag_raw_moto) ~ tag_raw_moto,
          !is.na(tag_raw_pheno) & is.na(tag_raw_moto) ~ tag_raw_pheno,
          TRUE ~ "")) %>%
        dplyr::select(-tag_check, -tag_raw_pheno, -tag_raw_moto)
      
      # check if tag_pheno_other & tag_moto_other overlap ----
      df <- df %>%                                                  
        dplyr::mutate(tag_check=dplyr::case_when(                   
          !is.na(tag_pheno_other) & !is.na(tag_moto_other) ~ "overlap",
          !is.na(tag_pheno_other) & is.na(tag_moto_other) ~ "pheno_only",
          is.na(tag_pheno_other) & !is.na(tag_moto_other) ~ "moto_only",
          TRUE ~ ""))
      
      # make blanks as real_NA for conditional statement below ----
      df$tag_check[df$tag_check == ""] <- NA                        
      
      # create single tag_other var (combine pheno and moto) ----
      df <- df %>%                                                   
        dplyr::mutate(tag_other=dplyr::case_when(
          is.na(tag_pheno_other) & !is.na(tag_moto_other) ~ tag_moto_other,
          !is.na(tag_pheno_other) & is.na(tag_moto_other) ~ tag_pheno_other,
          is.na(tag_pheno_other) & is.na(tag_moto_other) ~ NA_real_,
          TRUE ~ NA_real_)) %>%
        dplyr::select(-tag_check, -tag_moto_other, -tag_pheno_other)
      
      # unpack notes ----
      df$note1_gmerc_raw <- df$note1_gmerc                          
      df <- df %>%
        dplyr::mutate(note1_gmerc=tolower(stringr::str_squish(note1_gmerc)),
                      note1_gmerc=stringr::str_remove(note1_gmerc, "\\.$"),
                      note1_gmerc=stringr::str_remove(note1_gmerc, "^\\."),
                      note1_gmerc=stringr::str_remove(note1_gmerc, "^\\,"),
                      note1_gmerc=stringr::str_remove(note1_gmerc, "\\,$"),
                      note1_gmerc=stringr::str_remove(note1_gmerc, "\\,"),
                      note1_gmerc=stringr::str_replace(note1_gmerc, "^$", "empty"),
                      note1_gmerc=stringr::str_replace(note1_gmerc, "(?<![:alpha;])c\\)", ") control"),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "sada", "sasa"),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "tuma", "tuna"),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "sasa tuna", "namba mpya"),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "sasa ni", "namba mpya"),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "sasa namba mpya", "namba mpya"),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "mamba", "namba"),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "namba mpya ni", ""),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "namba ni", ""),
                      note1_gmerc=stringr::str_replace_all(note1_gmerc, "namba mpya", ""),
                      note3_gmerc_replace=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "replace") ~ note1_gmerc,
                        TRUE ~ "NA"),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "replace") ~ "NA",
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "sio") ~
                          stringr::str_extract(note1_gmerc, "(\\d)+(?= sio|[:punct:]|[:alpha:]|\\s)"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "(?<=namba\\s|sasa\\s|mba\\smpya\\s|sasani\\s)(\\d)+") ~
                          stringr::str_extract(note1_gmerc, "(\\d)+"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "(\\d)+(?=\\smpya)") ~
                          stringr::str_extract(note1_gmerc, "(\\d)+"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "(?<=\\()(\\d)+(?=\\))") ~
                          stringr::str_extract(note1_gmerc, "(\\d)+"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=stringr::str_squish(note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "(?<=\\(\\s|\\.|\\.\\s)(\\d)+") ~
                          stringr::str_extract(note1_gmerc, "(\\d)+"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc_raw, "Namba|namba") ~
                          stringr::str_extract(note1_gmerc, "(\\d)+"),
                        TRUE ~ note1_gmerc),
                      note3_gmerc_new_tag=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "mpya|\\smpya|mpya\\s") ~ "new tag",
                        TRUE ~ "NA"),
                      note3_gmerc_new_tag=dplyr::case_when(
                        stringr::str_detect(tolower(note1_gmerc_raw), "mpya|\\smpya|mpya\\s") ~ "new tag",
                        TRUE ~ "NA"),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "mpya|\\smpya|mpya\\s") ~
                          stringr::str_extract(note1_gmerc, "(\\d{3,})+"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "mpya|\\smpya|mpya\\s") ~ "NA",
                        TRUE ~ note1_gmerc),
                      note3_gmerc_new_tag=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "chaji|\\schaji|chaji\\s") ~ "new tag",
                        TRUE ~ note3_gmerc_new_tag),
                      note3_gmerc_new_tag=dplyr::case_when(
                        stringr::str_detect(tolower(note1_gmerc_raw), "chaji|\\schaji|chaji\\s") ~ "new tag",
                        TRUE ~ note3_gmerc_new_tag),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "chaji|\\schaji|chaji\\s") ~
                          stringr::str_extract(note1_gmerc, "(\\d{3,})+"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "chaji|\\schaji|chaji\\s") ~ "NA",
                        TRUE ~ note1_gmerc),
                      note3_gmerc_plot_control=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "control|\\scontrol|control\\s") ~ "control",
                        TRUE ~ "NA"),
                      note3_gmerc_plot_control=dplyr::case_when(
                        stringr::str_detect(tolower(note1_gmerc_raw), "control|\\scontrol|control\\s") ~ "control",
                        TRUE ~ note3_gmerc_plot_control),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "control|\\scontrol|control\\s") ~
                          stringr::str_extract(note1_gmerc, "(\\d{3,})+"),
                        TRUE ~ note1_gmerc),
                      note3_gmerc_plot_burn=dplyr::case_when(
                        stringr::str_detect(tolower(note1_gmerc_raw), "halija ungua|halijaungua") ~ "not_burned",
                        TRUE ~ "NA"),
                      note3_gmerc_plot_burn=dplyr::case_when(
                        stringr::str_detect(tolower(note1_gmerc_raw), "limeungua") ~ "burned",
                        TRUE ~ note3_gmerc_plot_burn),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "\\sheight|height\\s") ~ "NA",
                        TRUE ~ note1_gmerc),
                      note3_gmerc_plot_name=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "\\(") ~ stringr::str_extract(note1_gmerc, "(?<=\\().*"),
                        TRUE ~ "NA"),
                      note3_gmerc_plot_name=stringr::str_remove(note3_gmerc_plot_name, "\\)"),
                      note3_gmerc_plot_name=stringr::str_remove(note3_gmerc_plot_name, "\\."),
                      note3_gmerc_plot_name=stringr::str_remove(note3_gmerc_plot_name, "\\,"),
                      note1_gmerc=dplyr::case_when(
                        stringr::str_detect(note1_gmerc, "\\(") ~ stringr::str_remove(note1_gmerc, "(?<=\\().*"),
                        TRUE ~ note1_gmerc),
                      note1_gmerc=dplyr::case_when(
                        !is.na(note3_gmerc_plot_name) ~ stringr::str_remove(note1_gmerc, "\\(")),
                      note3_gmerc_plot_name=dplyr::case_when(
                        !is.na(note3_gmerc_plot_name) & stringr::str_detect(tolower(note1_gmerc_raw), "\\(") ~
                          stringr::str_extract(tolower(note1_gmerc_raw), "(?<=\\().*"),
                        TRUE ~ note3_gmerc_plot_name),
                      note3_gmerc_plot_name=stringr::str_remove(note3_gmerc_plot_name, "\\)"),
                      note3_gmerc_plot_name=stringr::str_remove(note3_gmerc_plot_name, "\\."),
                      note3_gmerc_plot_name=stringr::str_remove(note3_gmerc_plot_name, "\\,"))
      
      df$note1_gmerc_raw[df$note1_gmerc_raw == "NA"] <- NA  
      df$note3_gmerc_replace[df$note3_gmerc_replace == "NA"] <- NA  
      df$note3_gmerc_new_tag[df$note3_gmerc_new_tag == "NA"] <- NA  
      df$note3_gmerc_plot_control[df$note3_gmerc_plot_control == "NA"] <- NA  
      df$note3_gmerc_plot_burn[df$note3_gmerc_plot_burn == "NA"] <- NA  
      df$note3_gmerc_plot_name[df$note3_gmerc_plot_name == "NA"] <- NA  
      
      # process observer data ----
      df  <- df  %>%                                                                    
        dplyr::mutate(observer=tolower(observer),
                      observer_other=tolower(observer_other),
                      observer=dplyr::case_when(
                        observer=="tofauti" ~ observer_other,
                        TRUE ~ observer)) %>% 
        dplyr::select(-observer_other)
      
      # process dbh ----
      df$dbh <- stringr::str_replace(df$dbh, "\\.\\s", " ")
      df$dbh <- stringr::str_squish(df$dbh)
      df$dbh <- stringr::str_remove_all(df$dbh, "cm")
      df$dbh <- stringr::str_replace(df$dbh, "\\.\\,", ",")
      df$dbh <- stringr::str_remove_all(df$dbh, "\\.$")
      df$dbh <- stringr::str_replace_all(df$dbh, "(?<!\\,)\\s", ", ")
      df$dbh <- stringr::str_replace_all(df$dbh, "\\,\\.\\,", ",")
      
      # process pheno_moto errors ----
      location_pheno <- c("cam#2","gombe","jilani","junction","matawi","mchungwa","mgumu","mkutano", "mlumba1", "mlumba2")
      
      df  <- df  %>% 
        dplyr::mutate(pheno_moto=tolower(pheno_moto),
                      location=tolower(location),
                      pheno_moto=dplyr::case_when(
                        is.na(pheno_moto) & location %in% location_pheno ~ "pheno",
                        is.na(pheno_moto) & location=="motoplots" ~ "moto",
                        TRUE ~ pheno_moto),
                      location=dplyr::case_when(
                        is.na(location) & pheno_moto=="moto" ~ "moto",
                        location=="motoplots" ~ "moto",
                        TRUE ~ location))
      
      # process moto_plot errors ----
      df <- df %>% 
        dplyr::mutate(moto_plot=dplyr::case_when(
          is.na(moto_plot) ~ note3_gmerc_plot_name,
          TRUE ~ moto_plot),
          moto_plot=tolower(moto_plot),
          moto_plot=stringr::str_squish(moto_plot),
          moto_plot=stringr::str_replace(moto_plot, "\\s|\n|-|__|:", "_"),
          moto_plot=stringr::str_replace(moto_plot, "c$", "_control"),
          moto_plot=stringr::str_replace(moto_plot, "sarah_5", "sarah5"),
          moto_plot=stringr::str_replace(moto_plot, "sarah_10", "sarah10"),
          moto_plot=stringr::str_replace(moto_plot, "bs_03", "bs03"),
          moto_plot=stringr::str_replace(moto_plot, "bs_15", "bs15"),
          moto_plot=stringr::str_replace(moto_plot, "bs_8", "bs8"),
          moto_plot=stringr::str_replace(moto_plot, "2020_f|plot_2020 f|plot2020_f|plt2020f.", "plt2020f"),
          moto_plot=stringr::str_replace(moto_plot, "plot_2020 y", "plot_2020y"),
          moto_plot=stringr::str_replace(moto_plot, "\\s", "_"),
          moto_plot=stringr::str_replace(moto_plot, "\\.$", ""))
      
      # process moto day errors/omissions ----
      moto_day1 <- c("G02",	"G02_Control",	"Matawi_Plot",	"Matawi_Plot_Control",	"MOBALI_PLOT3",	"MOBALI_PLOT3_Control",
                     "PLT2020Y",	"PLT2020Y_Control",	"Y13-57",	"Y13-57_Control",	"Y19-29",	"Y19-29_Control",	"Y19-45",
                     "Y19-45_Control",	"Y19-92",	"Y19-92_Control")
      
      moto_day2 <- c("SARAH5",	"SARAH5_Control",	"G55",	"G55_Control",	"BS8",	"BS8_Control",	"Y14-88",	"Y14-88_Control",
                     "BS03",	"BS03_Control",	"Y14-80",	"Y14-80_Control",	"PLT2020N",	"PLT2020N_Control",	"PLT2020O",	"PLT2020O_Control",
                     "Y19-46",	"Y19-46_Control",	"Y19-55",	"Y19-55_Control")
      
      moto_day3 <- c("G31",	"G31_Control",	"G34",	"G34_Control",	"G58",	"G58_Control",	"Y19-15",	"Y19-15_Control",	"Y19-41",
                     "Y19-41_Control",	"Y19-42",	"Y19-42_Control",	"Y19-64",	"Y19-64_Control")
      
      moto_day1 <- stringr::str_squish(tolower(moto_day1))
      moto_day1 <- stringr::str_replace(moto_day1, "-", "_")
      
      moto_day2 <- stringr::str_squish(tolower(moto_day2))
      moto_day2 <- stringr::str_replace(moto_day2, "-", "_")
      
      moto_day3 <- stringr::str_squish(tolower(moto_day3))
      moto_day3 <- stringr::str_replace(moto_day3, "-", "_")
      
      length(moto_day1); length(intersect(moto_day1, df$moto_plot))
      length(moto_day2); length(intersect(moto_day2, df$moto_plot))
      length(moto_day3); length(intersect(moto_day3, df$moto_plot))
      
      df <- df %>%
        dplyr::mutate(moto_day=tolower(moto_day),
                      moto_day=replace(moto_day, moto_plot %in% moto_day1 & is.na(moto_day), "moto1"),
                      moto_day=replace(moto_day, moto_plot %in% moto_day2 & is.na(moto_day), "moto2"),
                      moto_day=replace(moto_day, moto_plot %in% moto_day3 & is.na(moto_day), "moto3"))
      
      # process moto plot burned ----
      df <- df %>% 
        dplyr::mutate(moto_plot_burned=tolower(moto_plot_burned),
                      moto_plot_burned=dplyr::case_when(
                        note3_gmerc_plot_burn=="burned" ~ "burned",
                        note3_gmerc_plot_burn=="not_burned" ~ "not_burned",
                        TRUE ~ moto_plot_burned),
                      moto_plot_burned=stringr::str_replace(moto_plot_burned, "ndio", "burned"),
                      moto_plot_burned=stringr::str_replace(moto_plot_burned, "hapana", "not_burned"))
      
      # process moto plot burned last ----
      df <- df %>% 
        dplyr::mutate(moto_plot_burned_last=dplyr::case_when(
          moto_plot_burned_last=="Leo" ~ "today",
          moto_plot_burned_last=="Mwezi" ~ "this_month",
          moto_plot_burned_last=="Wiki" ~ "this_week",
          moto_plot_burned_last=="Zaidi_ya_mwezi" ~ "over_a_month",
          moto_plot_burned_last=="Zaidi" ~ "over_a_month",
          TRUE ~ moto_plot_burned_last))
      
      # process moto plot burned evidence ----
      df <- df %>% 
        dplyr::mutate(moto_plot_burned_evidence=dplyr::case_when(
          moto_plot_burned_evidence=="Hamna" ~ "no_evidence",
          moto_plot_burned_evidence=="Majivu" ~ "ash_fresh",
          moto_plot_burned_evidence=="Majivu2" ~ "ash_old",
          moto_plot_burned_evidence=="moshi" ~ "smoke_see",
          moto_plot_burned_evidence=="moshi2" ~ "smoke_smell",
          TRUE ~ moto_plot_burned_evidence))
      
      # process vital status ----
      df <- df %>% 
        dplyr::mutate(vital_status=tolower(vital_status),
                      vital_status=dplyr::case_when(
                        stringr::str_detect(vital_status, "mzima") ~ "alive",
                        stringr::str_detect(vital_status, "kufa") ~ "dead",
                        TRUE ~ ""))
      
      # process chimp nest 50m ----
      df <- df %>%                                           # temporarily recode No as 0, Yes as 1 (aids pivot)                                       
        dplyr::mutate(chimp_nest_50m=dplyr::recode(
          chimp_nest_50m, Hapana=0, Ndio=1))
      
      # process tag numbers ----
      df <- df %>% 
        dplyr::mutate(tag_raw=tolower(tag_raw),
                      tag_clean=dplyr::case_when(
                        tag_raw=="tofauti" | is.na(tag_raw) ~ as.character(tag_other),
                        TRUE ~ tag_raw)) %>% 
        dplyr::filter(!is.na(tag_clean)) %>% 
        dplyr::mutate(tag_clean=stringr::str_remove_all(tag_clean, "[:alpha:]"),
                      tag_clean=stringr::str_remove_all(tag_clean, "[:punct:]"),
                      tag_clean=as.numeric(tag_clean),
                      tag_rp=paste(tag_clean,"_16", sep="")) %>% 
        dplyr::select(-tag_raw, -tag_other)
      
      # identify and process duplicates ----
      df <- df %>%                                                      # create empty var to hold duplicate error flag
        dplyr::mutate(note_duplicate_error=as.character(NA_real_))
      df$note_duplicate_error[df$note_duplicate_error == "NA"] <- NA
      
      df <- df %>% 
        dplyr::distinct(meta_instanceid, .keep_all=TRUE)                # drop any meta_instanceid duplicates
      
      df <- df %>%                                                     # if duplicate tag, day, observer, and moto plot > retain only 1st record
        dplyr::arrange(tag_rp, date, time) %>% 
        dplyr::group_by(tag_rp, date, moto_plot, observer) %>%
        dplyr::filter(dplyr::row_number()==1) 
      
      df <- df %>%                                                 # duplicate tags on same day, by same observer, but moto plot recorded differently
        dplyr::arrange(tag_rp, date, time) %>%                    # genuine data collection error > make unique and flag
        dplyr::group_by(tag_rp, date) %>%
        dplyr::mutate(dummy=as.integer(dplyr::n_distinct(moto_plot)),
                      tag_rp=dplyr::case_when(
                        dummy>1 ~ paste(tag_rp, pheno_moto, moto_plot, sep="_"),
                        TRUE ~ tag_rp),
                      note_data_coll_error=as.character(NA_real_),
                      note_data_coll_error=dplyr::case_when(
                        dummy>1 ~ "data_collection_error",
                        TRUE ~ ""),
                      tag_rp=stringr::str_remove(tag_rp, "_NA"))  %>% 
        dplyr::select(-dummy)
      
      df$note_data_coll_error[df$note_data_coll_error == "NA"] <- NA 
      
      df <- df %>%                                                 # duplicate tags in same year-month, but moto plot recorded differently
        dplyr::arrange(tag_rp, year, month) %>%                    # genuine data collection error > make unique and flag
        dplyr::group_by(tag_rp, year, month) %>%
        dplyr::mutate(dummy=as.integer(dplyr::n_distinct(moto_plot)),
                      tag_rp=dplyr::case_when(
                        dummy>1 ~ paste(tag_rp, pheno_moto, moto_plot, sep="_"),
                        TRUE ~ tag_rp),
                      note_data_coll_error=dplyr::case_when(
                        dummy>1 ~ "data_collection_error",
                        TRUE ~ ""),
                      tag_rp=stringr::str_remove(tag_rp, "_NA"))  %>% 
        dplyr::select(-dummy)
      
      df$note_data_coll_error[df$note_data_coll_error == "NA"] <- NA 
      
      df <- df %>%                                                 # duplicate tags in same year-month, but location recorded differently
        dplyr::arrange(tag_rp, year, month) %>%                    # genuine data collection error > make unique and flag
        dplyr::group_by(tag_rp, year, month) %>%
        dplyr::mutate(dummy=as.integer(dplyr::n_distinct(location)),
                      tag_rp=dplyr::case_when(
                        dummy>1 ~ paste(tag_rp, pheno_moto, location, sep="_"),
                        TRUE ~ tag_rp),
                      note_data_coll_error=dplyr::case_when(
                        dummy>1 ~ "data_collection_error",
                        TRUE ~ ""),
                      tag_rp=stringr::str_remove(tag_rp, "_NA"))  %>% 
        dplyr::select(-dummy)
      
      df$note_data_coll_error[df$note_data_coll_error == "NA"] <- NA 
      
      # identify genuine tag-year-month duplicates
      
      df <- df %>%                                               # same tag recorded twice in a month
        dplyr::group_by(tag_rp, year, month) %>%                  # genuine duplicate error > make unique and flag
        dplyr::mutate(dummy=as.integer(dplyr::n()),
                      month=dplyr::case_when(
                        dummy>1 ~ make.unique(as.character(month), sep="."),
                        TRUE ~ as.character(month)),
                      month_num=dplyr::case_when(
                        dummy>1 ~ make.unique(as.character(month_num), sep="."),
                        TRUE ~ as.character(month_num)),
                      note_duplicate_error=dplyr::case_when(
                        dummy>1 ~ "genuine_dupe",
                        TRUE ~ ""),
                      month=stringr::str_replace_all(month, "(\\d+)$", function(x) as.numeric(x) + 1),
                      month_num=stringr::str_replace_all(month_num, "(\\d+)$", function(x) as.numeric(x) + 1)) %>% 
        dplyr::select(-dummy)
      
      df$note_duplicate_error[df$note_duplicate_error == "NA"] <- NA 
      
      # recode char vars (except vital status) to numeric ----
      df <- df %>% 
        dplyr::mutate(moto_plot_burned=dplyr::recode(             # allows for simple pivot - change back post-pivot
          moto_plot_burned, burned=1, not_burned=0),
          moto_plot_burned_last=dplyr::recode(
            moto_plot_burned_last, today=1, this_week=2, this_month=3, over_a_month=4),
          moto_plot_burned_evidence=dplyr::recode
          (moto_plot_burned_evidence, no_evidence=0, ash_fresh=1, ash_old=2, smoke_smell=3, smoke_see=4))
      
      df <- df %>% 
        dplyr::select(date_time:moto_plot, dbh, vital_status, everything()) %>% 
        tidyr::pivot_longer(., cols=moto_plot_burned_last:fallen_fruit, names_to="item", values_to="value_raw")
      
      df <- df %>% 
        dplyr::mutate(value_clean=as.character(value_raw)) %>% 
        dplyr::mutate(value_clean=dplyr::case_when(
          item=="moto_plot_burned" & value_clean==1 ~ "burned",
          item=="moto_plot_burned" & value_clean==0 ~ "not_burned",
          item=="moto_plot_burned_last" & value_clean==1 ~ "today",
          item=="moto_plot_burned_last" & value_clean==2 ~ "this_week",
          item=="moto_plot_burned_last" & value_clean==3 ~ "this_month",
          item=="moto_plot_burned_last" & value_clean==4 ~ "over_a_month",
          item=="moto_plot_burned_evidence" & value_clean==0 ~ "no_evidence",
          item=="moto_plot_burned_evidence" & value_clean==1 ~ "ash_fresh",
          item=="moto_plot_burned_evidence" & value_clean==2 ~ "ash_old",
          item=="moto_plot_burned_evidence" & value_clean==3 ~ "smoke_smell",
          item=="moto_plot_burned_evidence" & value_clean==4 ~ "smoke_see",
          TRUE ~ value_clean),
          value_clean_01=dplyr::case_when(                                                    # create binomial phenology values
            item %in% c("new_leaves", "mature_leaves", "old_leaves", "budding_flowers",
                        "mature_flowers", "unripe_fruit", "ripe_fruit", "fallen_fruit") & value_raw>0 ~ "1",
            item %in% c("new_leaves", "mature_leaves", "old_leaves", "budding_flowers",
                        "mature_flowers", "unripe_fruit", "ripe_fruit", "fallen_fruit") & value_raw==0 ~ "0",
            TRUE ~ "")) %>% 
        dplyr::select(-value_raw)
      
      # correct vital status errors and PIVOT items ----
      
      # where tag is later shown to be alive, change vital_status to mzima and overwrite NA phenol scores with zero
      df <- df  %>%
        dplyr::group_by(tag_rp) %>%  # watch this bit!
        dplyr::arrange(tag_rp, date_time, .by_group=T) %>% 
        dplyr::mutate(k_occ=cumsum(tidyr::replace_na(vital_status=="dead",0)),
                      vital_status_clean=ifelse(
                        dplyr::last(vital_status)=="alive" & vital_status=="dead", "alive",
                        ifelse(
                          dplyr::last(vital_status)=="alive" & vital_status=="alive", "alive",
                          ifelse(
                            dplyr::last(vital_status)=="dead" & vital_status=="alive" & k_occ >= 1, "dead", vital_status)))) %>% 
        dplyr::mutate(value_clean=ifelse(vital_status_clean=="alive" & vital_status=="alive", value_clean,
                                         ifelse(vital_status_clean=="dead" & vital_status=="dead", value_clean,
                                                ifelse(vital_status_clean=="dead" & vital_status=="alive", NA,
                                                       ifelse(vital_status_clean=="alive" & vital_status=="dead", 0, value_clean))))) %>% 
        dplyr::mutate(note_vital_status=as.character(NA_real_),
                      note_vital_status=ifelse(vital_status!=vital_status_clean, "jw_vital_status_correction", ""))
      
      df <- df %>% 
        dplyr::mutate(vital_status=dplyr::recode(vital_status_clean, alive=1, dead=0)) %>% 
        dplyr::select(-vital_status_clean, -k_occ)
      
      df <- df %>%                                                                              # pivot item names and values
        tidyr::pivot_longer(., cols=vital_status, names_to="item2", values_to="value_clean2")
      
      df <- df %>% 
        tidyr::pivot_longer(., cols=c("item", "item2"), names_to="item3", values_to="value_clean3")
      
      df <- df %>% 
        dplyr::mutate(value=dplyr::case_when(
          item3=="item2" ~ as.character(value_clean2),
          item3=="item" ~ value_clean,
          TRUE ~ "")) %>% 
        dplyr::select(-value_clean, -value_clean2, -item3) %>% 
        dplyr::rename(value_clean=value, item=value_clean3)
      
      df$note_data_coll_error[df$note_data_coll_error == ""] <- NA  
      df$value_clean_01[df$value_clean_01 == ""] <- NA 
      df$note_vital_status[df$note_vital_status == ""] <- NA 
      
      df <- df %>%                                              # convert vital_status back from numeric to character
        dplyr::mutate(value_clean=dplyr::case_when(
          item=="vital_status" & value_clean=="1" ~ "alive",
          item=="vital_status" & value_clean=="0" ~ "dead",
          TRUE ~ value_clean))
      
      df <- df %>% 
        tidyr::pivot_longer(., cols=value_clean_01, names_to="item2", values_to="value_clean2")   # pivot binomial scores
      
      df <- df %>% 
        dplyr::mutate(item2=dplyr::case_when(
          item %in% c("new_leaves", "mature_leaves", "old_leaves",
                      "budding_flowers", "mature_flowers", "unripe_fruit",
                      "ripe_fruit", "fallen_fruit") ~ stringr::str_replace(item2, "value_clean", item),
          TRUE ~ "delete"))
      
      df <- df %>% 
        tidyr::pivot_longer(., cols=c("item", "item2"), names_to="item3", values_to="value_clean3") %>% 
        dplyr::filter(value_clean3!="delete")
      
      df <- df %>% 
        dplyr::mutate(value=dplyr::case_when(
          item3=="item2" ~ value_clean2,
          item3=="item" ~ value_clean,
          TRUE ~ "")) %>% 
        dplyr::select(-value_clean, -value_clean2, -item3) %>% 
        dplyr::rename(value_clean=value, item=value_clean3)
      
      # add numeric suffix to month singles/duplicates ----
      df <- df %>%                                                  # if month and month_num are already suffixed bc of duplicates, do nothing
        dplyr::mutate(month=dplyr::case_when(                       # if no suffix, add ".1"
          stringr::str_detect(month, "\\.") ~ month,
          TRUE ~ paste(month, "1", sep=".")),
          month_num=dplyr::case_when(
            stringr::str_detect(as.character(month_num), "\\.") ~ as.character(month_num),
            TRUE ~ paste(month_num, "1", sep=".")),
          month_num=as.numeric(month_num),
          month_num=sprintf("%04.1f", month_num))
      
      # populate common_name, gps, scientific name etc. ----
      tag_key <- tag_key %>% 
        dplyr::select(-tag)
      
      df <- dplyr::left_join(df, tag_key, by=c("tag_rp"="tag_unique"))    # merge
      
      df <- df %>% 
        dplyr::mutate_at(c("lat_y.x", "lon_x.x", "alt_z.x", "geo_accuracy"), as.numeric) %>% 
        dplyr::mutate(
          lat_y.y=dplyr::case_when(
            is.na(lat_y.y) ~ lat_y.x,
            TRUE ~ lat_y.y),
          lon_x.y=dplyr::case_when(
            is.na(lon_x.y) ~ lon_x.x,
            TRUE ~ lon_x.y),
          alt_z.y=dplyr::case_when(
            is.na(alt_z.y) ~ alt_z.x,
            TRUE ~ alt_z.y),
          location2=dplyr::case_when(
            is.na(trail) ~ location,
            TRUE ~ trail)) %>% 
        dplyr::select(-lat_y.x, -lon_x.x, -alt_z.x, -trail, -location) %>% 
        dplyr::rename(lat_y=lat_y.y,
                      lon_x=lon_x.y,
                      alt_z=alt_z.y,
                      location=location2)
      
      # prepare to merge with master df ----
      df <- df %>% 
        dplyr::rename(tag_unique=tag_rp,
                      value=value_clean) %>% 
        dplyr::select(-tag_clean, -note1_gmerc_raw) %>% 
        dplyr::mutate_at(c("dbh", "date_time", "date"), as.character) %>%
        # dplyr::mutate_at(c("month_num"), as.numeric) %>%
        dplyr::mutate_at(c("lat_y", "lon_x", "alt_z", "year", "month_num"), as.factor) %>%
        dplyr::mutate(day=as.integer(stringr::str_sub(date_time, 9,10)))
      
      df <- tidyr::unite(df, note_gmerc, note1_gmerc, note3_gmerc_new_tag, note3_gmerc_plot_burn,
                         note3_gmerc_plot_control, note3_gmerc_plot_name, note3_gmerc_replace,
                         sep=";", na.rm=T, remove=T)
      
      df$note_gmerc[df$note_gmerc == ""] <- NA 
      df$note_duplicate_error[df$note_duplicate_error == ""] <- NA 
      
      df <- df %>% 
        dplyr::select(tag_unique,
                      scientific_name, genus, species, common_name, location,
                      lat_y, lon_x, alt_z, geo_accuracy, observer,
                      date_time, date, time, year, month, month_num, day,
                      pheno_moto, moto_day, moto_plot, dbh,
                      meta_instanceid, note_gmerc, note_vital_status,
                      note_duplicate_error, note_data_coll_error,
                      item, value) %>% 
        dplyr::distinct()
      
      # extract copies of any error-flagged records for AP examination ----
      df_errors <- df %>% 
        dplyr::filter(!is.na(note_duplicate_error) | !is.na(note_data_coll_error) | !is.na(note_vital_status)) %>% 
        dplyr::distinct(tag_unique, year, month, .keep_all=TRUE) %>% 
        dplyr::distinct()
      
      # create unique meta_instanceid code ----
      gmerc_pheno_master <- gmerc_pheno_master %>% 
        dplyr::mutate(meta_instanceid_unique=paste(tag_unique, meta_instanceid, item, sep="_"))
      
      df <- df %>% 
        dplyr::mutate(meta_instanceid_unique=paste(tag_unique, meta_instanceid, item, sep="_"))
      
      # merge new data with master df ----
      gmerc_pheno_master <- dplyr::bind_rows(gmerc_pheno_master, df)
      
      gmerc_pheno_master <- gmerc_pheno_master %>%
        dplyr::select(tag_unique,
                      scientific_name, genus, species, common_name, location, vegetation,
                      lat_y, lon_x, alt_z, geo_accuracy, observer,
                      date_time, date, time, year, month, month_num, day,
                      pheno_moto, moto_day, moto_plot, dbh,
                      meta_instanceid, meta_instanceid_unique, note_gmerc,
                      note_vital_status, note_duplicate_error, note_data_coll_error,
                      item, value) %>%
        dplyr::distinct()
      
      # drop rogue duplicates (created by rolling/overlapping additions to master df) ----
      gmerc_pheno_master <- gmerc_pheno_master %>% 
        dplyr::distinct(meta_instanceid_unique, .keep_all=TRUE)                                   # drop any meta_instanceid duplicates
      
      # create dynamic filenames ----
      df_filename <- paste("gmerc_pheno_data_",min(df$date),"_to_",max(df$date), "_clean",sep="")
      df_errors_filename <- paste("gmerc_pheno_data_",min(df$date),"_to_",max(df$date), "_errors",sep="")
      
      # save output files (long form) ----
      saveRDS(df, paste(df_filename,".rds", sep=""))
      saveRDS(df_errors, paste(df_errors_filename,".rds", sep=""))
      saveRDS(gmerc_pheno_master, "gmerc_pheno_master.rds")
      
      # data.table::fwrite(df, paste(df_filename,".csv", sep=""))                                   # save cleaned input data
      # data.table::fwrite(df_errors, paste(df_errors_filename,".csv", sep=""))                     # save errors df
      # data.table::fwrite(gmerc_pheno_master, file="gmerc_pheno_master.csv")                   # save updated master df
      
      # pivot all dfs to gmerc style ----
      
      # pivot new df
      df_wide <- df %>% 
        dplyr::select(tag_unique, year, month_num, item, value)
      
      df_info <- df %>% 
        dplyr::select(tag_unique, scientific_name, genus, species, common_name, lat_y, lon_x, alt_z, location) %>% 
        dplyr::distinct()
      
      df_wide <- df_wide %>% 
        tidyr::pivot_wider(.,
                           id_cols=tag_unique,
                           names_from=c(year, month_num, item),
                           values_from=value,
                           names_sep="_") 
      
      df_wide <- dplyr::left_join(df_wide, df_info, by="tag_unique") %>% 
        dplyr::select(tag_unique, scientific_name:location, everything())
      
      # pivot master df
      gmerc_pheno_master_wide <- gmerc_pheno_master %>% 
        tidyr::pivot_wider(.,
                           id_cols=tag_unique:location,
                           names_from=c(year, month_num, item),
                           values_from=value,
                           names_sep="_")
      
      # save updated gmerc_pheno_master & new data (wide form) ----
      df_wide_filename <- paste("gmerc_pheno_data_",min(df$date),"_to_",max(df$date), "_clean_wide",sep="")
      
      saveRDS(df_wide, paste(df_wide_filename,".rds", sep=""))
      saveRDS(gmerc_pheno_master_wide, "gmerc_pheno_master_wide.rds")
      
      # data.table::fwrite(df_wide, paste(df_wide_filename,".csv", sep=""))
      # data.table::fwrite(gmerc_pheno_master_wide, "gmerc_pheno_master_wide.csv")
      
      removeModal()
      
    })                                                                              # process input data - observeEvent end
    
    # filter and download queried dataset ----
    
    download_df <- reactive({
      if(input$download_password!="alex_piel")
        return(NULL)
      qdf <- gmerc_pheno_master %>%
        dplyr::mutate(date=lubridate::ymd(date)) %>%
        dplyr::filter(date >= input$query_date[1] & date <= input$query_date[2]) %>%
        dplyr::filter(tag_unique %in% input$tag_unique) %>%
        dplyr::filter(item %in% input$item) %>% 
        dplyr::filter(genus %in% input$genus) %>%
        dplyr::filter(species %in% input$species) %>%
        dplyr::filter(common_name %in% input$common_name) %>%
        dplyr::filter(location %in% input$location) %>%
        dplyr::filter(observer %in% input$observer) %>%
        dplyr::filter(pheno_moto %in% input$pheno_moto) %>%
        dplyr::filter(moto_day %in% input$moto_day) %>%
        dplyr::filter(moto_plot %in% input$moto_plot) %>%
        dplyr::filter(vegetation %in% input$vegetation)
      return(qdf)
    })
    
    # download queried dataset ----
    output$download_data <- downloadHandler(
      filename = function(){"gmerc_data_query.csv"},
      content = function(file){
        data.table::fwrite(download_df(), file, row.names = FALSE)
      }
    )
    
    # edit and overwrite data ----
    gmerc_pheno_master <- gmerc_pheno_master %>%
      dplyr::mutate_at(c("tag_unique", "scientific_name", "genus", "species", "common_name",
                         "lat_y", "lon_x", "alt_z", "location", "vegetation", "observer", "date",
                         "year", "month", "month_num", "pheno_moto",
                         "moto_day", "moto_plot", "meta_instanceid",
                         # "note_gmerc", "note_vital_status", "note_duplicate_error", "note_data_coll_error",
                         "item", "value", "date"), as.factor)
    
    output[["table"]] <- renderDT({
      datatable(gmerc_pheno_master,
                editable = "cell",
                options = list(
                  pageLength = 15,
                  lengthMenu = c(15, 50, 100, 200),
                  autoWidth = TRUE,
                  scrollX = TRUE,
                  fixedColumns = TRUE,
                  columnDefs = list(list(targets=c(0), visible=TRUE, width='25px'),
                                    list(targets=c(1), visible=TRUE, width='150px'),
                                    list(targets=c(2), visible=TRUE, width='225px'),
                                    list(targets=c(3), visible=TRUE, width='140px'),
                                    list(targets=c(4), visible=TRUE, width='140px'),
                                    list(targets=c(5), visible=TRUE, width='160px'),
                                    list(targets=c(6), visible=TRUE, width='100'),
                                    list(targets=c(7), visible=TRUE, width='90'),
                                    list(targets=c(8), visible=TRUE, width='145'),
                                    list(targets=c(9), visible=TRUE, width='105'),
                                    list(targets=c(10), visible=TRUE, width='120'),
                                    list(targets=c(11), visible=TRUE, width='100'),
                                    list(targets=c(12), visible=TRUE, width='100'),
                                    list(targets=c(13), visible=TRUE, width='100'),
                                    list(targets=c(14), visible=TRUE, width='150px'),
                                    list(targets=c(15), visible=TRUE, width='125px'),
                                    list(targets=c(16), visible=TRUE, width='100px'),
                                    list(targets=c(17), visible=TRUE, width='80px'),
                                    list(targets=c(18), visible=TRUE, width='100'),
                                    list(targets=c(19), visible=TRUE, width='80px'),
                                    list(targets=c(20), visible=TRUE, width='100'),
                                    list(targets=c(21), visible=TRUE, width='80px'),
                                    list(targets=c(22), visible=TRUE, width='120px'),
                                    list(targets=c(23), visible=TRUE, width='100px'),
                                    list(targets=c(24), visible=TRUE, width='400px'),
                                    list(targets=c(25), visible=TRUE, width='300px'),
                                    list(targets=c(26), visible=TRUE, width='200px'),
                                    list(targets=c(27), visible=TRUE, width='250px'),
                                    list(targets=c(28), visible=TRUE, width='140px'),
                                    list(targets=c(29), visible=TRUE, width='170px'),
                                    list(targets=c(30), visible=TRUE, width='100px'),
                                    list(targets='_all', visible=FALSE))),
                filter = list(position = "top", clear=FALSE, plain=TRUE)) %>% 
        DT::formatStyle(columns = names(gmerc_pheno_master),
                        target = "row",
                        lineHeight="35%",
                        `font-size`='12px')
    })
    
    df3 <- reactiveVal(gmerc_pheno_master)
    
    observeEvent(input[["table_cell_edit"]], {
      cell <- input[["table_cell_edit"]]
      newdf <- df3()
      newdf[cell$row, cell$col] <- cell$value
      df3(newdf)
    })
    
    observeEvent(input$save, {
      
      showModal(modalDialog("Overwriting data...", footer=NULL))
      
      req(input$edit_password=="alex_piel")
      # data.table::fwrite(df3(), "gmerc_pheno_master.csv")
      saveRDS(df3(), "gmerc_pheno_master.rds")
      
      removeModal()
    })
    
  }                                                       # server end (SINGLE)
)                                                           # shinyApp end (SINGLE)










