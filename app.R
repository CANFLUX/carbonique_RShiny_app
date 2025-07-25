# ------------------------------------------------------------------
# June, 2024
# Sara Knox
# sara.knox@mcgill.ca

# Adapted from the Ameriflux data visualization app from
# Sophie Ruehr
# sophie_ruehr@berkeley.edu
# ------------------------------------------------------------------

# 1. SET UP  -------------------------------------------------------

# Tim Elrick, 2024-12-19: speed up load time
#source("scripts/load_save_data.R")
library(tidyverse)
if (today() != read_lines("data/updated.txt")) {
  source("scripts/load_save_data.R")
} else {
  load("data/all_data.RData")
}


# 2. ATTACH REQUIRED PACKAGES -------------------------------------

# Tim Elrick, 2024-12-19: added necessary packages
library(hms)
library(shiny)
library(shinydashboard) 
library(shinycssloaders)
library(plotly)
library(gridExtra)
library(grid)

# 3. USER INTERFACE ----
ui <- dashboardPage(skin = 'black', # Begin UI 
                    
                    dashboardHeader(title = "Data Visualization Tool"),
                    
                    dashboardSidebar(sidebarMenu(
                      menuItem("Individual sites", tabName = "indiv"),
                      menuItem("All sites", tabName = "all"),
                      menuItem("About", tabName = "about")
                    )), # End dashboard sidebar
                    
                    
                    dashboardBody( # Begin dashboard body
                      
                      # Suppress warnings
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"),
                      
                      tabItems(
                        tabItem( # Begin 'Individual Sites' page
                          h1('Individual sites'),
                          
                          tabName = "indiv",
                          # Select site from dropdown list
                          selectInput('sites', 'Select site', sites,
                                      multiple = F),
                          
                          # 1. Site information (main panel)
                          
                          br(), br(),
                          
                          tabsetPanel( # Begin tab panels section
                            
                            tabPanel( # Begin time series tab
                              "Time series",
                              
                              br(),
                              
                              # Select main variable
                              fluidRow( column(6, selectInput('tscol', 'Variable', names(data[-c(1)]))),
                                        
                                        # Select date range for time series
                                        column(12,  sliderInput(inputId = "range", width = '80%',
                                                                label = "Date range",
                                                                min = min(data$datetime, na.rm = T),
                                                                max = max(data$datetime, na.rm = T),
                                                                value = c(min(data$datetime, na.rm = T),
                                                                          max(data$datetime, na.rm = T))))),
                              br(),
                              
                              # Ouput time series plot with a spinner
                              shinycssloaders::withSpinner(plotlyOutput('timeseries_plots'),
                                                           type = getOption("spinner.type", default = 5),
                                                           color = getOption("spinner.color", default = "#4D90FE")),
                              h5(em('Plot shows 30-min data'), align = 'center')
                              
                            ), # End time series tab 
                            
                            tabPanel( # Begin scatter plots and diagnostics
                              "Scatter plots",
                              
                              br(),
                              
                              fluidRow(column(6, # Make both inputs on same row
                                              # Input first (one) variable for scatter plot                               
                                              selectInput('onecol', 'First variable', names(data[-c(1)]))),
                                       column(6, 
                                              # Input second (two) variable for scatter plot 
                                              selectInput('twocol', 'Second variable', names(data[-c(1:2)])))),
                              br(),
                              
                              # Output scatter and density plots
                              shinycssloaders::withSpinner(plotlyOutput('scatter_plots', # For diurnal plot
                                                                        width = '100%', height = '60%'),
                                                           type = getOption("spinner.type", default = 5),
                                                           color = getOption("spinner.color", default = "dodgerblue")),
                              
                              shinycssloaders::withSpinner(plotOutput('scatter_diagnostics', # For cross correlation plot
                                                                      width = '100%', height = "400px")),
                              # type = getOption("spinner.type", default = 5),
                              # color = getOption("spinner.color", default = "dodgerblue")),
                              
                              br(),
                              
                              h5(em('Scatter plots and diagnostics of half-hourly data.'),
                                 align = 'center')
                              
                            ), # End scatter plots tab
                            
                            tabPanel('Diurnal Cycle',
                                     
                                     # CREATE VARIABLE INPUTS 
                                     fluidRow(column(6, selectInput('dicol', 'Variable', names(data[-c(1)]))),
                                              # Select date range for time series
                                              column(12,  sliderInput(inputId = "range", width = '80%',
                                                                      label = "Date range",
                                                                      min = min(data$datetime, na.rm = T),
                                                                      max = max(data$datetime, na.rm = T),
                                                                      value = c(min(data$datetime, na.rm = T),
                                                                                max(data$datetime, na.rm = T))))        
                                     ), # END FLUID ROW 
                                     
                                     # OUTPUT DIURNAL PLOTS (W/ SPINNER)
                                     shinycssloaders::withSpinner(
                                       plotlyOutput('diurnal', width = '100%', height = '100%'),  # Increased height to ensure equal panel sizes
                                       type = getOption("spinner.type", default = 5),
                                       color = getOption("spinner.color", default = "dodgerblue")
                                     ),
                                     
                                     br(),
                                     
                                     h5(em('Plot shows mean monthly diurnal variation over the specified period.'), 
                                        align = 'center')
                                     
                            ), # End diurnal cycle tab
                            
                            tabPanel('Radiation Diagnostics',
                                     
                                     # CREATE VARIABLE INPUTS 
                                     fluidRow(column(3, selectInput('radcol', 'Variable', var_rad)),
                                              column(3, 
                                                     # Input second (two) variable for scatter plot 
                                                     selectInput('site_yr', 'Year', yrs))
                                     ), # END FLUID ROW 
                                     
                                     # OUTPUT RADIATION PLOTS
                                     shinycssloaders::withSpinner(plotlyOutput('radiation1', # For diurnal plot
                                                                               width = '100%', height = '100%'),
                                                                  type = getOption("spinner.type", default = 5),
                                                                  color = getOption("spinner.color", default = "dodgerblue")),
                                     
                                     shinycssloaders::withSpinner(plotlyOutput('radiation2', # For cross correlation plot
                                                                               width = '100%', height = '60%'),
                                                                  type = getOption("spinner.type", default = 5),
                                                                  color = getOption("spinner.color", default = "dodgerblue")),
                                     
                                     br(),
                                     
                                     h5(em('Plot shows mean diurnal radiation pattern and cross correlation between measured radiation and potential radiation'), 
                                        align = 'center')
                                     
                            ), # End radiation diagnostics tab
                            
                            tabPanel( # Begin EBC plots and diagnostics
                              "Energy balance closure",
                              
                              br(),
                              
                              fluidRow(column(6, # Make both inputs on same row
                                              # Input first (one) variable for scatter plot                               
                                              selectInput('onecol_EBC', 'First variable', ae_columns <- grep("^AE_", colnames(data), value = TRUE))),
                                       column(6, 
                                              # Input second (two) variable for scatter plot 
                                              selectInput('twocol_EBC', 'Second variable', ae_columns <- grep("H_LE", colnames(data), value = TRUE)))),
                              br(),
                              
                              # Output scatter and density plots
                              shinycssloaders::withSpinner(plotlyOutput('EBC_plots', # For diurnal plot
                                                                        width = '100%', height = '60%'),
                                                           type = getOption("spinner.type", default = 5),
                                                           color = getOption("spinner.color", default = "dodgerblue")),
                              
                              shinycssloaders::withSpinner(plotOutput('EBC_diagnostics', # For cross correlation plot
                                                                      width = '100%', height = "400px")),
                              # type = getOption("spinner.type", default = 5),
                              # color = getOption("spinner.color", default = "dodgerblue")),
                              
                              br(),
                              
                              h5(em('Energy balance closure of half-hourly data.'),
                                 align = 'center')
                              
                            ), # End EBC tab
                            
                            tabPanel('Cumulative plots', # Begin cumulative plots
                                     
                                     # CREATE VARIABLE INPUTS 
                                     fluidRow(column(6, selectInput('cumcol', 'Variable', grep("^(NEE|FCH4).*_uStar_f$", colnames(data), value = TRUE)))
                                     ), # END FLUID ROW 
                                     
                                     # OUTPUT DIURNAL PLOTS (W/ SPINNER)
                                     shinycssloaders::withSpinner(
                                       plotlyOutput('cumulative', width = '100%', height = '100%'),  # Increased height to ensure equal panel sizes
                                       type = getOption("spinner.type", default = 5),
                                       color = getOption("spinner.color", default = "dodgerblue")
                                     ),
                                     
                                     br(),
                                     
                                     h5(em('Plot shows cumulative fluxes.'), 
                                        align = 'center')
                                     
                            ) # End cumulative plots tab
                            
                          ) # End tab panels section
                        ), # End 'Individual Sites' page
                        
                        tabItem( # Begin 'All Sites' page
                          tabName = "all",
                          h1('All sites'),
                          
                          # Select inputs for plot
                          fluidRow( column(4, selectInput('xcol_all', 'X-axis variable', xvars)), 
                                    column(4, selectInput('ycol_all', 'Y-axis variable', yvars)),
                                    
                                    br(),  br(),  br(), br(),  br(),
                                    
                                    # Output plot
                                    shinycssloaders::withSpinner(plotlyOutput('all_plots', 
                                                                              width = '100%',  height = '100%'),
                                                                 type = getOption("spinner.type", default = 5)),
                                    inline = TRUE,
                                    
                                    br()
                          ) # End 'All Sites' page
                        ), # End tab panels section
                        
                        tabItem( # Begin 'About' page
                          tabName = 'about',
                          
                          # Informational text 
                          #h4('About the data'),
                          #h5(style="text-align: justify;",
                          #   'Data displayed with this tool are from the ', tags$a(href="https://carboni-que.github.io/", "CARBONIQUE project.", target="_blank")),
                          
                          br(),
                          h4('About this visualization tool'),
                          h5(style="text-align: justify;", 
                             'This app was modified from the ', tags$a(href="https://ameriflux.shinyapps.io/version1/", "Ameriflux data visualization tool", target="_blank")),
                          h5(style="text-align: justify;",
                             'EDIT: If you are interested in learning more, improving this app, or building one yourself, check out the ', tags$a(href = 'https://shiny.rstudio.com/', 'RShiny package in R', target = 'blank'), ' or contact ', tags$a(href="https://sruehr.github.io/", "Sophie Ruehr.", target = 'blank'), 
                             'Code for this app is available on ', tags$a(href = 'https://github.com/sruehr?tab=repositories', 'GitHub.', target = 'blank')), 
                          #br(),
                          #h4('Citation'),
                          #h5('TBD'),
                          
                          
                          br(),
                          h4('Acknowledgements'),
                          h5(style="text-align: justify;",
                             'This application was developed by Sara Knox based on the original code by Sophie Ruehr, which is available is available on ', tags$a(href = 'https://github.com/sruehr?tab=repositories', 'GitHub.', target = 'blank')), 
                          
                        ) # End 'About' page
                      ), # End all pages         
                      hr(),
                      h5('App designed and developed by Sara Knox, 2024.')
                    ) # End dashboard body
) # End UI

# 4. Server
server <- function(input, output, session) { # Begin server
  
  observeEvent(input$sites, { # Change output based on site selection
    
    # Update data 
    yrs <- yrs_included(basepath,input$sites,level)
    data <- read_data_all_years(basepath,yrs,input$sites,level,tv_input)
    # add EBC columns
    data <- create_EBC_columns(data)
    data_units <- var_units(colnames(data),UnitCSVFilePath)
    
    # Update Y variable (time series) - find unique variable names
    names_data <- names(data)
    names_data_unique <- unique(gsub("_[[:digit:]]", "",names_data))  
    
    updateSelectInput(session, 'tscol', choices = names_data_unique[-1])
    
    # Update X variable (scatter plot)
    updateSelectInput(session, 'onecol', choices = names(data[-c(1)]))
    
    # Update Y variable (scatter plot)
    updateSelectInput(session, 'twocol',choices = names(data[,-c(1,2)]))
    
    # Update Y variable (diurnal)
    updateSelectInput(session, 'dicol', choices = names(data[-c(1)]))
    
    # Update Y variable (radiation diagnostics)
    updateSelectInput(session, 'radcol', choices = var_rad)
    
    # Update year (radiation diagnostics)
    updateSelectInput(session, 'site_yr',choices = yrs)
    
    # Update X variable (EBC plot)
    updateSelectInput(session, 'onecol_EBC', choices = grep("^AE_", colnames(data), value = TRUE))
    
    # Update Y variable (EBC plot)
    updateSelectInput(session, 'twocol_EBC',choices = grep("H_LE", colnames(data), value = TRUE))
    
    # Update Y variable (cumulative plot)
    updateSelectInput(session, 'cumcol', choices = grep("^(NEE|FCH4).*_uStar_f$", colnames(data), value = TRUE))
    
    # Update time series limits
    updateSliderInput(inputId = "range",
                      min = min(data$datetime, na.rm = T),
                      max = max(data$datetime, na.rm = T),
                      value = c(min(data$datetime, na.rm = T),
                                max(data$datetime, na.rm = T)))
    
    # Select data for plots based on user inputs
    selectedData <- reactive({ # Time series plots
      if (length(grep(paste0("\\b",input$tscol,"\\b","_*"), names(data), value=TRUE)) == 0) {
        data[, c("datetime",grep(paste0("^",input$tscol,"_"), names(data), value=TRUE))] # For cases with qualifiers
      } else {
        data[, c("datetime",grep(paste0("\\b",input$tscol,"\\b","_*"), names(data), value=TRUE))] # for cases without qualifiers
      }
    })
    
    selectedDatascatter<- reactive({ # Scatter plots
      data[, c(input$onecol, input$twocol)] 
    })
    
    selectedDataDiurnal <- reactive({ # Diurnal plots
      data[(data$datetime >= input$range[1]) & (data$datetime <= input$range[2]), c(input$dicol)] 
    })
    
    selectedDataRadiation <- reactive({ # Radiation diagnostic plots
      data[, c("datetime",input$radcol)] 
    })
    
    selectedDataEBC<- reactive({ # EBC plots
      data[, c(input$onecol_EBC, input$twocol_EBC)] 
    })
    
    selectedDataCumulative <- reactive({ # Radiation diagnostic plots
      data[, c("datetime",input$cumcol)] 
    })
    
    # OUTPUTS:  ----- 
    
    # a) Time series plots
    output$timeseries_plots <- renderPlotly({ 
      
      if (length(grep(paste0("\\b", input$tscol, "\\b", "_*"), names(data), value = TRUE)) == 0) {
        dat_names <- grep(paste0("^", input$tscol, "_"), names(data), value = TRUE)[1] # Get name of variable selected
        ylabel <- paste0(dat_names, ' (', data_units$units[which(data_units$name == dat_names)], ')')
      } else {
        dat_names <- grep(paste0("\\b", input$tscol, "\\b", "_*"), names(data), value = TRUE)[1] # Get name of variable selected
        ylabel <- paste0(dat_names, ' (', data_units$units[which(data_units$name == dat_names)], ')')
      }
      
      df.long<-gather(selectedData(),variable,value,-datetime)
      
      p1 <- ggplot(df.long,aes(datetime,value,color=variable))+geom_point(alpha = 0.3, size = 0.5)+
        theme_bw() + # plot theme
        theme(text=element_text(size=20), #change font size of all text
              axis.text=element_text(size=15), #change font size of axis text
              axis.title=element_text(size=12), #change font size of axis titles
              plot.title=element_text(size=20), #change font size of plot title
              legend.text=element_text(size=8), #change font size of legend text
              legend.title=element_text(size=8),
              plot.margin = margin(t = 20,  # Top margin
                                   r = 30,  # Right margin
                                   b = 30,  # Bottom margin
                                   l = 30)) + # Left margin +
        ylab(ylabel) +
        xlab('Date')  + # relabl X axis
        xlim(input$range[1], input$range[2]) # change date limits to user input 
      
      # Remove legend if there aren't multiple variables
      if (length(selectedData())<=2){
        p1 <- p1 + theme(legend.position="none")
      }
      p1 <- ggplotly(p1) %>% toWebGL()  # create plotly 
    }) # End plot render
    
    # b) diagnostics plots - Could add later
    
    # c) Scatter plots
    output$scatter_plots <- renderPlotly({ 
      y_names <- input$twocol # Get name of variable selected
      ylabel <- y_names # Could update this
      
      x_names <- input$onecol # Get name of variable selected
      xlabel <- x_names # Could update this
      
      df <- selectedDatascatter()
      df$year <- year(data$datetime) 
      
      scatter_plot_QCQA(df,xlabel,ylabel, xlabel,ylabel,1)
      
    }) # End plot render
    
    output$scatter_diagnostics <- renderPlot({ 
      y_names <- input$twocol # Get name of variable selected
      ylabel <- y_names # Could update this
      
      x_names <- input$onecol # Get name of variable selected
      xlabel <- x_names # Could update this
      
      df <- selectedDatascatter()
      df$year <- year(data$datetime) 
      
      R2_slope_QCQA(df,xlabel,ylabel)
      
    }) # End plot render
    
    # d) Diurnal plot
    output$diurnal <- renderPlotly({ 
      
      dat_names <- input$dicol # Get name of selected variable
      ylabel <- paste0(dat_names, ' (', data_units$units[which(data_units$name == dat_names)], ')')
      
      dt <- data$datetime[(data$datetime >= input$range[1]) & (data$datetime <= input$range[2])]
      DataDiurnal <- data.frame(datetime = dt)
      DataDiurnal$Month <- as.numeric(format(dt, "%m"))  
      DataDiurnal$Hour <- as.numeric(format(dt, "%H%M"))  
      
      DataDiurnal$Var <- selectedDataDiurnal()
      
      data_diurnal <- DataDiurnal %>% 
        group_by(Month, Hour) %>% 
        summarise(Average = mean(Var, na.rm = TRUE), .groups = "drop")
      
      # Compute global min/max with a buffer to avoid extreme compression
      global_min <- min(data_diurnal$Average, na.rm = TRUE)
      global_max <- max(data_diurnal$Average, na.rm = TRUE)
      range_buffer <- (global_max - global_min) * 0.1  # Add 10% buffer to limits
      
      # Ensure reasonable limits that work across different variable ranges
      y_min <- global_min - range_buffer
      y_max <- global_max + range_buffer
      
      month_labs <- c('1' = 'January', '2' = 'February', '3' = 'March',
                      '4' = 'April', '5' = 'May', '6' = 'June',
                      '7' = 'July', '8' = 'August', '9' = 'September',
                      '10' = 'October', '11' = 'November', '12' = 'December')
      
      month_labeller <- labeller(Month = function(levels) month_labs[as.character(levels)])
      
      diurnal_plot <- ggplot(data = data_diurnal, aes(x = Hour, y = Average)) +
        geom_line(color = 'black') +
        facet_wrap(~ Month, nrow = 3, ncol = 4, scales = "fixed", labeller = month_labeller) +  
        scale_x_continuous(breaks = c(0, 600, 1200, 1800, 2330),
                           labels = c("00:00", "06:00", "12:00", "18:00", "24:00")) +
        scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +  
        coord_cartesian(ylim = c(y_min, y_max)) +  # Ensures consistent y-axis scaling
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.spacing.x = unit(3, "mm"),  # Reduce horizontal spacing
          panel.spacing.y = unit(3, "mm"),  # Reduce vertical spacing
          strip.text = element_text(size = 12, margin = margin(t = 1, b = 1)),  # Reduce label padding
          strip.placement = "outside",
          strip.background = element_blank()
        ) +  
        xlab("Hour:Minute") +
        ylab(ylabel)
      
      ggplotly(diurnal_plot)  
    }) # End plot render
    
    # e) radiation plots
    output$radiation1 <- renderPlotly({ 
      rad_name <- input$radcol # Get name of variable selected
      #ylabel <- rad_names # Could update this
      
      # Rename variables to remove variable prefix
      df <- selectedDataRadiation()
      
      df <- df[year(df$datetime) == as.numeric(input$site_yr), ]
      
      names(df) <- sub(paste0('^',rad_name,'.'), '', names(df))
      
      # Calculate potential radiation
      sites_coordinates_filtered <-  sites_coordinates[sites_coordinates['Site'] == input$sites, ] # extra standard meridian/lat/long from excel file
      df$pot_rad <- potential_rad_generalized(as.numeric(sites_coordinates_filtered$Standard_Meridian), as.numeric(sites_coordinates_filtered$Longitude), as.numeric(sites_coordinates_filtered$Latitude), df$datetime,yday(df$datetime))
      
      # Calculate diurnal composite
      diurnal.composite <- diurnal_composite_rad_single_var(df,'pot_rad',rad_name,15,48)
      
      # Summarize data to filter out dates where diurnal composite has all -Inf values
      df_summary <- diurnal.composite %>%
        group_by(firstdate) %>%
        summarize(rad_na_count = sum(is.na(!!sym(rad_name))), # find number of -Inf values for rad var
                  count = n())                                 # Count number of observations per date
      
      # Remove dates with only -Inf points
      keep <- df_summary %>%
        filter(rad_na_count != count) 
      
      diurnal.composite <- diurnal.composite[diurnal.composite$firstdate %in% keep$firstdate, ]
      
      if (grepl("SW_IN", rad_name, fixed = TRUE) == TRUE) {
        p_rad_dirnal <- SWIN_vs_potential_rad(diurnal.composite,rad_name)} else {
          p_rad_dirnal <- PPFDIN_vs_potential_rad(diurnal.composite,rad_name)
        }
    }) # End plot render
    
    output$radiation2 <- renderPlotly({ 
      rad_name <- input$radcol # Get name of variable selected
      #ylabel <- rad_names # Could update this
      
      # Rename variables to remove variable prefix
      df <- selectedDataRadiation()
      
      df <- df[year(df$datetime) == as.numeric(input$site_yr), ]
      
      names(df) <- sub(paste0('^',rad_name,'.'), '', names(df))
      
      # Calculate potential radiation
      sites_coordinates_filtered <-  sites_coordinates[sites_coordinates['Site'] == input$sites, ] # extra standard meridian/lat/long from excel file
      df$pot_rad <- potential_rad_generalized(as.numeric(sites_coordinates_filtered$Standard_Meridian), as.numeric(sites_coordinates_filtered$Longitude), as.numeric(sites_coordinates_filtered$Latitude), df$datetime,yday(df$datetime))
      
      # Calculate diurnal composite
      diurnal.composite <- diurnal_composite_rad_single_var(df,'pot_rad',rad_name,15,48)
      #diurnal.composite <- diurnal_composite_rad_single_var[is.finite(diurnal.composite$potential_radiation), ]
      
      # Summarize data to filter out dates where diurnal composite has all -Inf values
      df_summary <- diurnal.composite %>%
        group_by(firstdate) %>%
        summarize(rad_na_count = sum(is.na(!!sym(rad_name))), # find number of -Inf values for rad var
                  count = n())                                 # Count number of observations per date
      
      # Remove dates with only -Inf points
      keep <- df_summary %>%
        filter(rad_na_count != count) 
      
      diurnal.composite <- diurnal.composite[diurnal.composite$firstdate %in% keep$firstdate, ]
      
      xcorr_rad(diurnal.composite,rad_name)
    }) # End plot render
    
    # f) Energy balance closure plots
    output$EBC_plots <- renderPlotly({ 
      y_names <- input$twocol_EBC # Get name of variable selected
      ylabel <- y_names # Could update this
      
      x_names <- input$onecol_EBC # Get name of variable selected
      xlabel <- x_names # Could update this
      
      df <- selectedDataEBC()
      df$year <- year(data$datetime) 
      
      scatter_plot_QCQA(df,xlabel,ylabel, xlabel,ylabel,1)
      
    }) # End plot render
    
    output$EBC_diagnostics <- renderPlot({ 
      y_names <- input$twocol_EBC # Get name of variable selected
      ylabel <- y_names # Could update this
      
      x_names <- input$onecol_EBC # Get name of variable selected
      xlabel <- x_names # Could update this
      
      df <- selectedDataEBC()
      df$year <- year(data$datetime) 
      
      R2_slope_QCQA(df,xlabel,ylabel)
      
    }) # End plot render
    
    # g) Cumulative plot
    output$cumulative <- renderPlotly({
      
      if (any(grepl("ThirdStage", level))) {
        
        # Create conversion factors
        conv_factor <- data.frame(12.01/(10^6)*(60*60*24),12.01/(10^9)*(60*60*24),(60*60*24)/(10^6))
        colnames(conv_factor) <- c("CO2","CH4","Energy")
        
        # Create string for conversion factor variable names
        conv_factor_vars <- c("CO2","CH4","Energy")
        
        # Create labels for units
        conv_factor_units_cum <- data.frame("gC m-2","gC m-2","MJ m-2")
        colnames(conv_factor_units_cum) <- c("CO2","CH4","Energy")
        
        cumulative_names <- input$cumcol # Get name of selected variable
        
        # Initialize index variable
        index <- NULL
        
        # Check if NEE or FCH4 is in the matching columns and assign index
        if (grepl("^NEE", cumulative_names) == TRUE) {
          index <- 1
        } else if (grepl("^FCH4", cumulative_names) == TRUE) {
          index <- 2
        }
        
        df <- selectedDataCumulative()
        colnames(df)[2] <- "var"
        
        df$year <- year(data$datetime) 
        df$DOY <- yday(data$datetime) 
        
        cf <- as.numeric(conv_factor[index])
        units_cf <- conv_factor_units_cum[index]
        
        # Determine which years are a full year of data
        nyrs <- unique(df$year)
        
        count_yr <- df %>% 
          group_by(year) %>%
          dplyr::summarize(count = sum(!is.na(var)))
        
        yrs <- nyrs[count_yr$count >=17520] # Greater or equal to 365 days (i.e. 17520 observations)
        
        df2 <- with(df,df[(year >= yrs[1] & year <= yrs[length(yrs)]),])
        
        daily.cum <- df2 %>%
          group_by(year,DOY) %>%
          dplyr::summarize(var = mean(var)*cf, 
                           DOY = first(DOY),
                           year = as.factor(first(year)))
        
        flux.cum <- daily.cum %>%
          group_by(year) %>%
          dplyr::summarize(var_cum = cumsum(var),
                           DOY = DOY)%>%
          ungroup()
        
        p <- ggplot() +
          geom_hline(yintercept = 0, linetype = "dotted", color = "grey10", size = 0.1)+
          geom_line(data = flux.cum, aes(x = DOY, y = var_cum, color = year))+ 
          ylab(paste0(cumulative_names," (",units_cf,")",sep = ""))
        ggplotly(p) 
        
      }else{
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No Third Stage Data Available", size = 6, hjust = 0.5, vjust = 0.5) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      }
    }) # End plot render
    
    # e) All sites plot
    output$all_plots <- renderPlotly({ 
      xlab <- paste(input$xcol_all)
      ylab <- paste(input$ycol_all)
      
      if(xlab != 'datetime') {
        dat_names <- xlab # Get name of variable selected
        xlab <- paste0(dat_names, ' (', data_units_all$units[which(data_units_all$name == dat_names)], ')')
      }
      
      dat_names2 <- ylab
      ylab <- paste0(dat_names2, ' (', data_units_all$units[which(data_units_all$name == dat_names2)], ')')
      
      p3 <- ggplot(data_all) +
        geom_point(aes(x = .data[[input$xcol_all]],
                       y = .data[[input$ycol_all]],
                       color = .data[['site']],
                       group = .data[['site']]),
                   na.rm = T, alpha = 0.3, size = 0.5) + # color of line
        theme_bw() + # plot theme
        theme(text=element_text(size=20), #change font size of all text
              axis.text=element_text(size=15), #change font size of axis text
              axis.title=element_text(size=15), #change font size of axis titles
              plot.title=element_text(size=20), #change font size of plot title
              legend.text=element_text(size=8), #change font size of legend text
              legend.title=element_text(size=10)) +
        xlab(xlab) +
        ylab(ylab)
      
      p3 <- ggplotly(p3) %>% toWebGL() 
    }) # End all sites plot
  })# End server
}
# 5. RUN APP -----
shinyApp(ui = ui, server = server)
