library(shiny)
library(magrittr)
library(sf)

# setwd("C:/Users/gabri/Documents/rivage_app")

library(ggplot2)

data_all <- readRDS("30_all_VU_components_45_isl_266_BM_sp.rds")
exposure_data <-
  read.csv("threats.csv", 
           stringsAsFactors = F, 
           fileEncoding = "windows-1252")
source("Calculate_components_VU_FUN.R")

island_shp <-
readRDS("01_shp_45_major_isl_clean.rds")

CC_cols <- 
  c("CC")

LU_cols <- 
  c("LU")

BI_cols <- 
  c('BI')

E_cols <- 
  c("sed_tot_med", 
    "mean_HM_change", 
    "rdens_osm",
    "nb_alien", 
    "alien_vert_cover")

S_cols <- 
  c("aoh_km2", 
    "nb_diet", 
    "nb_hab", 
    "gen_length_y")

AC_cols <- 
  c('Area', 
    'max_elev', 
    'mean_tri', 
    'PA_prop', 
    'fred',
    'dispersal')

filter_var <-
  function(x, val) {
    if (is.numeric(x)) {
      !is.na(x) & x >= val[1] & x <= val[2]
    } else if (is.factor(x)|is.character(x)) {
      x %in% val
    } else {
      # No control, so don't filter
      TRUE
    }
  }

create_beautiful_radarchart <- function(data, color = "#00AFBB",
                                        vlabels = colnames(data), vlcex = 1.3,
                                        caxislabels = NULL, title = NULL){
  
  par(mar=c(0,0,0,0))
  
  fmsb::radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey",
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title,
    centerzero = TRUE
  )
}

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Proof of concept
  
  weight_out <-
    reactiveValues(data = {
      data.frame(`Exposure` = 1,
                 `Sensitivity` = 1,
                 `Adaptive Capacity` = 1)
    })
  
  weight_e <-
    reactiveValues(data = {
      data.frame(`Climate Change` = 1,
                 `Land Use Change` = 1,
                 `Biological Invasions` = 1)
    })
  
  weight_ac <-
    reactiveValues(data = {
      data.frame(`Abiotic Components` = 1,
                 `Biotic Components` = 1)
    })
  
  #output the datatable based on the dataframe (and make it editable)
  
  output$weight_out <- DT::renderDT({
    
    DT::datatable(weight_out$data,
                  editable = TRUE,
                  options = list(sDom  = 't',
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                                 scrollX = TRUE,
                                 autoWidth = TRUE
                  ),
                  rownames = FALSE) %>%
      DT::formatStyle(
        columns = names(weight_out$data),  # or a specific column
        color = "black",           # text color
        backgroundColor = "white"  # background color (optional)
      )
    
  })
  
  output$weight_e <- DT::renderDT({
    
    DT::datatable(weight_e$data,
                  editable = TRUE,
                  options = list(sDom  = 't',
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                                 scrollX = TRUE,
                                 autoWidth = TRUE
                  ),
                  rownames = FALSE) %>%
      DT::formatStyle(
        columns = names(weight_e$data),  # or a specific column
        color = "black",           # text color
        backgroundColor = "white"  # background color (optional)
      )
    
  })
  
  output$weight_ac <- DT::renderDT({
    
    DT::datatable(weight_ac$data,
                  editable = TRUE,
                  options = list(sDom  = 't',
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:1)),
                                 scrollX = TRUE,
                                 autoWidth = TRUE
                  ),
                  rownames = FALSE) %>%
      DT::formatStyle(
        columns = names(weight_ac$data),  # or a specific column
        color = "black",           # text color
        backgroundColor = "white"  # background color (optional)
      )
    
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  
  observeEvent(input$weight_out_cell_edit, {
    #get values
    info = input$weight_out_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    weight_out$data[i,j+1] <- k
  })
  
  observeEvent(input$weight_e_cell_edit, {
    #get values
    info = input$weight_e_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    weight_e$data[i,j+1] <- k
  })
  
  observeEvent(input$weight_ac_cell_edit, {
    #get values
    info = input$weight_ac_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    weight_ac$data[i,j+1] <- k
  })
  
  # Selection input
  
  class_levs <-
    sort(unique(as.character(data_all$Class)))
  
  archip_levs <-
    sort(unique(as.character(data_all$Archip)))
  
  island_levs <-
    sort(unique(as.character(data_all$Island)))
  
  var_levs <- c("Vulnerability", "Exposure", "Sensitivity", "Adaptive Capacity")
  
  output$classSelection <- renderUI({
    selectizeInput("Class",
                   "Select class:",
                   choices = class_levs,
                   selected = class_levs,
                   multiple = TRUE,
                   width = "100%")
  })
  
  output$archipSelection <- renderUI({
    selectizeInput("Archip",
                   "Select archipelago:",
                   choices = archip_levs,
                   selected = archip_levs,
                   multiple = TRUE,
                   width = "100%")
  })
  
  # Create table
  
  data_sel <- reactive({
    
    selected_class <-
      filter_var(data_all$Class, input$Class)
    
    selected_archip <-
      filter_var(data_all$Archip, input$Archip)
    
    data_all_sel <-
      data_all[
        selected_class & selected_archip,]
    
    out <-
      vulnerabilityFUN(data_all_sel,
                       ac_abiotic_cols = c('Area', 'max_elev', 'mean_tri', 'PA_prop'),
                       ac_sp_cols = c('dispersal'),
                       ac_isl_cols = c('Area', 'max_elev', 'mean_tri', 'PA_prop', 'fred'),
                       lu_cols = c("mean_HM_change", "rdens_osm"),
                       cc_cols = "sed_tot_med",
                       weight_out = unlist(weight_out$data),
                       weight_e = unlist(weight_e$data),
                       weight_ac = unlist(weight_ac$data))
    
    isl_data <-
      data_all_sel[names(data_all_sel) %in% c("Archip",
                                              "Island",
                                              E_cols,
                                              S_cols,
                                              AC_cols,
                                              "ID")]

    out <- data.frame(out,
                      isl_data[match(out$ID, isl_data$ID),])
    
    out <- out[order(out$VU, decreasing = TRUE),]
    
    out
    
  })
  
  sel_df <- eventReactive(input$run, {
    data_sel()
  },
  ignoreNULL = FALSE)
  
  output$sel_df <-
    DT::renderDataTable({
      
      out_table <- sel_df()
      
      names(out_table)[names(out_table) == "Archip"] <- "Archipelago"
      names(out_table)[names(out_table) == "sci_name"] <- "Species"
      names(out_table)[names(out_table) == "VU"] <- "Vulnerability"
      names(out_table)[names(out_table) == "VU_rank"] <- "Vulnerability Rank"
      names(out_table)[names(out_table) == "E"] <- "Exposure"
      names(out_table)[names(out_table) == "S"] <- "Sensitivity"
      names(out_table)[names(out_table) == "AC"] <- "Adaptive Capacity"
      
      out_table$Species <- paste0("<i>", out_table$Species, "</i>")
      
      DT::datatable(out_table[c(9,10,2:7)], 
                    rownames = FALSE, 
                    selection = "single", 
                    escape = FALSE) %>%
        DT::formatRound(columns = c(4,6,7,8),
                        digits = 3)
 
    })
  
  shinyjs::onclick("sel_df",
                   DT::selectRows(DT::dataTableProxy("sel_df"), 
                                  selected = input$sel_df_rows_selected))
  
  # Download data
  
  filename <-
    eventReactive(input$run, {
      function() {
        
        paste("vu3_", paste(input$Class, collapse = "-"), "_", paste(input$Archip, collapse = "-"),".csv", sep = "")
      }
    },
    ignoreNULL = FALSE)
  
  content <-
    eventReactive(input$run, {
      function(file) {
        write.csv(data_sel(), file, row.names = FALSE)
      }
    },
    ignoreNULL = FALSE)
  
  output$downloadData <-
    downloadHandler(filename = filename(),
                    content = content())
  
  download_button <-
    eventReactive(input$run, {
      downloadButton("downloadData", "Download data table")
    },
    ignoreNULL = FALSE)
  
  output$download_button <- renderUI(download_button())
  
  # Radar plot
  
  radar_data_E <-  eventReactive(input$sel_df_rows_selected, {
    
    if(isTruthy(input$sel_df_rows_selected)){
      
      data_all_sel <- sel_df()
      
      var_E_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% E_cols],
                   Value = unlist(data_all_sel[input$sel_df_rows_selected[1],
                                               names(data_all_sel) %in% E_cols]))
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_E_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% E_cols],
                   Value = NA)
      
    }
    
  })
  
  radar_data_E_ecdf <-  eventReactive(input$sel_df_rows_selected, {
    
    if(isTruthy(input$sel_df_rows_selected)){
      
      data_all_sel <- sel_df()
      
      data_all_sel <- data_all_sel[names(data_all_sel) %in% E_cols]
      
      data_all_sel <- apply(data_all_sel, 2, function(x) ecdf(x)(x))
      
      out <- data_all_sel[input$sel_df_rows_selected[1],]
      
      out
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_E_df <-
        data_all_sel[1,names(data_all_sel) %in% E_cols]
      
      var_E_df[1,] <- 0
      
      var_E_df
      
    }
    
  })
  
  radar_data_S <-  eventReactive(input$sel_df_rows_selected, {
    
    if(isTruthy(input$sel_df_rows_selected)){
      
      data_all_sel <- sel_df()
      
      var_S_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% S_cols],
                   Value = unlist(data_all_sel[input$sel_df_rows_selected[1],
                                               names(data_all_sel) %in% S_cols]))
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_S_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% S_cols],
                   Value = NA)
      
    }
    
  })
  
  radar_data_S_ecdf <-  eventReactive(input$sel_df_rows_selected, {
    
    if(isTruthy(input$sel_df_rows_selected)){
      
      data_all_sel <- sel_df()
      
      data_all_sel <- data_all_sel[names(data_all_sel) %in% S_cols]
      
      data_all_sel <- apply(data_all_sel, 2, function(x) ecdf(x)(x))
      
      out <- data_all_sel[input$sel_df_rows_selected[1],]
      
      out
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_S_df <-
        data_all_sel[1,names(data_all_sel) %in% S_cols]
      
      var_S_df[1,] <- 0
      
      var_S_df
      
    }
    
  })
  
  radar_data_AC <-  eventReactive(input$sel_df_rows_selected, {
    
    if(isTruthy(input$sel_df_rows_selected)){
      
      data_all_sel <- sel_df()
      
      var_AC_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% AC_cols],
                   Value = unlist(data_all_sel[input$sel_df_rows_selected[1],
                                               names(data_all_sel) %in% AC_cols]))
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_AC_df <-
        data.frame(Variable = names(data_all_sel)[names(data_all_sel) %in% AC_cols],
                   Value = NA)
      
    }
    
  })
  
  radar_data_AC_ecdf <-  eventReactive(input$sel_df_rows_selected, {
    
    if(isTruthy(input$sel_df_rows_selected)){
      
      data_all_sel <- sel_df()
      
      data_all_sel <- data_all_sel[names(data_all_sel) %in% AC_cols]
      
      data_all_sel <- apply(data_all_sel, 2, function(x) ecdf(x)(x))
      
      out <- data_all_sel[input$sel_df_rows_selected[1],]
      
      out
      
    } else {
      
      data_all_sel <- sel_df()
      
      var_AC_df <-
        data_all_sel[1,names(data_all_sel) %in% AC_cols]
      
      var_AC_df[1,] <- 0
      
      var_AC_df
      
    }
    
  })
  
  output$radar_data_E_df <-
    DT::renderDataTable({
      
      radar_table <- radar_data_E()
      
      radar_table$Variable[radar_table$Variable == "sed_tot_med"] <- "Climate Change (SED)"
      radar_table$Variable[radar_table$Variable == "mean_HM_change"] <- "Human Modification"
      radar_table$Variable[radar_table$Variable == "rdens_osm"] <- "Road Density (km/km2)"
      radar_table$Variable[radar_table$Variable == "nb_alien"] <- "Number of Alien Species"
      radar_table$Variable[radar_table$Variable == "alien_vert_cover"] <- "Coverage of Alien Species"
      
      DT::formatRound(
        DT::datatable(radar_table,
                      options = list(dom = 't'),
                      rownames = FALSE),
        columns = 2,
        digits = 3)
      
      
    })
  
  output$radar_data_S_df <-
    DT::renderDataTable({
      
      radar_table <- radar_data_S()
      
      radar_table$Variable[radar_table$Variable == "aoh_km2"] <- "Area of habitat (km2)"
      radar_table$Variable[radar_table$Variable == "nb_diet"] <- "Dietary Breadth"
      radar_table$Variable[radar_table$Variable == "nb_hab"] <- "Habitat Use Breadth"
      radar_table$Variable[radar_table$Variable == "gen_length_y"] <- "Generation Length (years)"
      
      DT::formatRound(
        DT::datatable(radar_table,
                      options = list(dom = 't'),
                      rownames = FALSE),
        columns = 2,
        digits = 3)
    })
  
  output$radar_data_AC_df <-
    DT::renderDataTable({
      
      radar_table <- radar_data_AC()
      
      radar_table$Variable[radar_table$Variable == "Area"] <- "Island Area (km2)"
      radar_table$Variable[radar_table$Variable == "max_elev"] <- "Maximum Island Elevation (m2)"
      radar_table$Variable[radar_table$Variable == "mean_tri"] <- "Terrain Ruggednness"
      radar_table$Variable[radar_table$Variable == "PA_prop"] <- "Protected Area Coverage"
      radar_table$Variable[radar_table$Variable == "fred"] <- "Functional Redundancy"
      radar_table$Variable[radar_table$Variable == "dispersal"] <- "Dispersal Capacity"
      
      DT::formatRound(
        DT::datatable(radar_table,
                      options = list(dom = 't'),
                      rownames = FALSE),
        columns = 2,
        digits = 3)
    })
  
  output$radarplot_E <- renderPlot({
    
    radar_data_df <- radar_data_E_ecdf()
    
    names(radar_data_df)[names(radar_data_df) == "sed_tot_med"] <- "Climate Change (SED)"
    names(radar_data_df)[names(radar_data_df) == "mean_HM_change"] <- "Human Modification"
    names(radar_data_df)[names(radar_data_df) == "rdens_osm"] <- "Road Density (km/km2)"
    names(radar_data_df)[names(radar_data_df) == "nb_alien"] <- "Number of Alien Species"
    names(radar_data_df)[names(radar_data_df) == "alien_vert_cover"] <- "Coverage of Alien Species"
    
    chart_df <-
      rbind(Max = 1,
            Min = 0,
            radar_data_df) %>% 
      as.data.frame()
    
    # chart_df <-
    #   rbind(Max = 1,
    #         Min = 0,
    #         radar_data_df())
    
    create_beautiful_radarchart(chart_df, caxislabels = rep("", length(E_cols)))
    
  }, res = 72)
  
  output$radarplot_S <- renderPlot({
    
    radar_data_df <- radar_data_S_ecdf()
    
    names(radar_data_df)[names(radar_data_df) == "aoh_km2"] <- "Area of habitat (km2)"
    names(radar_data_df)[names(radar_data_df) == "nb_diet"] <- "Dietary Breadth"
    names(radar_data_df)[names(radar_data_df) == "nb_hab"] <- "Habitat Use Breadth"
    names(radar_data_df)[names(radar_data_df) == "gen_length_y"] <- "Generation Length (years)"
    
    chart_df <-
      rbind(Max = 1,
            Min = 0,
            radar_data_df) %>% 
      as.data.frame()
    
    # chart_df <-
    #   rbind(Max = 1,
    #         Min = 0,
    #         radar_data_df())
    
    create_beautiful_radarchart(chart_df, caxislabels = rep("", length(S_cols)))
    
  }, res = 72)
  
  output$radarplot_AC <- renderPlot({
    
    radar_data_df <- radar_data_AC_ecdf()
    
    names(radar_data_df)[names(radar_data_df) == "Area"] <- "Island Area (km2)"
    names(radar_data_df)[names(radar_data_df) == "max_elev"] <- "Maximum Island Elevation (m2)"
    names(radar_data_df)[names(radar_data_df) == "mean_tri"] <- "Terrain Ruggednness"
    names(radar_data_df)[names(radar_data_df) == "PA_prop"] <- "Protected Area Coverage"
    names(radar_data_df)[names(radar_data_df) == "fred"] <- "Functional Redundancy"
    names(radar_data_df)[names(radar_data_df) == "dispersal"] <- "Dispersal Capacity"
    
    chart_df <-
      rbind(Max = 1,
            Min = 0,
            radar_data_df) %>% 
      as.data.frame()
    
    # chart_df <-
    #   rbind(Max = 1,
    #         Min = 0,
    #         radar_data_df())
    
    create_beautiful_radarchart(chart_df, caxislabels = rep("", length(AC_cols)))
    
  }, res = 72)
  
  # Species name
  
  sci_name <- reactive(
      if(isTruthy(input$sel_df_rows_selected)){
        
        sel_df_name <- sel_df()
        
        paste0("<b><i>", 
               sel_df_name$sci_name[input$sel_df_rows_selected[1]], 
               "</i> at ",
               sel_df_name$Island[input$sel_df_rows_selected[1]],
               "</b>")
        
      } else {"<b>Click a species name</b>"}
  )
  
  output$sci_name <- renderText(sci_name())
  
  output$graph_all <- renderPlot({
    
      VU_sp_isl_info <- sel_df()
      # calculate mean, median and sd value per island
      VU_median_mean <-
        VU_sp_isl_info |>
        dplyr::group_by(Archip, Island) |>
        dplyr::summarise(
          Median = median(VU),
          Mean = mean(VU),
          SD = sd(VU)) |> dplyr::ungroup()
      isl_order <- VU_median_mean$Island[match(sort(VU_median_mean$Mean), VU_median_mean$Mean)]
      VU_median_mean$Island_order <- factor(VU_median_mean$Island, levels = isl_order)
      VU_sp_isl_info$Island_order <- factor(VU_sp_isl_info$Island, levels = isl_order)
      
      ggplot() +
        geom_point(data = VU_sp_isl_info, aes(x = VU, y = Island_order),
                   col = "grey", alpha = .2)+
        geom_point(data = as.data.frame(VU_median_mean), 
                   aes(x=Mean, y=Island, col=Archip), size = 2) +
        geom_errorbar(data = as.data.frame(VU_median_mean), 
                      aes(x=Mean, xmin=Mean-SD, xmax=Mean+SD, y=Island_order, col=Archip)) +
        theme_bw() +
        ggtitle('Vulnerability') +
        xlab('Vulnerability per species per island (Mean +/- SD)') +
        # scale_color_manual("Archipelago", values = archip_col) +
        theme(legend.position = "right") +
        ylab('Island name') +
        labs(color = "Archipelago")
      
    })
  
  # Map
  
  output$archipSelection_map <- renderUI({
    selectizeInput("Archip_map",
                   "Select one archipelago:",
                   choices = archip_levs,
                   selected = NULL,
                   multiple = FALSE,
                   width = "100%")
  })
  
  output$varSelection_map <- renderUI({
    selectizeInput("var_map",
                   "Select Variable to display:",
                   choices = var_levs,
                   selected = NULL,
                   multiple = FALSE,
                   width = "100%")
  })
  
  output$costsmap <- leaflet::renderLeaflet({
    
    archi_shp <- island_shp[island_shp$ARCHIP == input$Archip_map,]
    out_table <- sel_df()
    
    archi_shp$VU <- out_table$VU[match(archi_shp$ID, out_table$ID)]
    archi_shp$E <- out_table$E[match(archi_shp$ID, out_table$ID)]
    archi_shp$S <- out_table$S[match(archi_shp$ID, out_table$ID)]
    archi_shp$AC <- out_table$AC[match(archi_shp$ID, out_table$ID)]
    
    names(archi_shp)[names(archi_shp) == "VU"] <- "Vulnerability"
    names(archi_shp)[names(archi_shp) == "E"] <- "Exposure"
    names(archi_shp)[names(archi_shp) == "S"] <- "Sensitivity"
    names(archi_shp)[names(archi_shp) == "AC"] <- "Adaptive Capacity"
    
    selected_var <- input$var_map
    
    selected_var[selected_var == "VU"] <- "Vulnerability"
    selected_var[selected_var == "E"] <- "Exposure"
    selected_var[selected_var == "S"] <- "Sensitivity"
    selected_var[selected_var == "AC"] <- "Adaptive Capacity"
    
    # Create a color palette for the selected metric using 'viridis'
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = archi_shp[[selected_var]],
      na.color = "grey90"
    )
    
    pal_2 <- leaflet::colorNumeric(
      palette = "viridis",
      domain = archi_shp[[selected_var]],
      na.color = "grey90",
      reverse = TRUE
    )
    
    # Create popup/label text for each island
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      archi_shp$Island,
      selected_var,
      round(archi_shp[[selected_var]], 3)
    ) %>% lapply(htmltools::HTML)
    
    epsg4326 <- 
      leaflet::leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:4326",
                          resolutions = 2^(-1.5:-13))
    
    map_bbox <- sf::st_bbox(archi_shp)
    
    # Build the leaflet map
    leaflet::leaflet(archi_shp,
                     options = leaflet::leafletOptions(worldCopyJump = FALSE,
                                                       crs = epsg4326,
                                                       minZoom = 0, maxZoom = 12)) %>%
      leaflet::addPolygons(
        fillColor = ~pal(get(selected_var)),
        weight = 1,
        opacity = 1,
        color = "#666",
        dashArray = "1",
        fillOpacity = 0.7,
        highlight = leaflet::highlightOptions(
          weight = 2,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(
        pal = pal_2,
        na.label = NA,
        values = ~get(selected_var),
        opacity = 0.7,
        title = selected_var,
        position = "bottomright",
        labFormat = leaflet::labelFormat(transform = function(x)  sort(x, decreasing = TRUE))
      )
  })
  
  # Exposure
  
  archip_levs_expo <-
    sort(unique(as.character(exposure_data$Archip)))
  
  output$archipSelection_expo <- renderUI({
    selectizeInput("Archip_expo",
                   "Select archipelagos:",
                   choices = archip_levs_expo,
                   selected = NULL,
                   multiple = TRUE,
                   width = "100%")
  })
  
  data_sel_expo <- reactive({
    
    selected_archip <-
      filter_var(exposure_data$Archip, input$Archip_expo)
    
    data_all_sel <-
      exposure_data[selected_archip,]
    
    isl_data <-
      data_all_sel[names(data_all_sel) %in% c("Archip",
                                              "Island",
                                              "ID",
                                              CC_cols,
                                              LU_cols,
                                              BI_cols,
                                              "dominant_th",
                                              "E")]
    
    isl_data
    
  })
  
  sel_df_expo <- eventReactive(input$run_expo, {
    data_sel_expo()
  },
  ignoreNULL = FALSE)
  
  output$sel_df_expo <-
    DT::renderDataTable({
      
      out_table <- sel_df_expo()
      
      out_table <- dplyr::arrange(out_table, desc(E))
      
      out_table <- out_table[c(1:3, 8, 7, 4:6)]
      
      out_table$dominant_th[out_table$dominant_th == "CC"] <- "Climate Change"
      out_table$dominant_th[out_table$dominant_th == "LU"] <- "Land Use Change"
      out_table$dominant_th[out_table$dominant_th == "BI"] <- "Biological Invasions"
      
      names(out_table)[names(out_table) == "Archip"] <- "Archipelago"
      names(out_table)[names(out_table) == "CC"] <- "Climate Change"
      names(out_table)[names(out_table) == "LU"] <- "Land Use Change"
      names(out_table)[names(out_table) == "BI"] <- "Biological Invasions"
      names(out_table)[names(out_table) == "dominant_th"] <- "Dominant Threat"
      names(out_table)[names(out_table) == "E"] <- "Total Exposure"
      
      DT::datatable(out_table, 
                    rownames = FALSE, 
                    selection = "single", 
                    escape = FALSE) %>%
        DT::formatRound(columns = c(4, 6:8),
                        digits = 3)
    })
  
  # Download data
  
  filename_expo <-
    eventReactive(input$run_expo, {
      
      function() {
        paste("Exposure_", paste(input$Archip_expo, collapse = "-"), "_", paste(input$Island_expo, collapse = "-"), ".csv", sep = "")
      }
    },
    ignoreNULL = FALSE)
  
  content_expo <-
    eventReactive(input$run_expo, {
      function(file) {
        write.csv(data_sel_expo(), file, row.names = FALSE)
      }
    },
    ignoreNULL = FALSE)
  
  output$downloadData_expo <-
    downloadHandler(filename = filename_expo(),
                    content = content_expo())
  
  download_button_expo <-
    eventReactive(input$run_expo, {
      downloadButton("downloadData_expo", "Download data table")
    },
    ignoreNULL = FALSE)
  
  output$download_button_expo <- renderUI(download_button_expo())
  
  # Radar plot
  
  radar_data_ecdf_expo <-  eventReactive(input$sel_df_expo_rows_selected, {
    
    if(isTruthy(input$sel_df_expo_rows_selected)){
      
      data_all_sel <- sel_df_expo()
      
      data_all_sel <- dplyr::arrange(data_all_sel, desc(E))
      
      data_all_sel <- data_all_sel[names(data_all_sel) %in% c(CC_cols,
                                                              LU_cols,
                                                              BI_cols)]
      
      var_df <-
        data_all_sel[input$sel_df_expo_rows_selected[1],]
      
      var_df
      
    } else {
      
      data_all_sel <- sel_df_expo()
      
      var_df <-
        data_all_sel[1,names(data_all_sel) %in% c(CC_cols,
                                                  LU_cols,
                                                  BI_cols)]
      
      var_df[1,] <- 0
      
      var_df
      
    }
    
  })
  
  output$radarplot_expo <- renderPlot({
    
    radar_data_df <- radar_data_ecdf_expo()
    
    names(radar_data_df)[names(radar_data_df) == "CC"] <- "Climate Change"
    names(radar_data_df)[names(radar_data_df) == "LU"] <- "Land Use Change"
    names(radar_data_df)[names(radar_data_df) == "BI"] <- "Biological Invasions"
    
    chart_df <-
      rbind(Max = 1,
            Min = 0,
            radar_data_df) %>% 
      as.data.frame()
    
    create_beautiful_radarchart(chart_df, caxislabels = rep("", 3))
    
  }, res = 72)
  
  # Island name
  
  island_name <- reactive(
    if(isTruthy(input$run_expo)){
      if(isTruthy(input$sel_df_expo_rows_selected)){
        
        sel_df_name <- sel_df_expo()
        
        sel_df_name <- dplyr::arrange(sel_df_name, desc(E))
        
        paste0("<b>", 
               sel_df_name$Island[input$sel_df_expo_rows_selected[1]], 
               " at ",
               sel_df_name$Archip[input$sel_df_expo_rows_selected[1]],
               "</b>")
        
      } else {"<b>Click on an island</b>"}
    }
    
  )
  
  output$island_name <- renderText(island_name())
  
  
}
