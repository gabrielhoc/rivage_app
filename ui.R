library(shiny)
library(shinydashboard)

ui <- 
  dashboardPage(
    header = dashboardHeader(title = "RIVAGE App"),
    sidebar = dashboardSidebar(
      tags$style(HTML("
  .sidebar .dataTables_scrollHead th { color: #fff !important; }
")),
      tags$style(".skin-blue .sidebar .shiny-download-link { color: #444; }"),
      width = 400,
      div("Pages"),
      sidebarMenu(
        id = "menu",
        menuItem("Home", tabName = "home"),
        menuItem("Proof of concept", tabName = "proof",
                                 menuSubItem("Table", tabName = "proof_table"),
                                 menuSubItem("Graph", tabName = "proof_graph"),
                                 menuSubItem("Components", tabName = "proof_components"),
                                 menuSubItem("Maps", tabName = "proof_map")),
        menuItem("Global Exposure", tabName = "exposure",
                 menuSubItem("Table", tabName = "exposure_table"),
                 menuSubItem("Components", tabName = "exposure_components"))
      ),
      conditionalPanel(
        condition = 'input.menu=="proof_table"|input.menu=="proof_graph"|input.menu=="proof_components"',
        HTML('<b>Filters:</b>'),
        HTML("<br>"),
        uiOutput("classSelection"),
        uiOutput("archipSelection"),
        # HTML("<br>"),
        # HTML('<b>Component weights:</b>'),
        # DT::DTOutput("weight_out"),
        # HTML("<br>"),
        # HTML('<b>Exposure weights:</b>'),
        # DT::DTOutput("weight_e"),
        # HTML("<br>"),
        # HTML('<b>Adaptive Capacity weights:</b>'),
        # DT::DTOutput("weight_ac"),
        HTML("<br>"),
        actionButton("run", label = "Update results"),
        HTML("<br>"),
        uiOutput("download_button")
      ),
      conditionalPanel(
        condition = 'input.menu=="exposure_table"|input.menu=="exposure_components"',
        HTML('<b>Filters:</b>'),
        HTML("<br>"),
        uiOutput("archipSelection_expo"),
        HTML("<br>"),
        actionButton("run_expo", label = "Update results"),
        HTML("<br>"),
        uiOutput("download_button_expo")
      ),
      conditionalPanel(
        condition = 'input.menu=="proof_map"',
        actionButton("run_map", label = "Update results"),
        HTML('<b>Filters:</b>'),
        HTML("<br>"),
        uiOutput("archipSelection_map"),
        HTML("<br>"),
        uiOutput("varSelection_map")
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "home",
          h1("Home page - RIVAGE Project"),
          htmltools::tagList(HTML("<font color=\"red\" size = 5><center>* This is a beta version of the application. 
                           Please help us improve it by reporting any errors or suggesting technical and aesthetic 
                           improvements at this "), 
                             htmltools::a("Google Document", 
                                          href  = "https://docs.google.com/document/d/166CSBkV98yWBcTrXGGZbfheUDYZvtG9nFVkvBglk0D8/edit?tab=t.0"),
                             HTML("*</center></font>")),
          p("Welcome to the RIVAGE project application."),
          p(
            "Here you can find interactive graphs displaying the results of publications resulting from the RIVAGE project. The objective of RIVAGE is to assess the vulnerability of island assemblages to climate change, land use change, and biological invasions."
          ),
          h4("The Island Vulnerability Framework:"),
          p(
            "RIVAGE's Island Vulnerability Framework considers multiple quantitative
             markers of exposure (i.e., the extent of modification of the island’s physical environment) for
             different threats, in addition to characteristics reflecting insular species’ sensitivity (i.e.,
             species’ intrinsic capacity to respond to these threats) to global change."
          ),
          img(src='vu_index.png', align = "center", width = "50%"),
          h4("Helpful links:"),
          p("RIVAGE website : https://rivage.cnrs.fr/"),
          h4("Publications:"),
          p(
            "Bellard, C., Marino, C., Butt, N., Fernández-Palacios, J.M., Rigal, F., Robuchon, M., Lenoir, J., Irl, S., Benítez-López, A., Capdevila, P. and Zhu, G., 2025. A framework to quantify the vulnerability of insular biota to global changes. Peer Community Journal, 5."
          ),
          p(
            "Marino, C., Benítez-López, A., Butt, N., Caetano, G., Capdevila, P., Denelle, P., Etard, A., Palacios, J.M.F., Ferreiro-Arias, I., Leclerc, C. and Lenoir, J., 2025. Mismatches in exposure, sensitivity, and adaptive capacity to global change lead to contrasting vulnerability profiles for insular vertebrates."
          )
        ),
        tabItem(
          tabName = "proof_table",
          h1("Proof of concept - Table"),
          p(
            "Here you can see the results of RIVAGE's island vulnerability framework applied to six archipelagos around the world. You can filter the results by archipelago or taxonomic group using the side bar. You can also change the weighting of different components in the vulnerability calculation, and see their effect on the results. You can also click any line on the result tables to display the detailed data for the exposure, sensitivity and adaptive capacity of each species in each island."
          ),
          fluidRow(
            DT::dataTableOutput("sel_df")
          )
        ),
        tabItem(
          tabName = "proof_graph",
          h1("Proof of concept - Graph"),
          p(
            "Here you can see the results of RIVAGE's island vulnerability framework applied to six archipelagos around the world. You can filter the results by archipelago or taxonomic group using the side bar. You can also change the weighting of different components in the vulnerability calculation, and see their effect on the results. You can also click any line on the result tables to display the detailed data for the exposure, sensitivity and adaptive capacity of each species in each island."
          ),
          fluidRow(
            plotOutput("graph_all", 
                       width = "100%",
                       height = "400px")
          )
        ),
        tabItem(
          tabName = "proof_components",
          h1("Proof of concept - Components"),
          p(
            "Select a row at the tab `Proof of concept - Table` to display the vulnerability components for a species on an island."
          ),
          fluidRow(
            htmlOutput("sci_name"),
            HTML("<br>"),
            HTML('<b>Exposure</b>'),
            splitLayout(DT::dataTableOutput("radar_data_E_df"),
                        plotOutput("radarplot_E",
                                   height = "300px"),
                        cellWidths = c("35%", "45%"),
                        cellArgs = list(style = "vertical-align: top")),
            HTML("<br>"),
            HTML('<b>Sensitivity</b>'),
            splitLayout(DT::dataTableOutput("radar_data_S_df"),
                        plotOutput("radarplot_S",
                                   height = "300px"),
                        cellWidths = c("35%", "45%"),
                        cellArgs = list(style = "vertical-align: top")),
            HTML("<br>"),
            HTML('<b>Adaptive Capacity</b>'),
            splitLayout(DT::dataTableOutput("radar_data_AC_df"),
                        plotOutput("radarplot_AC",
                                   height = "300px"),
                        cellWidths = c("35%", "45%"),
                        cellArgs = list(style = "vertical-align: top"))
          )
        ),
        tabItem(
          tabName = "proof_map",
          h1("Proof of concept - Maps"),
          p(
            "Here you can see the results of RIVAGE's island vulnerability framework applied to six archipelagos around the world. You can filter the results by archipelago or taxonomic group using the side bar. You can also change the weighting of different components in the vulnerability calculation, and see their effect on the results. You can also click any line on the result tables to display the detailed data for the exposure, sensitivity and adaptive capacity of each species in each island."
          ),
          leaflet::leafletOutput("costsmap", height = "500px")
        ),
        tabItem(
          tabName = "exposure_table",
          h1("Global Exposure"),
          p(
            "Here you can see the results of RIVAGE's exposure metric for all islands in the world."
          ),
          fluidRow(
            DT::dataTableOutput("sel_df_expo")
          )
        ),
        tabItem(
          tabName = "exposure_components",
          h1("Global Exposure - Components"),
          p(
            "Select a row at the tab `Global Exposure - Table` to display the vulnerability components for an island."
          ),
          fluidRow(
            htmlOutput("island_name"),
            HTML("<br>"),
            plotOutput("radarplot_expo"),
            HTML("<br>")
          )
        )
      )
    )
  )