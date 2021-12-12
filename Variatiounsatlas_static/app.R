#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("global.R")

## UI
ui <- function(request) {
  autoWaiter()
  dashboardPage(
    preloader = preloader,
    fullscreen = TRUE,
    scrollToTop = TRUE,
    
    dashboardHeader(title = dashboardBrand(
      title = h4(HTML("Variatiounsatlas vum<br/>Lëtzebuergeschen")),
      #color = "primary",
      #href = "https://adminlte.io/themes/v3",
      image = "logo.png"
    ),
    titleWidth = 500),
    
    dashboardSidebar(
      # sidebarUserPanel(
      #   image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
      #   name = "Welcome Onboard!"
      # ),
      collapsed = FALSE,
      skin = "light",
      # selectInput(
      #   inputId = "map_category",
      #   label = "1. Kategorie",
      #   choices = list("Foneetik" = "Foneetik",
      #                  "Wuertschatz & Sproochkontakt" = "Wuertschatz",
      #                  "Grammatik" = "Grammatik"),
      #   selectize = FALSE,
      #   size = 3
      #   # options = list(
      #   # placeholder = 'Type to search for ingredient',
      #   # onInitialize = I('function() { this.setValue(""); }')
      #   #)
      # ),
      selectInput("kaart", "Kaart", choices = make_choices(), selectize = FALSE, size =20)
      
      #tags$p("Note: All nutrient information is based on the Canadian Nutrient File. Nutrient amounts do not account for variation in nutrient retention and yield losses of ingredients during preparation. % daily values (DV) are taken from the Table of Daily Values from the Government of Canada. This data should not be used for nutritional labeling.")
    ),
    dashboardBody(
      # fluidRow(
      #   valueBoxOutput("calories"),
      #   valueBoxOutput("over_nutrient"),
      #   valueBoxOutput("rich_nutrient")
      # ),
      fluidRow(
        box(title = "Iwwerbléckskaart", solidHeader = T,
            width = 12, collapsible = T, status = "primary",
            shinycssloaders::withSpinner(
              plotOutput("Iwwerbléckskaart", height = "700px")))),
      fluidRow(
        box(title = "Variantekaarten", solidHeader = T,
            width = 12, collapsible = T, status = "primary",
            plotOutput("Variantekaarten", height = "700px"))),
      fluidRow(
        tabBox(title = "Sozialdaten", width = 12,
               type = "tabs",
               tabPanel("Alter", plotOutput("plotAlter")),
               tabPanel("Geschlecht", plotOutput("plotGeschlecht")),
               tabPanel("Dialektgebiet", plotOutput("plotDialektgebiet")),
               tabPanel("Ausbildung", plotOutput("plotAusbildung")),
               tabPanel("Kompetenz Däitsch", plotOutput("plotKompetenzD")),
               tabPanel("Kompetenz Franséisch", plotOutput("plotKompetenzF"))
        )
      ),
      fluidRow(
        box(title = "Inference decision tree", solidHeader = T,
            width = 12, collapsible = T,
            plotOutput("plotTree", height = "600px"))
      ),
      fluidRow(
        box(title = "Audio", solidHeader = T,
            width = 12, collapsible = T,
            DTOutput("plotAudio", height = "600px"))
      ),
      
      dashboardFooter(
        left = a(
          href = "https://twitter.com/PeterGilles",
          target = "_blank", "Peter Gilles, Universitéit Lëtzebuerg"
        ),
        right = "2021"
      )
    )
  )
}

# Server
server <- function(input, output, session) {
  
  # map_category <- reactive({
  #   req(input$map_category)
  #   filter(variables, map_category == input$map_category)
  # })
  # input_choice <- reactive({
  #   req(input$input_choice)
  #   filter(map_category(), input_choice == input$input_choice)
  # })
  
  kaart <- reactive({
    req(input$kaart)
  })
  
  ### Iwwerbléckskaart
  output$Iwwerbléckskaart <- renderPlot({
    # prepare data
    variableFonInput <- input$kaart
    #variableFonInput <- input_choice()$variable
    variable <- variableFonInput
    
    # load google table according to input selection
    #google_df_name <- variables %>%
    #  filter(variable == variableFonInput) %>% pull(google_df)
    
    # if al RDS with data are preloaded on startup
    #google_df <<- get(google_df_name)
    # load RDS only when selected in menu - not really faster
    #google_df <<- readRDS(paste0(google_df_name, ".RDS"))
    
    # pull variant selection list from tribble
    #selection <- variables %>% 
    #  filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    #selection <- strsplit(selection, " ")[[1]]
    
    # pull lsa number from tribble
    #lsa_map_number <- variables %>% filter(variable == variableFonInput) %>% pull(lsa_map_number) %>% as.character()
    
    # map title Todo: custom title
    #map_title <- variable
    
    # pull item number from tribble
    #item_number <- variables %>% filter(variable == variableFonInput) %>% pull(item_number) %>% as.character()
    
    # pull item text from tribble
    #item_text <- variables %>% filter(variable == variableFonInput) %>% pull(item_text) %>% as.character()
    
    # run prepare function, returns plot_data as global
    #data_fon <- prepare_data(variable, "Gemeng_alt", selection)
    
    download.file(url = paste0("https://luxappsdata.uni.lu/qs_files_variatiounsatlas/", variable, "_Iwwerbleckskaart.qs"),
                  destfile = "temp_map1.qs")
    qread("temp_map1.qs")
    
    #make_summary_plot(data_fon, lsa_map_number, selection, length(selection), map_title, item_number, item_text)
    
  })
  
  ### Variantekaarten
  output$Variantekaarten <- renderPlot({
    
    # prepare data
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # load google table according to input selection
    google_df_name <- variables %>%
      filter(variable == variableFonInput) %>% pull(google_df)
    
    # if al RDS with data are preloaded on startup
    #google_df <<- get(google_df_name)
    # load RDS only when selected in menu - not really faster
    #google_df <<- readRDS(paste0(google_df_name, ".RDS"))
    
    # pull variant selection list from tribble
    #selection <- variables %>% 
    #  filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    #selection <- strsplit(selection, " ")[[1]]
    
    # pull lsa number from tribble
    #lsa_map_number <- variables %>% filter(variable == variableFonInput) %>% pull(lsa_map_number) %>% as.character()
    
    # map title Todo: custom title
    #map_title <- variable
    
    # pull item number from tribble
    #item_number <- variables %>% filter(variable == variableFonInput) %>% pull(item_number) %>% as.character()
    
    # pull item text from tribble
    #item_text <- variables %>% filter(variable == variableFonInput) %>% pull(item_text) %>% as.character()
    
    # run prepare function, returns plot_data as global
    #data_fon <- prepare_data(variable, "Gemeng_alt", selection)
    
    all_maps <- function(dataset) {
      plots <- dataset %>%
        group_by(variants, color)   %>%
        nest()  %>%
        drop_na() %>%
        mutate(plots = pmap(list(variable = variants,
                                 dataset = data,
                                 color = color),
                            make_plot_m))
      
      p <- cowplot::plot_grid(plotlist = plots$plots, nrow = round(length(plots$plots)/3))
      
      qsave(p, file = paste0(map_title, "_Variantekaarten.qs"))
      
      #ggsave(plot = p, filename = paste0("variantekaart_", ".pdf"), units = "cm", width = 22)
      #p
    }
    
    #all_maps(data_fon)
    
    download.file(url = paste0("https://luxappsdata.uni.lu/qs_files_variatiounsatlas/", variable, "_Variantekaarten.qs"),
                  destfile = "temp_map2.qs")
    qread("temp_map2.qs")
    
  })
  
  output$plotAlter <- renderPlot({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # load google table according to input selection
    google_df_name <- variables %>%
      filter(variable == variableFonInput) %>% pull(google_df)
    
    # if al RDS with data are preloaded on startup
    #google_df <<- get(google_df_name)
    # load RDS only when selected in menu - not really faster
    google_df <<- readRDS(paste0(google_df_name, ".RDS"))
    
    # pull variant selection list from tribble
    selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    selection <- strsplit(selection, " ")[[1]]
    
    caption = "D'Zuel an de faarwege Segmenter steet fir d'Unzuel vu Participantë pro Kategorie. D'Prozenter pro Variant léisst sech iwwert d'Gréisst vum Segment ofliesen."
    
    plot_social_categories(Alter, variable, selection, caption)
    
  }) 
  
  # plot function Geschlecht
  output$plotGeschlecht <- renderPlot({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # pull variant selection list from tribble
    selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    selection <- strsplit(selection, " ")[[1]]
    
    caption = "D'Zuel an de faarwege Segmenter steet fir d'Unzuel vu Participantë pro Kategorie. D'Prozenter pro Variant léisst sech iwwert d'Gréisst vum Segment ofliesen."
    
    plot_social_categories(Geschlecht, variable, selection, caption)
    
  }) 
  
  # plot function Dialektgebiet
  output$plotDialektgebiet <- renderPlot({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # pull variant selection list from tribble
    selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    selection <- strsplit(selection, " ")[[1]]
    
    caption = "D'Zuel an de faarwege Segmenter steet fir d'Unzuel vu Participantë pro Kategorie. D'Prozenter pro Variant léisst sech iwwert d'Gréisst vum Segment ofliesen."
    
    plot_social_categories(Dialektgebiet, variable, selection, caption)
    
  }) 
  
  # plot Ausbildung
  output$plotAusbildung <- renderPlot({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # pull variant selection list from tribble
    selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    selection <- strsplit(selection, " ")[[1]]
    
    caption = "D'Zuel an de faarwege Segmenter steet fir d'Unzuel vu Participantë pro Kategorie. D'Prozenter pro Variant léisst sech iwwert d'Gréisst vum Segment ofliesen."
    
    plot_social_categories(Ausbildung, variable, selection, caption)
    
  })
  
  # plot function Kompetenz D
  output$plotKompetenzD <- renderPlot({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # pull variant selection list from tribble
    selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    selection <- strsplit(selection, " ")[[1]]
    
    caption = "1 = bal guer keng Kompetenz, 7 = perfekt Kompetenz. Fir d'Interpretatioun si just d'Stufe vu 5 bis 7 reliabel"
    
    plot_social_categories(`Kompetenz am Däitschen`, variable, selection, caption)
    
  })
  
  # plot function Kompetenz F
  output$plotKompetenzF <- renderPlot({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # pull variant selection list from tribble
    selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    selection <- strsplit(selection, " ")[[1]]
    
    caption = "1 = bal guer keng Kompetenz, 7 = perfekt Kompetenz. Fir d'Interpretatioun si just d'Stufe vun 3 bis 7 reliabel"
    
    plot_social_categories(`Kompetenz am Franséischen`, variable, selection, caption)
    
  })
  
  # plot decision tree
  output$plotTree <- renderPlot({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    # pull variant selection list from tribble
    selection <- variables %>% filter(variable == variableFonInput) %>% pull(selection) %>% as.character() %>% as.vector()
    selection <- strsplit(selection, " ")[[1]]
    
    plot_decision_tree(variable = variable, selection)
  })
  
  # plot function Audio
  output$plotAudio <- renderDT({
    
    variableFonInput <- input$kaart
    variable <- variableFonInput
    
    plot_datatable(variable)
  })
  
  # observer for bookmark
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
  
  # # observe wenn Kartenkategorie geändert wird > dann Kartenauswahl anpassen
  # observeEvent(map_category(), {
  #   updateSelectInput(session, "input_choice", choices = map_category()$input_choice, selected = "Fra")
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
