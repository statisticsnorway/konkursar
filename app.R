

source("pakker.R")
source("data.R")

options(scipen = 10000)
options(encoding = "UTF-8")


#### UI #####

ui <- dashboardPage(dashboardHeader(title = "Konkursar i Noreg", titleWidth = 250),
                    
                    dashboardSidebar(width = 250,
                                     
                                     sidebarMenu(
                                       
                                       br(),
                                       br(),
                                       
                                       menuItem("Konkursar over tid", tabName = "konkurser"),
                                       
                                       br(),
                                       
                                       menuItem("Fordeling på næring", tabName = "naering",
                                                menuSubItem("Rangering", tabName = "histonar"),
                                                menuSubItem("Samanlikning", tabName = "scatternar")
                                       ),
                                       
                                       br(),
                                       
                                       menuItem("Geografisk fordeling", tabName = "geografi")
                                       
                                     ),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     HTML('<left><img src="ssb-logo.png", width = "200"></left>')
                                     
                    ),
                    
                    dashboardBody(
                      
                      tags$head(
                        
                        tags$style(
                          HTML(".content-wrapper {overflow: auto;}",
                               
                               ".small-box.bg-green { background-color: #274247 !important; color: #e3f1e6 !important; }",
                               
                               "
                                 .multicol { 
                                   height: 900px;
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "
                          
                        )
                      ),
                      
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                      
                      fluidRow(valueBoxOutput("konkursidag", width = 4),
                               valueBoxOutput("meanoms", width = 4),
                               valueBoxOutput("meansyss", width = 4)
                      ),
                      
                      tabItems(
                        
                        tabItem(tabName = "konkurser",
                                br(),
                                h1("Konkursar over tid"),
                                br(),
                                
                                fluidRow(
                                  
                                  column(width = 12, wellPanel(
                                    
                                    h2("Rullerende gjennomsnitt for siste syv dagar."),
                                    br(),
                                  
                                    prettyCheckbox("rollmean_dag", 
                                                   label = "Se tal for enkeltdagar."),
                                    
                                    plotlyOutput("konkurslinje_perdag")
                                    
                                  )),
                                  
                                  br(),
                                  
                                  column(width = 12, wellPanel(
                                    
                                    h2("Kumulative tal for konkursar."),
                                    
                                    plotlyOutput("konkurslinje_akkumulert")
                                    
                                  )),
                                  
                                  br(),
                                  
                                  column(width = 12,
                                         helpText(a("Tala er henta fra statistikkbanktabell 12972.",
                                                    href = "https://www.ssb.no/statbank/table/12972/",
                                                    target = "blank")))
                                  
                                )),
                        
                        tabItem(tabName = "histonar",
                                br(),
                                h1("Rangering av næringar"),
                                br(),
                                
                                fluidRow(
                                  column(width = 12,
                                         br(),
                                         h2("Tal for næringar hittil i år."),
                                         helpText("Samanlikn 2020 med tal fram til same uke i 2019 ved å trykke på årstala."),                                         br(),
                                         br())),
                                
                                fluidRow(
                                  column(width = 12,
                                         wellPanel(
                                           selectInput("varchoice_nar", "Velg variabel:", 
                                                       choices = c("Konkursar", 
                                                                   "Omsetning (1 000 kr)",
                                                                   "Tilsette",
                                                                   "Eksportverdi (1 000 kr)",
                                                                   "Importverdi (1 000 kr)"))),
                                         
                                         plotlyOutput("naeringsfordeling",
                                                      height = 1500, width = 1500))),
                                
                                br(),
                                
                                fluidRow(
                                  column(width = 12,
                                         
                                         helpText(a("Tala er henta fra statistikkbanktabell 12972.",
                                                    href = "https://www.ssb.no/statbank/table/12972/",
                                                    target = "blank"))))
                                
                        ),
                        
                        tabItem(tabName = "scatternar",
                                br(),
                                h1("Samanlikning av næringar"),
                                br(),
                                
                                fluidRow(
                                  column(width = 12,
                                         br(),
                                         h2("Tal for næringar hittil i år."),
                                         helpText("Velg næringar under."),
                                         br(),
                                         br())),
                                
                                fluidRow(
                                  column(width = 6,
                                         wellPanel(
                                           selectInput("varchoice_nar2", "Velg variabel for X-akse:", 
                                                       choices = c("Konkursar", 
                                                                   "Omsetning (1 000 kr)",
                                                                   "Tilsette",
                                                                   "Eksportverdi (1 000 kr)",
                                                                   "Importverdi (1 000 kr)"),
                                                       selected = "Konkursar"))),
                                  
                                  column(width = 6,
                                         wellPanel(
                                           selectInput("varchoice_nar3", "Velg variabel for Y-akse:", 
                                                       choices = c("Konkursar", 
                                                                   "Omsetning (1 000 kr)",
                                                                   "Tilsette",
                                                                   "Eksportverdi (1 000 kr)",
                                                                   "Importverdi (1 000 kr)"),
                                                       selected = "Tilsette")))),
                                
                                fluidRow(
                                  
                                  column(width = 1),
                                  
                                  column(width = 11,
                                         
                                         plotlyOutput("scatternaering",
                                                      inline = TRUE,
                                                      height = 800, width = 1500))),
                                
                                br(),
                                
                                fluidRow(
                                  
                                  column(width = 9,
                                         wellPanel(
                                           
                                           list(h3("Filtrer ut næring:"),
                                                tags$div(align = 'left',
                                                         class = 'multicol',
                                                         prettyCheckboxGroup("naringsvalg", "",
                                                              choices = c(levels(factor(konkurser_naring$Næring))[1:74]),
                                                              selected = c(levels(factor(konkurser_naring$Næring))[1:74])
                                                              ))))),
                                  
                                  column(width = 2,
                                         wellPanel(
                                           
                                           actionButton("checkall", label = "Velg alle næringar"),
                                           br(),
                                           br(),
                                           actionButton("uncheck", label = "Velg bort alle næringar"),
                                           br()
                                  
                                ))),
                                
                                br(),
                                
                                fluidRow(
                                  
                                  column(width = 10,
                                         
                                         helpText(a("Tala er henta fra statistikkbanktabell 12972.",
                                                    href = "https://www.ssb.no/statbank/table/12972/",
                                                    target = "blank"))))
                                
                        ),
                        
                        tabItem(tabName = "geografi",
                                br(),
                                h1("Geografisk fordeling"),
                                br(),
                                fluidRow(
                                  
                                  column(width = 3,
                                         
                                         prettyCheckbox("check_geo", label = "Hittil i år."),
                                         
                                         selectInput("varchoice_geo", "Velg variabel:", 
                                                     choices = c("Konkursar", 
                                                                 "Omsetning (1 000 kr)",
                                                                 "Tilsette",
                                                                 "Eksportverdi (1 000 kr)",
                                                                 "Importverdi (1 000 kr)")),
                                         
                                         selectInput("varchoice_uke", "Velg uke:",
                                                     choices = c(levels(factor(komoek2$uke)))[1:max(komoek2$uke)+1],
                                                     selected = max(komoek2$uke)),
                                         
                                         br(),
                                         
                                         helpText(a("Tala er henta fra statistikkbanktabell 12972.",
                                                    href = "https://www.ssb.no/statbank/table/12972/",
                                                    target = "blank"))
                                         
                                         ),
                                  
                                  column(width = 9,
                                         
                                         plotlyOutput("geografi",
                                                      height = 1000, width = 1000))
                                         
                                  )
                                )
                        
                      ),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      )
                    )
                    
)

server <- function(input, output, session){
  
  output$konkursidag <- renderValueBox({
    valueBox(value = konk2020 %>%
               dplyr::select(dato, Konkursar) %>%
               filter(dato == Sys.Date()) %>% 
               pull(Konkursar),
             subtitle = paste("Konkursar ",
                              as.character(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%d. %h %Y"))),
             icon = icon("star"), color = "green")
  })
  
  output$meanoms <- renderValueBox({
    valueBox(value = konk2020 %>%
               dplyr::select(dato, `Omsetning (1 000 kr)`) %>%
               filter(dato == Sys.Date()) %>% 
               pull(`Omsetning (1 000 kr)`),
             subtitle = paste("Omsetning (1000 kr) for konkursar ", 
                              as.character(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%d. %h %Y"))),
             icon = icon("money-bill"), color = "green")
  })
  
  output$meansyss <- renderValueBox({
    valueBox(value = konk2020 %>%
               dplyr::select(dato, `Tilsette`) %>%
               filter(dato == Sys.Date()) %>% 
               pull(`Tilsette`),
             subtitle = paste("Tilsette for konkursar ",
                              as.character(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%d. %h %Y"))),
             icon = icon("user"), color = "green")
  })
  
  output$konkurslinje_perdag <- renderPlotly({
    
    if(input$rollmean_dag != 0) {
    
    konk2020gg <- konk2020 %>%
      rbind(konk2019) %>% # Setter sammen med data for 2019
      filter(uke <= 52) %>%
      mutate(datofilter = format(as.Date(dato, "%Y%m%d"), format = "%m-%d")) %>%
      filter(datofilter <= format(as.Date(Sys.Date(), "%Y%m%d"), format = "%m-%d")) %>% 
      mutate(datoplot = as.Date(datofilter, format = "%m-%d")) %>%
      mutate(Dato = format(as.Date(datoplot, format = "%m-%d"), "%d. %h")) %>%
      ggplot(aes(x = datoplot, y = Konkursar, group = År, Dato = Dato)) +  # Lager plot
      geom_line(aes(color = År), size = 0.6) + 
      scale_color_manual(values = c("#90cc93", "#00824d")) +
      labs(x = "Måned", y = "Tal på konkursar") + 
      theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(konk2020gg,
             tooltip = c("Dato", "Konkursar")) %>%
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent')
    
    } else {
      
      konk2020gg <- konk2020 %>%
        rbind(konk2019) %>% 
        filter(uke <= 52) %>%
        mutate(datofilter = format(as.Date(dato, "%Y%m%d"), format = "%m-%d")) %>%
        filter(datofilter <= format(as.Date(Sys.Date(), "%Y%m%d"), format = "%m-%d")) %>% 
        mutate(datoplot = as.Date(datofilter, format = "%m-%d")) %>%
        mutate(Dato = format(as.Date(datoplot, format = "%m-%d"), "%d. %h")) %>%
        group_by(År) %>%
        mutate(Konkursar = round(zoo::rollapply(Konkursar, 7, mean, align='right', fill=NA), 2)) %>%
        ungroup() %>%
        ggplot(aes(x = datoplot, y = Konkursar, group = År, Dato = Dato)) +  
        geom_line(aes(color = År), size = 0.6) + 
        scale_color_manual(values = c("#90cc93", "#00824d")) +
        labs(x = "Måned", y = "Tal på konkursar") + 
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(konk2020gg,
               tooltip = c("Dato", "Konkursar")) %>%
        layout(plot_bgcolor='transparent') %>% 
        layout(paper_bgcolor='transparent')
      
    }
    
  })
  
  output$konkurslinje_akkumulert <- renderPlotly({
    
    konk2020akkgg <- konk2020akk %>%
      rbind(konk2019akk) %>%
      rename(Konkursar = n) %>%
      ggplot(aes(x = datoplot, y = Konkursar, group = År, Dato = Dato)) + 
      geom_line(aes(color = År), size = 0.6) + 
      scale_color_manual(values = c("#90cc93", "#00824d")) + 
      labs(x = "Måned", y = "Tal på konkursar") + 
      theme_light() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(konk2020akkgg,
             tooltip = c("Dato", "Konkursar")) %>%
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent')
    
  })
  
  output$naeringsfordeling <- renderPlotly({
      
    konkurser_naring19 %>%
        filter(var %in% as.character(input$varchoice_nar)) %>%
        spread(År, Verdi) %>%
        group_by(Næring) %>%
        plot_ly(x =  ~as.numeric(`2020`),
                name = "2020",
                y =  ~reorder(Næring, `2020`),
                type = "bar",
                orientation = "h",
                marker= list(color = c("#00824d")),
                height = 1500, width = 1500) %>%
        add_trace(x = ~as.numeric(`2019`), name = "2019",
                  marker = list(color = c("#90cc93"))) %>%
        layout(xaxis = list(title = as.character(input$varchoice_nar), side = "top",
                            tickformat = "digit"),
               yaxis = list(title = ""),
               barmode = 'group',
               plot_bgcolor='transparent',
               paper_bgcolor='transparent')
    
  })
  
  varinput2 <- reactive({
    
    konkurser_naring <- konkurser_naring %>%
      spread(var, Verdi) %>%
      filter(Næring %in% c(input$naringsvalg))
    
    switch(input$varchoice_nar2,
           "Konkursar" = konkurser_naring$Konkursar,
           "Omsetning (1 000 kr)" = konkurser_naring$`Omsetning (1 000 kr)`,
           "Tilsette" = konkurser_naring$Tilsette,
           "Eksportverdi (1 000 kr)" = konkurser_naring$`Eksportverdi (1 000 kr)`,
           "Importverdi (1 000 kr)" = konkurser_naring$`Importverdi (1 000 kr)`)
    
  })
  
  varinput3 <- reactive({
    
    konkurser_naring <- konkurser_naring %>%
      spread(var, Verdi) %>%
      filter(Næring %in% c(input$naringsvalg))
    
    switch(input$varchoice_nar3,
           "Konkursar" = konkurser_naring$Konkursar,
           "Omsetning (1 000 kr)" = konkurser_naring$`Omsetning (1 000 kr)`,
           "Tilsette" = konkurser_naring$Tilsette,
           "Eksportverdi (1 000 kr)" = konkurser_naring$`Eksportverdi (1 000 kr)`,
           "Importverdi (1 000 kr)" = konkurser_naring$`Importverdi (1 000 kr)`)
    
  })
  
  observe({
    if (input$uncheck > 0) {
      updatePrettyCheckboxGroup(session = session,
                                "naringsvalg",
                                choices = c(levels(factor(konkurser_naring$Næring))[1:74]),
                                selected = NULL)
    }
    
    })
  
  observe({
    if (input$checkall > 0) {
      updatePrettyCheckboxGroup(session = session,
                                "naringsvalg",
                                choices = c(levels(factor(konkurser_naring$Næring))[1:74]),
                                selected = c(levels(factor(konkurser_naring$Næring))[1:74]))
    }
    
  })
  
  output$scatternaering <- renderPlotly({
    
    scatter <- konkurser_naring %>%
      spread(var, Verdi) %>%
      filter(Næring %in% c(input$naringsvalg)) %>%
      ggplot(aes(as.integer(round(varinput2(), 0)), round(as.integer(varinput3(), 0)),
                 Næring = Næring, Konkursar = Konkursar, Tilsette = Tilsette,
                 `Eksportverdi (1 000 kr)` = `Eksportverdi (1 000 kr)`,
                 `Importverdi (1 000 kr)` = `Importverdi (1 000 kr)`, `Omsetning (1 000 kr)` = `Omsetning (1 000 kr)`,
                 label = c(input$naringsvalg), 
                 color = c(input$naringsvalg))) + 
      geom_point(aes(color = c(input$naringsvalg)), 
                 size = 3, alpha = 0.6) + 
      scale_color_manual(values = rep(c("#90cc93", "#1a9d49", "#00824d", "#075745",
                                        "#a2baba", "#6f9090", "#4b7272", "#274247"), 12)) +
      labs(color = "Næring",
           x = paste(as.character(input$varchoice_nar2)), 
           y = paste(as.character(input$varchoice_nar3))) +
      theme_light() +
      theme(plot.background = element_rect(fill = "#90cc93"),
            legend.position = "right")
    
    ggplotly(scatter, 
             tooltip = c("Næring", "Konkursar", "Tilsette", "Eksportverdi (1 000 kr)", 
                         "Importverdi (1 000 kr)", "Omsetning (1 000 kr)"),
             height = 800, width = 1500) %>%
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      htmlwidgets::onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")

    
  }) 
  
  observe({
    if (input$check_geo == 1) {
      updateSelectInput(session = session,
                        "varchoice_uke",
                        choices = c(levels(factor(komoek2$uke)))[1:max(komoek2$uke)+1],
                        selected = c(levels(factor((komoek2$uke))))["Total"])
    }
    
  })
  
  output$geografi <- renderPlotly({
    
    if(input$check_geo != 1) {
      
      ggplotly(komoek2 %>% 
                 filter(uke %in% c(0, input$varchoice_uke)) %>%
                 filter(var %in% as.character(input$varchoice_geo)) %>%
                 ggplot() +
                 geom_sf(aes(geometry = geometry, fill = Verdi, group = Region), size = 0.1) + 
                 theme_void() + 
                 scale_fill_gradientn(colors = c("white", "#a2baba", "#6f9090", "#4b7272", "#274247")) +
                 theme(legend.position = "none"),
               height = 1000, width = 1000) %>%
        layout(plot_bgcolor='transparent') %>% 
        layout(paper_bgcolor='transparent')
      
    } else {
        
        ggplotly(komoek1 %>%
                   filter(var %in% as.character(input$varchoice_geo)) %>%
                   ggplot() +
                   geom_sf(aes(geometry = geometry, fill = Verdi, group = Region), size = 0.1) + 
                   theme_void() + 
                   scale_fill_gradientn(colors = c("white", "#a2baba", "#6f9090", "#4b7272", "#274247")) +
                   theme(legend.position = "none"),
                 height = 1000, width = 1000) %>%
        layout(plot_bgcolor='transparent') %>% 
        layout(paper_bgcolor='transparent')
      
    }
    
    }) 
    

}

shiny::shinyApp(ui = ui, server = server)


