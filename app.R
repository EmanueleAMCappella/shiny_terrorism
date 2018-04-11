



# Load packages -----------------------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(leaflet)
library(maps)
library(maptools)
library(raster)
library(htmltools)
library(wordcloud)

# USER --------------------------------------------------------------------------------------------------------

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "A Terrorism Odyssey", titleWidth = 350), #disable = TRUE to disable the header
  dashboardSidebar(width = 350, 
    sidebarMenu(
      menuItem("Dashboard", icon = icon("bomb"), tabName = "Dash"),
      menuItem("Terrorism by Country", icon = icon("globe"), tabName = "World"),
      menuItem("Terrorism by Group", icon= icon("flag"), tabName = "Groups")
    )
  ), 
  
  dashboardBody(
    tabItems(

      tabItem(tabName = "Dash",
          fluidPage(
            fluidRow(
            box(height = 650, style = "font-size: 120%;", background = "light-blue", solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
              h3("Dataset"),
              tags$hr(),
              p("This app is meant to help visualizing data from the Global Terrorism Database (GTD), an open-source collection of data available at" 
                , a(strong("Kaggle"), href = "https://www.kaggle.com/START-UMD/gtd",target="_blank", style = "color :white; font-weight : bold;"),
                ". The GTD can boast 170,000 observations and over 50 features, giving information about location, targets and perpetrators of terrorist acts. 
                The major strenght of the dataset - in addition to its richness - is its time span, which covers from 1970 to 2016. This feature allows to follow the evolution of terrorist
                acts and terrorist groups over almost 50 years. The database is maintained by researchers at the National Consortium for the Study 
                of Terrorism and Responses to Terrorism (START), University of Maryland."),
                tags$br(),
              p("In my opinion, the two major limitations of the GDT include:"),
                tags$ul(
                  tags$li("a too broad definition of terrorism, described as 'threatened or actual use of illegal force and violence by a non-state actor to attain a political,
                  economic, religious, or social goal through fear, coercion, or intimidation'. Please note that this is only my personal opinion, but I find it is strange
                  to compare a bombing attack by religious extremists which kills 10 people with a kidnapping by politically-oriented groups. In short, I think the 
                  definition is too broad, and that it would be better to define different homogeneous terrorism categories. 
                  The issue is not only linguistic, because different phenomena require different strategical countermeasures."),
                  tags$li("Lack of data for the 1993, due to a problem in data collection.")),
              p("For further information about the dataset, read the ",
              a(strong("GDT codebook."), href = "https://www.kaggle.com/START-UMD/gtd",target="_blank", style = "color :white; font-weight : bold;"))),
            
            box(height = 650, style = "font-size: 120%;", background = 'navy', solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                h3("Research Questions"),
                tags$hr(),
                p("In developing this app I had two major research questions in mind:"),
                tags$ol(
                  tags$li("What area suffered most from terrorism since 1970?"),
                  tags$li("How have terrorist groups changed over time?")),
                p("To answer the first question I realized to tiny shiny apps:"),
                tags$ul(
                  tags$li("A choropleth map drawn in leaflet showing the number of people killed per country byt terrorist actions.
                          Please note that there is a small bug that may affect the map's legend - as detailed in this ", a(strong("resource"), href = "https://www.kaggle.com/START-UMD/gtd",target="_blank", style = "color :white; font-weight : bold;"),
                          ".To solve this visualization issue, please set the zoom level of your browser <90%"),
                  tags$li("A first linechart where it is possible to visualize the different trends in terrorism experienced by country since 1970.")),
                p("To analyze terrorism at group level:"),
                tags$ul(
                  tags$li("A simple wordcloud visualizing the most violent terrorist groups in terms of killings. One caveat: to fit a better wordcloud I chose to use the acronyms of some of the the various 
                  groups, e.g. 'LTTE' instead of 'Liberation Tigers of Tamil Eelam'."),
                  tags$li("A second linechart comparing the terror groups over time.")),
                p("A shiny app was in my view the better approach to tackle such broad questions. This way, it is possible to interactively modify our search to focus on
                  the country or group of interest, thus testing multiple hypotheses with a few clics. The full code of the app may be found on",
                  a(strong("Github."), href = "https://github.com/EmanueleAMCappella/shiny_terrorism",target="_blank", style = "color :white; font-weight : bold;")))
            ))),
                
      
      tabItem(tabName = "World",
          fluidPage(
            column(width= 12, box(status = "primary", title = "Terrorism Map", width = 12, solidHeader = TRUE, collapsible = TRUE,
                fluidRow(
                  box(width= 3, selectInput("colors", "Choose Color Palette", choices = c("Purples", "Greys", "Greens","YIGn", "YlOrRd", "Blues", "YlOrBr"))),
                  box(height = 550, width= 9, title = "Map", leafletOutput("map", height = 500)))))),  
          
          fluidPage(
            column(width = 12, box(title = "Country comparison", width = 12, solidHeader = TRUE, collapsible = TRUE,
              fluidRow(
                box(width= 4, selectizeInput("cnt", "Select Country:", choices = deathXyear$country_txt, selected = "United Kingdom", multiple = TRUE)),
                box(width= 8, height= 350, title = "Country Linechart", 
                plotOutput("plot2", height= 300))))))),
       
      
      tabItem(tabName = "Groups",
                  
          fluidPage(
            column(width = 10, height = 400, box(status = "primary", title = "Group Wordcloud", width = 12, solidHeader = TRUE, collapsible = TRUE,
              fluidRow(
                box(width = 4, title = "Choose Parameters", 
                radioButtons("color", "Select color theme", c("Accent", "Dark", "Set1"), selected= "Dark"),
                hr(),
                sliderInput("freq", "Minimum Number of Deaths:", min = 50,  max = 1000, value = 75),
                sliderInput("max", "Maximum Number of Groups:", min = 1,  max = 300,  value = 200)),
                box(width = 6, title = "Wordcloud", 
                plotOutput("plot3", width= 550, height= 550)))))),  
                                                           
          fluidPage(
            column(width = 10, height = 150, box(title = "Group analysis", width = 12, solidHeader = TRUE, collapsible = TRUE,
              fluidRow(
                box(width= 3, selectizeInput("terror", "Select Terror group:", choices = terrorg$gname, selected = "ETA", multiple = TRUE)),
                box(width= 7, title = "Group Linechart", 
                plotOutput("plot4", width= 750)))))))
      )
    )
  )

          
                

# SERVER ------------------------------------------------------------------------------------------------------
server <- function(input,output,server){
  
  #first app: map  
  bins <- c(0, 50, 100, 1000, 2000, 5000, 10000, 30000, 100000)
  colorpal <- reactive({colorBin(input$colors, domain = datamap$density, bins = bins)})
    
  output$map <- renderLeaflet({
    paletta<- colorpal()
    leaflet(datamap) %>% addTiles() %>%
    setView(lat = 9.1383, lng = 38.7223, zoom = 1) %>%
    addPolygons(data = datamap, weight = 1, fillColor = ~paletta(datamap$killed), fillOpacity = 0.5, 
    highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",fillOpacity = 0.7,bringToFront = TRUE),label = labels) %>%
    addLegend(pal = paletta, values = ~ killed, opacity = 0.7, title = "Number of terror deaths", position = 'bottomright')
  })
  
  #second app: linechart by country
  output$plot2<- renderPlot({
    deathXyear1 <- deathXyear[deathXyear$country_txt %in% input$cnt, ]
    
    ggplot(data= deathXyear1) + geom_line(aes(x= iyear, y= killed, colour=country_txt)) + 
      xlab("Year") + ylab("Terror deaths") + scale_x_continuous(name="Years", (breaks=seq(1970,2016,3))) + 
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+ scale_color_discrete("Countries")  
      })
  
  #third app: wordcloud
  
  output$plot3 <- renderPlot({
    z <- input$selection
    wc_color= brewer.pal(8, "Set2")  
    
    if (input$color== "Accent"){
      wc_color = brewer.pal(8, "Accent")} 
    else if (input$color== "Set1"){
      wc_color = brewer.pal(8, "Set1")}
    else{
      wc_color= brewer.pal(8, "Dark2")}
    
    x <- dbK
    wordcloud(x$gname, x$killed, min.freq= input$freq, max.words= input$max, colors= wc_color, random.order=T, rot.per= .20)
  })
  
  #fourth app: linechart by group
  output$plot4<- renderPlot({
    terrorg1 <- terrorg[terrorg$gname %in% input$terror, ]
    
    ggplot(terrorg1) + geom_line(aes(x= iyear, y= killed, colour=gname)) + 
    xlab("Year") + ylab("Terror deaths") + scale_x_continuous(name="Years", (breaks=seq(1970,2016,3))) + 
    ggtitle("Terror groups Comparison")+  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=45))+
    scale_color_discrete("Terror groups")  
    })

  
}

shinyApp(ui,server)





