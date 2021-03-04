#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
#          dev.off()
#      }
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(dashboardthemes)

# Load and clean data ----------------------------------------------
ev.sales <- read.csv("new ZEV sales.csv")
ev.population <- read.csv("vehicle_population.csv")

#cleaning EV data
ev.chargers <- read.csv("ev_chargers.csv")
#only CA and EVs
ev.chargers1 <- ev.chargers[ev.chargers$State == "CA" & 
                                ev.chargers$Fuel.Type.Code == "ELEC"
                            ,c("Fuel.Type.Code","City","EV.Level1.EVSE.Num",
                               "EV.Level2.EVSE.Num","EV.DC.Fast.Count"  )]

#groupby City
ev <- ev.chargers1 %>% group_by(City)%>% 
    summarise(count =n(),
              Level_one = sum(EV.Level1.EVSE.Num, na.rm=TRUE),
              Level_two = sum(EV.Level2.EVSE.Num, na.rm = TRUE),
              DC_Fast = sum(EV.DC.Fast.Count, na.rm =TRUE)) %>% 
    mutate(total = Level_one +Level_two +DC_Fast)
ev <- ev[order(-ev$total),]

## dataset2
counties = unique(ev.sales$County)
county_wise_sales <- ev.sales %>% group_by(Data.Year, County) %>% summarise( total_sales = sum(Number.of.Vehicles, na.rm = TRUE))
county_wise_sales$Data.Year <- as.factor(county_wise_sales$Data.Year)
## datset3
fuel <- ev.population %>% group_by(Data.Year, Fuel.Type) %>% summarise(vehicles_population =  sum(Number.of.Vehicles,na.rm =TRUE))
fuel$Data.Year <- as.factor(fuel$Data.Year)
fuel_type <- unique(fuel$Fuel.Type)
ggplot(data = fuel, aes(x = Data.Year, color = Fuel.Type))+
    geom_line(aes(y = vehicles_population, group =1))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
    xlab("City in CA") + ylab("Numbers of charging stations") + facet_wrap(.~Fuel.Type, scales = "free")

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Electric Vehicles (CA)",
                          
                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Forecasting done!", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "EV lists")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                           from = "Arun",
                                           message = HTML("Let's put on our green SWAG!! <br> Go Green!."),
                                           icon = icon("exclamation-circle"))
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    
    sidebarMenu(
        id = "tabs",
        # Menu Items ----------------------------------------------
        menuItem("Electric vehicle Sale", icon = icon("car"), tabName = "sale"),
        
        menuItem("Total Vehicle Population", icon = icon("truck"), tabName = "newvehicle"),
        
        menuItem("Electric Vehicle Chargers", icon = icon("battery-half"), tabName = "EVchargers"),
        
        menuItem("DATA TableEVC", icon = icon("table"), tabName = "tableevc"), #badgeLabel = "new", badgeColor = "green"),
        
        br(), # break for better visibility
        
        #INputs: county selected
        selectInput("county",
                    "Select County For Total Vehicle Population:",
                    choices = counties,
                    multiple = FALSE,
                    #selectize = TRUE,
                    selected = "Los Angeles"),
        
        #input: Fuel type wise vehicles sold
        selectInput("fuel",
                    "Select Fuel Type for sold electric vehicles:",
                    choices = fuel_type,
                    multiple = FALSE,
                    #selectize = TRUE,
                    selected = "Diesel"),
        
        # top x Selection ----------------------------------------------
        sliderInput("topSelect",
                    "How many top cities-number of chargers?",
                    min = 5,
                    max = 25,
                    value = 5,
                    step = 1), 
        
        # Inputs: select level of chargers to plot ----------------------------------------------
        selectInput("chargeLevel",
                    "Charger Level Trend:",
                    choices = c("Level_one", "Level_two", "DC_Fast", "total"),
                    multiple = FALSE,
                    #selectize = TRUE,
                    selected = "total"),
        br(),
        br(),
        br(),
        # Write sampled data as csv ------------------------------------------
        actionButton(inputId = "write_csv", 
                     label = "Write CSV")
       
    )
)


# Dashboard body ----------------------------------------------
body <- dashboardBody(shinyDashboardThemes(theme = "blue_gradient"),
                      
                      
    # Input and Value Boxes ----------------------------------------------
    fluidRow(
        infoBoxOutput("EVcars"),
        infoBoxOutput("Totalchargers"),
        valueBoxOutput("Totalcars")
    ),
                      tabItems(
                          
                          # Plot page ----------------------------------------------
                          tabItem("EVchargers",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Electric Vehicle Charger: Status across CA",
                                             width = 12,
                                             tabPanel("Chargers for top cities", plotlyOutput("plot_charger")),
                                             tabPanel("Inequity within charger distribution", plotlyOutput("plot_char")))
                                  )
                          ),
                          uiOutput(outputId = "n"),
                          # Data Table Page ----------------------------------------------
                          tabItem("tableevc",
                                  fluidPage(
                                      box(title = "List of level-wise Chargers", DT::dataTableOutput("table_evc"), width = 12))
                          ),
                          tabItem("newvehicle",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Fueltype wise Total Vehicle Population",
                                             width = 12,
                                             tabPanel("Yearly trend for vehicles", plotlyOutput("plot_fuel")),
                                             tabPanel("Fuel wise trend", plotlyOutput("plot_facet")))
                                  )
                          ),
                          
                          tabItem("sale",
                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Total Electric Vehicle Sales",
                                             width = 12,
                                             tabPanel("Sales trend for a County", plotlyOutput("sales_county")),
                                             #tabPanel("A2", plotlyOutput("plot_char"))),
                                      tabPanel("Fuel Type wise distribution", plotlyOutput("pie")))
                                  )
                           )
                          
                      )
)

ui <- dashboardPage( header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    
    
    # Reactive data function -------------------------------------------
    evInput <- reactive({
        ev1 <- select(ev, c("City",input$chargeLevel))
        # Return dataframe ----------------------------------------------
        return(ev1)
    })
    
    salesInput <- reactive({
        ev1 <- filter(county_wise_sales, County == input$county)
        # Return dataframe ----------------------------------------------
        return(ev1)
    })
    
    fuelInput <- reactive({
        ev1 <- filter(fuel, Fuel.Type == input$fuel)
        # Return dataframe ----------------------------------------------
        return(ev1)
    })
    
    
    # A plot showing the mass of characters -----------------------------
    output$plot_charger <- renderPlotly({
        dat <- evInput()
        
        # Generate Plot ----------------------------------------------
        ggplot(data = dat[1:input$topSelect[1],], aes_string(x = "City", y = input$chargeLevel))+
            geom_line(aes(group =1), color = "blue")+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
            xlab("City in CA") + ylab("Numbers of charging stations")
    })
    
    output$sales_county <- renderPlotly({
    dat <- salesInput()
    
    # Generate Plot ----------------------------------------------
    ggplot(data = dat, aes_string(x = "Data.Year", y = "total_sales"))+
        geom_line(aes(group =1), color = "blue")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
        xlab("Year") + ylab("Numbers of Electric Vehicles")+ ggtitle(paste("Year wise sales for", input$county))
})
    output$pie <- renderPlotly({
        type.sales <- ev.sales %>% group_by(Fuel.Type) %>% summarise( total_sales = sum(Number.of.Vehicles, na.rm = TRUE))
        ggplot(type.sales, aes(x="Fuel.Type", y=total_sales, fill=Fuel.Type)) +
            geom_bar(stat="identity", position = "dodge", color = "white") + ggtitle("Vehicle sales by Fuel type")
    })
    
    # A plot showing the height of characters -----------------------------------
    output$plot_char <- renderPlotly({
        # Generate Plot ----------------------------------------------
        ggplot(data = ev, aes(x = City))+
            geom_line(aes(y = total, group =1), color = "blue")+
            # geom_line(aes(y = Level_two ,  group=1), color = "black")+
            # geom_line(aes(y = DC_Fast, group =1), color = "green")+
            # geom_point(aes(y = total, group =1), color = "orange")+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
            xlab("City in CA") + ylab("Numbers of charging stations")+theme(axis.text.x=element_blank())+
            ggtitle("Plot showing uneven distribution across CA")
    })
    
    output$plot_fuel <- renderPlotly({
        
        dat <- fuelInput()
        # Generate Plot ----------------------------------------------
        ggplot(data = dat, aes(x = Data.Year, color = Fuel.Type))+
            geom_line(aes(y = vehicles_population, group =1))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
            xlab("Year") + ylab(paste("Number of vehicles:", input$fuel)) + ggtitle(paste("Sales trend for", input$fuel))
    })
    
    output$plot_facet <- renderPlotly({
        
        dat <- fuel
        # Generate Plot ----------------------------------------------
        ggplot(data = dat, aes(x = Data.Year, color = Fuel.Type))+
            geom_line(aes(y = vehicles_population, group =1))+
            theme_bw()+
            #theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
            xlab("City in CA") + ylab("Numbers of charging stations")+facet_wrap(.~Fuel.Type, scales ="free")+theme(axis.text.x=element_blank())
    })
    
    # Data table of characters ----------------------------------------------
    output$table_evc <- DT::renderDataTable({
        ev
    })
   
    # Mass mean info box ----------------------------------------------
    output$EVcars <- renderInfoBox({
        sw <- evInput()
        num <- round(sum(sw[,input$chargeLevel], na.rm = T), 2)
        
        infoBox("Chargers", value = num, subtitle = paste("Charger Type:", input$chargeLevel), icon = icon("battery-half"), color = "purple")
    })
    output$Totalcars <- renderInfoBox({
        sw <- salesInput()
        num <- sum(sw$total_sales , na.rm = T)
        
        infoBox("EV vehicles", value = num, subtitle = paste("County:", input$county), icon = icon("car"), color = "purple")
    })
    
    # Height mean value box ----------------------------------------------
    output$Totalchargers<- renderValueBox({
        sw <- fuelInput()
        num <- sum(sw$vehicles_population, na.rm = T)
        
        valueBox(subtitle = paste("Vehicles by Fuel Type in CA:", input$fuel), value = num, icon = icon("truck"))
    })
    
    # Write sampled data as csv ---------------------------------------
    observeEvent(eventExpr = input$write_csv, 
                 handlerExpr = {
                     filename <- paste0("Chargercountries", str_replace_all(Sys.time(), ":|\ ", "_"), ".csv")
                     write.csv(movies_sample(), file = filename, row.names = FALSE) 
                 }
    )
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)