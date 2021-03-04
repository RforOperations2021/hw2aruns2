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
        menuItem("Total Vehicle population", icon = icon("truck"), tabName = "sale"),
        br(),
        selectInput("county",
                    "Select County:",
                    choices = counties,
                    multiple = FALSE,
                    #selectize = TRUE,
                    selected = "Los Angeles"),
        
        menuItem("Electric Vehicle Sales", icon = icon("car"), tabName = "newvehicle"),
        
        
       
        
        
        
        menuItem("Electric Vehicle Chargers", icon = icon("battery-half"), tabName = "EVchargers"),
        
        sliderInput("topSelect",
                    "How many top cities-number of chargers?",
                    min = 5,
                    max = 25,
                    value = 5,
                    step = 1), 
        # Inputs: select variables to plot ----------------------------------------------
        selectInput("chargeLevel",
                    "Charger Level Trend:",
                    choices = c("Level_one", "Level_two", "DC_Fast", "total"),
                    multiple = FALSE,
                    #selectize = TRUE,
                    selected = "total"),
        
        menuItem("TableEVC", icon = icon("table"), tabName = "tableevc"), #badgeLabel = "new", badgeColor = "green"),
        
        numericInput(inputId = "n_samp", 
                     label = "Sample size:", 
                     min = 1, max = nrow(movies), 
                     value = 50),
        # Write sampled data as csv ------------------------------------------
        actionButton(inputId = "write_csv", 
                     label = "Write CSV"),
       
        
        #INputs: county selected
        
        
    
    #input: Fuel type
        selectInput("fuel",
                    "Select Fuel Type:",
                    choices = fuel_type,
        multiple = FALSE,
        #selectize = TRUE,
        selected = "Diesel")
        # top x Selection ----------------------------------------------
        
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
                                      tabBox(title = "Infrastructre Charger Status Across California",
                                             width = 12,
                                             tabPanel("A1", plotlyOutput("plot_charger")),
                                             tabPanel("A2", plotlyOutput("plot_char")))
                                  )
                          ),
                          uiOutput(outputId = "n"),
                          # Data Table Page ----------------------------------------------
                          tabItem("tableevc",
                                  fluidPage(
                                      box(title = "Selected Character Stats", DT::dataTableOutput("table_evc"), width = 12))
                          ),
                          tabItem("newvehicle",


                                  # Input and Value Boxes ----------------------------------------------
                                  


                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Plot",
                                             width = 12,
                                             tabPanel("A1", plotlyOutput("plot_fuel")),
                                             tabPanel("A2", plotlyOutput("plot_facet")))
                                  )
                          ),
                          tabItem("sale",



                                  # Input and Value Boxes ----------------------------------------------
                                  # fluidRow(
                                  #     infoBoxOutput("Avg"),
                                  #     valueBoxOutput("maxcity")
                                  # ),


                                  # Plot ----------------------------------------------
                                  fluidRow(
                                      tabBox(title = "Plot",
                                             width = 12,
                                             tabPanel("sales", plotlyOutput("sales_county")),
                                             #tabPanel("A2", plotlyOutput("plot_char"))),
                                      tabPanel("Bar Chart", plotlyOutput("pie")))
                                  )
                           )
                          
                      )
)

ui <- dashboardPage( header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    # Update the maximum allowed n_samp for selected type movies ------
    observe({
        updateNumericInput(session, 
                           inputId = "n_samp",
                           value = min(50, nrow(ev)),
                           max = nrow(ev)
        )
    })
    
    # Create new df that is n_samp obs from selected type movies ------
    movies_sample <- reactive({ 
        req(input$n_samp) # ensure availablity of value before proceeding
        sample_n(movies_subset(), input$n_samp)
    })
    
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
        xlab("City in CA") + ylab("Numbers of charging stations")
})
    output$pie <- renderPlotly({
        type.sales <- ev.sales %>% group_by(Fuel.Type) %>% summarise( total_sales = sum(Number.of.Vehicles, na.rm = TRUE))
        ggplot(type.sales, aes(x="Fuel.Type", y=total_sales, fill=Fuel.Type)) +
            geom_bar(stat="identity", position = "dodge", color = "white")
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
            xlab("City in CA") + ylab("Numbers of charging stations")
    })
    
    output$plot_fuel <- renderPlotly({
        
        dat <- fuelInput()
        # Generate Plot ----------------------------------------------
        ggplot(data = dat, aes(x = Data.Year, color = Fuel.Type))+
            geom_line(aes(y = vehicles_population, group =1))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 60, hjust =1, vjust =1))+
            xlab("City in CA") + ylab("Numbers of charging stations")
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
        ev[1:input$n_samp]
    })
   
    # Mass mean info box ----------------------------------------------
    output$EVcars <- renderInfoBox({
        sw <- ev
        num <- round(sum(ev$total, na.rm = T), 2)
        
        infoBox("Chargers", value = num, subtitle = paste(3, "charger type"), icon = icon("battery-half"), color = "purple")
    })
    output$Totalcars <- renderInfoBox({
        #sw <- county_wise_sales
        num <- sum(county_wise_sales$total_sales , na.rm = T)
        
        infoBox("EV vehicles", value = num, subtitle = paste(nrow(county_wise_sales), "County"), icon = icon("car"), color = "purple")
    })
    
    # Height mean value box ----------------------------------------------
    output$Totalchargers<- renderValueBox({
        #sw <- ev
        num <- sum(fuel$vehicles_population, na.rm = T)
        
        valueBox(subtitle = "Total vehicles All", value = num, icon = icon("truck"))
    })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)