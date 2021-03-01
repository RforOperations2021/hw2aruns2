#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

# Load and clean data ----------------------------------------------
ev.sales <- read.csv("new ZEV sales.csv")
ev.population <- read.csv("vehicle_population.csv")
ev.chargers <- read.csv("evchargers.csv")

# Avoid plotly issues ----------------------------------------------
pdf(NULL)


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Dashboard: Forecasting for Electric Vehicles in California",
                          
                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "New Plot Added", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "Midichlorians")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                           from = "Arun",
                                           message = HTML("Get the EV swag on! <br> The World's running out of time."),
                                           icon = icon("exclamation-circle"))
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        # Menu Items ----------------------------------------------
        menuItem("Infrastructure", icon = icon("battery-half"), tabName = "Infra-Charging"),
        menuItem("evsales", icon = icon("car-side"), tabName = "New EV sales", badgeLabel = "new", badgeColor = "green"),
        menuItem("carsales", icon = icon("truck-monster"), tabName = "Vehicle Population", badgeLabel = "new", badgeColor = "green"),
        
       
)