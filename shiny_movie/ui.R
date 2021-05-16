## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
# install.packages("devtools")
# devtools::install_github("stefanwilhelm/ShinyRatingInput")
library(shinyjs)

source('functions/helpers.R')

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("By genre", tabName = "dashboard1", icon = icon("dashboard")),
        menuItem("By user ratings", tabName = "dashboard2", icon = icon("dashboard"))
    )
)

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard1",
                h2("Recommend by genre"),
                fluidPage(
                    selectInput("sel_genre", "Choose a genre:",
                                genre_list),
                    box(
                        width = 12, solidHeader = TRUE, status = "info", 
                        title = "Highly rated (top row) and most popular movies (bottom row)",
                        tableOutput("recom_movies")
                    )
                )
        ),
        
        tabItem(tabName = "dashboard2",
                h2("Recommend based on user ratings"),
                includeCSS("css/movies.css"),
                fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                ),
                fluidRow(
                    useShinyjs(),
                    box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                            actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results")
                    )
                )
        )
    )
)

# Put them together into a dashboardPage
dashboardPage(
    dashboardHeader(title = "Method"),
    sidebar,
    body
    
)