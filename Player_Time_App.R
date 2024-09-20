##--This App is for a Player Time Calculator--##

library(shiny)
library(shinydashboard)
library(shinyjs)
library(bslib)
library(DT)


# mandatory fields
fieldsMandatory <- c("n_players_team",
                     "n_players_field",
                     "n_game_length")


# add an asterisk to a required input label
labelMandatory <- function(label){
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


# CSS to use in the app for mandatory star
appCSS <- ".mandatory_star { color:#2d9ced; }"


##--User Interface--###################
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  
  # App Title
  titlePanel( 
    HTML(paste0(
      h1("PLAYER TIME CALCULATOR", align = "center"),
      h6("Complete the fields to calculate how long each player should play in the game", align = "center"),
      "<br>"
    ))
  ),
  
  # Apply theme
  theme = bs_theme(
    version = 5,
    bootswatch = "materia"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      class = "sidebar",
      style = "height: 90vh; overflow-y: auto;",
      card(
        numericInput(
          "n_players_team",
          labelMandatory(HTML(paste0("<b>", "Number of Players on Team", "</b>"))), 
          value = 6, 
          min   = 0,
          max   = 100, 
        )
      ),
      
      card(
        sliderInput(
          "n_players_field",
          labelMandatory(HTML(paste0("<b>", "Number of Players on Field/Court", "</b>"))), 
          value = 4,
          min   = 0, 
          max   = 50, 
          step  = 1 
        )
      ),
      
      card(
        numericInput(
          "n_game_length",
          labelMandatory(HTML(paste0("<b>", "Length of Game (Minutes)", "</b>"))), 
          value = 40, 
          min   = 0,
          max   = 300, 
        )
      ),
      
      card(
        card_header(HTML(paste0("<b>", "Enter Each Player's Name (Optional)", "</b>"))),
        div(textInput("player_names", "Add Player names with a comma between each"), style="font-size:80%"),
        actionButton("add_player_names", "Add Player Names")
      )
    ),
    
    mainPanel(
      card(
        card_header(h5("Total Player Time")),
        htmlOutput("player_time"),
      ),
      
      card(
        card_header(h5("Player Substitution Time")),
        htmlOutput("sub_time"),

      ),
      card(
        card_header(h5("Substitution Schedule")),
        DTOutput("player_table")
      ),
      textOutput("text")
    )
  )
)



##--Server Logic--#####################
server <- function(input, output) {
  
  # calculate total player time
  player_time_value <- reactive({
    round(((input$n_game_length / input$n_players_team) * input$n_players_field), 2)
  })
  
  # calculate substitution time
  sub_time_value <- reactive({
    round((input$n_game_length / input$n_players_team), 2)
  })
  
  # put together column names based on Number of Players on the field
  column_names <- reactive({
    c(paste("Playing Period ", seq_len(input$n_players_field)))
  })
  
  # put together player list after button is pressed
  player_list <- eventReactive(input$add_player_names, {
    unlist(strsplit(input$player_names, ","))
  })

  
  # output Total Player Time
  output$player_time <- renderUI({
    HTML(paste0("Each Player Plays a Total of ", "<font color=\"#2d9ced\">", player_time_value(), " Minutes", "</font color>"))
  })
  
  # output Substitution Time
  output$sub_time <- renderUI({
    HTML(paste0("Substitute Each Player every ", "<font color=\"#2d9ced\">", sub_time_value(), " Minutes", "</font color>"))
  })
  
  # output Substitution Schedule
  output$player_table <- renderDataTable({
    data.frame(matrix(
      data = if(input$add_player_names == 0) {
        rep(paste0("player_", sequence(input$n_players_team)), times = input$n_players_field)
      } else {
        rep(player_list(), times = input$n_players_field)
      },
      #data = rep(player_list(), times = input$n_players_field),
      #data  = rep(paste0("player_", sequence(input$n_players_team)), times = input$n_players_field),
      ncol  = input$n_players_field,
      nrow  = input$n_players_team,
      byrow = T
    )) %>% 
      setNames(column_names())
  },
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(
    dom = 't', 
    columnDefs = list(list(className = "dt-center", targets = "_all"))
  ))
}

# Run the application 
app <- shinyApp(ui = ui, server = server)
runApp(app)
#shinyApp(ui = ui, server = server)
