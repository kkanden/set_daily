ui <- dashboardPage(
  # skin = "blue",
  header = shinydashboard::dashboardHeader(title = "Set Tracking"),
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Daily results", 
                               tabName = "daily_results", 
                               icon = icon("calendar-days"))
    )
  ),
  body = shinydashboard::dashboardBody(
    
    ### MODALS ----
    # add record
    shinyBS::bsModal(
      id = "add_record_modal",
      title = "Add Record",
      trigger = "add_record_modal_bttn",
      size = "m",
      
      shiny::dateInput(
        inputId = "add_record_date",
        label = "Select date",
        weekstart = 1,
        min = "2024-05-27",
        max = lubridate::today()
      ),
      
      shinyWidgets::pickerInput(
        inputId = "add_record_player",
        label = "Select player",
        choices = sort(players[, name]),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          title = "Nothing selected",
          maxOptions = 1
        )
      ),
      
      shiny::textInput(
        inputId = "add_record_time",
        label = "Input time ([min:sec(.ms)])",
      ),
      
      shinyWidgets::actionBttn(
        inputId = "add_record",
        label = "Add record",
        icon = icon("plus"),
        style = "simple",
        color = "success"
      )
    ),
    
    # remove record
    shinyBS::bsModal(
      id = "remove_record_modal",
      title = "Remove Record",
      trigger = "remove_record_modal_bttn",
      size = "m",
      
      shiny::dateInput(
        inputId = "remove_record_date",
        label = "Select date",
        weekstart = 1,
        min = "2024-05-27",
        max = lubridate::today()
      ),
      
      shinyWidgets::pickerInput(
        inputId = "remove_record_player",
        label = "Select player",
        choices = sort(players[, name]),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          title = "Nothing selected",
          maxOptions = 1
        )
      ),
      
      
      shinyWidgets::actionBttn(
        inputId = "remove_record",
        label = "Remove record",
        icon = icon("minus"),
        style = "simple",
        color = "danger"
      )
    ),
    
    tabItems(
      tabItem(
        tabName = "daily_results",
        fluidRow(
          shinydashboard::box(
            width = 3,
            shinyWidgets::actionBttn(
              inputId = "add_record_modal_bttn",
              label = "Add Record",
              icon = icon("plus"),
              style = "simple",
              color = "primary",
              size = "s"
            ),
            
            shinyWidgets::actionBttn(
              inputId = "remove_record_modal_bttn",
              label = "Remove Record",
              icon = icon("minus"),
              style = "simple",
              color = "danger",
              size = "s"
            ),
          )
        ),
        fluidRow(
          shinydashboardPlus::box(
            title = "Daily Results",
            width = 12,
            height = 6,
            status = "primary",
            solidHeader = TRUE,
            dataTableOutput(
              outputId = "daily_results_table"
            )
          )
        )
      )
    )
  )
)
