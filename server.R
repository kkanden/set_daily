server <- function(input, output) {
  rv <- reactiveValues(
    completion_times = completion_times,
    players = players
  )

  daily_results <- reactive({
    daily_results <- rv$players[rv$completion_times, on = c(id = "player_id")][
      , ":="(id = NULL)
    ]

    data.table::setnames(daily_results, "name", "player")

    daily_results
  })

  ### DAILY RESULTS ----

  output$daily_results_table <- renderDataTable(
    expr = {
      dat <- daily_results()[, ":="(time_sec = sapply(time_sec, seconds_to_string))]

      dat <- dcast(dat,
        date ~ player,
        value.var = "time_sec"
      )

      setnames(dat, "date", "Date")

      DT::datatable(
        data = dat[order(-Date)],
        rownames = NULL,
        extensions = c("FixedHeader"),
        options = list(
          dom = "fti",
          paging = FALSE,
          scrollY = "600px",
          scrollCollapse = TRUE,
          scrollX = 200,
          FixedHeader = TRUE
        )
      )
    }
  )

  #### add record ----

  shiny::observeEvent(input$add_record, {
    date_input <- input$add_record_date
    player_input <- input$add_record_player
    time_input <- as.character(input$add_record_time)

    # check if player provided
    if (is.null(player_input)) {
      shinyalert::shinyalert(
        text = "Player not provided",
        type = "warning",
        timer = 5000
      )
    } else {
      existing_record <- daily_results()[date == date_input & player == player_input]
      # check if player already has record with that date
      if (nrow(existing_record) > 0) {
        shinyalert::shinyalert(
          text = sprintf(
            "%s already has a record on %s",
            player_input, date_input
          ),
          type = "warning",
          timer = 5000
        )
      } else {
        # check if time is of correct format
        if (!stringi::stri_detect(time_input, regex = "^[0-9]+:[0-9]{1,2}(\\.[0-9]{1,3})?$") ||
          time_input == "") {
          shinyalert::shinyalert(
            text = "Time not provided or time is not in the correct format.
            Examples of correct format:\n
            1:23.927\n
            1:23\n
            0:34.9\n
            0:34.93\n
            1:00",
            type = "warning"
          )
        } else {
          time_sec <- string_to_seconds(time_input)
          player_id <- rv$players[name == player_input, id]

          query <- glue::glue('INSERT INTO completion_times("date", "time_sec", "player_id")
                             VALUES (\'{date_input}\', {time_sec}, {player_id})')

          con <- connect()

          sent <- DBI::dbSendQuery(con, query)
          DBI::dbClearResult(sent)

          rv$completion_times <- dbGetQuery(con, "SELECT * FROM completion_times") %>%
            as.data.table()

          disconnect(con)

          shinyalert::shinyalert(
            text = "Successfully added record",
            type = "success"
          )
        }
      }
    }
  })
  
  #### remove record ----
  
  shiny::observeEvent(input$remove_record, {
    date_input <- input$remove_record_date
    player_input <- input$remove_record_player
    
    # check if player provided
    if (is.null(player_input)) {
      shinyalert::shinyalert(
        text = "Player not provided",
        type = "warning",
        timer = 5000
      )
    } else {
      player_id <- rv$players[name == player_input, id]
      
      
      query <- glue::glue("DELETE FROM completion_times WHERE date = '{date_input}'
                          AND player_id = {player_id}")
          
      con <- connect()
      
      sent <- DBI::dbSendQuery(con, query)
      DBI::dbClearResult(sent)
      
      rv$completion_times <- dbGetQuery(con, "SELECT * FROM completion_times") %>%
        as.data.table()
      
      disconnect(con)
      
      shinyalert::shinyalert(
        text = "Successfully removed record",
        type = "success"
      )
    }
  })
}
