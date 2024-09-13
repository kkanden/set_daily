server <- function(input, output) {
  rv <- shiny::reactiveValues(
    completion_times = completion_times,
    players = players
  )
  
  daily_results <- reactive({
    daily_results <- players[completion_times, on = c(id = "player_id")][
      , ":="(id = NULL)
    ]
    
    data.table::setnames(daily_results, "name", "player")
    
    daily_results
  })
  
  
  ### DAILY RESULTS ----

  output$daily_results_table <- DT::renderDT(
    expr = {
      dat <- daily_results() %>% 
        mutate(time_sec = sapply(time_sec, seconds_to_string))

      dat <- data.table::dcast(dat,
        date ~ player,
        value.var = "time_sec"
      )

      data.table::setnames(dat, "date", "Date")

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
  
  ### STATS ----
  
  #### running avg 7 day  ----
  
  output$stats_runavg_7day <- plotly::renderPlotly(
    expr = {
      
      dat <- data.table::dcast(daily_results(),
                               date ~ player,
                               value.var = "time_sec"
      )
      
      
      dat <- dat[order(date)][, ':='(cummean_hubert = frollmean(Hubert, n = 7, na.rm = TRUE),
                 cummean_jula = frollmean(Jula, n = 7, na.rm = TRUE),
                 cummean_oliwka = frollmean(Oliwka, n = 7, na.rm = TRUE))][
                   , -c("Hubert", "Jula", "Oliwka")
                 ] %>% 
        rename("Hubert" = "cummean_hubert", "Jula" = "cummean_jula", "Oliwka" = "cummean_oliwka") %>% 
        pivot_longer(
          cols = c("Hubert", "Jula", "Oliwka"),
        )
        
      
      plotly::plot_ly(
        data = dat,
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines",
        color = ~name,
        text = ~sapply(value, seconds_to_string),
        hoverinfo = 'text',
        hovertemplate = "%{text}"
      ) %>% 
        layout(
          title = "Rolling average (7 day)",
          hovermode = 'x unified',
          xaxis = list(
            title = "Date"
          ),
          yaxis = list(
            title = "Time",
            # type = 'date',
            tickvals = seq(0, 300, 30),
            ticktext = sapply(seq(0, 300, 30), seconds_to_string),
            tickformat = "%M:%S"
          )
        )
    }
  )
  
  #### running avg 30 day  ----
  
  output$stats_runavg_30day <- plotly::renderPlotly(
    expr = {
      
      dat <- data.table::dcast(daily_results(),
                               date ~ player,
                               value.var = "time_sec"
      )
      
      
      dat <- dat[order(date)][, ':='(cummean_hubert = frollmean(Hubert, n = 30, na.rm = TRUE),
                                     cummean_jula = frollmean(Jula, n = 30, na.rm = TRUE),
                                     cummean_oliwka = frollmean(Oliwka, n = 30, na.rm = TRUE))][
                                       , -c("Hubert", "Jula", "Oliwka")
                                     ] %>% 
        rename("Hubert" = "cummean_hubert", "Jula" = "cummean_jula", "Oliwka" = "cummean_oliwka") %>% 
        pivot_longer(
          cols = c("Hubert", "Jula", "Oliwka"),
        )
      
      
      plotly::plot_ly(
        data = dat,
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines",
        color = ~name,
        text = ~sapply(value, seconds_to_string),
        hoverinfo = 'text',
        hovertemplate = "%{text}"
      ) %>% 
        layout(
          title = "Rolling average (30 day)",
          hovermode = 'x unified',
          xaxis = list(
            title = "Date"
          ),
          yaxis = list(
            title = "Time",
            # type = 'date',
            tickvals = seq(0, 300, 30),
            ticktext = sapply(seq(0, 300, 30), seconds_to_string, ms = FALSE),
            tickformat = "%M:%S"
          )
        )
    }
  )
  
  #### histogram  ----
  
  output$stats_histogram <- plotly::renderPlotly(
    expr = {
      dat <- daily_results()
      
      plotly::plot_ly(
        dat,
        x = ~time_sec,
        type = "histogram",
        histnorm = 'probability',
        color = ~player,
        nbinsx = 30,
        hoverinfo = 'skip'
      ) %>% 
        layout(
          # barmode = 'overlay',
          title = "Completion Time Histogram",
          xaxis = list(
            title = "Time",
            range = c(0, 300),
            tickvals = seq(0, 300, 30),
            ticktext = sapply(seq(0, 300, 30), seconds_to_string, ms = FALSE)
          ),
          yaxis = list(
            title = "Prob"
          )
        )
    }
  )
  
  #### weekday bar  ----
  
  output$stats_weekdaybar <- plotly::renderPlotly(
    expr = {
      dat <- daily_results()
      
      dat <- dat[, .(mean_time = mean(time_sec)), by = .(day_of_week = weekdays(date), player)] 
      
      plotly::plot_ly(
        data = dat,
        x = ~day_of_week,
        y = ~mean_time,
        type = "bar",
        color = ~player,
        text = ~sapply(mean_time, seconds_to_string),
        hoverinfo = 'text',
        hovertemplate = "%{text}"
      ) %>% layout(
        title = "Mean time by day of week",
        xaxis = list(
          title = "Day of Week",
          tickvals = c("Monday", "Tuesday", "Wednesday", "Thursday",
                       "Friday", "Saturday", "Sunday")
        ),
        yaxis = list(
          title = "Time",
          tickvals = seq(0, 300, 30),
          ticktext = sapply(seq(0, 300, 30), seconds_to_string, ms = FALSE)
        )
      )
    }
  )
  
  
  
}
