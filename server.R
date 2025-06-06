source("string_seconds.R")

server <- function(input, output, session) {
  shinytitle::change_window_title(
    title = "SET Tracking"
  )

  shinytitle::busy_window_title(
    title = "SET Tracking"
  )

  ### REACTIVE VALUES ----

  rv <- shiny::reactiveValues(
    completion_times = completion_times,
    players = players
  )

  daily_results <- reactive({
    daily_results <- rv$players[
      rv$completion_times,
      on = c(player_id = "player_id")
    ]
    data.table::setnames(daily_results, "name", "player")
    daily_results
  })

  daily_results_30days <- reactive({
    req(daily_results)
    last_dates <- daily_results()[, .(last_date = max(date)), by = .(player)]
    daily_results_last_date <- daily_results()[last_dates, on = "player"]
    # filter out players who haven't played for 30 days
    dt <- daily_results_last_date[last_date >= Sys.Date() - 30]
    if (nrow(dt) == 0) {
      return(daily_results())
    }
    dt
  })

  output$topn_header <- renderText({
    paste0("Top ", input$topn_picker, " Best Times")
  })

  ### MODALS ----
  # add record
  shiny::observeEvent(input$add_record_modal_bttn, {
    shiny::showModal(
      shiny::modalDialog(
        footer = modalButton("Close"),
        tags$head(
          tags$script(
            HTML(
              '
              $(document).keyup(function(event) {
              if (event.key == "Enter" && $("#add_record_modal").is(":visible")) {
              $("#add_record").click();
              }
              });
              '
            )
          )
        ),
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
          label = "Input time [min:sec(.msec)]",
        ),
        shiny::br(),
        shiny::div(
          bslib::input_task_button(
            id = "add_record",
            label = "Add record",
            icon = icon("plus"),
            type = "success"
          ),
          style = "float: right"
        )
      )
    )
  })

  # remove record
  shiny::observeEvent(input$remove_record_modal_bttn, {
    shiny::showModal(
      shiny::modalDialog(
        footer = modalButton("Close"),
        tags$head(
          tags$script(
            HTML(
              '
              $(document).keyup(function(event) {
              if (event.key == "Enter" && $("#remove_record_modal").is(":visible")) {
              $("#remove_record").click();
              }
              });
              '
            )
          )
        ),
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
        shiny::br(),
        shiny::div(
          bslib::input_task_button(
            id = "remove_record",
            label = "Remove record",
            icon = icon("minus"),
            type = "danger"
          ),
          style = "float: right"
        )
      )
    )
  })

  ### DAILY RESULTS ----

  #### table ----

  output$daily_results_table <- DT::renderDT({
    all_dates <- seq(
      min(daily_results_30days()[, date]),
      Sys.Date(),
      by = "days"
    )
    unique_players <- sort(unique(daily_results_30days()[, player]))

    dat <- daily_results_30days()[order(date)]

    full_dates <- data.table::data.table(
      player = rep(unique_players, each = length(all_dates)),
      date = rep(all_dates, length(unique_players))
    )

    dat <- dat[full_dates, on = c("player", "date")][,
      ":="(time_sec = seconds_to_string(time_sec))
    ]

    dat <- data.table::dcast(dat, date ~ player, value.var = "time_sec")

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
  })

  #### add record ----

  shiny::observeEvent(input$add_record, {
    date_input <- input$add_record_date
    player_input <- input$add_record_player
    time_input <- as.character(input$add_record_time)

    # check if player provided
    if (is.null(player_input) || player_input == "") {
      shinyalert::shinyalert(
        text = "Player not provided",
        type = "error",
        timer = 5000
      )
      return()
    }

    existing_record <- daily_results()[
      date == date_input & player == player_input
    ]
    # check if player already has record with that date
    if (nrow(existing_record) > 0) {
      shinyalert::shinyalert(
        text = sprintf(
          "%s already has a record on %s",
          player_input,
          date_input
        ),
        type = "error",
        timer = 5000
      )
      return()
    }

    # check if time is of correct format
    if (
      !stringi::stri_detect(
        time_input,
        regex = "^[0-9]+:(?:[0-5][0-9]|[0-9])(?:\\.[0-9]{1,3})?$"
      ) ||
        time_input == ""
    ) {
      shinyalert::shinyalert(
        text = "Time not provided or time is not in the correct format.
        Examples of correct format:\n
        1:23.927\n
        1:23\n
        0:34.9\n
        0:34.93\n
        1:00",
        type = "error"
      )
      return()
    }

    time_sec <- string_to_seconds(time_input)
    player_id <- rv$players[name == player_input, player_id]

    query <- glue::glue(
      'INSERT INTO completion_times("date", "time_sec", "player_id")
      VALUES (\'{date_input}\', {time_sec}, {player_id})'
    )

    con <- connect()

    sent <- DBI::dbSendQuery(con, query)
    DBI::dbClearResult(sent)

    rv$completion_times <- DBI::dbGetQuery(
      con,
      "SELECT * FROM completion_times"
    ) |>
      data.table::as.data.table()

    disconnect(con)

    shiny::removeModal()

    shinyalert::shinyalert(
      text = "Successfully added record",
      type = "success",
      timer = 2000
    )
  })

  #### remove record ----

  shiny::observeEvent(input$remove_record, {
    date_input <- input$remove_record_date
    player_input <- input$remove_record_player

    # check if player provided
    if (is.null(player_input)) {
      shinyalert::shinyalert(
        text = "Player not provided",
        type = "error",
        timer = 5000
      )
      return()
    }
    player_id_input <- rv$players[name == player_input, player_id]

    query <- glue::glue(
      "DELETE FROM completion_times WHERE date = '{date_input}'
      AND player_id = {player_id_input}"
    )

    con <- connect()

    sent <- DBI::dbSendQuery(con, query)
    DBI::dbClearResult(sent)

    rv$completion_times <- DBI::dbGetQuery(
      con,
      "SELECT * FROM completion_times"
    ) |>
      data.table::as.data.table()

    disconnect(con)

    shiny::removeModal()

    shinyalert::shinyalert(
      text = "Successfully removed record",
      type = "success",
      timer = 2000
    )
  })

  ### STATS ----

  #### top10 ----

  output$stats_top10 <- DT::renderDT({
    dat <- daily_results()

    dat <- dat[order(time_sec)][
      1:input$topn_picker,
      .(
        Player = player,
        Time = seconds_to_string(time_sec),
        Date = date
      )
    ]

    DT::datatable(
      data = dat,
      rownames = TRUE,
      extensions = c("FixedHeader"),
      options = list(
        paging = FALSE,
        dom = "t",
        scrollCollapse = TRUE,
        scrollX = 200,
        FixedHeader = TRUE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        )
      )
    ) |>
      DT::formatStyle(
        columns = 0:ncol(dat),
        target = "cell",
        color = JS("\"unset\""),
        backgroundColor = JS("\"unset\"")
      ) |>
      DT::formatStyle(
        0,
        target = "row",
        backgroundColor = DT::styleEqual(
          c(1, 2, 3),
          c("#FFD700", "#C0C0C0", "#CD7F32")
        )
      ) |>
      DT::formatStyle(
        columns = colnames(dat),
        fontWeight = "bold"
      )
  })

  #### best, mean time by player ----

  output$stats_besttimeplayer <- DT::renderDT({
    dat <- daily_results_30days()[,
      .(
        `Best Time` = seconds_to_string(min(time_sec)),
        `Mean Time` = seconds_to_string(mean(time_sec)),
        `Median Time` = seconds_to_string(median(time_sec)),
        `Standard Deviation` = seconds_to_string(sd(time_sec)),
        `Daily Sets Completed` = .N
      ),
      by = .(Player = player)
    ][order(Player)]

    DT::datatable(
      data = dat,
      rownames = NULL,
      extensions = c("FixedHeader"),
      options = list(
        paging = FALSE,
        dom = "t",
        scrollY = "600px",
        scrollCollapse = TRUE,
        scrollX = 200,
        FixedHeader = TRUE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        )
      )
    ) |>
      DT::formatStyle(
        columns = colnames(dat),
        fontWeight = "bold"
      )
  })

  #### running avg all time ----

  output$stats_runavg_alltime <- plotly::renderPlotly({
    min_date_by_player <- daily_results_30days()[,
      .(
        min_date = min(date)
      ),
      by = .(player)
    ]

    full_dates_by_player <- min_date_by_player[,
      .(
        date = seq(min_date, Sys.Date(), by = "days")
      ),
      by = .(player)
    ]

    dat <- daily_results_30days()[order(date)][,
      .(
        date,
        cummean = cumsum(time_sec) / seq_len(.N)
      ),
      by = .(player)
    ]

    dat <- dat[full_dates_by_player, on = c("player", "date")][,
      ":="(cummean = data.table::nafill(cummean, type = "locf"))
    ]

    min_time <- min(
      dat[date %between% c(Sys.Date() - 90, Sys.Date()), cummean]
    ) -
      15
    max_time <- max(
      dat[date %between% c(Sys.Date() - 90, Sys.Date()), cummean]
    ) +
      15
    dat |>
      plotly::plot_ly(
        x = ~date,
        y = ~cummean,
        color = ~player,
        text = ~ seconds_to_string(cummean),
        type = "scatter",
        mode = "lines",
        hoverinfo = "text",
        hovertemplate = "%{text}"
      ) |>
      plotly::layout(
        title = NA,
        hovermode = "x unified",
        margin = list(
          t = 0
        ),
        xaxis = list(
          title = NA,
          range = c(Sys.Date() - 90, Sys.Date())
        ),
        yaxis = list(
          title = NA,
          range = c(min_time, max_time),
          tickvals = seq(0, max_time, 30),
          ticktext = seconds_to_string(seq(0, max_time, 30), ms = FALSE)
        ),
        legend = list(
          orientation = "h"
        )
      )
  })

  #### rolling avg 7 day  ----

  output$stats_runavg_7day <- plotly::renderPlotly({
    dat <- data.table::dcast(
      daily_results_30days(),
      date ~ player,
      value.var = "time_sec"
    )

    means <- frollmean(dat[order(date), -c("date")], n = 7, na.rm = T)

    names(means) <- colnames(dat[, -c("date")])

    means$date <- dat[order(date), date]

    data.table::setDT(means)

    means_long <- data.table::melt(
      means,
      id.vars = "date",
      variable.name = "name"
    )

    min_time <- min(
      means_long[date %between% c(Sys.Date() - 90, Sys.Date()), value],
      na.rm = TRUE
    ) -
      15
    max_time <- max(
      means_long[date %between% c(Sys.Date() - 90, Sys.Date()), value],
      na.rm = TRUE
    ) +
      15

    means_long |>
      plotly::plot_ly(
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines",
        color = ~name,
        text = ~ seconds_to_string(value),
        hoverinfo = "text",
        hovertemplate = "%{text}"
      ) |>
      plotly::layout(
        title = NA,
        hovermode = "x unified",
        margin = list(
          t = 0
        ),
        xaxis = list(
          title = NA,
          range = c(Sys.Date() - 90, Sys.Date())
        ),
        yaxis = list(
          title = NA,
          range = c(min_time, max_time),
          tickvals = seq(0, max_time, 30),
          ticktext = seconds_to_string(seq(0, max_time, 30), ms = FALSE)
        ),
        legend = list(
          orientation = "h"
        )
      )
  })

  #### rolling avg 30 day  ----

  output$stats_runavg_30day <- plotly::renderPlotly({
    dat <- data.table::dcast(
      daily_results_30days(),
      date ~ player,
      value.var = "time_sec"
    )

    means <- frollmean(dat[order(date), -c("date")], n = 30, na.rm = T)

    names(means) <- colnames(dat[, -c("date")])

    means$date <- dat[order(date), date]

    data.table::setDT(means)

    means_long <- data.table::melt(
      means,
      id.vars = "date",
      variable.name = "name"
    )

    min_time <- min(
      means_long[date %between% c(Sys.Date() - 90, Sys.Date()), value],
      na.rm = TRUE
    ) -
      15
    max_time <- max(
      means_long[date %between% c(Sys.Date() - 90, Sys.Date()), value],
      na.rm = TRUE
    ) +
      15

    means_long |>
      plotly::plot_ly(
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines",
        color = ~name,
        text = ~ seconds_to_string(value),
        hoverinfo = "text",
        hovertemplate = "%{text}"
      ) |>
      plotly::layout(
        title = NA,
        hovermode = "x unified",
        margin = list(
          t = 0
        ),
        xaxis = list(
          title = NA,
          range = c(Sys.Date() - 90, Sys.Date())
        ),
        yaxis = list(
          title = NA,
          range = c(min_time, max_time),
          tickvals = seq(0, max_time, 30),
          ticktext = seconds_to_string(seq(0, max_time, 30), ms = FALSE)
        ),
        legend = list(
          orientation = "h"
        )
      )
  })

  #### histogram  ----

  output$stats_histogram <- plotly::renderPlotly({
    dat <- daily_results_30days()

    formatted_ranges <- sapply(seq(0, 300, length.out = 30), function(x) {
      start <- seconds_to_string(x, ms = FALSE)
      end <- seconds_to_string(x + 20, ms = FALSE) # Adjust width based on bin size
      paste0("(", start, " - ", end, ")")
    })

    plotly::plot_ly(
      dat,
      x = ~time_sec,
      type = "histogram",
      histnorm = "probability",
      color = ~player,
      nbinsx = 30,
      hovertemplate = "(%{x} s) %{y:.2%}"
    ) |>
      plotly::layout(
        title = NA,
        margin = list(
          t = 0
        ),
        xaxis = list(
          title = NA,
          range = c(0, 300),
          tickvals = seq(20, 300, 20),
          ticktext = seconds_to_string(seq(20, 300, 20), ms = FALSE)
        ),
        yaxis = list(
          title = NA,
          tickformat = ".0%"
        ),
        legend = list(
          orientation = "h"
        )
      )
  })

  #### weekday bar  ----

  output$stats_weekdaybar <- plotly::renderPlotly({
    dat <- daily_results_30days()

    dat <- dat[,
      .(mean_time = mean(time_sec)),
      by = .(
        day_of_week = factor(
          weekdays(date),
          levels = c(
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday",
            "Sunday"
          )
        ),
        player
      )
    ]

    max_time <- max(dat[, mean_time], na.rm = TRUE) + 15

    plotly::plot_ly(
      data = dat,
      x = ~day_of_week,
      y = ~mean_time,
      type = "bar",
      color = ~player,
      text = ~ seconds_to_string(mean_time),
      hoverinfo = "text",
      hovertemplate = "%{text}"
    ) |>
      plotly::layout(
        title = NA,
        margin = list(
          t = 0
        ),
        xaxis = list(
          title = NA
        ),
        yaxis = list(
          title = NA,
          tickvals = seq(0, max_time, 30),
          ticktext = seconds_to_string(seq(0, max_time, 30), ms = FALSE)
        ),
        legend = list(
          orientation = "h"
        )
      )
  })
}
