ui <- bslib::page_navbar(
  theme = bs_theme(preset = "shiny"),
  title = HTML('<img src="set_logo.png" height=40> Tracking'),
  fillable = FALSE,

  # BODY ----

  header = {
    # Custom CSS for sticky header
    tags$head(
      tags$style(HTML("
      .bslib-page-navbar .navbar {
        position: sticky;
        top: 0;
        z-index: 1000;
        background-color: white; /* Change this to match your navbar color */
      }
    "))
    )
    shinytitle::use_shiny_title()
  },


  ### STATS ----
  bslib::nav_panel(
    title = "Statistics",
    class = "bslib-page-dashboard",
    icon = bsicons::bs_icon("graph-up-arrow"),
    bslib::layout_column_wrap(
      width = 1 / 2,
      heights_equal = "row",
      bslib::layout_columns(
        col_widths = 12,
        bslib::card(
          bslib::card_header(textOutput("topn_header")),
          bslib::card_body(
            shinyWidgets::pickerInput(
              inputId = "topn_picker",
              label = "Show Top",
              choices = seq(10, 50, 10),
              selected = 10,
              width = "fit",
              inline = TRUE
            ),
            DT::DTOutput(outputId = "stats_top10")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Rolling Average (7 day)"),
          bslib::card_body(
            plotly::plotlyOutput(outputId = "stats_runavg_7day")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Completion Time Histogram"),
          bslib::card_body(
            plotly::plotlyOutput(outputId = "stats_histogram")
          )
        )
      ),
      bslib::layout_columns(
        col_widths = 12,
        bslib::card(
          fill = FALSE,
          bslib::card_header("Best, Mean, Median Time"),
          bslib::card_body(
            DT::DTOutput(outputId = "stats_besttimeplayer")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Cumulative Average (all time)"),
          bslib::card_body(
            plotly::plotlyOutput(outputId = "stats_runavg_alltime")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Rolling Average (30 day)"),
          bslib::card_body(
            plotly::plotlyOutput(outputId = "stats_runavg_30day")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Completion Time by Weekday"),
          bslib::card_body(
            plotly::plotlyOutput(outputId = "stats_weekdaybar")
          )
        )
      )
    )
  ),

  ### DAILY RESULTS ----
  bslib::nav_panel(
    title = "Daily results",
    class = "bslib-page-dashboard",
    icon = bsicons::bs_icon("calendar3"),
    bslib::card(
      height = 900,
      bslib::card_header("Daily Results"),
      shiny::div(
        style = "display: flex; gap: 10px",
        shinyWidgets::actionBttn(
          inputId = "add_record_modal_bttn",
          label = "Add Record",
          icon = icon("plus"),
          style = "simple",
          color = "primary",
          size = "s",
        ),
        shinyWidgets::actionBttn(
          inputId = "remove_record_modal_bttn",
          label = "Remove Record",
          icon = icon("minus"),
          style = "simple",
          color = "danger",
          size = "s",
        ),
      ),
      DT::DTOutput(
        outputId = "daily_results_table"
      )
    )
  ),
  bslib::nav_spacer(),
  bslib::nav_item(
    bslib::tooltip(
      tags$a(
        tags$span(
          bsicons::bs_icon("github")
        ),
        href = "https://github.com/kkanden/set_daily",
        target = "_blank"
      ),
      "Source code",
    ),
  ),
  bslib::nav_item(
    bslib::input_dark_mode(
      mode = ifelse(data.table::hour(
        data.table::as.ITime(
          Sys.time(),
          tz = "Europe/Warsaw"
        )
      ) %in% c(18:23, 0:5),
      "dark",
      "light"
      )
    ),
  )
)
