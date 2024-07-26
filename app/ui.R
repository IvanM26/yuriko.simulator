shiny::tagList(
  shiny::tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  bslib::page_navbar(
    title = "Yuriko Simulator",
    bslib::nav_panel(
      title = "Your Decklist",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 400,
          shiny::fileInput(
            inputId = "upload_decklist",
            label = "Upload Decklist"
          )
        ),
        bslib::card(
          bslib::card_header("Decklist"),
          reactable::reactableOutput(outputId = "table_decklist")
        )
      )
    ),
    bslib::nav_panel(
      title = "Simulate",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 400,
          shiny::numericInput(
            inputId = "n_sim",
            label = "Number of Hands to Draw",
            min = 1000,
            max = 100000,
            value = 1000
          ),
          shiny::selectizeInput(
            inputId = "fixed_cards",
            label = bslib::tooltip(
              trigger = list(
                "Select Fixed Card(s) for All Hands",
                bsicons::bs_icon("info-circle")
              ),
              "Selected card(s) will be present in all simulated hands drawn"
            ),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              maxItems = 7
            )
          ) |> 
            shiny::tagAppendAttributes(class = "fixed-cards"),
          shiny::actionButton(
            inputId = "simulate_from_decklist",
            label = "Run Simulation"
          )
        ),
        bslib::card(
          bslib::card_header("Simulation Results"),
          shiny::uiOutput("simulation_summary_from_decklist")
        )
      )
    ),
    bslib::nav_panel(
      title = "Test Hand",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 400,
          shiny::actionButton(
            inputId = "random_hand",
            label = "Draw Random Hand"
          ),
          "Cards in Hand:",
          shiny::selectInput(
            inputId = "card1",
            label = NULL,
            choices = NULL
          ),
          shiny::selectInput(
            inputId = "card2",
            label = NULL,
            choices = NULL
          ),
          shiny::selectInput(
            inputId = "card3",
            label = NULL,
            choices = NULL
          ),
          shiny::selectInput(
            inputId = "card4",
            label = NULL,
            choices = NULL
          ),
          shiny::selectInput(
            inputId = "card5",
            label = NULL,
            choices = NULL
          ),
          shiny::selectInput(
            inputId = "card6",
            label = NULL,
            choices = NULL
          ),
          shiny::selectInput(
            inputId = "card7",
            label = NULL,
            choices = NULL
          )
        ),
        bslib::card(
          bslib::card_header("Your Hand"),
          bslib::layout_columns(
            col_widths = c(
              -2, 2, 2, 2, 2, -2, # 4 cards in row one
              -3, 2, 2, 2, -3   # 3 cards in row two
            ),
            shiny::uiOutput("image1", inline = TRUE),
            shiny::uiOutput("image2", inline = TRUE),
            shiny::uiOutput("image3", inline = TRUE),
            shiny::uiOutput("image4", inline = TRUE),
            shiny::uiOutput("image5", inline = TRUE),
            shiny::uiOutput("image6", inline = TRUE),
            shiny::uiOutput("image7", inline = TRUE)
          )
        ),
        shiny::uiOutput("test_hand_output")
      )
    ),
    bslib::nav_panel(
      title = "About"
    )
  )
)
