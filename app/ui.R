bslib::page_navbar(
  title = "Mulligan Simulator",
  bslib::nav_panel(
    title = "Your Decklist",
    waiter::useWaiter(),
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
    title = "Simulate"
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
          shiny::uiOutput("image1", inline = TRUE),
          shiny::uiOutput("image2", inline = TRUE),
          shiny::uiOutput("image3", inline = TRUE),
          shiny::uiOutput("image4", inline = TRUE),
          shiny::uiOutput("image5", inline = TRUE),
          shiny::uiOutput("image6", inline = TRUE),
          shiny::uiOutput("image7", inline = TRUE)
        ),
        shiny::uiOutput("test_hand_output")
      )
    )
  ),
  bslib::nav_panel(
    title = "About"
  )
)
