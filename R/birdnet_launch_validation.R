# ui ----------------------------------------------------------------------

birdnet_validation_ui <- function(request) {
  bslib::page_sidebar(

    # add title
    title = "birdnetTools / Audio Validation platform",

    # side bar setting
    sidebar_width = 350,

    # add side bar
    sidebar = bslib::sidebar(
      bslib::card(
        "File import",

        # input: import_file
        shiny::fileInput(
          inputId = "import_file",
          label = "Selection table",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          ),
          width = "100%"
        ),

        # button: dir
        shiny::p("Audio folder"),
        shinyFiles::shinyDirButton(
          id = "dir",
          label = "Select",
          title = "Select the species recording folder"
        ),

        # output: dir
        shiny::verbatimTextOutput(
          outputId = "dir",
          placeholder = TRUE
        )
      ), bslib::card(
        "Actions",

        # button: save_file
        shiny::downloadButton("save_file", "Save"),

        # button: praise_me
        shiny::actionButton("praise_me", "Praise me")
      ), bslib::card(
        "Spectrogram settings",

        # input: flim, wl
        shinyWidgets::numericRangeInput(
          inputId = "flim",
          "Frequency range:",
          min = 1, max = 10,
          value = c(0, 6)
        ),
        shiny::numericInput(
          inputId = "wl",
          "Window length:",
          value = 512
        )
      )
    ),

    # main content

    # output: main_table
    bslib::card(
      DT::DTOutput("main_table")
    ),

    # output: spectrogram
    bslib::card(
      shiny::plotOutput("spectrogram")
    )
  )
}





# server ------------------------------------------------------------------

birdnet_validation_server <- function(input, output, session) {
  # Helper functions --------------------------------------------------------

  # the function to play audio
  play_audio <- function(filepath, start, end) {
    if (Sys.info()["sysname"] == "Darwin") {
      tuneR::setWavPlayer("afplay")
    }
    song <- tuneR::readWave(filepath, from = start - 3, to = end + 3, units = "seconds")
    tuneR::play(song)
  }

  # the function to view spectrogram
  view_spectrogram <- function(filepath, flim, wl, start, end) {
    song <- tuneR::readWave(filepath, from = start - 3, to = end + 3, units = "seconds")
    seewave::spectro(song,
      f = song@samp.rate, flim = flim, ovlp = 50,
      collevels = seq(-40, 0, 1), wl = wl, scale = FALSE,
      main = stringr::str_extract(filepath, "[^/]+(?=\\.wav$)")
    )
  }


  # reactive value for audio file path ----------------------------------

  roots <- if (.Platform$OS.type == "windows") {
    # list all available drives on Windows
    drives <- system("wmic logicaldisk get name", intern = TRUE)
    drives <- gsub("\\s", "", drives[-1])

    # add drives with their names as keys
    c(
      Desktop = file.path(Sys.getenv("USERPROFILE"), "Desktop"),
      Documents = file.path(Sys.getenv("USERPROFILE"), "Documents"),
      Working_dir = ".",
      setNames(drives, drives)
    )
  } else {
    # for unix-like systems, include common mount points
    external_drives <- list.files("/Volumes", full.names = TRUE)

    # use volume names
    c(
      Desktop = "~/Desktop",
      Documents = "~/Documents",
      Working_dir = ".",
      setNames(external_drives, basename(external_drives))
    )
  }


  # provide options for the roots when searching for file
  shinyFiles::shinyDirChoose(
    input,
    id = "dir",
    roots = roots
  )

  dir_path <- shiny::reactive({
    shinyFiles::parseDirPath(roots, input$dir)
  })

  output$dir <- shiny::renderText({
    dir_path()
  })


  # reactive value for the dataframe ------------------------------------

  rv <- shiny::reactiveValues()

  # if importing file, create data_editable and data_display reactive values
  shiny::observeEvent(input$import_file, {
    # read in the csv file and clean names based on the path input
    tryCatch(
      {
        rv$data_editable <- input$import_file$datapath |>
          readr::read_csv(show_col_types = FALSE) |>
          birdnet_clean_names()

        # check if the data frame has the validation column already
        if (!"validation" %in% names(rv$data_editable)) {
          rv$data_editable <- rv$data_editable |>
            dplyr::mutate(validation = "U")
        }
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error reading CSV:", e$message),
          type = "error"
        )
      }
    )


    # render the data table with a play button
    rv$data_display <- rv$data_editable |>
      dplyr::mutate(filepath = filepath |> basename()) |>
      dplyr::select(
        "filepath", "scientific_name", "common_name",
        "start", "end", "validation"
      ) |>
      dplyr::mutate(
        Spectrogram = '<button class="spectrogram">Spectrogram</button>',
        Audio = '<button class="play-audio">Audio</button>'
      )
  })


  # Render DT table ----------------------------------------------------

  output$main_table <- DT::renderDT(
    {
      # ensure the file has been uploaded before trying to render
      req(rv$data_display)

      DT::datatable(rv$data_display,
        editable = TRUE,
        # list(target = "column", disable = list(columns = c(1, 2, 3, 4, 5))),
        escape = FALSE, # to render HTML properly
        selection = "none",
        options = list(
          pageLength = 10,
          columnDefs = list(
            list(targets = 7, orderable = FALSE),
            list(targets = 8, orderable = FALSE)
          )
        )
      )
    },
    server = FALSE
  )


  # when the table is clicked to change values
  observeEvent(input$main_table_cell_edit, {
    # get the edit information
    info <- input$main_table_cell_edit

    # update the value in the reactive dataframe
    rv$data_editable$validation[info$row] <- info$value
  })


  # Button observers -----------------------------------------------------

  output$save_file <- downloadHandler(
    filename = function() {
      paste0("validated_", input$import_file, ".csv")
    },
    content = function(file) {
      readr::write_csv(rv$data_editable, file)
    }
  )

  shiny::observeEvent(input$praise_me, {
    shiny::showNotification(praise::praise(), type = "message")
  })


  # Spectrogram on button click -----------------------------------------

  shiny::observeEvent(input$main_table_cell_clicked, {
    info <- input$main_table_cell_clicked
    if (is.null(info$value)) {
      return()
    }

    if (info$col == 7) { # Spectrogram button column
      selected_row <- rv$data_display[info$row, ]
      filepath <- file.path(dir_path(), basename(selected_row$filepath))

      if (file.exists(filepath)) {
        output$spectrogram <- shiny::renderPlot({
          view_spectrogram(
            filepath = filepath,
            flim = input$flim,
            wl = input$wl,
            start = selected_row$start,
            end = selected_row$end
          )
        })
      } else {
        shiny::showNotification("Audio file not found.", type = "error")
      }
    }

    if (info$col == 8) { # Audio play button column
      selected_row <- rv$data_display[info$row, ]
      filepath <- file.path(dir_path(), basename(selected_row$filepath))

      if (file.exists(filepath)) {
        play_audio(
          filepath = filepath,
          start = selected_row$start,
          end = selected_row$end
        )
      } else {
        shiny::showNotification("Audio file not found.", type = "error")
      }
    }
  })
}




# main app ----------------------------------------------------------------

#' Launch BirdNET validation app
#'
#' @export
birdnet_launch_validation <- function() {
  shiny::shinyApp(
    ui = birdnet_validation_ui,
    server = birdnet_validation_server
  )
}
