#' User interface for BirdNET audio validation platform
#'
#' Constructs the user interface layout for the BirdNET audio validation platform
#' within the `birdnetTools` package. This UI includes inputs for file upload,
#' audio folder selection, spectrogram settings, and outputs for data display
#' and spectrogram plotting.
#'
#' @param request Internal parameter for `{shiny}` applications. Typically not set by the user.
#'
#' @return A Shiny UI definition built with `bslib::page_sidebar()`.
#'
#' @details This internal UI function is called by the Shiny validation app
#' launched through higher-level user-facing functions. It sets up sidebar inputs
#' for file import and settings, and main panel outputs for data and spectrogram display.
#'
#' @seealso [birdnet_validation_server], [birdnet_launch_validation]
#'
#' @keywords internal
#' @importFrom bslib page_sidebar sidebar card
#' @importFrom shiny fileInput p verbatimTextOutput downloadButton actionButton
#' @importFrom shinyWidgets numericRangeInput
#' @importFrom shinyFiles shinyDirButton
#' @importFrom DT DTOutput
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
        "Settings",


        # input: duration
        shiny::numericInput(
          inputId = "duration",
          label = "Duration:",
          value = 9,
          min = 3
        ),

        # input: flim
        shinyWidgets::numericRangeInput(
          inputId = "flim",
          label = "Frequency range:",
          min = 1, max = 10,
          value = c(0, 6)
        ),

        # input: wl
        shiny::numericInput(
          inputId = "wl",
          label = "Window length:",
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






#' Server logic for BirdNET audio validation platform
#'
#' Defines the server-side logic for the BirdNET audio validation Shiny app within the
#' `birdnetTools` package. This function handles file imports, audio directory
#' selection, spectrogram generation, interactive table editing, and audio playback.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return Called for its side effects within a `shinyApp` context.
#'
#' @details This internal server function powers the interactive audio validation
#' interface. Users can import a CSV of BirdNET detections, link to local audio
#' directories, view spectrograms, and manually validate detections through a
#' Shiny DataTable interface. Audio playback and validation column editing are supported.
#'
#' @seealso [birdnet_validation_ui], [birdnet_launch_validation]
#'
#' @keywords internal
#' @importFrom shiny observeEvent reactive reactiveValues renderText renderPlot downloadHandler showNotification
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @importFrom DT renderDT datatable
#' @importFrom tuneR readWave play setWavPlayer
#' @importFrom seewave spectro
#' @importFrom stringr str_extract
#' @importFrom praise praise
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate select
birdnet_validation_server <- function(input, output, session) {
  # Helper functions --------------------------------------------------------

  # the function to play audio
  play_audio <- function(filepath, start, end, buffer) {
    if (Sys.info()["sysname"] == "Darwin") {
      tuneR::setWavPlayer("afplay")
    }
    song <- tuneR::readWave(filepath,
                            from = start - buffer,
                            to = end + buffer,
                            units = "seconds")
    tuneR::play(song)
  }

  # the function to view spectrogram
  view_spectrogram <- function(filepath, flim, wl, start, end, buffer) {
    song <- tuneR::readWave(filepath,
                            from = start - buffer,
                            to = end + buffer,
                            units = "seconds")
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
    c(Desktop = file.path(Sys.getenv("USERPROFILE"), "Desktop"),
      Documents = file.path(Sys.getenv("USERPROFILE"), "Documents"),
      Working_dir = ".",
      setNames(drives, drives))
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
          readr::read_csv(show_col_types = FALSE)

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

    rv$cols <- shiny::reactive({
      birdnet_detect_columns(rv$data_editable)
    })


    if (is.na(rv$cols()$filepath)) {
      # handle missing filepath column: warning, error, or skip basename step
      filepath_vec <- NA_character_
    } else {
      filepath_vec <- basename(rv$data_editable[[rv$cols()$filepath]])
    }

    # render the data table with a play button
    rv$data_display <- rv$data_editable |>
      dplyr::mutate(!!dplyr::sym(rv$cols()$filepath) := filepath_vec) |>
      dplyr::select(
        !!dplyr::sym(rv$cols()$filepath),
        !!dplyr::sym(rv$cols()$common_name),
        !!dplyr::sym(rv$cols()$start),
        !!dplyr::sym(rv$cols()$end),
        !!dplyr::sym(rv$cols()$confidence),
        "validation"
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
      paste0("validated.csv")
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
      filepath <- file.path(dir_path(),
                            basename(selected_row[[rv$cols()$filepath]]))

      if (file.exists(filepath)) {
        output$spectrogram <- shiny::renderPlot({
          view_spectrogram(
            filepath = filepath,
            flim = input$flim,
            wl = input$wl,
            start = selected_row[[rv$cols()$start]],
            end = selected_row[[rv$cols()$end]],
            buffer = (input$duration - 3)/2
          )
        })
      } else {
        shiny::showNotification("Audio file not found.", type = "error")
      }
    }

    if (info$col == 8) { # Audio play button column
      selected_row <- rv$data_display[info$row, ]
      filepath <- file.path(dir_path(),
                            basename(selected_row[[rv$cols()$filepath]]))

      if (file.exists(filepath)) {
        play_audio(
          filepath = filepath,
          start = selected_row[[rv$cols()$start]],
          end = selected_row[[rv$cols()$end]],
          buffer = (input$duration - 3)/2
        )
      } else {
        shiny::showNotification("Audio file not found.", type = "error")
      }
    }
  })
}




#' Launch the BirdNET audio validation app
#'
#' Starts an interactive Shiny application for manually validating BirdNET detections
#' using audio clips and spectrograms.This tool is designed to support manual review of BirdNET results.
#'
#' @details This function launches a user-friendly Shiny app that allows you to:
#' \itemize{
#'   \item Import a CSV file of BirdNET detections (selection table format),
#'   \item Select the directory containing your `.wav` audio files,
#'   \item View and validate each detection using spectrograms and audio playback,
#'   \item Edit validation results interactively and save the updated table.
#' }
#'
#' @return A `shiny.appobj` that runs the BirdNET audio validation platform in a web browser.
#'
#' @examples
#' \dontrun{
#' birdnet_launch_validation()
#' }
#'
#' @seealso [birdnet_validation_ui], [birdnet_validation_server]
#' @import shiny
#' @export
birdnet_launch_validation <- function() {
  shiny::shinyApp(
    ui = birdnet_validation_ui,
    server = birdnet_validation_server
  )
}
