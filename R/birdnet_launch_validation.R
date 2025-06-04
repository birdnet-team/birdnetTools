
# ui ----------------------------------------------------------------------

birdnet_validation_ui <- function(request) {

  bslib::page_sidebar(

    # add title
    title = "Audio Validation platform (birdnetTools R package)",

    # side bar setting
    sidebar_width = 350,

    # add side bar
    sidebar = bslib::sidebar(
      bslib::card("File import",

                  # input: detection_list
                  shiny::fileInput(inputId = "detection_list",
                                   label = "Metadata csv file",
                                   multiple = FALSE,
                                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                   width = "100%"),

                  # button: dir
                  shiny::p("Recording folder"),
                  shinyFiles::shinyDirButton(id = "dir",
                                             label = "Select",
                                             title = "Select the species recording folder"),

                  # output: dir
                  shiny::verbatimTextOutput(outputId = "dir",
                                            placeholder = TRUE)


      ), bslib::card("Settings",

                  # input: flim, wl
                  shinyWidgets::numericRangeInput(inputId = "flim",
                                                  "Frequency range:",
                                                  min = 1, max = 10,
                                                  value = c(0, 6)),
                  shiny::numericInput(inputId = "wl",
                                      "Window length:",
                                      value = 512)
      ), bslib::card("Actions",

                  # button: praise_me
                  shiny::actionButton("praise_me", "Praise me")
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
    seewave::spectro(song, f = song@samp.rate, flim = flim, ovlp = 50,
                     collevels = seq(-40, 0, 1), wl = wl, scale = FALSE,
                     main = stringr::str_extract(filepath, "[^/]+(?=\\.wav$)"))
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
    c(Desktop = "~/Desktop",
      Documents = "~/Documents",
      Working_dir = ".",
      setNames(external_drives, basename(external_drives)))
  }


  # provide options for the roots when searching for file
  shinyFiles::shinyDirChoose(
    input,
    id = "dir",
    roots = roots)

  dir_path <- shiny::reactive({
    shinyFiles::parseDirPath(roots, input$dir)
  })

  output$dir <- shiny::renderText({
    dir_path()
  })


  # reactive value for the dataframe ------------------------------------

  rv <- shiny::reactiveValues()

  shiny::observeEvent(input$detection_list, {

    # read in the csv file and clean names based on the path input
    tryCatch({
      rv$data_display <- input$detection_list$datapath |>
        readr::read_csv(show_col_types = FALSE) |>
        birdnet_clean_names()

      # check if the data frame has the validation column already
      if (!"validation" %in% names(rv$data_display)) {
        rv$data_display <- rv$data_display |>
          dplyr::mutate(validation = "U")
      }

    }, error = function(e) {
      shiny::showNotification(
        paste("Error reading CSV:", e$message),
        type = "error")
    })
  })


  # Render DT table ----------------------------------------------------

  output$main_table <- DT::renderDT({

    # ensure the file has been uploaded before trying to render
    req(rv$data_display)


    # render the data table with a play button
    rv$data_display |>
      dplyr::select("filepath", "scientific_name", "common_name", "start", "end", "validation") |>

      dplyr::mutate(Spectrogram = '<button class="spectrogram">Spectrogram</button>',
                    Audio = '<button class="play-audio">Audio</button>') |>

    # dat$Decision <- sapply(seq_len(nrow(dat)), function(i) {
    #   val <- dat$validation[i]
    #   color <- switch(val, "Y" = "success", "N" = "danger", "U" = "primary")
    #   label <- switch(val, "Y" = "✔️", "N" = "❌", "U" = "❔")
    #   paste0('<button class="btn btn-', color,
    #          ' btn-sm toggle-val" data-row="', i, '">', label, '</button>')
    # })


    DT::datatable(editable = list(target = "column", disable = list(columns = c(1, 2, 3, 4, 5))),
                  escape = FALSE, # to render HTML properly
                  selection = "none",
                  options = list(
                    pageLength = 10,
                    columnDefs = list(
                      list(targets = 7, orderable = FALSE),
                      list(targets = 8, orderable = FALSE)
                    )
                  )
    #               ,
    #               callback = JS("
    #   table.on('click', '.toggle-val', function() {
    #     var row = $(this).data('row');
    #     Shiny.setInputValue('toggle_val', row, {priority: 'event'});
    #   });
    # ")
    )
  }, server = FALSE)



  # when the value is edited
  observeEvent(input$main_table_cell_edit, {

    info <- input$data_display_cell_edit

    # find the next value based on the current value
    row <- info$row
    value <- info$value

    # update the value in the reactive dataframe
    rv$data_display$validation[row] <- value
  })


  # Button observers -----------------------------------------------------

  shiny::observeEvent(input$praise_me, {
    shiny::showNotification(praise::praise(), type = "message")
  })


  # Spectrogram on button click -----------------------------------------

  shiny::observeEvent(input$main_table_cell_clicked, {
    info <- input$main_table_cell_clicked
    if (is.null(info$value)) return()

    if (info$col == 7) { # Spectrogram button column
      selected_row <- rv$data_display[info$row, ]
      filepath <- file.path(dir_path(), basename(selected_row$filepath))

      if (file.exists(filepath)) {
        output$spectrogram <- shiny::renderPlot({
          view_spectrogram(filepath = filepath,
                           flim = input$flim,
                           wl = input$wl,
                           start = selected_row$start,
                           end = selected_row$end)
        })
      } else {
        shiny::showNotification("Audio file not found.", type = "error")
      }
    }

    if (info$col == 8) { # Audio play button column
      selected_row <- rv$data_display[info$row, ]
      filepath <- file.path(dir_path(), basename(selected_row$filepath))

      if (file.exists(filepath)) {
        play_audio(filepath = filepath,
                   start = selected_row$start,
                   end = selected_row$end)
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

  shiny::shinyApp(ui = birdnet_validation_ui,
                  server = birdnet_validation_server)

}

