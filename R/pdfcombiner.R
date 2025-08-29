#' @name pdfcombiner
#' @title Launches PDF Combiner
#'
#' @description
#' Launches the PDF Combiner \link[shiny]{shiny} App. Instructions on usage are
#' included on the left hand side of the page. By default it uses the bootstrap theme,
#' allowing minimization of the sidebar. Set argument \code{boostrap_theme = FALSE} to turn this
#' off.
#'
#' @param max_file_size      Max upload file size in MB, change if needed
#' @param bootstrap_theme    When TRUE, uses bslib bootstrap theme to allow minimizing sidebar
#' @param sidebar_width      Only applicable when bootstrap theme is used, in pixels
#' @param defaultwm_fontsize Default Watermark font size
#' @param defaultwm_col      Default Watermark color
#' @param defaultwm_alpha    Default Watermark alpha
#' @param defaultwm_rot      Default Watermark rotation angle
#' @param defaultwm_fontface Default Watermark fontface ("plain", "italic", "bold", "bold.italic")
#' @param defaultwm_height   Default Watermark height in inches (US letter size = 11,  A4 = 11.69)
#' @param defaultwm_width    Default Watermark height in inches (US letter size = 8.5, A4 = 8.27)
#' @param image_dpi          Dots per inch for use when converting to images
#'
#' @details
#' The user is highly recommended to also install the \link[staplr]{staplr} package as it supports bookmarks, however
#' it uses Java so you may need to install Java separately if your system does not currently have it.
#' In addition, the \link[magick]{magick} package is recommended for supporting uploading of image files.
#'
#' @examples
#' \dontrun{
#' pdfcombiner(bootstrap_theme = FALSE) # Do not use bootstrap theme (if `shiny` version is < 1.7.4)
#' }
#' @note
#' All PDF files are stored locally in a per-session temporary directory, given by the \code{tempdir()} function.
#' @seealso
#' \url{https://github.com/stevechoy/pdfcombiner}, \link[pdftools]{pdftools}, \link[qpdf]{qpdf}
#'
#'
#' @returns a Shiny App
#' @export
pdfcombiner <- function(max_file_size      = 500,
                        bootstrap_theme    = TRUE,
                        sidebar_width      = 700,
                        defaultwm_fontsize = 50,
                        defaultwm_col      = "gray80",
                        defaultwm_alpha    = 0.6,
                        defaultwm_rot      = 45,
                        defaultwm_fontface = "bold",
                        defaultwm_height   = 11,
                        defaultwm_width    = 8.5,
                        image_dpi          = 300) {

  # Sanity checks for arguments that should be a non-negative numeric
  is_non_negative_numeric(max_file_size)
  is_non_negative_numeric(sidebar_width)
  is_non_negative_numeric(defaultwm_fontsize)
  is_non_negative_numeric(defaultwm_alpha)
  is_non_negative_numeric(defaultwm_rot)
  is_non_negative_numeric(defaultwm_height)
  is_non_negative_numeric(defaultwm_width)

  options(shiny.maxRequestSize = max_file_size * 1024^2)

  # Check if shinythemes is installed, optional
  if (requireNamespace("shinythemes", quietly = TRUE)) {
    requireNamespace("shinythemes")
    app_theme <- shinythemes::shinytheme("flatly")
  } else {
    app_theme <- NULL  # Default to no theme if shinythemes is not installed
  }

  # Check if staplr is installed, recommended for handling bookmarks
  if (requireNamespace("staplr", quietly = TRUE)) {
    #requireNamespace(rJava) # If running this line fails, that means you need to install Java separately
    requireNamespace("staplr")
  }

  # UI
  ui <- if (requireNamespace("bslib", quietly = TRUE) && bootstrap_theme && packageVersion("shiny") >= "1.7.4") { # Loads a bootstrap UI if bslib is installed
    requireNamespace("bslib")
    bslib::page_sidebar(
      theme = bslib::bs_theme(version = 5, # Use Bootstrap 5
                              preset = "flatly", # cerulean
                              font_scale = 1),
      sidebar = bslib::sidebar(
        width = sidebar_width,
        title = tags$span("PDF Combiner", style = "font-size: 20px; font-weight: bold;"), # Title text styling
        p("1. Upload PDF or Image file(s). All files will be combined automatically by default."),
        p("2. Use the file selector to choose which files to include, and click the 'Update / Combine PDF' button (delete unwanted files, order matters)."),
        p("3. Verify changes on the right and download the updated PDF."),
        p(HTML("<strong>Note:</strong> You can also apply any optional features below, <em>before</em> downloading the updated PDF.")),

        bslib::card(class ="shadow border-primary",
                    bslib::card_header("Input Files"),
                    if (requireNamespace("magick", quietly = TRUE)) {
                      fileInput("pdf_files", paste0("Upload PDF or Image File(s): [Max ", max_file_size, " MB]"), multiple = TRUE,
                                accept = magick_formats)
                    } else {
                      fileInput("pdf_files", paste0("Upload PDF File(s): [Max ", max_file_size, " MB]"), multiple = TRUE, accept = ".pdf")  # Allow multiple file uploads
                    },

                    # Selector for choosing PDFs to combine
                    selectInput("selected_pdfs", "Select Files to Combine (in this order - select / delete files below):", choices = NULL, multiple = TRUE),

                    textInput("save_as_name", label = "(Optional) File Name to Save as...", value = "", placeholder = "Default if not provided: 'updated_pdf_YYYY-MM-DD.pdf'"),

                    # Combine button and compress checkbox
                    fluidRow(
                      column(
                        width = 4,
                        actionButton("combine_btn", "Update / Combine PDF", style = "width: 100%; margin-top: 0px;")
                      ),
                      column(
                        width = 5,
                        uiOutput("download_ui")
                      ),
                      column(
                        width = 3,
                        div(style = "height: 5px;"),  # Empty div to add space
                        checkboxInput("compress",
                                      tagList(
                                        HTML("&nbsp;Compress"),
                                        tags$span(
                                          "?",
                                          style = "color: blue; cursor: help; font-weight: bold; margin-left: 0px; font-size: 16px;",
                                          title = "Smart lossless compression, will always return the smallest file (details will be shown on bottom right)."
                                        )
                                      ),
                                      value = TRUE
                        )
                      ) # end of column
                    ) # end of fluidRow for Combine button
        ), # end of card

        bslib::card(class ="shadow",
                    bslib::card_header("Page Editor"),
                    # Custom text with inline question mark tooltip
                    tags$div(
                      style = "display: flex; align-items: center;",  # Align label and input inline
                      tags$label(
                        "Enter (Current) Page Numbers to Remove / Select: ",
                        tags$span(
                          "?",
                          style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                          title = "Note: Bookmarks will not be retained when pages are removed or selected."
                        )
                      )
                    ),

                    # Page removal input
                    textInput("remove_pages", label = NULL, value = "", placeholder = page_placeholder_text), # Input without default label

                    # Buttons side by side with 20px gap
                    div(
                      style = "display: flex; align-items: center; gap: 20px;",
                      actionButton("remove_pages_btn",
                                   label = tagList(icon("xmark", class = "fa-lg"), "Remove Pages")),  # Remove Pages button with xmark icon
                      actionButton("select_pages_btn",
                                   label = tagList(icon("check", class = "fa-lg"), "Select Pages")),  # Select Pages button with check icon
                      actionButton("reset_btn",
                                   label = tagList(icon("sync-alt", class = "fa-lg"), "Reset"))  # Reset button with refresh icon
                    ),

                    #tags$hr(style = "border: 2px solid #ccc;"), # Grey divider line

                    # Custom text with inline question mark tooltip
                    tags$div(
                      style = "display: flex; align-items: center;",  # Align label and input inline
                      tags$label(
                        "Enter (Current) Page Numbers to Rotate: ",
                        tags$span(
                          "?",
                          style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                          title = "Note: Bookmarks will not be retained when pages are rotated."
                        )
                      )
                    ),

                    # Page rotate input
                    textInput("rotate_pages", label = NULL, value = "", placeholder = page_placeholder_text),

                    # Buttons side by side with 20px gap
                    div(
                      style = "display: flex; align-items: center; gap: 20px;",
                      actionButton("rotate_pages_btn",
                                   label = tagList(icon("rotate-left", class = "fa-lg"), "Rotate Pages (90\u00B0 counterclockwise)")),  # Remove Pages button with xmark icon
                      actionButton("reset_btn_rot",
                                   label = tagList(icon("sync-alt", class = "fa-lg"), "Reset"))#,  # Reset button with refresh icon
                    ),

                    # Insert Watermark with inline question mark tooltip
                    tags$div(
                      style = "display: flex; align-items: center;",  # Align label and input inline
                      tags$label(
                        "(Optional) Watermark Stamp: ",
                        tags$span(
                          "?",
                          style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                          title = "Applies a see-through text across all pages of the PDF when downloaded."
                        )
                      )
                    ),

                    # Watermark input and settings
                    fluidRow(
                      column(9, textInput("watermark_text", label = NULL, placeholder = "e.g. 'For Internal Use Only'")),
                      column(1, actionButton("customize_watermark", label = NULL, icon = icon("gear"), class = "btn-secondary")),
                      column(2, actionButton("render_preview2", label = NULL, icon = icon("play"), class = "btn-primary")),
                    ),

        ), # end of card

        bslib::card(class = "shadow",
                    bslib::card_header("Remove Password Protection"),

                    # Custom text with inline question mark tooltip
                    tags$div(
                      style = "display: flex; align-items: center;",  # Align label and input inline
                      tags$label(
                        paste0("Upload Password-protected PDF File: [Max ", max_file_size, " MB]"),
                        tags$span(
                          "?",
                          style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                          title = "Note: Bookmarks will not be retained when PDFs are unlocked."
                        )
                      )
                    ),

                    fileInput("locked_pdf", label = NULL, multiple = FALSE, accept = ".pdf"),
                    textInput("password", label = "Password:", value = "", placeholder = "Enter password here"),
                    div(
                      style = "display: flex; align-items: center; gap: 20px;", # Flexbox layout with 20px gap
                      actionButton("unlock_btn",
                                   label = tagList(icon("lock-open", class = "fa-lg"), "Unlock PDF")), # Unlock button
                      uiOutput("download_unlocked_ui")                        # UI for downloading unlocked file
                    )
        ),

        bslib::card(class = "bg-light shadow",
                    bslib::card_header("PDF Conversion"),
                    # Dropdown for selecting output format
                    selectInput(
                      "convert_format",
                      "(Experimental) Convert Updated PDF to:",
                      choices = c("Word (.docx)", "Excel (.xlsx)", "PowerPoint (.pptx)", "Images (.png as a zip file)"),
                      selected = "Word (.docx)"
                    ),

                    div(
                      style = "display: flex; align-items: center; gap: 20px;", # Flexbox layout with 20px gap
                      actionButton("convert_btn", "Convert PDF"),               # Conversion button
                      uiOutput("download_conversion_ui")                        # UI for downloading converted file
                    )
        ),

        tags$p("Author: Steve Choy (v1.9.7)",
               a(href = "https://github.com/stevechoy/pdfcombiner", "(GitHub Repo)", target = "_blank"),
               style = "font-size: 0.9em; color: #555; text-align: left;")
      ), # end of sidebar

      htmlOutput("pdfviewer")  # Embedded PDF viewer
    ) # end of page_sidebar

  } else { # if not using Bootstrap theme, then use regular sidebar
    fluidPage(
      theme = app_theme,
      tags$head(tags$title("PDF Combiner")),

      sidebarLayout(
        sidebarPanel(
          width = 5,
          tags$span("PDF Combiner", style = "font-size: 20px; font-weight: bold;"), # Title text styling
          br(),br(),
          p("1. Upload PDF or Image file(s). All files will be combined automatically by default."),
          p("2. Use the file selector to choose which files to include, and click the 'Update / Combine PDF' button (delete unwanted files, order matters)."),
          p("3. Verify changes on the right and download the updated PDF."),
          p(HTML("<strong>Note:</strong> You can also apply any optional features below, <em>before</em> downloading the updated PDF.")),
          br(),

          if (requireNamespace("magick", quietly = TRUE)) {
            fileInput("pdf_files", paste0("Upload PDF or Image File(s): [Max ", max_file_size, " MB]"), multiple = TRUE,
                      accept = magick_formats)
          } else {
            fileInput("pdf_files", paste0("Upload PDF File(s): [Max ", max_file_size, " MB]"), multiple = TRUE, accept = ".pdf")  # Allow multiple file uploads
          },

          # Selector for choosing PDFs to combine
          selectInput("selected_pdfs", "Select Files to Combine (in this order - select / delete files below):", choices = NULL, multiple = TRUE),

          textInput("save_as_name", label = "(Optional) File Name to Save as...", value = "", placeholder = "Default if not provided: 'updated_pdf_YYYY-MM-DD.pdf'"),

          # Combine button and compress checkbox
          fluidRow(
            column(
              width = 4,
              actionButton("combine_btn", "Update / Combine PDF", style = "width: 100%; margin-top: 0px;")
            ),
            column(
              width = 5,
              uiOutput("download_ui")
            ),
            column(
              width = 3,
              div(style = "height: 5px;"),  # Empty div to add space
              checkboxInput("compress",
                            tagList(
                              HTML("&nbsp;Compress"),
                              tags$span(
                                "?",
                                style = "color: blue; cursor: help; font-weight: bold; margin-left: 0px; font-size: 16px;",
                                title = "Smart lossless compression, will always return the smallest file (details will be shown on bottom right)."
                              )
                            ),
                            value = TRUE
              )
            ) # end of column
          ), # end of fluidRow for Combine button

          tags$hr(style = "border: 2px solid #ccc;"), # Grey divider line

          # Custom text with inline question mark tooltip
          tags$div(
            style = "display: flex; align-items: center;",  # Align label and input inline
            tags$label(
              "Enter (Current) Page Numbers to Remove / Select: ",
              tags$span(
                "?",
                style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                title = "Note: Bookmarks will not be retained when pages are removed or selected."
              )
            )
          ),

          # Page removal input
          textInput("remove_pages", label = NULL, value = "", placeholder = page_placeholder_text), # Input without default label

          # Buttons side by side with 20px gap
          div(
            style = "display: flex; align-items: center; gap: 20px;",
            actionButton("remove_pages_btn",
                         label = tagList(icon("xmark", class = "fa-lg"), "Remove Pages")),  # Remove Pages button with xmark icon
            actionButton("select_pages_btn",
                         label = tagList(icon("check", class = "fa-lg"), "Select Pages")),  # Select Pages button with check icon
            actionButton("reset_btn",
                         label = tagList(icon("sync-alt", class = "fa-lg"), "Reset"))  # Reset button with refresh icon
          ),

          tags$hr(style = "border: 2px solid #ccc;"), # Grey divider line

          # Custom text with inline question mark tooltip
          tags$div(
            style = "display: flex; align-items: center;",  # Align label and input inline
            tags$label(
              "Enter (Current) Page Numbers to Rotate: ",
              tags$span(
                "?",
                style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                title = "Note: Bookmarks will not be retained when pages are rotated."
              )
            )
          ),

          # Page rotate input
          textInput("rotate_pages", label = NULL, value = "", placeholder = page_placeholder_text),

          # Buttons side by side with 20px gap
          div(
            style = "display: flex; align-items: center; gap: 20px;",
            actionButton("rotate_pages_btn",
                         label = tagList(icon("rotate-left", class = "fa-lg"), "Rotate Pages (90\u00B0 counterclockwise)")),  # Remove Pages button with xmark icon
            actionButton("reset_btn_rot",
                         label = tagList(icon("sync-alt", class = "fa-lg"), "Reset"))#,  # Reset button with refresh icon
          ),

          tags$hr(style = "border: 2px solid #ccc;"), # Grey divider line

          # Insert Watermark with inline question mark tooltip
          tags$div(
            style = "display: flex; align-items: center;",  # Align label and input inline
            tags$label(
              "(Optional) Watermark Stamp: ",
              tags$span(
                "?",
                style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                title = "Applies a see-through text across all pages of the PDF when downloaded."
              )
            )
          ),

          # Watermark input and settings
          fluidRow(
            column(9, textInput("watermark_text", label = NULL, placeholder = "e.g. 'For Internal Use Only'")),
            column(1, actionButton("customize_watermark", label = NULL, icon = icon("gear"), class = "btn-secondary")),
            column(2, actionButton("render_preview2", label = NULL, icon = icon("play"), class = "btn-primary")),
          ),

          tags$hr(style = "border: 2px solid #ccc;"), # Grey divider line

          tags$div(
            style = "display: flex; align-items: center;",  # Align label and input inline
            tags$label(
              paste0("Upload Password-protected PDF File: [Max ", max_file_size, " MB]"),
              tags$span(
                "?",
                style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                title = "Note: Bookmarks will not be retained when PDFs are unlocked."
              )
            )
          ),

          fileInput("locked_pdf", label = NULL, multiple = FALSE, accept = ".pdf"),
          textInput("password", label = "Password:", value = "", placeholder = "Enter password here"),
          div(
            style = "display: flex; align-items: center; gap: 20px;", # Flexbox layout with 20px gap
            actionButton("unlock_btn",
                         label = tagList(icon("lock-open", class = "fa-lg"), "Unlock PDF")), # Unlock button
            uiOutput("download_unlocked_ui")                        # UI for downloading unlocked file
          ),

          tags$hr(style = "border: 2px solid #ccc;"), # Grey divider line

          # Dropdown for selecting output format
          selectInput(
            "convert_format",
            "(Experimental) Convert Updated PDF to:",
            choices = c("Word (.docx)", "Excel (.xlsx)", "PowerPoint (.pptx)", "Images (.png as a zip file)"),
            selected = "Word (.docx)"
          ),

          div(
            style = "display: flex; align-items: center; gap: 20px;", # Flexbox layout with 20px gap
            actionButton("convert_btn", "Convert PDF"),               # Conversion button
            uiOutput("download_conversion_ui")                        # UI for downloading converted file
          ),

          br(),
          tags$p("Author: Steve Choy (v1.9.7)",
                 a(href = "https://github.com/stevechoy/PDF_Combiner", "(GitHub Repo)", target = "_blank"),
                 style = "font-size: 0.9em; color: #555; text-align: left;")
        ), # end of sidebarPanel

        mainPanel(
          width = 7,
          htmlOutput("pdfviewer")  # Embedded PDF viewer
        )
      ) # end of sidebarLayout
    ) # end of ui regular
  } # end of ui conditional check

  # Server
  server <- function(input, output, session) {

    # Reactive values to store uploaded PDFs and their original names
    uploaded_pdfs <- reactiveVal(list())  # List to store uploaded PDFs
    combined_pdf <- reactiveVal(NULL)     # Stores the combined PDF path
    original_pdf <- reactiveVal(NULL)     # Stores the original combined PDF path
    temp_dir <- tempdir()                 # Temporary directory for storing combined PDFs
    shiny::addResourcePath("pdfs", temp_dir)  # Serve files from the temp directory
    #rotation_angle <- reactiveVal(0)      # Reactive value to store the current rotation angle for staplr, not currently used
    unlocked_pdf <- reactiveVal(NULL)     # Stores unlocked PDF path (singular file)
    combined_pdf_nowm <- reactiveVal(NULL) # Stores combined_pdf without watermarks for final download
    original_file_sizes <- reactiveVal(NULL) # Stores sum of original files disk spaces

    # Default watermark settings
    default_settings <- list(
      fontsize = defaultwm_fontsize,
      col      = defaultwm_col,
      alpha    = defaultwm_alpha,
      rot      = defaultwm_rot,
      fontface = defaultwm_fontface,
      height   = defaultwm_height,
      width    = defaultwm_width
    )

    # Reactive values to store watermark settings
    watermark_settings <- reactiveValues(
      fontsize   = default_settings$fontsize,
      col        = default_settings$col,
      alpha      = default_settings$alpha,
      rot        = default_settings$rot,
      fontface   = default_settings$fontface,
      height     = default_settings$height,
      width      = default_settings$width
    )

    # Helper function to combine PDFs
    combine_pdfs <- function(pdf_paths, output_path) {
      original_file_sizes(sum_disk_space(pdf_paths))
      print(paste("Sum of File Sizes (KB):", original_file_sizes())) # Debugging
      # If there's only 1 PDF file, don't do anything to it, just copy it to temp_dir
      if(length(pdf_paths) == 1) {
        file.copy(unlist(pdf_paths), output_path)
        print(paste("Single File: copying from ", unlist(pdf_paths), " to ", output_path))# Debugging
      } else {
        # Interesting that when PDFs are read, they are already somehow compressed
        if(package_check("staplr", bookmarks = TRUE)) {
          staplr::staple_pdf(input_files = unlist(pdf_paths), output_filepath = output_path)
        } else {
          pdf_combine(unlist(pdf_paths), output = output_path)
        }
      }
      combined_pdf(output_path)
      combined_pdf_nowm(output_path)
      original_pdf(output_path) # Save the original combined PDF
      print(paste("Combined PDF Path:", output_path)) # Debugging: Print combined PDF path
    }

    # Observe file upload
    observeEvent(input$pdf_files, {
      shiny::req(input$pdf_files)

      # Add the new PDFs to the list of uploaded PDFs
      current_pdfs <- uploaded_pdfs()
      for (i in seq_along(input$pdf_files$name)) {

        file_name <- input$pdf_files$name[i]
        file_path <- input$pdf_files$datapath[i]
        file_extension <- tools::file_ext(file_name)  # Get the file extension (e.g., "png", "pdf")

        if (requireNamespace("magick", quietly = TRUE)) {
          # Check if the file extension is in magick_formats
          if (paste0(".", file_extension) %in% magick_formats) {
            if (file_extension == "pdf") { # If PDF, store the file as is
              # Check if the file is encrypted
              pdf_is_locked <- (pdf_info(file_path)$locked | pdf_info(file_path)$encrypted)
              if (pdf_is_locked) {
                showNotification(paste0(file_name, " could not be read. It may not be a valid PDF, or it is password-protected. If so, please first unlock it down below."), type = "error", duration = 12)
                return()
              } else {
                current_pdfs[[file_name]] <- file_path
              }
            } else {
              img <- magick::image_read(file_path)  # Read the image
              new_path <- file.path(temp_dir, paste0("converted_image_", i, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))
              magick::image_write(img, path = new_path, format = "pdf")  # Write the image as a PDF
              current_pdfs[[file_name]] <- new_path
              #file.remove(file_path) # Delete original image file
            }
          }
        } else {
          # Get PDF metadata
          pdf_is_locked <- (pdf_info(file_path)$locked | pdf_info(file_path)$encrypted)
          if(pdf_is_locked) {
            showNotification(paste0(file_name, " could not be read. It may not be a valid PDF, or it is password-protected. If so, please first unlock it down below."), type = "error", duration = 12)
            return()
          } else {
            current_pdfs[[file_name]] <- file_path
          }
        }
      }
      uploaded_pdfs(current_pdfs) # List of uploaded pdfs

      # Update the selector choices
      updateSelectInput(session, "selected_pdfs", choices = names(current_pdfs), selected = names(current_pdfs))

      # Automatically combine all uploaded PDFs
      combine_pdfs(current_pdfs, file.path(temp_dir, paste0("combined_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf")))

      # Debugging: Print uploaded PDFs
      print(paste("Uploaded PDFs:", paste(names(current_pdfs), collapse = ", ")))
    })

    # Combine selected PDFs
    observeEvent(input$combine_btn, {
      shiny::req(input$selected_pdfs)

      # Get the selected PDFs
      selected_paths <- uploaded_pdfs()[input$selected_pdfs]

      # Combine the selected PDFs
      combine_pdfs(selected_paths, file.path(temp_dir, paste0("combined_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf")))

      showNotification("Selected PDFs combined successfully!", type = "message")
    })

    # Render PDF Viewer
    output$pdfviewer <- renderUI({
      shiny::req(combined_pdf())

      # JavaScript to dynamically adjust iframe height
      tags$div(
        style = "position: relative;",
        tags$iframe(
          id = "pdfIframe",
          src = paste0("pdfs/", basename(combined_pdf())),  # Serve the file via the resource path
          width = "100%",
          style = "border: none; height: 100%;"
        ),
        tags$script(HTML("
      function resizeIframe() {
        var iframe = document.getElementById('pdfIframe');
        if (iframe) {
          // Calculate available height dynamically
          var windowHeight = window.innerHeight;
          var iframeTop = iframe.getBoundingClientRect().top;
          var availableHeight = windowHeight - iframeTop - 0; // Add some padding if required
          iframe.style.height = availableHeight + 'px';
        }
      }

      // Call resizeIframe on window resize and load
      window.addEventListener('resize', resizeIframe);
      window.addEventListener('load', resizeIframe);
      resizeIframe(); // Initial call
    "))
      )
    })

    # Helper function to process pages
    process_pdf_pages <- function(action, input_pages, input_pdf_path) {
      shiny::req(input_pdf_path, input_pages)

      # Parse the page numbers
      pages <- parse_pages_to_remove(input_pages)

      # Debugging: Print pages
      print(paste("Pages to ", action, ":", paste(pages, collapse = ", ")))

      # Read the combined PDF
      total_pages <- pdf_info(input_pdf_path)$pages

      # Validate page numbers
      if (any(pages < 1 | pages > total_pages)) {
        showNotification("Invalid page numbers. Please enter valid page numbers within the range of the PDF.", type = "error")
        return(NULL)
      }

      # Determine pages to keep based on the action
      pages_to_keep <- switch(
        action,
        "Remove" = setdiff(seq_len(total_pages), pages),
        "Select" = intersect(seq_len(total_pages), pages),
        stop("Invalid action specified")
      )

      # Check if there are any pages left to keep
      if (length(pages_to_keep) == 0) {
        showNotification(paste("Cannot remove all pages from the PDF. At least one page must remain."), type = "error")
        return(NULL)
      }

      # Create a new PDF with the remaining pages
      updated_pdf_path <- file.path(temp_dir, paste0("updated_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))

      #if(package_check("staplr", silent = TRUE)) {
      #  staplr::select_pages(selpages = pages_to_keep, input_filepath = pdf_path, output_filepath = updated_pdf_path)
      #} else {
      pdf_subset(input_pdf_path, pages = pages_to_keep, output = updated_pdf_path)
      #}

      # Update reactive variables
      combined_pdf(updated_pdf_path)
      combined_pdf_nowm(updated_pdf_path)

      # Debugging: Print updated PDF path
      print(paste("Updated PDF Path:", updated_pdf_path))
      showNotification(
        switch(
          action,
          "Remove" = paste0("Pages Removed Successfully!"),
          "Select" = paste0("Pages Selected Successfully!")
        ),
        type = "message")
    }

    # Remove Pages
    observeEvent(input$remove_pages_btn, {
      process_pdf_pages("Remove", input$remove_pages, combined_pdf())
    })

    # Select Pages
    observeEvent(input$select_pages_btn, {
      process_pdf_pages("Select", input$remove_pages, combined_pdf())
    })

    # Rotate Pages
    observeEvent(input$rotate_pages_btn, {
      shiny::req(combined_pdf(), input$rotate_pages)

      # Parse the page numbers to remove
      pages_to_rotate <- parse_pages_to_remove(input$rotate_pages)

      # Debugging: Print pages to remove
      print(paste("Pages to Rotate:", paste(pages_to_rotate, collapse = ", ")))

      # Read the combined PDF
      pdf_path <- combined_pdf()
      total_pages <- pdf_info(pdf_path)$pages

      # Validate page numbers
      if (any(pages_to_rotate < 1 | pages_to_rotate > total_pages)) {
        showNotification("Invalid page numbers. Please enter valid page numbers within the range of the PDF.", type = "error")
        return()
      }

      # Check if there are any pages left to rotate
      if (length(pages_to_rotate) == 0) {
        return()
      }

      # Create a new PDF with the remaining pages
      updated_pdf_path <- file.path(temp_dir, paste0("updated_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))

      # if(package_check("staplr", silent = TRUE)) {
      #   new_angle <- (rotation_angle() - 90) %% 360  # # Decrement the rotation angle by 90 degrees (counterclockwise), Cycle back to 270 after -90
      #   if (new_angle < 0) new_angle <- new_angle + 360  # Handle negative values
      #   rotation_angle(new_angle)
      #   # Debugging: Print Rotation angle
      #   print(paste("Rotation angle:", rotation_angle()))
      #   staplr::rotate_pages(rotatepages = pages_to_rotate, page_rotation = rotation_angle(), input_filepath = pdf_path, output_filepath = updated_pdf_path)
      # } else {
      #requireNamespace("qpdf") # "qpdf" is an Import from pdftools
      qpdf::pdf_rotate_pages(pdf_path, pages = pages_to_rotate, angle = 270, relative = TRUE, output = updated_pdf_path)
      #}
      combined_pdf(updated_pdf_path)  # Update the combined PDF
      combined_pdf_nowm(updated_pdf_path)  # Update the combined PDF without watermarks

      # Debugging: Print updated PDF path
      print(paste("Updated PDF Path:", updated_pdf_path))
      showNotification("Pages rotated successfully!", type = "message")
    })

    # Reset Button
    observeEvent(input$reset_btn, {
      shiny::req(original_pdf())  # Ensure there is an original combined PDF
      combined_pdf(original_pdf())  # Restore the original combined PDF
      combined_pdf_nowm(original_pdf())  # Restore the original combined PDF no watermarks
      updateTextInput(session, "remove_pages", value = "")  # Reset page removal input
      showNotification("Page reset successful! Restored to the current list of PDFs.", type = "message")
    })

    # Reset Button from page rotation section
    observeEvent(input$reset_btn_rot, {
      shiny::req(original_pdf())  # Ensure there is an original combined PDF
      combined_pdf(original_pdf())  # Restore the original combined PDF
      combined_pdf_nowm(original_pdf())  # Restore the original combined PDF no watermarks
      updateTextInput(session, "rotate_pages", value = "")  # Reset page removal input
      showNotification("Page reset successful! Restored to the current list of PDFs.", type = "message")
    })

    # Show a modal dialog for customizing watermark settings
    observeEvent(input$customize_watermark, {
      showModal(
        modalDialog(
          title = "Customize Watermark Settings",
          easyClose = TRUE, fade = TRUE,
          fluidRow(
            # First column
            column(6,
                   textInput("col", HTML('Font Color <a href="https://r-charts.com/colors/" target="_blank">(character or Hex)</a>'), value = watermark_settings$col),
                   numericInput("fontsize", "Font Size", value = watermark_settings$fontsize, min = 1),
                   selectInput("fontface", "Font Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = "bold", multiple = FALSE),
                   numericInput("rot", "Rotation Angle", value = watermark_settings$rot, min = 0, max = 360),
            ),
            # Second column
            column(6,
                   sliderInput("alpha", "Transparency (Alpha)", min = 0, max = 1, value = watermark_settings$alpha),
                   numericInput("height", "Overlay Height (inches)", value = watermark_settings$height, min = 1),
                   numericInput("width", "Overlay Width (inches)", value = watermark_settings$width, min = 1)
            )
          ),
          footer = tagList(
            actionButton("reset_wm", "Reset", class = "btn-warning"),
            actionButton("render_preview", "Render Preview", class = "btn-info"),
            actionButton("apply", "Apply Settings", class = "btn-success"),
            modalButton("OK")
          )
        )
      )
    })

    # Reset settings to default values
    observeEvent(input$reset_wm, {
      # Reset the reactive values
      watermark_settings$fontsize <- default_settings$fontsize
      watermark_settings$col      <- default_settings$col
      watermark_settings$alpha    <- default_settings$alpha
      watermark_settings$rot      <- default_settings$rot
      watermark_settings$fontface <- default_settings$fontface
      watermark_settings$height   <- default_settings$height
      watermark_settings$width    <- default_settings$width

      # Update the modal input fields
      updateNumericInput(session, "fontsize", value = default_settings$fontsize)
      updateTextInput(session, "col",         value = default_settings$col)
      updateSliderInput(session, "alpha",     value = default_settings$alpha)
      updateNumericInput(session, "rot",      value = default_settings$rot)
      updateSelectInput(session, "fontface",  selected = default_settings$fontface)
      updateNumericInput(session, "height",   value = default_settings$height)
      updateNumericInput(session, "width",    value = default_settings$width)
    })

    # Apply new settings to update the watermark_settings reactive
    observeEvent(input$apply, {
      # Update the reactive values
      watermark_settings$fontsize <- input$fontsize
      watermark_settings$col      <- input$col
      watermark_settings$alpha    <- input$alpha
      watermark_settings$rot      <- input$rot
      watermark_settings$fontface <- input$fontface
      watermark_settings$height   <- input$height
      watermark_settings$width    <- input$width

      # Update the modal input fields
      updateNumericInput(session, "fontsize", value = input$fontsize)
      updateTextInput(session, "col",         value = input$col)
      updateSliderInput(session, "alpha",     value = input$alpha)
      updateNumericInput(session, "rot",      value = input$rot)
      updateSelectInput(session, "fontface",  selected = input$fontface)
      updateNumericInput(session, "height",   value = input$height)
      updateNumericInput(session, "width",    value = input$width)
    })

    # Render preview
    observeEvent(input$render_preview, {
      shiny::req(original_pdf())  # Ensure there is an original combined PDF
      shiny::req(combined_pdf())
      shiny::req(input$watermark_text)

      # Update the reactive values
      watermark_settings$fontsize <- input$fontsize
      watermark_settings$col      <- input$col
      watermark_settings$alpha    <- input$alpha
      watermark_settings$rot      <- input$rot
      watermark_settings$fontface <- input$fontface
      watermark_settings$height   <- input$height
      watermark_settings$width    <- input$width

      # Update the modal input fields
      updateNumericInput(session, "fontsize", value = input$fontsize)
      updateTextInput(session, "col",         value = input$col)
      updateSliderInput(session, "alpha",     value = input$alpha)
      updateNumericInput(session, "rot",      value = input$rot)
      updateSelectInput(session, "fontface",  selected = input$fontface)
      updateNumericInput(session, "height",   value = input$height)
      updateNumericInput(session, "width",    value = input$width)

      tmp_pdf <- watermark_stamp(input_pdf          = combined_pdf_nowm(),
                                 output_pdf         = file.path(temp_dir, paste0("preview_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf")),
                                 watermark_text     = input$watermark_text,
                                 watermark_fontsize = watermark_settings$fontsize,
                                 watermark_col      = watermark_settings$col,
                                 fallback_col       = defaultwm_col,
                                 watermark_alpha    = watermark_settings$alpha,
                                 watermark_rot      = watermark_settings$rot,
                                 watermark_fontface = watermark_settings$fontface,
                                 watermark_height   = watermark_settings$height,
                                 watermark_width    = watermark_settings$width)
      combined_pdf(tmp_pdf)
    })

    # Render preview from main UI
    observeEvent(input$render_preview2, {
      shiny::req(original_pdf())  # Ensure there is an original combined PDF
      shiny::req(combined_pdf())
      #shiny::req(input$watermark_text) # Want to show users if no text is applied too
      tmp_pdf <- watermark_stamp(input_pdf          = combined_pdf_nowm(),
                                 output_pdf         = file.path(temp_dir, paste0("preview_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf")),
                                 watermark_text     = input$watermark_text,
                                 watermark_fontsize = watermark_settings$fontsize,
                                 watermark_col      = watermark_settings$col,
                                 fallback_col       = defaultwm_col,
                                 watermark_alpha    = watermark_settings$alpha,
                                 watermark_rot      = watermark_settings$rot,
                                 watermark_fontface = watermark_settings$fontface,
                                 watermark_height   = watermark_settings$height,
                                 watermark_width    = watermark_settings$width)
      combined_pdf(tmp_pdf)
    })

    # Download UI
    output$download_ui <- renderUI({
      shiny::req(combined_pdf())
      downloadButton("download", "Download Updated PDF", class = "btn-primary")
    })

    output$download <- downloadHandler(
      # Dynamically set the file name
      filename = function() {
        # Use the user-provided name, or default to "updated_pdf_YYYY-MM-DD.pdf" if empty
        if (input$save_as_name == "" | is.null(input$save_as_name)) {
          paste0("updated_pdf_", Sys.Date(), ".pdf")
        } else {
          paste0(sanitize_filename(input$save_as_name), ".pdf")  # Append ".pdf" to the user-provided name
        }
      },

      content = function(file) {
        if(input$watermark_text == "" | is.null(input$watermark_text)) {
          pdf_to_save <- combined_pdf_nowm()
        } else {
          pdf_to_save <- watermark_stamp(input_pdf          = combined_pdf_nowm(), # using the no wm version for applying final wm
                                         output_pdf         = file.path(temp_dir, paste0("stamped_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf")),
                                         watermark_text     = input$watermark_text,
                                         watermark_fontsize = watermark_settings$fontsize,
                                         watermark_col      = watermark_settings$col,
                                         fallback_col       = defaultwm_col,
                                         watermark_alpha    = watermark_settings$alpha,
                                         watermark_rot      = watermark_settings$rot,
                                         watermark_fontface = watermark_settings$fontface,
                                         watermark_height   = watermark_settings$height,
                                         watermark_width    = watermark_settings$width)
          showNotification(paste0("Watermark (", input$watermark_text, ") applied!"), type = "message")
        }

        if(input$compress) {
          compressed_path <- file.path(temp_dir, paste0("compressed_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))
          showNotification("Compressing PDF File...", type = "message")
          pdf_compress(input = pdf_to_save, output = compressed_path, linearize = FALSE)
          # Compare file sizes
          #original_size <- file.info(pdf_to_save)$size / 1024 # Convert bytes to KB
          original_size <- original_file_sizes()
          compressed_size <- file.info(compressed_path)$size / 1024 # Convert bytes to KB
          space_saved <- original_size - compressed_size
          percentage_saved <- space_saved / original_size * 100
          cat("Original size(s):", original_size, "KB\n")
          cat("Compressed size:", compressed_size, "KB\n")
          cat("Space saved:", space_saved, "KB\n")
          cat("Percentage saved:", round(percentage_saved, 2), "%\n")
          showNotification(paste0("Original size(s): ", round(original_size), " KB, ",
                                  "Compressed size: ", round(compressed_size), " KB (",
                                  round(percentage_saved, 2), "% reduction)"), type = "message", duration = 15)
          if(space_saved > 0) {
            file.copy(compressed_path, file)
          } else {
            showNotification(paste0("Compression resulted in a larger file. Saving uncompressed version instead..."), type = "warning", duration = 10)
            file.copy(pdf_to_save, file)
          }

        } else {
          file.copy(pdf_to_save, file)
        }
      }
    )

    observeEvent(input$convert_btn, {
      shiny::req(combined_pdf())
      format <- input$convert_format
      converted_file <- NULL

      # Progress bar for conversion
      withProgress(message = paste("Converting PDF to", format, "..."), value = 0, {
        incProgress(0.2, detail = "Preparing conversion...")

        # Perform conversion based on selected format
        if (format == "Word (.docx)" && package_check("officer")) {
          converted_file <- file.path(temp_dir, "converted.docx")
          convert_to_word(combined_pdf(), converted_file)
          showNotification(paste("PDF converted to", format, "successfully!"), type = "message")

        } else if (format == "Excel (.xlsx)" && package_check("openxlsx")) {
          converted_file <- file.path(temp_dir, "converted.xlsx")
          convert_to_excel(combined_pdf(), converted_file)
          showNotification(paste("PDF converted to", format, "successfully!"), type = "message")

        } else if (format == "PowerPoint (.pptx)" && package_check("officer")) {
          converted_file <- file.path(temp_dir, "converted.pptx")
          convert_to_powerpoint(combined_pdf(), converted_file)
          showNotification(paste("PDF converted to", format, "successfully!"), type = "message")

        } else if (format == "Images (.png as a zip file)" && package_check("magick")) {
          converted_file <- convert_to_images(combined_pdf(), temp_dir, dpi = image_dpi)
          showNotification(paste("PDF converted to", format, "successfully!"), type = "message")
        }

        setProgress(0.9, detail = "Finalizing conversion...")

        output$download_conversion_ui <- renderUI({
          shiny::req(converted_file)
          downloadButton("download_conversion", paste0("Download ", format), class = "btn-primary") # requires a new variable here
        })

        output$download_conversion <- downloadHandler(
          filename = function() {
            basename(converted_file)
          },
          content = function(file) {
            file.copy(converted_file, file)
          }
        )
      }) # end of progress bar
    }) # end of convert_btn

    # Observe password-file upload
    observeEvent(input$unlock_btn, {
      shiny::req(input$locked_pdf)
      shiny::req(input$password)

      # Add the new PDFs to the list of uploaded locked PDFs
      current_locked_pdf <- list()

      for (i in seq_along(input$locked_pdf$name)) {
        current_locked_pdf[[input$locked_pdf$name[i]]] <- input$locked_pdf$datapath[i]
      }

      unlocked_file_path <- file.path(temp_dir, paste0("unlocked_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))

      # Remove the password and save as a regular PDF
      tryCatch({
        pdftools::pdf_combine(input = unlist(current_locked_pdf),
                              output = unlocked_file_path,
                              password = input$password)

      }, error = function(e) { # Fails gracefully and alert the user if password is incorrect
        showNotification(paste("PDF unlock failed. Wrong password?"), type = "error")
        unlocked_pdf(NULL)
      })

      # Check if the output file was created
      if (file.exists(unlocked_file_path)) {
        showNotification(paste("PDF unlocked successfully!"), type = "message")
        unlocked_pdf(unlocked_file_path)
      }
    })

    # Download UI for unlocked PDF
    output$download_unlocked_ui <- renderUI({
      shiny::req(unlocked_pdf())
      downloadButton("download_unlocked", "Download Unlocked PDF", class = "btn-primary")
    })

    output$download_unlocked <- downloadHandler(
      filename = function() paste0(sub("\\.pdf$", "", input$locked_pdf$name), "_unlocked.pdf"),
      content = function(file) file.copy(unlocked_pdf(), file)
    )

  } # end of server

  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}
