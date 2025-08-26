### PDF Combiner - Options #####################################################

max_file_size      <- 500      # max file size in MB, change if needed
bootstrap_theme    <- FALSE     # When TRUE, uses bslib bootstrap theme to allow minimizing sidebar
sidebar_width      <- 700      # Only applicable when bootstrap theme is used, in pixels
watermark_fontsize <- 50       # Watermark font size
watermark_col      <- "gray80" # Watermark color
watermark_alpha    <- 0.6      # Watermark alpha

### Setup ######################################################################

library(shiny)
library(pdftools)

options(shiny.maxRequestSize = max_file_size * 1024^2)

# Check if shinythemes is installed, optional
if (requireNamespace("shinythemes", quietly = TRUE)) {
  library(shinythemes)
  app_theme <- shinytheme("flatly")
} else {
  app_theme <- NULL  # Default to no theme if shinythemes is not installed
}

# Check if staplr is installed, recommended for handling bookmarks
if (requireNamespace("staplr", quietly = TRUE)) {
  library(rJava) # If running this line fails, that means you need to install Java separately
  library(staplr)
} 
### Functions ##################################################################

package_check <- function(pkg_name, bookmarks = FALSE, silent = FALSE) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    if(!silent) {
      if(bookmarks) {
        showNotification(
          paste0("The '", pkg_name, "' package is not installed. Please install it to retain bookmarks."),
          type = "warning"
        )      
      } else {
        showNotification(
          paste0("The '", pkg_name, "' package is not installed. Please install it to use this feature."),
          type = "error"
        )
      }
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}

convert_to_word <- function(pdf_path, output_path) {
  # Extract text from the PDF
  pdf_text <- pdf_text(pdf_path)
  
  # Create a Word document
  doc <- officer::read_docx()
  
  # Add extracted text to the Word document
  for (page in seq_along(pdf_text)) {
    doc <- officer::body_add_par(doc, paste0("Page ", page), style = "heading 1")
    doc <- officer::body_add_par(doc, pdf_text[page], style = "Normal")
  }
  
  # Save the Word document
  print(doc, target = output_path)
}

convert_to_excel <- function(pdf_path, output_path) {
  # Extract text from the PDF
  pdf_text <- pdf_text(pdf_path)
  
  # Create an Excel workbook
  wb <- openxlsx::createWorkbook()
  
  # Add each page's text to a separate sheet
  for (page in seq_along(pdf_text)) {
    openxlsx::addWorksheet(wb, paste0("Page ", page))
    openxlsx::writeData(wb, paste0("Page ", page), pdf_text[page])
  }
  
  # Save the Excel workbook
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}

convert_to_powerpoint <- function(pdf_path, output_path) {
  # Extract text from the PDF
  pdf_text <- pdf_text(pdf_path)
  
  # Create a PowerPoint presentation
  ppt <- officer::read_pptx()
  
  # Add each page's text to a separate slide
  for (page in seq_along(pdf_text)) {
    ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
    ppt <- officer::ph_with(ppt, paste0("Page ", page), location = officer::ph_location_type(type = "title"))
    ppt <- officer::ph_with(ppt, pdf_text[page], location = officer::ph_location_type(type = "body"))
  }
  
  # Save the PowerPoint presentation
  print(ppt, target = output_path)
}

convert_to_images <- function(pdf_path, output_dir, dpi = 300) {
  # Read the PDF as an image object
  pdf_images <- magick::image_read_pdf(pdf_path, density = dpi)
  
  # Get the total number of pages
  total_pages <- length(pdf_images)
  
  # Initialize a vector to store the paths of the PNG files
  png_files <- c()
  
  # Loop through each page and save it as a PNG file
  for (i in seq_len(total_pages)) {
    # Extract the current page as an image
    page_image <- pdf_images[i]
    
    # Define the output file path
    png_path <- file.path(output_dir, paste0("page_", i, ".png"))
    
    # Save the image as a PNG file
    magick::image_write(page_image, path = png_path, format = "png")
    png_files <- c(png_files, png_path)
  }
  
  # If there are multiple pages, compress them into a ZIP file
  if (length(png_files) > 1) {
    zip_path <- file.path(output_dir, "images.zip")
    utils::zip(zipfile = zip_path, files = png_files)
    return(zip_path)
  } else if (length(png_files) == 1) {
    return(png_files[1])  # Return single PNG file if only one page
  } else {
    stop("No pages were successfully converted to images.")
  }
}

parse_pages_to_remove <- function(input_string) {
  parts <- unlist(strsplit(input_string, ",")) # Split by commas
  pages <- unlist(lapply(parts, function(part) {
    if (grepl("-", part)) { # Handle ranges (e.g., "5-10")
      range <- as.numeric(unlist(strsplit(part, "-")))
      if (length(range) == 2 && !any(is.na(range))) seq(range[1], range[2])
    } else { # Handle single page numbers
      as.numeric(part)
    }
  }))
  unique(pages[!is.na(pages)])
}

magick_formats <- c(".pdf", ".png", ".jpeg", ".jpg", ".bmp", ".gif", ".tiff",
                    ".tif", ".webp", ".ico", ".heic", ".heif", ".svg", ".eps")

sanitize_filename <- function(filename) {
  # Define illegal characters
  illegal_chars <- "[\\\\/:*?\"<>|]"
  
  # Check if the filename contains illegal characters
  if (grepl(illegal_chars, filename)) {
    # Remove illegal characters from the filename
    sanitized_filename <- gsub(illegal_chars, "_", filename)
    # Notify the user about illegal characters
    shiny::showNotification(paste0("Illegal characters are replaced with underscores (new filename: ", sanitized_filename, ".pdf)."),
                            type = "warning", duration = 12)
    return(sanitized_filename)
  }
  
  # If no illegal characters, return the original filename
  return(filename)
}

watermark_stamp <- function(input_pdf,
                            output_pdf,
                            watermark_text,
                            watermark_fontsize = 50,
                            watermark_col = "gray80",
                            watermark_alpha = 0.6) {
  
  # Create a temporary PDF with the watermark text
  temp_watermark_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_watermark_pdf, width = 8.5, height = 11)  # Standard US Letter size, i.e. approximately 216 mm by 279 mm, is different than A4 at 210 mm by 297 mm
  grid::grid.text(
    label = watermark_text,
    x = 0.5, y = 0.5, gp = grid::gpar(fontsize = watermark_fontsize, col = watermark_col, alpha = watermark_alpha, fontface = "bold"),
    rot = 45  # Rotate the watermark text
  )
  dev.off()
  
  # Overlay the watermark PDF onto the input PDF (preserves bookmarks)
  qpdf::pdf_overlay_stamp(
    input = input_pdf,
    stamp = temp_watermark_pdf,
    output = output_pdf
  )
  
  # Clean up the temporary watermark PDF
  unlink(temp_watermark_pdf)
  
  # Return the path to the output PDF
  return(output_pdf)
}

page_placeholder_text <- "Separate by commas (e.g. `1,2,3`), use hyphens for ranges (e.g. `5-10`), or do both."

### Shiny App ##################################################################

# UI
ui <- if (requireNamespace("bslib", quietly = TRUE) && bootstrap_theme) { # Loads a bootstrap UI if bslib is installed
  library(bslib)
  page_sidebar(
    theme = bs_theme(version = 5, # Use Bootstrap 5
                     preset = "flatly", # cerulean
                     font_scale = 1), 
    sidebar = sidebar(
      width = sidebar_width,
      title = tags$span("PDF Combiner", style = "font-size: 20px; font-weight: bold;"), # Title text styling
      p("1. Upload PDF or Image file(s). All files will be combined automatically by default."),
      p("2. Use the file selector to choose which files to include, and click the 'Update / Combine PDF' button (delete unwanted files, order matters)."),
      p("3. Verify changes on the right and download the updated PDF."),
      p(HTML("<strong>Note:</strong> You can also apply any optional features below, <em>before</em> downloading the updated PDF.")),
      
      card(card_header("Input Files"),
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
                                 title = "Performs lossless compression when saved. Space saved will be shown on the bottom right."
                               )
                             ),
                             value = TRUE
               )
             ) # end of column
           ) # end of fluidRow for Combine button
      ), # end of card
      
      card(card_header("Page Editor"),
           # Custom text with inline question mark tooltip
           tags$div(
             style = "display: flex; align-items: center;",  # Align label and input inline
             tags$label(
               "Enter (Current) Page Numbers to Remove: ",
               tags$span(
                 "?",
                 style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
                 title = "Note: Bookmarks will not be retained when pages are removed."
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
                 title = "Applies a grey see-through text across all pages of the PDF when downloaded."
               )
             )
           ),
           
           # Watermark input
           textInput("watermark_text", label = NULL, value = "", placeholder = "e.g. 'For Internal Use Only'")
           
      ), # end of card
      
      card(card_header("Remove Password Protection"),
           
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
      
      card(card_header("PDF Conversion"),
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
      
      tags$p("Author: Steve Choy (v1.9.1)",
             a(href = "https://github.com/stevechoy/PDF_Combiner", "(GitHub Repo)", target = "_blank"),
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
                              title = "Performs lossless compression when saved. Space saved will be shown on the bottom right."
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
            "Enter (Current) Page Numbers to Remove: ",
            tags$span(
              "?",
              style = "color: blue; cursor: help; font-weight: bold; margin-left: 5px; font-size: 20px;",
              title = "Note: Bookmarks will not be retained when pages are removed."
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
              title = "Applies a grey see-through text across all pages of the PDF when downloaded."
            )
          )
        ),
        
        # Watermark input
        textInput("watermark_text", label = NULL, value = "", placeholder = "e.g. 'For Internal Use Only'"),
        
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
        tags$p("Author: Steve Choy (v1.9.1)",
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
  
  # Helper function to combine PDFs
  combine_pdfs <- function(pdf_paths, output_path) {
    if(package_check("staplr", bookmarks = TRUE)) {
      staplr::staple_pdf(input_files = unlist(pdf_paths), output_filepath = output_path)
    } else {
      pdf_combine(unlist(pdf_paths), output = output_path)
    }
    combined_pdf(output_path)
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
  
  # Remove Pages
  observeEvent(input$remove_pages_btn, {
    shiny::req(combined_pdf(), input$remove_pages)
    
    # Parse the page numbers to remove
    pages_to_remove <- parse_pages_to_remove(input$remove_pages)
    
    # Debugging: Print pages to remove
    print(paste("Pages to Remove:", paste(pages_to_remove, collapse = ", ")))
    
    # Read the combined PDF
    pdf_path <- combined_pdf()
    total_pages <- pdf_info(pdf_path)$pages
    
    # Validate page numbers
    if (any(pages_to_remove < 1 | pages_to_remove > total_pages)) {
      showNotification("Invalid page numbers. Please enter valid page numbers within the range of the PDF.", type = "error")
      return()
    }
    
    # Keep only the pages that are not in the removal list
    pages_to_keep <- setdiff(seq_len(total_pages), pages_to_remove)
    
    # Check if there are any pages left to keep
    if (length(pages_to_keep) == 0) {
      showNotification("Cannot remove all pages from the PDF. At least one page must remain.", type = "error")
      return()
    }
    
    # Create a new PDF with the remaining pages
    updated_pdf_path <- file.path(temp_dir, paste0("updated_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))
    
    #if(package_check("staplr", silent = TRUE)) {
    #  staplr::select_pages(selpages = pages_to_keep, input_filepath = pdf_path, output_filepath = updated_pdf_path)
    #} else {
    pdf_subset(pdf_path, pages = pages_to_keep, output = updated_pdf_path)
    #}
    combined_pdf(updated_pdf_path)  # Update the combined PDF
    
    # Debugging: Print updated PDF path
    print(paste("Updated PDF Path:", updated_pdf_path))
    showNotification("Pages removed successfully!", type = "message")
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
    qpdf::pdf_rotate_pages(pdf_path, pages = pages_to_rotate, angle = 270, relative = TRUE, output = updated_pdf_path)
    #}
    combined_pdf(updated_pdf_path)  # Update the combined PDF
    
    # Debugging: Print updated PDF path
    print(paste("Updated PDF Path:", updated_pdf_path))
    showNotification("Pages rotated successfully!", type = "message")
  })
  
  # Reset Button
  observeEvent(input$reset_btn, {
    shiny::req(original_pdf())  # Ensure there is an original combined PDF
    combined_pdf(original_pdf())  # Restore the original combined PDF
    updateTextInput(session, "remove_pages", value = "")  # Reset page removal input
    showNotification("Page reset successful! Restored to the current list of PDFs.", type = "message")
  })
  
  # Reset Button
  observeEvent(input$reset_btn_rot, {
    shiny::req(original_pdf())  # Ensure there is an original combined PDF
    combined_pdf(original_pdf())  # Restore the original combined PDF
    updateTextInput(session, "rotate_pages", value = "")  # Reset page removal input
    showNotification("Page reset successful! Restored to the current list of PDFs.", type = "message")
  })
  
  # Download UI
  output$download_ui <- renderUI({
    shiny::req(combined_pdf())
    downloadButton("download", "Download Updated PDF")
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
        pdf_to_save <- combined_pdf()
      } else {
        pdf_to_save <- watermark_stamp(input_pdf          = combined_pdf(),
                                       output_pdf         = file.path(temp_dir, paste0("stamped_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf")),
                                       watermark_text     = input$watermark_text,
                                       watermark_fontsize = watermark_fontsize,
                                       watermark_col      = watermark_col,
                                       watermark_alpha    = watermark_alpha)
        showNotification("Watermark applied!", type = "message")
      }
      
      if(input$compress) {
        compressed_path <- file.path(temp_dir, paste0("compressed_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf"))
        showNotification("Compressing PDF File...", type = "message")
        pdf_compress(input = pdf_to_save, output = compressed_path, linearize = FALSE)
        # Compare file sizes
        original_size <- file.info(pdf_to_save)$size / 1024 # Convert bytes to KB
        compressed_size <- file.info(compressed_path)$size / 1024 # Convert bytes to KB
        space_saved <- original_size - compressed_size
        percentage_saved <- space_saved / original_size * 100
        cat("Original size:", original_size, "KB\n")
        cat("Compressed size:", compressed_size, "KB\n")
        cat("Space saved:", space_saved, "KB\n")
        cat("Percentage saved:", round(percentage_saved, 2), "%\n")
        showNotification(paste0("Original size: ", round(original_size), " KB, ",
                                "Compressed size: ", round(compressed_size), " KB (",
                                round(percentage_saved, 2), "% reduction)"), type = "message", duration = 15)
        file.copy(compressed_path, file)
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
        converted_file <- convert_to_images(combined_pdf(), temp_dir)
        showNotification(paste("PDF converted to", format, "successfully!"), type = "message")
      }
      
      incProgress(0.8, detail = "Finalizing conversion...")
      
      output$download_conversion_ui <- renderUI({
        shiny::req(converted_file)
        downloadButton("download_conversion", paste0("Download ", format)) # requires a new variable here
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
    downloadButton("download_unlocked", "Download Unlocked PDF")
  })
  
  output$download_unlocked <- downloadHandler(
    filename = function() paste0(sub("\\.pdf$", "", input$locked_pdf$name), "_unlocked.pdf"),
    content = function(file) file.copy(unlocked_pdf(), file)
  )
  
} # end of server

shinyApp(ui = ui, server = server)