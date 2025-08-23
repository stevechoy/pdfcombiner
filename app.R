### PDF Combiner - Options #####################################################

max_file_size    <- 300 # max file size in MB, change if needed

### Setup ######################################################################

library(shiny)
library(pdftools)
library(magick)

options(shiny.maxRequestSize = max_file_size * 1024^2)

# Check if shinythemes is installed, optional
if (requireNamespace("shinythemes", quietly = TRUE)) {
  library(shinythemes)
  app_theme <- shinytheme("flatly")
} else {
  app_theme <- NULL  # Default to no theme if shinythemes is not installed
}

### Functions ##################################################################

package_check <- function(pkg_name) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    showNotification(
      paste0("The '", pkg_name, "' package is not installed. Please install it to use this feature."),
      type = "error"
    )
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
  pdf_images <- image_read_pdf(pdf_path, density = dpi)
  
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
    image_write(page_image, path = png_path, format = "png")
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

### Shiny App ##################################################################

# UI
ui <- fluidPage(
  theme = app_theme,
  tags$head(tags$title("PDF Combiner")),
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      h4("PDF Combiner - Instructions"),
      p("1. Upload PDF file(s). All files will be combined automatically by default."),
      p("2. Use the file selector to choose which files to include, and click the 'Update / Combine PDF' button."),
      tags$div(
        tags$p("3. Use the page removal option to remove unwanted pages from the combined PDF:"),
        tags$p("- Enter page numbers separated by commas (e.g., `1,2,3`).", style = "text-indent: 20px;"),
        tags$p("- Use hyphens for ranges (e.g., `5-10`).", style = "text-indent: 20px;"),
        tags$p("- Both options can be used together (e.g., `1,2,3,5-10`).", style = "text-indent: 20px;"),
        tags$p("4. Verify changes on the right and download the updated PDF.")
      ),
      br(),
      
      fileInput("pdf_files", paste0("Upload PDF File(s): [Max ", max_file_size, " MB]"), multiple = TRUE, accept = ".pdf"),  # Allow multiple file uploads
      
      # Selector for choosing PDFs to combine
      selectInput("selected_pdfs", "Select PDFs to Combine (in order):", choices = NULL, multiple = TRUE),
      
      # Combine PDF button directly below the selector
      actionButton("combine_btn", "Update / Combine PDF", style = "margin-top: 0px;"),
      
      tags$hr(style = "border: 2px solid #ccc;"), # Grey divider line
      
      # Page removal input
      textInput("remove_pages", "Enter (Current) Page Numbers to Remove:", value = ""),
      
      # Buttons side by side with 20px gap
      div(
        style = "display: flex; align-items: center; gap: 20px;",
        actionButton("remove_pages_btn", 
                     label = tagList(icon("xmark", class = "fa-lg"), "Remove Pages")),  # Remove Pages button with xmark icon
        actionButton("reset_btn", 
                     label = tagList(icon("sync-alt", class = "fa-lg"), "Reset")),  # Reset button with refresh icon
        uiOutput("download_ui")
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
      tags$p("Author: Steve Choy (v1.3)",
             a(href = "https://github.com/stevechoy/PDF_Combiner", "(GitHub Repo)", target = "_blank"),
             style = "font-size: 0.9em; color: #555; text-align: left;")
    ), # end of sidebarPanel
    
    mainPanel(
      width = 7,
      htmlOutput("pdfviewer")  # Embedded PDF viewer
    )
  ) # end of sidebarLayout
) # end of ui

# Server
server <- function(input, output, session) {
  # Reactive values to store uploaded PDFs and their original names
  uploaded_pdfs <- reactiveVal(list())  # List to store uploaded PDFs
  combined_pdf <- reactiveVal(NULL)     # Stores the combined PDF path
  original_pdf <- reactiveVal(NULL)     # Stores the original combined PDF path
  temp_dir <- tempdir()                 # Temporary directory for storing combined PDFs
  shiny::addResourcePath("pdfs", temp_dir)  # Serve files from the temp directory
  
  # Helper function to combine PDFs
  combine_pdfs <- function(pdf_paths, output_path) {
    pdf_combine(unlist(pdf_paths), output = output_path)
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
      current_pdfs[[input$pdf_files$name[i]]] <- input$pdf_files$datapath[i]
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
          var availableHeight = windowHeight - iframeTop - 20; // Add some padding (20px)
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
    pdf_subset(pdf_path, pages = pages_to_keep, output = updated_pdf_path)
    combined_pdf(updated_pdf_path)  # Update the combined PDF
    
    # Debugging: Print updated PDF path
    print(paste("Updated PDF Path:", updated_pdf_path))
    showNotification("Pages removed successfully!", type = "message")
  })
  
  # Reset Button
  observeEvent(input$reset_btn, {
    shiny::req(original_pdf())  # Ensure there is an original combined PDF
    combined_pdf(original_pdf())  # Restore the original combined PDF
    updateTextInput(session, "remove_pages", value = "")  # Reset page removal input
    showNotification("Page reset successful! Restored to the current list of PDFs.", type = "message")
  })
  
  # Download UI
  output$download_ui <- renderUI({
    shiny::req(combined_pdf())
    downloadButton("download", "Download Updated PDF")
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("updated_pdf_", Sys.Date(), ".pdf"),
    content = function(file) file.copy(combined_pdf(), file)
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
        
      } else if (format == "Images (.png as a zip file)") {
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
} # end of server

shinyApp(ui = ui, server = server)