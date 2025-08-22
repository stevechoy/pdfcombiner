# PDF Combiner

PDF Combiner is a simple <a href="https://lagom.shinyapps.io/PDF_Combiner/" target="_blank">R Shiny App</a> that allows merging of pdf files with page removal option.  
It is a **fast, free, and secure alternative** to commercial software such as Adobe Acrobat and/or various online websites which require users to sign-up, and it avoids any potential risks associated with uploading files elsewhere.    

Features include:  
- Supports multiple upload of PDFs, either one at a time or all at once  
- Select PDFs to be combined and change the order of files  
- Remove pages by either using commas (1,2,3), hyphens (5-10), or a combination of both (1,2,3,5-10)  
- Built-in PDF viewer / editor to verify changes  
- Experimental feature of PDF conversion into Word, Excel, PowerPoint, or .png images  

## Pre-requisites

To run PDF Combiner locally, you may need to install a few R packages:

``` r
install.packages(shiny)
install.packages(pdftools)
install.packages(magick)
# install.packages(officer)  # Optional, only used for PDF conversion
# install.packages(openxlsx) # Optional, only used for PDF conversion
```

## Running the App

Simply save the <a href="https://github.com/stevechoy/PDF_Combiner/blob/main/app.R" target="_blank">app.R</a> file locally and run the App in R.  

Alternatively, you may launch the App *directly* from R console (assuming you have the required packages listed above):

``` r
shiny::runGitHub("PDF_Combiner", username = "stevechoy", launch.browser = TRUE)
```
Instructions are included on the left hand side of the page.

![](example.png)

## Notes

- All PDF files are stored locally in a per-session temporary directory, given by the `tempdir()` function.  
- Each original PDF file name must be different from each other.  
