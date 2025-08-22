# PDF Combiner

R Shiny App to allow merging of pdf files with page removal option.  

Features include:  
- Supports multiple upload of PDFs, either one at a time or all at once  
- Select PDFs to be combined and change the order of files  
- Remove pages by either using commas (1,2,3), or hyphens (5-10), or a combination of both (1,2,3,5-10)  
- Built-in PDF viewer / editor to verify changes  
- Experimental feature of PDF conversion into Word, Excel, PowerPoint, or .png images  

## Pre-requisites

To run PDF Combiner locally, you may need to install a few R packages:

``` r
install.packages(shiny)
install.packages(pdftools)
install.packages(magick)
install.packages(officer)  # Optional, required for PDF conversion
install.packages(openxlsx) # Optional, required for PDF conversion
```

## Running the App

Simply save the app.R file locally and run the App in R.  

Alternatively, you may run the App directly *without* installation (assuming you have the required packages):

``` r
shiny::runGitHub("PDF_Combiner", username = "stevechoy", launch.browser = TRUE)
```
Instructions are included on the left hand side of the page.

![](example.png)
