setwd("/Users/diego.lopes/Library/CloudStorage/OneDrive-Personal/R")

# Install and load the necessary R packages
install.packages("pdftools")
library(pdftools) 

# Path to the PDF
pdf_path <- "/Users/diego.lopes/Library/CloudStorage/OneDrive-Personal/R/1stcom/1967/1538.pdf"

# Extract text from the PDF page-by-page
pdf_text_list <- pdf_text(pdf_path)

# Function to split text based on middle whitespace (assumes roughly even columns)
split_columns <- function(page_text) {
  lines <- unlist(strsplit(page_text, split = "\n"))
  left_column <- c()
  right_column <- c()
  
  for (line in lines) {
    midpoint <- nchar(line) %/% 2
    left_side <- substr(line, 1, midpoint)
    right_side <- substr(line, midpoint + 1, nchar(line))
    left_column <- c(left_column, left_side)
    right_column <- c(right_column, right_side)
  }
  
  c(left_column, right_column)
}

# Apply the function to each page
split_text <- unlist(lapply(pdf_text_list, split_columns))

# Combine split text into one coherent text
coherent_text <- paste(split_text, collapse = "\n")

# Eliminate consecutive newline characters
coherent_text <- gsub("\n{2,}", "\n", coherent_text)

# Preview the extracted text
cat(substr(coherent_text, 1, 1000))  # Display the first 1000 characters

coherent_text

# Export the coherent_text to a .txt file
writeLines(coherent_text, "coherent_text.txt")

coherent_text
