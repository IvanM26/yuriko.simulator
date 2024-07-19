# Define the folder containing the R scripts and the output file
input_folder <- "R"
output_file <- "app/global.R"

# Get a list of all R script files in the input folder
r_files <- list.files(path = input_folder, pattern = "\\.R$", full.names = TRUE)

# Initialize an empty string to hold the concatenated content
all_content <- ""

# Loop through each file and read its content
for (file in r_files) {
  # Read the file content
  file_content <- readLines(file, warn = FALSE)
  
  # Add a header with the filename
  header <- paste0("# ---- Start of ", basename(file), " ----\n")
  footer <- paste0("\n# ---- End of ", basename(file), " ----\n\n")
  
  # Append the content to the all_content string
  all_content <- paste(all_content, header, paste(file_content, collapse = "\n"), footer, sep = "\n")
}

# Write the concatenated content to the output file
writeLines(all_content, con = output_file)
