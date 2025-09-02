# Requries the hexSticker package
library("hexSticker")

# Create the hex logo
tmp <- sticker(
  subplot = "test8-removebg.png", 
  package = "pdfcombiner", 
  p_color = "#000000", 
  p_size = 16, # Adjust the size of the package title
  p_x = 1, # Center the package title horizontally
  p_y = 0.58, # Position the package title slightly above the bottom
  h_fill = "#E7E7E7", # Adobe Grey for the background
  h_color = "#F40F02", # Adobe Red for the border
  s_x = 1, # Center the image horizontally
  s_y = 1.13, # Move the image slightly higher to avoid text overlap
  s_width = 0.95, # Make the image slightly wider
  s_height = 0.95, # Make the image slightly taller
  filename = "logof.png" # Output file name
)

# print(tmp)
