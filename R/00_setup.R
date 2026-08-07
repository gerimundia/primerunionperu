
# ============================================================================
# File structure
# This script must be executed just once. 
# ============================================================================


files <- c("R", "data", "output")

for (file in files) {
  if (!dir.exists(file)) {
    dir.create(file, recursive = TRUE)
    message("Carpeta generada: ", file)
  } else {
    message("Carpeta ya incluida en el directorio: ", file)
  }
}