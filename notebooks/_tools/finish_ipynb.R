# =============================================================================
# finish_ipynb.R
#
# Runs after `quarto render notebooks --profile ipynb --to ipynb`. Quarto writes the
# notebooks with a Python kernel by default; here we mark them as R
# notebooks (IRkernel), so GitHub shows R syntax and Jupyter opens them with
# R. The generated files are placed alongside their Quarto sources.
# =============================================================================

library(jsonlite)
library(here)

here::i_am("notebooks/_tools/finish_ipynb.R")
from <- here("notebooks", "_notebook-build")
notebooks <- list.files(from, pattern = "\\.ipynb$", recursive = TRUE)
if (length(notebooks) == 0L) stop("No rendered notebooks found in ", from)

for (file in notebooks) {
  nb <- read_json(file.path(from, file), simplifyVector = FALSE)
  nb$metadata$kernelspec    <- list(display_name = "R", language = "R", name = "ir")
  nb$metadata$language_info <- list(name = "R", file_extension = ".r",
                                    mimetype = "text/x-r-source")
  first_code <- which(vapply(nb$cells, function(cell) cell$cell_type == "code", logical(1)))[1]
  source_path <- paste0("notebooks/", sub("\\.ipynb$", ".qmd", file))
  root_setup <- paste0("here::i_am(", encodeString(source_path, quote = '"'), ")\n")
  nb$cells[[first_code]]$source <- c(list(root_setup), nb$cells[[first_code]]$source)

  # links to other pages: page.qmd -> page.ipynb
  for (i in seq_along(nb$cells)) {
    if (nb$cells[[i]]$cell_type == "markdown") {
      source <- unlist(nb$cells[[i]]$source)
      nb$cells[[i]]$source <- as.list(gsub("\\.qmd\\)", ".ipynb)", source))
    }
  }
  destination <- here("notebooks", file)
  write_json(nb, destination, auto_unbox = TRUE, pretty = 1, null = "null")
}

unlink(from, recursive = TRUE)
cat("Notebooks written to notebooks/:", length(notebooks), "\n")
