if (interactive()) {
  suppressPackageStartupMessages(
    if (!requireNamespace('pacman')) {
      install.packages('pacman', dependencies = TRUE, quiet = TRUE)
    }
  )
  pacman::p_load(conflicted, usethis)
  pacman::p_load_gh('ChrisDonovan307/projecter')

  suppressMessages(require(devtools))

  # Set up vim function for kj escape
  # try(vim <- function() rstudiovim::rsvim_exec_file())

  # Set conflict winners
  conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    dplyr::rename(),
    dplyr::summarize(),
    .quiet = TRUE
  )
}

options(
  max.print = 950,
  pillar.print_max = 950,
  pillar.print_min = 950
)

source("renv/activate.R")
