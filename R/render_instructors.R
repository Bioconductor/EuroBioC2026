render_instructors <- function(csv_path = "data/instructors.csv") {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  cat("#### Instructors\n\n")

  cat('<div class="grid instructor-grid">\n')

  for (i in seq_len(nrow(df))) {

    cat('<div class="g-col-12 g-col-sm-6 g-col-lg-4 g-col-xl-3">\n')

    cat('<div class="instructor-card">\n')

    cat(sprintf(
      '<div class="instructor-img-wrapper"><img src="%s" alt="%s"></div>\n',
      df$image[i], df$name[i]
    ))

    cat(sprintf("<h3>%s</h3>\n", df$name[i]))
    cat(sprintf("<div class='instructor-bio'>%s</div>\n", df$bio[i]))

    cat('</div>\n') # card
    cat('</div>\n') # column
  }

  cat('</div>\n') # grid
}
