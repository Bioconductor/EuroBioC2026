render_instructors <- function(csv_path = "data/instructors.csv") {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  cat("#### Instructors\n\n")
  cat("::: {.grid}\n\n")

  for (i in seq_len(nrow(df))) {

    cat("::: {.g-col-12 .g-col-md-6 .g-col-lg-4}\n")

    cat(sprintf(
      "![](%s){fig-alt=\"%s\" class=\"instructor-img\"}\n\n",
      df$image[i], df$name[i]
    ))

    cat(sprintf("**%s**  \n", df$name[i]))
    cat(sprintf("%s\n", df$bio[i]))

    cat(":::\n\n")
  }

  cat(":::\n")
}
