library(dplyr)
library(stringr)
library(kableExtra)

render_program_schedule <- function(program_csv, full_width = TRUE) {

  # ---- Read + clean program ----
  df <- read.csv(program_csv, stringsAsFactors = FALSE, na.strings = c("", "NA")) |>
    mutate(
      day        = as.integer(day),
      start_date = str_trim(coalesce(start_date, "")),
      time       = str_trim(time),
      type       = str_trim(type),
      author     = str_trim(coalesce(author, "")),
      title      = str_trim(coalesce(title, "")),
      info       = str_trim(coalesce(info, "")),
      color      = str_trim(coalesce(color, ""))
    ) |>
    arrange(day, time)

  # Optional: shorten repeated phrasing
  df$author <- gsub("Contributed\\s*from\\s*submitted\\s*abstracts", "Contributed abstracts", df$author)
  df$author <- gsub("Contributed\\s*until\\s*the\\s*beginning\\s*of\\s*the\\s*conference", "Contributed pre-conference", df$author)

  # ---- Day headers from CSV ----
  start_date_str <- df$start_date[which(nzchar(df$start_date))[1]]
  if (is.na(start_date_str) || !nzchar(start_date_str)) {
    stop("program_csv must contain a start_date value (YYYY-MM-DD) in at least one row.")
  }
  conference_start <- as.Date(start_date_str)
  if (is.na(conference_start)) {
    stop("start_date must be in ISO format YYYY-MM-DD, e.g. 2026-06-03.")
  }

  day_headers_df <- df |>
    distinct(day) |>
    arrange(day) |>
    mutate(
      date   = conference_start + (day - 1),
      header = paste0("Day ", day, " — ", format(date, "%a. %b %d, %Y"))
    )

  day_headers <- day_headers_df$header
  names(day_headers) <- as.character(day_headers_df$day)

  # Output table data
  df_out <- df |> select(time, type, author, title)

  # Row indices by day
  idx_by_day <- split(seq_len(nrow(df_out)), df$day)

  # ---- Build table ----
  tbl <- kbl(
    df_out,
    escape = TRUE,
    row.names = FALSE,
    col.names = c("Time", "Type", "Author", "Title")
  ) |>
    kable_material(full_width = full_width) |>
    column_spec(1, width = "12%") |>
    column_spec(2, width = "18%") |>
    column_spec(3, width = "28%") |>
    column_spec(4, width = "42%")

  # ---- Apply row colors from program.csv ----
  for (i in seq_len(nrow(df))) {
    bg <- df$color[i]
    if (!is.na(bg) && nzchar(bg)) {
      tbl <- tbl |> row_spec(i, background = bg)
    }
  }

  # ---- Group by day (only if that day exists) ----
  for (key in names(day_headers)) {
    if (!is.null(idx_by_day[[key]])) {
      tbl <- tbl |>
        pack_rows(
          day_headers[[key]],
          min(idx_by_day[[key]]),
          max(idx_by_day[[key]])
        )
    }
  }

  tbl
}
