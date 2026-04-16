library(dplyr)
library(stringr)
library(kableExtra)
library(htmltools)

render_program_schedule <- function(
    program_csv = "../data/program.csv",
    sessions_csv = "../data/sessions.csv",
    full_width = TRUE
) {

  # -----------------------------
  # Read program.csv
  # -----------------------------
  program <- read.csv(
    program_csv,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  ) |>
    mutate(
      date   = as.Date(date),
      time   = str_trim(coalesce(time, "")),
      type   = str_trim(coalesce(type, "")),
      author = str_trim(coalesce(author, "")),
      title  = str_trim(coalesce(title, "")),
      info   = str_trim(coalesce(info, "")),
      color  = str_trim(coalesce(color, ""))
    ) |>
    arrange(date, time)

  conference_start <- min(program$date, na.rm = TRUE)

  if (is.na(conference_start)) {
    stop("date must be in ISO format YYYY-MM-DD, e.g. 2026-06-03.")
  }

  program <- program |>
    mutate(day_num = as.integer(date - conference_start) + 1)

  day_headers_df <- program |>
    distinct(day_num, date) |>
    arrange(day_num) |>
    mutate(
      header = format(date, "%a. - %b. %d, '%y")
    )

  day_headers <- day_headers_df$header
  names(day_headers) <- as.character(day_headers_df$day_num)

  # -----------------------------
  # Read sessions.csv
  # -----------------------------
  sessions <- read.csv(
    sessions_csv,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  ) |>
    mutate(
      day       = str_trim(coalesce(day, "")),
      type      = str_trim(coalesce(type, "")),
      title     = str_trim(coalesce(title, "")),
      authors   = str_trim(coalesce(authors, "")),
      presenter = str_trim(coalesce(presenter, "")),
      abstract  = str_trim(coalesce(abstract, ""))
    )

  # Fill missing presenter from first author
  sessions <- sessions |>
    mutate(
      presenter = if_else(
        presenter == "",
        str_trim(vapply(
          strsplit(authors, ","),
          function(x) if (length(x) > 0) x[1] else "",
          character(1)
        )),
        presenter
      )
    )

  # Map short day names to actual dates
  day_map <- c(
    "Wed" = as.character(conference_start),
    "Thu" = as.character(conference_start + 1),
    "Fri" = as.character(conference_start + 2)
  )

  sessions <- sessions |>
    mutate(
      date = as.Date(unname(day_map[day])),
      type_norm = case_when(
        tolower(type) %in% c("short talk", "short talks") ~ "Short talks",
        tolower(type) %in% c("workshop", "workshops") ~ "Workshops",
        tolower(type) %in% c("flash talk", "flash talks") ~ "Flash talks",
        tolower(type) %in% c("poster", "poster pitch", "poster pitches", "poster session") ~ "Poster session",
        tolower(type) %in% c("bof", "bof session", "bof sessions") ~ "BoF sessions",
        tolower(type) == "keynote" ~ "Keynote",
        TRUE ~ tools::toTitleCase(type)
      )
    )

  # -----------------------------
  # Collapsible title HTML
  # -----------------------------
  make_collapsible_title <- function(title, authors, abstract) {
    title_esc <- htmlEscape(title)
    authors_esc <- htmlEscape(authors)
    abstract_esc <- htmlEscape(abstract)

    if (abstract == "") {
      return(title_esc)
    }

    paste0(
      "<details class='schedule-details'>",
      "<summary>",
      title_esc,
      "</summary>",
      "<div class='schedule-details-body'>",
      if (authors != "") {
        paste0("<div class='schedule-details-authors'>Author(s): ", authors_esc, "</div>")
      } else "",
      "<div>", abstract_esc, "</div>",
      "</div>",
      "</details>"
    )
  }

  # -----------------------------
  # Build one combined table
  # -----------------------------
  out_rows <- list()
  out_bg <- character()

  add_row <- function(time = "", type = "", author = "", title = "", bg = "") {
    out_rows[[length(out_rows) + 1]] <<- data.frame(
      Time = time,
      Type = type,
      Author = author,
      Title = title,
      stringsAsFactors = FALSE
    )
    out_bg[length(out_bg) + 1] <<- bg
  }

  for (i in seq_len(nrow(program))) {
    pr <- program[i, ]

    # Main schedule row
    add_row(
      time = pr$time,
      type = pr$type,
      author = pr$author,
      title = pr$title,
      bg = pr$color
    )

    # Matching detailed rows from sessions.csv
    matching_sessions <- sessions |>
      filter(date == pr$date, type_norm == pr$type)

    if (nrow(matching_sessions) > 0) {
      matching_sessions <- matching_sessions |>
        arrange(presenter, title)

      for (j in seq_len(nrow(matching_sessions))) {
        ss <- matching_sessions[j, ]

        add_row(
          time = "",
          type = "",
          author = htmlEscape(ss$presenter),
          title = make_collapsible_title(ss$title, ss$authors, ss$abstract),
          bg = pr$color
        )
      }
    }
  }

  df_out <- bind_rows(out_rows)

  # -----------------------------
  # Group row indices by day
  # -----------------------------
  row_counts_per_program_row <- integer(nrow(program))

  for (i in seq_len(nrow(program))) {
    pr <- program[i, ]
    n_details <- sessions |>
      filter(date == pr$date, type_norm == pr$type) |>
      nrow()
    row_counts_per_program_row[i] <- 1 + n_details
  }

  idx_by_day <- split(
    seq_len(nrow(df_out)),
    rep(program$day_num, times = row_counts_per_program_row)
  )

  # -----------------------------
  # Render table
  # -----------------------------
  tbl <- kbl(
    df_out,
    escape = FALSE,
    row.names = FALSE,
    col.names = c("TIME", "TYPE", "AUTHOR", "TITLE")
  ) |>
    kable_material(full_width = full_width) |>
    column_spec(1, width = "12%") |>
    column_spec(2, width = "18%") |>
    column_spec(3, width = "26%") |>
    column_spec(4, width = "44%")

  # Apply row backgrounds only to main program rows
  for (i in seq_len(nrow(df_out))) {
    if (!is.na(out_bg[i]) && nzchar(out_bg[i])) {
      tbl <- tbl |> row_spec(i, background = out_bg[i])
    }
  }

  # Group rows by day
  for (key in names(day_headers)) {
    rows_this_day <- idx_by_day[[key]]
    if (!is.null(rows_this_day) && length(rows_this_day) > 0) {
      tbl <- tbl |>
        pack_rows(
          day_headers[[key]],
          min(rows_this_day),
          max(rows_this_day)
        )
    }
  }

  tbl |> cat()
}
