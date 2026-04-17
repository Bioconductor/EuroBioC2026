library(dplyr)
library(stringr)
library(kableExtra)
library(htmltools)

# =========================================================
# Helpers
# =========================================================

fill_presenter <- function(df) {
  df |>
    mutate(
      presenter = if_else(
        str_trim(coalesce(presenter, "")) == "",
        str_trim(vapply(
          strsplit(coalesce(authors, ""), ","),
          function(x) if (length(x) > 0) x[1] else "",
          character(1)
        )),
        str_trim(coalesce(presenter, ""))
      )
    )
}

normalize_type <- function(x) {
  case_when(
    tolower(x) %in% c("keynote", "keynotes") ~ "Keynote",
    tolower(x) %in% c("short talk", "short talks") ~ "Short talks",
    tolower(x) %in% c("flash talk", "flash talks") ~ "Flash talks",
    tolower(x) %in% c("workshop", "workshops") ~ "Workshops",
    tolower(x) %in% c("bof", "bof session", "bof sessions") ~ "BoF sessions",
    tolower(x) %in% c("poster pitch", "poster pitches") ~ "Poster pitches",
    tolower(x) %in% c("poster", "posters", "poster session") ~ "Poster session",
    TRUE ~ tools::toTitleCase(x)
  )
}

make_collapsible_title <- function(title, authors = "", abstract = "") {
  title_esc <- htmlEscape(coalesce(title, ""))
  authors_esc <- htmlEscape(coalesce(authors, ""))
  abstract_esc <- htmlEscape(coalesce(abstract, ""))

  if (isTRUE(abstract == "") || is.na(abstract)) {
    return(title_esc)
  }

  paste0(
    "<details class='schedule-details'>",
    "<summary>",
    title_esc,
    "</summary>",
    "<div class='schedule-details-body'>",
    if (!isTRUE(authors == "") && !is.na(authors)) {
      paste0("<div class='schedule-details-authors'>Author(s): ", authors_esc, "</div>")
    } else "",
    "<div>", abstract_esc, "</div>",
    "</div>",
    "</details>"
  )
}

parse_hm <- function(x) {
  x <- str_trim(coalesce(x, ""))
  out <- suppressWarnings(as.POSIXct(x, format = "%H:%M", tz = "UTC"))
  as.numeric(format(out, "%H")) * 60 + as.numeric(format(out, "%M"))
}

# =========================================================
# 1) Old/simple schedule from program.csv only
# =========================================================
render_program_schedule <- function(
    program_csv = "../data/program.csv",
    full_width = TRUE
) {

  df <- read.csv(
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

  conference_start <- min(df$date, na.rm = TRUE)

  if (is.na(conference_start)) {
    stop("date must be in ISO format YYYY-MM-DD, e.g. 2026-06-03.")
  }

  df <- df |>
    mutate(day = as.integer(date - conference_start) + 1)

  day_headers_df <- df |>
    distinct(day, date) |>
    arrange(day, date) |>
    mutate(
      header = format(date, "%a. - %b. %d, '%y")
    )

  day_headers <- day_headers_df$header
  names(day_headers) <- as.character(day_headers_df$day)

  df_out <- df |>
    select(time, type, author, title)

  idx_by_day <- split(seq_len(nrow(df_out)), df$day)

  tbl <- kbl(
    df_out,
    escape = TRUE,
    row.names = FALSE,
    col.names = c("TIME", "TYPE", "AUTHOR", "TITLE")
  ) |>
    kable_material(full_width = full_width) |>
    column_spec(1, width = "12%") |>
    column_spec(2, width = "18%") |>
    column_spec(3, width = "28%") |>
    column_spec(4, width = "42%")

  for (i in seq_len(nrow(df))) {
    bg <- df$color[i]
    if (!is.na(bg) && nzchar(bg)) {
      tbl <- tbl |> row_spec(i, background = bg)
    }
  }

  for (key in names(day_headers)) {
    rows_this_day <- idx_by_day[[key]]
    if (!is.null(rows_this_day) && length(rows_this_day) > 0) {
      tbl <- tbl |>
        pack_rows(day_headers[[key]], min(rows_this_day), max(rows_this_day))
    }
  }

  tbl |> cat()
}

# =========================================================
# Posters section appended after the combined schedule
# If poster day is missing, render one single table without day title
# If poster day exists, use program order and alphabetical author order
# =========================================================
render_posters_section <- function(
    sessions,
    day_header_map,
    full_width = TRUE
) {
  posters <- sessions |>
    filter(type_norm %in% c("Poster", "Poster session"))

  if (nrow(posters) == 0) {
    return(invisible(NULL))
  }

  cat("<h2 style='margin-top: 2.5em; margin-bottom: 0.4em;'>POSTERS</h2>\n")
  cat("<hr style='margin-top: 0; margin-bottom: 1.2em;'>\n")
  cat("<p>(In alphabetical order.)</p>\n")

  posters_with_day <- posters |>
    filter(!is.na(day) & str_trim(day) != "")

  posters_without_day <- posters |>
    filter(is.na(day) | str_trim(day) == "")

  # -------------------------
  # Undated posters: one single table, no day title
  # -------------------------
  if (nrow(posters_without_day) > 0) {
    undated_df <- posters_without_day |>
      arrange(presenter, title) |>
      mutate(idx = seq_len(n()))

    out <- undated_df |>
      mutate(
        Author = paste0(idx, " ", htmlEscape(presenter)),
        Title = mapply(
          make_collapsible_title,
          title,
          authors,
          abstract,
          USE.NAMES = FALSE
        )
      ) |>
      select(Author, Title)

    tbl_posters <- kbl(
      out,
      escape = FALSE,
      row.names = FALSE,
      col.names = c("# AUTHOR", "TITLE")
    ) |>
      kable_material(full_width = full_width) |>
      column_spec(1, width = "28%") |>
      column_spec(2, width = "72%")

    tbl_posters |> cat()
  }

  # -------------------------
  # Dated posters: grouped by day in program order
  # -------------------------
  if (nrow(posters_with_day) > 0) {
    day_levels <- names(day_header_map)

    posters_with_day <- posters_with_day |>
      mutate(day = factor(day, levels = day_levels)) |>
      arrange(day, presenter, title)

    for (d in day_levels) {
      day_df <- posters_with_day |>
        filter(as.character(day) == d) |>
        arrange(presenter, title)

      if (nrow(day_df) == 0) next

      day_df <- day_df |>
        mutate(idx = seq_len(n()))

      heading <- day_header_map[[d]]
      if (is.null(heading) || is.na(heading) || heading == "") {
        heading <- d
      }

      cat(
        paste0(
          "<h3 style='margin-top: 1.2em; margin-bottom: 0.5em;'>",
          htmlEscape(heading),
          "</h3>\n"
        )
      )

      out <- day_df |>
        mutate(
          Author = paste0(idx, " ", htmlEscape(presenter)),
          Title = mapply(
            make_collapsible_title,
            title,
            authors,
            abstract,
            USE.NAMES = FALSE
          )
        ) |>
        select(Author, Title)

      tbl_posters <- kbl(
        out,
        escape = FALSE,
        row.names = FALSE,
        col.names = c("# AUTHOR", "TITLE")
      ) |>
        kable_material(full_width = full_width) |>
        column_spec(1, width = "28%") |>
        column_spec(2, width = "72%")

      tbl_posters |> cat()
    }
  }
}

# =========================================================
# 2) Detailed/combined schedule from program.csv + sessions.csv
# =========================================================
render_detailed_program <- function(
    program_csv = "../data/program.csv",
    sessions_csv = "../data/sessions.csv",
    full_width = TRUE
) {

  # -----------------------------
  # Read overview program
  # -----------------------------
  program <- read.csv(
    program_csv,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  ) |>
    mutate(
      date      = as.Date(date),
      time      = str_trim(coalesce(time, "")),
      type      = str_trim(coalesce(type, "")),
      author    = str_trim(coalesce(author, "")),
      title     = str_trim(coalesce(title, "")),
      info      = str_trim(coalesce(info, "")),
      color     = str_trim(coalesce(color, "")),
      time_min  = parse_hm(time),
      type_norm = normalize_type(type)
    ) |>
    arrange(date, time_min)

  conference_start <- min(program$date, na.rm = TRUE)
  if (is.na(conference_start)) {
    stop("program.csv needs valid ISO dates.")
  }

  # -----------------------------
  # Read sessions
  # -----------------------------
  sessions <- read.csv(
    sessions_csv,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  ) |>
    mutate(
      day       = str_trim(coalesce(day, "")),
      time      = str_trim(coalesce(time, "")),
      type      = str_trim(coalesce(type, "")),
      title     = str_trim(coalesce(title, "")),
      authors   = str_trim(coalesce(authors, "")),
      presenter = str_trim(coalesce(presenter, "")),
      abstract  = str_trim(coalesce(abstract, "")),
      time_min  = parse_hm(time),
      type_norm = normalize_type(type)
    ) |>
    fill_presenter()

  # -----------------------------
  # Map session day names to real dates from program.csv
  # Posters without day stay NA and will be handled separately
  # -----------------------------
  session_day_levels <- sessions |>
    filter(day != "") |>
    distinct(day) |>
    pull(day) |>
    unique()

  day_date_map <- tibble(
    day = session_day_levels,
    date = conference_start + seq_along(session_day_levels) - 1
  )

  sessions <- sessions |>
    left_join(day_date_map, by = "day")

  # -----------------------------
  # Build formatted day headers from program.csv
  # -----------------------------
  program <- program |>
    mutate(day_num = as.integer(date - conference_start) + 1)

  day_headers_df <- program |>
    distinct(day_num, date) |>
    arrange(day_num, date) |>
    mutate(
      header = format(date, "%a. - %b. %d, '%y")
    )

  day_headers <- day_headers_df$header
  names(day_headers) <- as.character(day_headers_df$day_num)

  day_header_map <- setNames(
    day_headers_df$header[seq_len(min(length(session_day_levels), nrow(day_headers_df)))],
    session_day_levels[seq_len(min(length(session_day_levels), nrow(day_headers_df)))]
  )

  # -----------------------------
  # Compute the end time of each program block
  # -----------------------------
  program <- program |>
    group_by(date) |>
    arrange(time_min, .by_group = TRUE) |>
    mutate(
      next_time_min = lead(time_min),
      block_end_min = if_else(is.na(next_time_min), 24 * 60, next_time_min)
    ) |>
    ungroup()

  # -----------------------------
  # Build combined schedule rows
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

  row_counts_per_program <- integer(nrow(program))

  for (i in seq_len(nrow(program))) {
    pr <- program[i, ]

    add_row(
      time = pr$time,
      type = pr$type,
      author = pr$author,
      title = pr$title,
      bg = pr$color
    )

    expandable_types <- c(
      "Short talks",
      "Flash talks",
      "Workshops",
      "BoF sessions",
      "Poster pitches",
      "Poster session"
    )

    attached_n <- 0L

    if (pr$type_norm %in% expandable_types) {
      ss <- sessions |>
        filter(
          !is.na(date),
          date == pr$date,
          type_norm == pr$type_norm,
          time_min >= pr$time_min,
          time_min < pr$block_end_min
        ) |>
        arrange(time_min, presenter, title)

      if (nrow(ss) > 0) {
        for (j in seq_len(nrow(ss))) {
          s <- ss[j, ]

          add_row(
            time = "",
            type = "",
            author = htmlEscape(s$presenter),
            title = make_collapsible_title(s$title, s$authors, s$abstract),
            bg = pr$color
          )
        }
        attached_n <- nrow(ss)
      }
    }

    row_counts_per_program[i] <- 1 + attached_n
  }

  df_out <- bind_rows(out_rows)

  # -----------------------------
  # Group rows by day
  # -----------------------------
  idx_by_day <- split(
    seq_len(nrow(df_out)),
    rep(program$day_num, times = row_counts_per_program)
  )

  # -----------------------------
  # Render the combined table
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

  for (i in seq_len(nrow(df_out))) {
    bg <- out_bg[i]
    if (!is.na(bg) && nzchar(bg)) {
      tbl <- tbl |> row_spec(i, background = bg)
    }
  }

  for (key in names(day_headers)) {
    rows_this_day <- idx_by_day[[key]]
    if (!is.null(rows_this_day) && length(rows_this_day) > 0) {
      tbl <- tbl |>
        pack_rows(day_headers[[key]], min(rows_this_day), max(rows_this_day))
    }
  }

  tbl |> cat()

  # -----------------------------
  # Render posters after the main schedule
  # -----------------------------
  render_posters_section(
    sessions = sessions,
    day_header_map = day_header_map,
    full_width = full_width
  )
}
