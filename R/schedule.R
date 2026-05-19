library(dplyr)
library(htmltools)
library(kableExtra)
library(lubridate)
library(stringr)

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

# Robust time parser: handles 9:40 and 09:40
parse_hm <- function(x) {
  x <- str_trim(coalesce(x, ""))
  x[x == ""] <- NA_character_

  parts <- strsplit(x, ":", fixed = TRUE)

  out <- vapply(parts, function(p) {
    if (length(p) < 2) return(NA_real_)
    h <- suppressWarnings(as.numeric(p[1]))
    m <- suppressWarnings(as.numeric(p[2]))
    if (is.na(h) || is.na(m)) return(NA_real_)
    h * 60 + m
  }, numeric(1))

  out
}

# Normalize session day labels like Wed / Wednesday / Wed.
normalize_day_label <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("\\.", "") |>
    substr(1, 3)
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
    arrange(date, parse_hm(time))

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
# If poster day is missing, render one single table, no day title
# If poster day exists, use program day order
# Author names alphabetical within each day
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

  if( !"poster_nro" %in% colnames(posters) ){
    stop("Add poster numbers with add_poster_number()")
  }

  cat("<h2 style='margin-top: 2.5em; margin-bottom: 0.4em;'>POSTERS</h2>\n")
  cat("<hr style='margin-top: 0; margin-bottom: 1.2em;'>\n")
  # cat("<p>(In alphabetical order.)</p>\n")

  # Sort the posters based on date and name of presenter
  weekday_order <- wday(1:7, label = TRUE, abbr = TRUE, week_start = 1)
  posters$day <- factor(posters$day, level = levels(weekday_order))
  posters <- posters |>
    arrange(is.na(day), day, presenter, title)

  posters <- posters |>
    arrange(poster_nro)

  posters_with_day <- posters |>
    filter(!is.na(day) & str_trim(day) != "")

  posters_without_day <- posters |>
    filter(is.na(day) | str_trim(day) == "")

  # Dated posters: grouped by day in program order
  if (nrow(posters_with_day) > 0) {
    day_levels <- names(day_header_map)

    posters_with_day <- posters_with_day |>
      mutate(day = factor(day, levels = day_levels))

    for (d in day_levels) {
      day_df <- posters_with_day |>
        filter(as.character(day) == d)

      if (nrow(day_df) == 0) next

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
          Author = paste0(poster_nro, " ", htmlEscape(presenter)),
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

  # Undated posters: one single table
  if (nrow(posters_without_day) > 0) {

    out <- posters_without_day |>
      mutate(
        Author = paste0(poster_nro, " ", htmlEscape(presenter)),
        Title = mapply(
          make_collapsible_title,
          title,
          authors,
          abstract,
          USE.NAMES = FALSE
        )
      ) |>
      select(Author, Title)

    cat( paste0( "<h3 style='margin-top: 1.2em; margin-bottom: 0.5em;'>", "Not yet assigned to a day", "</h3>\n" ) )

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
  # Map sessions day names to actual program dates by weekday
  # This avoids wrong placement when sessions.csv is mixed
  # -----------------------------
  program_day_lookup <- program |>
    distinct(date) |>
    arrange(date) |>
    mutate(
      day_key = normalize_day_label(format(date, "%a"))
    )

  sessions <- sessions |>
    mutate(day_key = if_else(day == "", NA_character_, normalize_day_label(day))) |>
    left_join(
      program_day_lookup |>
        select(day_key, date),
      by = "day_key"
    )

  # -----------------------------
  # Build formatted day headers from program.csv
  # -----------------------------
  program <- program |>
    mutate(day_num = as.integer(date - conference_start) + 1)

  day_headers_df <- program |>
    distinct(day_num, date) |>
    arrange(day_num, date) |>
    mutate(
      header = format(date, "%a. - %b. %d, '%y"),
      day_key = normalize_day_label(format(date, "%a"))
    )

  day_headers <- day_headers_df$header
  names(day_headers) <- as.character(day_headers_df$day_num)

  # Map raw session day labels to formatted day headers from program.csv
  sessions_day_header_map <- sessions |>
    filter(!is.na(day), str_trim(day) != "", !is.na(date)) |>
    distinct(day, day_key) |>
    left_join(
      day_headers_df |>
        select(day_key, header),
      by = "day_key"
    ) |>
    filter(!is.na(header))

  day_header_map <- setNames(sessions_day_header_map$header, sessions_day_header_map$day)

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

add_poster_number <- function(sessions_csv = "../data/sessions.csv"){
  df <- read.csv(
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

  posters <- df[df$type == "poster", , drop = FALSE]
  df <- df[df$type != "poster", , drop = FALSE]

  # Sort in alphabetical order
  weekday_order <- wday(1:7, label = TRUE, abbr = TRUE, week_start = 1)
  posters$day <- factor(posters$day, level = levels(weekday_order))
  posters <- posters |>
    arrange(is.na(day), day, presenter, title)

  # Add poster number
  if( "poster_nro" %in% colnames(posters) ){
    posters <- posters |>
      arrange(is.na(poster_nro), poster_nro, is.na(day), day, presenter, title)
    posters <- posters |>
      mutate(
        poster_nro = coalesce(poster_nro, seq_len(n()))
      )
  } else{
    posters <- posters |>
      mutate(poster_nro = seq_len(n()))
  }

  # Add poster_nro column to original table if it does not exist yet
  df[["poster_nro"]] <- NA
  # Add posters back
  df <- rbind(df, posters)
  write.csv(df, sessions_csv, row.names = FALSE)
  return(NULL)
}
