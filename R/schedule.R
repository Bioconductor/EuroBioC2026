library(dplyr)
library(stringr)
library(kableExtra)

# ---- Read + clean program ----
df <- read.csv(program_csv, stringsAsFactors = FALSE, na.strings = c("", "NA")) |>
    mutate(
        day    = as.integer(day),
        time   = str_trim(time),
        type   = str_trim(type),
        author = str_trim(coalesce(author, "")),
        title  = str_trim(coalesce(title, "")),
        info   = str_trim(coalesce(info, ""))
    ) |>
    arrange(day, time)

# Optional: shorten repeated phrasing
df$author <- gsub("Contributed\\s*from\\s*submitted\\s*abstracts", "Contributed abstracts", df$author)
df$author <- gsub("Contributed\\s*until\\s*the\\s*beginning\\s*of\\s*the\\s*conference", "Contributed pre-conference", df$author)

# Day headers (edit dates if needed)
day_headers <- c(
    "Day 1 — Wed. Sep 3, 2026",
    "Day 2 — Thu. Sep 4, 2026",
    "Day 3 — Fri. Sep 5, 2026"
)

# ---- Read palette (optional) ----
pal <- if (file.exists(colors_csv)) {
    read.csv(colors_csv, stringsAsFactors = FALSE) |>
        mutate(
            type  = str_trim(type),
            color = str_trim(color)
        )
} else {
    data.frame(type = character(), color = character())
}

# Attach color per row (keeps row order)
df2 <- df |>
    left_join(pal, by = "type")

# Output table data
df_out <- df2 |> select(time, type, author, title)

# Row indices by day
idx_by_day <- split(seq_len(nrow(df_out)), df2$day)

# ---- Build table ----
tbl <- kbl(
    df_out,
    escape = TRUE,
    row.names = FALSE,
    col.names = c("Time", "Type", "Author", "Title")
) |>
    kable_material(full_width = TRUE) |>
    column_spec(1, width = "12%") |>
    column_spec(2, width = "18%") |>
    column_spec(3, width = "28%") |>
    column_spec(4, width = "42%")

# ---- Apply row colors from CSV ----
for (i in seq_len(nrow(df2))) {
    bg <- df2$color[i]
    if (!is.na(bg) && nzchar(bg)) {
        tbl <- tbl |> row_spec(i, background = bg)
    }
}

# ---- Group by day (only if that day exists) ----
for (d in seq_along(day_headers)) {
    key <- as.character(d)
    if (!is.null(idx_by_day[[key]])) {
        tbl <- tbl |>
            pack_rows(
                day_headers[d],
                min(idx_by_day[[key]]),
                max(idx_by_day[[key]])
            )
    }
}

tbl |> cat()
