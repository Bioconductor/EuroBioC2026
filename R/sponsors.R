
read_sponsors <- function(csv_path) {
  read.csv(csv_path, stringsAsFactors = FALSE)
}

render_sponsor_grid <- function(df, ncol = 4, img_prefix = "", show_name = FALSE) {
  stopifnot(all(c("image", "website") %in% names(df)))
  width <- floor(100 / ncol)

  url_join <- function(prefix, path) {
    if (is.null(prefix) || prefix == "") return(path)
    prefix <- sub("/+$", "", prefix)
    path   <- sub("^/+", "", path)
    paste0(prefix, "/", path)
  }

  out <- c()
  out <- c(out, ":::: {.columns}")
  out <- c(out, "::: {.column width=\"10%\"}")
  out <- c(out, ":::")
  out <- c(out, "::: {.column width=\"90%\"}", "")
  out <- c(out, ":::: {.columns}")

  for (i in seq_len(nrow(df))) {
    img <- url_join(img_prefix, df$image[i])

    out <- c(out, sprintf("::: {.column width=\"%s%%\"}", width))

    # Use HTML img + CSS class (NO markdown width=)
    out <- c(out, sprintf(
      "<a href='%s' target='_blank' class='sponsor-link'>%s</a>",
      df$website[i],
      sprintf("<img src='%s' alt='%s' class='sponsor-logo'>",
              img,
              if ("name" %in% names(df)) df$name[i] else "Sponsor logo")
    ))

    if (show_name && ("name" %in% names(df)) && nzchar(df$name[i])) {
      out <- c(out, sprintf("<div class='sponsor-name'>%s</div>", df$name[i]))
    }

    out <- c(out, ":::")
  }

  out <- c(out, "::::", "")
  out <- c(out, ":::")
  out <- c(out, "::: {.column width=\"10%\"}")
  out <- c(out, ":::")
  out <- c(out, "::::")

  cat(paste(out, collapse = "\n"))
}


render_sponsors_home <- function(csv_path, title = "", ncol = 4) {
  df <- read_sponsors(csv_path)
  cat(sprintf("# %s\n\n", title))
  render_sponsor_grid(df, ncol = ncol)
}

render_sponsors_by_level <- function(
    csv_path,
    level_order = c("Diamond", "Gold", "Silver", "Bronze", "Supporter"),
    ncol_by_level = c(Diamond = 2, Gold = 3, Silver = 5, Bronze = 6, Supporter = 6),
    heading = c("bold", "h2"),
    img_prefix = ""
) {
  heading <- match.arg(heading)

  df <- read_sponsors(csv_path)
  if (!("level" %in% names(df))) stop("CSV must include a 'level' column.")

  df$level <- factor(df$level, levels = level_order)

  levels_present <- unique(as.character(df$level))
  levels_present <- level_order[level_order %in% levels_present]

  for (lvl in levels_present) {
    df_lvl <- df[as.character(df$level) == lvl, , drop = FALSE]
    if (nrow(df_lvl) == 0) next

    if (heading == "h2") {
      cat(sprintf("\n## %s\n\n", lvl))
    } else {
      cat(sprintf("\n**%s**\n\n", lvl))
    }

    ncol <- unname(ncol_by_level[lvl])
    if (is.na(ncol)) ncol <- 4

    render_sponsor_grid(df_lvl, ncol = ncol, img_prefix = img_prefix)
    cat("\n")
  }
}
