
read_sponsors <- function(csv_path) {
  read.csv(csv_path, stringsAsFactors = FALSE)
}

url_join <- function(prefix, path) {
  if (is.null(prefix) || prefix == "") return(path)
  prefix <- sub("/+$", "", prefix)
  path   <- sub("^/+", "", path)
  paste0(prefix, "/", path)
}

# ---- (1) Harmonize one logo into a fixed canvas and save it
harmonize_logo <- function(input_fs,
                           output_fs,
                           canvas_w = 800,
                           canvas_h = 240,
                           padding = 24) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required for harmonization. Install with install.packages('magick').")
  }

  img <- magick::image_read(input_fs)

  # Scale logo to fit inside (canvas - padding) while preserving aspect ratio
  target_w <- max(1, canvas_w - 2 * padding)
  target_h <- max(1, canvas_h - 2 * padding)

  img2 <- magick::image_resize(img, geometry = paste0(target_w, "x", target_h, ">"))

  # Create transparent canvas and center the resized logo
  canvas <- magick::image_blank(width = canvas_w, height = canvas_h, color = "none")
  composed <- magick::image_composite(canvas, img2, operator = "over", gravity = "center")

  # Ensure output directory exists
  out_dir <- dirname(output_fs)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  magick::image_write(composed, path = output_fs, format = "png")
  invisible(output_fs)
}

# ---- (1) Harmonize all sponsors in a df and return new image paths ----------
harmonize_sponsor_logos <- function(df,
                                    fs_prefix = "",
                                    harmonized_dir = "images/partners_harmonized",
                                    canvas_w = 800,
                                    canvas_h = 240,
                                    padding = 24) {
  stopifnot("image" %in% names(df))

  # If magick missing, do nothing (keep original df$image)
  if (!requireNamespace("magick", quietly = TRUE)) {
    warning("Package 'magick' not installed; skipping harmonization.")
    return(df)
  }

  df2 <- df

  for (i in seq_len(nrow(df2))) {
    in_rel <- df2$image[i]

    # filesystem input (prefix applies)
    in_fs <- file.path(fs_prefix, in_rel)

    # stable output name
    base <- tools::file_path_sans_ext(basename(in_rel))
    out_rel <- file.path(harmonized_dir, paste0(base, ".png"))   # WEB PATH (no fs_prefix)
    out_fs  <- file.path(fs_prefix, out_rel)                     # FILESYSTEM PATH

    # Only harmonize if we can read the input
    if (file.exists(in_fs)) {

      # render if missing or stale
      need <- !file.exists(out_fs) ||
        (file.info(out_fs)$mtime < file.info(in_fs)$mtime)

      if (need) {
        harmonize_logo(
          input_fs  = in_fs,
          output_fs = out_fs,
          canvas_w  = canvas_w,
          canvas_h  = canvas_h,
          padding   = padding
        )
      }

      # Only switch to harmonized image if the file actually exists
      if (file.exists(out_fs)) {
        df2$image[i] <- out_rel
      }
    } else {
      # input not found -> keep original df2$image[i]
      # (this is the critical difference vs your current code)
    }
  }

  df2
}


# ---- (2) Render grid; optionally harmonize and USE harmonized images --------
render_sponsor_grid <- function(df,
                                ncol = 4,
                                fs_prefix = "",
                                web_prefix = "",
                                show_name = FALSE,
                                harmonize = TRUE,
                                harmonized_dir = "images/partners_harmonized",
                                canvas_w = 800,
                                canvas_h = 240,
                                padding = 24) {
  stopifnot(all(c("image", "website") %in% names(df)))
  if (nrow(df) == 0) return(invisible(NULL))

  ncol <- min(ncol, nrow(df))
  width <- floor(100 / ncol)

  # Harmonize first (updates df$image to harmonized paths)
  if (harmonize) {
    df <- harmonize_sponsor_logos(
      df,
      fs_prefix = fs_prefix,
      harmonized_dir = harmonized_dir,
      canvas_w = canvas_w,
      canvas_h = canvas_h,
      padding = padding
    )
  }

  out <- c()
  out <- c(out, ":::: {.columns}")
  out <- c(out, "::: {.column width=\"10%\"}")
  out <- c(out, ":::")
  out <- c(out, "::: {.column width=\"90%\"}", "")
  out <- c(out, ":::: {.columns}")

  for (i in seq_len(nrow(df))) {
    # df$image is now the harmonized relative path (or original if harmonize=FALSE)
    img_src <- df$image[i]

    out <- c(out, sprintf("::: {.column width=\"%s%%\"}", width))

    out <- c(out, sprintf(
      "<a href='%s' target='_blank' class='sponsor-link'>%s</a>",
      df$website[i],
      sprintf(
        "<img src='%s' alt='%s' class='sponsor-logo'>",
        img_src,
        if ("name" %in% names(df)) df$name[i] else "Sponsor logo"
      )
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

render_sponsors_home <- function(csv_path, title = "", ncol = 4,
                                 harmonize = TRUE,
                                 harmonized_dir = "images/partners_harmonized") {
  df <- read_sponsors(csv_path)
  cat(sprintf("# %s\n\n", title))
  render_sponsor_grid(
    df,
    ncol = ncol,
    fs_prefix = "",
    web_prefix = "",
    harmonize = harmonize,
    harmonized_dir = harmonized_dir
  )
}

render_sponsors_by_level <- function(
    csv_path,
    level_order = c("Diamond", "Gold", "Silver", "Bronze", "Supporter"),
    ncol_by_level = c(Diamond = 2, Gold = 3, Silver = 5, Bronze = 6, Supporter = 6),
    heading = c("bold", "h2"),
    fs_prefix = "",
    web_prefix = "",
    harmonize = TRUE,
    harmonized_dir = "images/partners_harmonized"
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

    render_sponsor_grid(
      df_lvl,
      ncol = ncol,
      fs_prefix = fs_prefix,
      web_prefix = web_prefix,
      harmonize = harmonize,
      harmonized_dir = harmonized_dir
    )
    cat("\n")
  }
}
