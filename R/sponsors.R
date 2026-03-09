read_sponsors <- function(csv_path) {
    read.csv(csv_path, stringsAsFactors = FALSE)
}

url_join <- function(prefix, path) {
    if (is.null(prefix) || prefix == "") {
        return(path)
    }

    # Keep "/" as site-root prefix
    if (identical(prefix, "/")) {
        path <- sub("^/+", "", path)
        return(paste0("/", path))
    }

    prefix <- sub("/+$", "", prefix)
    path <- sub("^/+", "", path)
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
harmonize_sponsor_logos <- function(input_dir = "images/partners/raw",
                                    fs_prefix = "",
                                    out_dir = "images/partners",
                                    canvas_w = 800,
                                    canvas_h = 240,
                                    padding = 24) {
    imgs <- list.files(input_dir)

    # If magick missing, do nothing (keep original df$image)
    if (!requireNamespace("magick", quietly = TRUE)) {
        warning("Package 'magick' not installed; skipping harmonization.")
        return(NULL)
    }

    for (img in imgs) {
        # filesystem input (prefix applies)
        in_fs <- file.path(input_dir, img)
        out_fs <- file.path(out_dir, img)

        harmonize_logo(
            input_fs  = in_fs,
            output_fs = out_fs,
            canvas_w  = canvas_w,
            canvas_h  = canvas_h,
            padding   = padding
        )
    }

    return(NULL)
}

# ---- (2) Render grid --------
render_sponsor_grid <- function(df,
                                ncol = 4,
                                fs_prefix = "",
                                web_prefix = "",
                                show_name = FALSE,
                                canvas_w = 800,
                                canvas_h = 240,
                                padding = 24,
                                left_margin = 10,
                                inner_width = 80) {
    stopifnot(all(c("image", "website") %in% names(df)))
    if (nrow(df) == 0) {
        return(invisible(NULL))
    }

    # Order based on levels
    df$level <- df$level |>
      factor(level = c(level_order, unique(df$level)) |> unique())
    df <- df[order(df$level), , drop = FALSE]

    ncol <- min(ncol, nrow(df))
    width <- floor(100 / ncol)

    out <- character()

    out <- c(out, ":::: {.columns}")
    out <- c(out, sprintf("::: {.column width=\"%s%%\"}", left_margin))
    out <- c(out, ":::")
    out <- c(out, sprintf("::: {.column width=\"%s%%\"}", inner_width), "")
    out <- c(out, ":::: {.columns}")

    for (i in seq_len(nrow(df))) {
        img_src <- url_join(web_prefix, df$image[i])
        alt_txt <- ""

        out <- c(out, sprintf("::: {.column width=\"%s%%\"}", width))

        # Quarto supports target="_blank" on markdown links with an attribute block
        # If your setup doesn’t, you can remove `{target="_blank"}`
        out <- c(out, sprintf(
            "[![%s](%s)](%s){target=\"_blank\"}",
            alt_txt, img_src, df$website[i]
        ))

        if (isTRUE(show_name) && ("name" %in% names(df)) && nzchar(df$name[i])) {
            out <- c(out, "", df$name[i])
        }

        out <- c(out, ":::")
    }

    out <- c(out, "::::", "")
    out <- c(out, ":::") # close inner width column
    out <- c(out, sprintf("::: {.column width=\"%s%%\"}", left_margin))
    out <- c(out, ":::")
    out <- c(out, "::::") # close outer columns

    cat(paste(out, collapse = "\n"))
}

render_sponsors_home <- function(csv_path, title = "", ncol = 4) {
    df <- read_sponsors(csv_path)

    render_sponsor_grid(
        df,
        ncol = ncol
    )
}

level_order = c("Diamond", "Gold", "Silver", "Bronze", "Supporter")

render_sponsors_by_level <- function(
  csv_path,
  ncol_by_level = c(Diamond = 5, Gold = 5, Silver = 5, Bronze = 5, Supporter = 5),
  heading = c("bold", "h2")
) {
    heading <- match.arg(heading)

    df <- read_sponsors(csv_path)
    if (!("level" %in% names(df))) stop("CSV must include a 'level' column.")

    # Get directory where the CSV lives
    csv_dir <- dirname(csv_path)
    if (grepl("../", csv_dir)) {
        df$image <- file.path(
            "..",
            df$image
        )
    }


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
            ncol = ncol
        )
        cat("\n")
    }
}
