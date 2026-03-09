library(readr)
library(dplyr)
library(knitr)
library(kableExtra)

create_organizer_table <- function(table.path = file.path("..", "data", "organizers.csv"),
                                   img.path   = file.path("..", "images", "organizers"),
                                   ncol = 5L,
                                   align = "l") {

  organizers <- read.csv(table.path, stringsAsFactors = FALSE)
  organizers$name <- organizers$name |> trimws()
  organizers$name_order <- organizers$name |> rank()

  # Ensure expected columns exist (safe defaults)
  if (!"role" %in% names(organizers)) organizers$role <- ""
  if (!"order" %in% names(organizers)) organizers$order <- NA
  if (!"img_path" %in% names(organizers)) organizers$img_path <- ""

  # Normalize order: if missing/blank, treat as Inf
  organizers <- organizers |>
    mutate(order = ifelse(is.na(order) | trimws(order) == "", Inf, suppressWarnings(as.numeric(order))))

  # Detect whether committee column has any real information
  has_committee_col  <- "committee" %in% names(organizers)
  has_committee_info <- has_committee_col && any(trimws(organizers$committee) != "" & !is.na(organizers$committee))

  # Add full image paths (handle missing paths)
  organizers <- organizers |>
    mutate(
      img_path = ifelse(is.na(img_path), "", img_path),
      full_img_path = ifelse(img_path != "", file.path(img.path, img_path), "")
    )

  # Helper to render a single table chunk
  render_one_table <- function(df_comm) {

    df_comm <- df_comm |>
      arrange(order, name_order) |>
      mutate(
        img = ifelse(
          full_img_path != "",
          sprintf("![](%s){height=150}", full_img_path),
          ""
        ),
        label = ifelse(
          !is.na(role) & trimws(role) != "",
          paste0("**", name, "**<br>*", role, "*"),
          paste0("**", name, "**")
        )
      )

    # Pad to multiple of ncol
    n_missing <- ncol - (nrow(df_comm) %% ncol)
    if (n_missing < ncol) {
      df_comm <- bind_rows(df_comm, data.frame(
        name = rep("", n_missing),
        role = rep("", n_missing),
        order = rep(Inf, n_missing),
        img_path = rep("", n_missing),
        full_img_path = rep("", n_missing),
        img = rep("", n_missing),
        label = rep("", n_missing),
        .is_local = rep(FALSE, n_missing),
        stringsAsFactors = FALSE
      ))
    }

    img_mtx  <- matrix(df_comm$img,   ncol = ncol, byrow = TRUE)
    name_mtx <- matrix(df_comm$label, ncol = ncol, byrow = TRUE)
    ij <- rep(seq_len(nrow(img_mtx)), each = 2) + c(0, nrow(img_mtx))

    tbl <- data.frame(rbind(img_mtx, name_mtx)[ij, ])

    kable(tbl,
          format = "html", align = align, col.names = NULL, escape = FALSE,
          table.attr = 'class="organizers-table table table-borderless"'
    ) |>
      kable_styling(full_width = FALSE, position = "left") |>
      print()

    cat("\n\n")
  }

  # If committee has no information -> render once, no separation
  if (!has_committee_info) {
    render_one_table(organizers)
    return(invisible(NULL))
  }

  # Otherwise: committee-separated rendering
  committees <- unique(organizers$committee)
  committees <- committees[!is.na(committees) & trimws(committees) != ""]

  # Sort committees: Local first, then others
  if ("Local" %in% committees) {
    committees <- c("Local", setdiff(committees, "Local"))
  }

  for (committee_name in committees) {
    cat(paste0("\n## ", committee_name, "\n\n"))
    df_comm <- organizers |> filter(committee == committee_name)
    render_one_table(df_comm)
  }

  invisible(NULL)
}
