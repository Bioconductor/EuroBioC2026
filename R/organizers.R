library(readr)
library(dplyr)
library(knitr)
library(kableExtra)

create_organizer_table <- function(organizers.path = file.path("..", "data", "organizers.csv"),
                                   roles.path = file.path("..", "data", "roles.csv"),
                                   img.path = file.path("..", "images", "organizers"),
                                   ncol = 5L,
                                   align = "l") {

  # Read both tables
  organizers <- read.csv(organizers.path, stringsAsFactors = FALSE)
  roles <- read.csv(roles.path, stringsAsFactors = FALSE)

  # Merge the data
  df <- merge(organizers, roles, by = "name", all = TRUE)

  # Add full image path
  df[["img_path"]] <- ifelse(
    !is.na(df[["img_path"]]) & df[["img_path"]] != "",
    file.path(img.path, df[["img_path"]]),
    ""
  )

  # Normalize order: if missing/blank, treat as Inf
  df <- df |>
    mutate(order = ifelse(is.na(order) | order == "", Inf, as.numeric(order)))

  # Get unique committees (excluding NA/empty)
  committees <- unique(df$committee)
  committees <- committees[!is.na(committees) & committees != ""]

  # Process each committee
  for(committee_name in committees) {
    cat(paste0("\n## ", committee_name, "\n\n"))

    # Filter for current committee
    df_comm <- df |>
      filter(committee == committee_name) |>
      arrange(order, name)

    # Format each member with image, name, and role
    df_comm <- df_comm |>
      mutate(
        img = ifelse(
          !is.na(img_path) & img_path != "",
          sprintf("![](%s){height=150}", img_path),
          ""
        ),
        label = ifelse(
          !is.na(role) & role != "",
          paste0("**", name, "**<br>*", role, "*"),
          paste0("**", name, "**")
        )
      )

    # Pad to multiple of ncol
    n_missing <- ncol - (nrow(df_comm) %% ncol)
    if (n_missing < ncol) {
      df_comm <- bind_rows(df_comm, data.frame(
        name = rep("", n_missing),
        committee = rep("", n_missing),
        role = rep("", n_missing),
        img_path = rep("", n_missing),
        order = rep(Inf, n_missing),
        img = rep("", n_missing),
        label = rep("", n_missing),
        stringsAsFactors = FALSE
      ))
    }

    # Make matrix with alternating rows (image row, name row)
    img_mtx <- matrix(df_comm$img, ncol = ncol, byrow = TRUE)
    name_mtx <- matrix(df_comm$label, ncol = ncol, byrow = TRUE)
    ij <- rep(seq_len(nrow(img_mtx)), each = 2) + c(0, nrow(img_mtx))

    tbl <- data.frame(rbind(img_mtx, name_mtx)[ij, ])

    kable(tbl, format = "html", align = align, col.names = NULL, escape = FALSE,
          table.attr = 'class="organizers-table table table-borderless"') |>
      kable_styling(full_width = FALSE, position = "left") |>
      print()

    cat("\n\n")
  }
}
