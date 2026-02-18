library(readr)
library(dplyr)
library(knitr)
library(kableExtra)

create_organizer_table <- function(table.path = file.path("..", "data", "organizers.csv"),
                                   img.path = file.path("..", "images", "organizers"),
                                   ncol = 5L,
                                   align = "l") {
    # Read the combined organizers table
    organizers <- read.csv(table.path, stringsAsFactors = FALSE)

    # Add full image paths (handle missing paths)
    organizers[["full_img_path"]] <- ifelse(
        !is.na(organizers[["img_path"]]) & organizers[["img_path"]] != "",
        file.path(img.path, organizers[["img_path"]]),
        ""
    )

    # Normalize order: if missing/blank, treat as Inf
    organizers <- organizers |>
        mutate(order = ifelse(is.na(order) | order == "", Inf, as.numeric(order)))

    # Get unique committees (excluding NA/empty)
    committees <- unique(organizers$committee)
    committees <- committees[!is.na(committees) & committees != ""]

    # Sort committees: Local first, then others
    if ("Local" %in% committees) {
        committees <- c("Local", setdiff(committees, "Local"))
    }

    # Process each committee
    for (committee_name in committees) {
        cat(paste0("\n## ", committee_name, "\n\n"))

        # Filter for current committee
        df_comm <- organizers |>
            filter(committee == committee_name) |>
            arrange(order, name)

        # Format each member with image, name, and role
        df_comm <- df_comm |>
            mutate(
                img = ifelse(
                    full_img_path != "",
                    sprintf("![](%s){height=150}", full_img_path),
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
                local = rep(FALSE, n_missing),
                order = rep(Inf, n_missing),
                img_path = rep("", n_missing),
                full_img_path = rep("", n_missing),
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

        kable(tbl,
            format = "html", align = align, col.names = NULL, escape = FALSE,
            table.attr = 'class="organizers-table table table-borderless"'
        ) |>
            kable_styling(full_width = FALSE, position = "left") |>
            print()

        cat("\n\n")
    }
}
