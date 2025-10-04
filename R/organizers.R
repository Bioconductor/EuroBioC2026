library(readr)
library(dplyr)
library(knitr)
library(kableExtra)

create_organizer_table <- function(
        table.path = file.path("..", "data", "organizers.csv"),
        img.path = file.path("..", "images", "organizers"),
        ncol = 5L,
        align = "l"
    ){
    # Read organizer table
    organizers <- read.csv(table.path, stringsAsFactors = FALSE)
    organizers[["img_path"]] <- file.path(img.path, organizers[["img_path"]])

    # Normalize order: if missing/blank, treat as Inf
    organizers <- organizers |>
        mutate(order = ifelse(is.na(order) | order == "", Inf, as.numeric(order)))

    # Add organizer group
    groups <- c("Local", "Community")
    organizers[["group"]] <- ifelse(organizers[["local"]], groups[[1L]], groups[[2L]])

    # Loop over local and community
    for (grp in groups) {
        cat("##", grp, "\n\n")

        df <- organizers |>
            filter(group == grp) |>
            arrange(order, name) |>
            mutate(
                img = sprintf("![](%s){height=150}", img_path),
                label = name
            )

        # Fill empty slots so table is rectangular
        n_missing <- ncol - (nrow(df) %% ncol)
        if (n_missing < ncol) {
            df <- bind_rows(df, data.frame(
                name = rep("", n_missing),
                local = rep(FALSE, n_missing),
                order = rep(Inf, n_missing),
                img_path = rep("", n_missing),
                group = rep(grp, n_missing),
                img = rep("", n_missing),
                label = rep("", n_missing)
            ))
        }

        # Make matrix with alternating rows (image row, name row)
        img_mtx <- matrix(df$img, ncol = ncol, byrow = TRUE)
        name_mtx <- matrix(df$label, ncol = ncol, byrow = TRUE)
        ij <- rep(seq_len(nrow(img_mtx)), each = 2) + c(0, nrow(img_mtx))

        tbl <- data.frame(rbind(img_mtx, name_mtx)[ij, ])

        kable(tbl, align = align, col.names = NULL, escape = FALSE) |>
            kable_styling(full_width = FALSE, position = "left") |>
            print()

        cat("\n\n")
    }
}

