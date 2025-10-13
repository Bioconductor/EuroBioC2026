library(magick)

addBackgroundToHexagon <- function(img.path,
                                   width = 1000,
                                   height = 600,
                                   edge_fade = 40, # 0 = no fade on left/right edges
                                   center_fade = 40, # 0 = no horizontal blend in center
                                   glow_fade = 40, # 0 = no central radial glow
                                   color_scale = 0.5 # how light the colors are
) {
    stopifnot(edge_fade >= 0, edge_fade <= 100)
    stopifnot(center_fade >= 0, center_fade <= 100)
    stopifnot(glow_fade >= 0, glow_fade <= 100)

    bioc_blue <- lighten("#1a81c2", color_scale)
    bioc_green <- lighten("#87b13f", color_scale)

    # --- Read and scale logo to maximum size ---
    img_logo <- image_read(img.path) |>
        image_convert(colorspace = "sRGB")

    info <- image_info(img_logo)
    scale_factor <- min(width / info$width, height / info$height)
    logo_scaled <- image_scale(img_logo, paste0(round(info$width * scale_factor), "x", round(info$height * scale_factor)))

    # --- Base: blue/green halves ---
    base <- image_blank(width, height, "white") |> image_draw()
    rect(0, 0, width / 2, height, col = bioc_blue, border = NA)
    rect(width / 2, 0, width, height, col = bioc_green, border = NA)
    dev.off()

    # --- Edge fade ---
    fade_width <- width * edge_fade / 300
    left_fade <- image_blank(width, height, "none") |> image_draw()
    for (x in seq(0, fade_width, by = 5)) {
        alpha <- 1 - (x / fade_width)
        rect(x, 0, x + 5, height, col = rgb(1, 1, 1, alpha), border = NA)
    }
    dev.off()

    right_fade <- image_blank(width, height, "none") |> image_draw()
    for (x in seq(width - fade_width, width, by = 5)) {
        alpha <- (x - (width - fade_width)) / fade_width
        rect(x, 0, x + 5, height, col = rgb(1, 1, 1, alpha), border = NA)
    }
    dev.off()

    # --- Center radial glow ---
    max_r <- min(width, height) / 1.5
    center_glow <- image_blank(width, height, "none") |> image_draw()
    for (r in seq(max_r, 0, length.out = 80)) {
        alpha <- (1 - r / max_r)^1.2 * (glow_fade / 100)
        symbols(width / 2, height / 2,
            circles = r, inches = FALSE,
            add = TRUE, bg = rgb(1, 1, 1, alpha), fg = NA
        )
    }
    dev.off()

    # --- Horizontal center fade ---
    blend_width <- width * center_fade / 200
    cx <- width / 2
    center_band <- image_blank(width, height, "none") |> image_draw()
    for (x in seq(cx - blend_width / 2, cx + blend_width / 2, by = 2)) {
        dist <- abs(x - cx)
        alpha <- (1 - (dist / (blend_width / 2)))^2 * (center_fade / 100)
        rect(x, 0, x + 2, height, col = rgb(1, 1, 1, alpha), border = NA)
    }
    dev.off()

    # --- Combine layers ---
    combined <- base |>
        image_composite(left_fade, operator = "over") |>
        image_composite(right_fade, operator = "over") |>
        image_composite(center_glow, operator = "over") |>
        image_composite(center_band, operator = "over")

    # --- Add logo centered ---
    final <- image_composite(combined, logo_scaled, operator = "over", gravity = "center")

    return(final)
}

lighten <- function(col, factor = 0.7) {
    # factor = 0 → no change, factor = 1 → white
    rgb_val <- col2rgb(col) / 255
    rgb_val_new <- rgb_val + (1 - rgb_val) * factor
    rgb(rgb_val_new[1], rgb_val_new[2], rgb_val_new[3])
}

################################################################################
input_dir <- file.path("images", "carousel", "original")
output_dir <- file.path("images", "carousel")

img_names <- c(
    "EuroBioC2022.png", "EuroBioC2023.png",
    "EuroBioC2024.png", "EuroBioC2025.png",
    "group_picture.jpg", "aurajoki.jpg"
)

df <- data.frame(
    name = c(
        "EuroBioC2022.png", "EuroBioC2023.png",
        "EuroBioC2024.png", "EuroBioC2025.png",
        "eurobioc2025_group.jpg", "eurobioc2025_1.jpg", "eurobioc2025_2.jpg",
        "aurajoki.jpg"
    ),
    edge_fade = c(60, 60, 60, 60, 60, 60, 60, 60),
    center_fade = c(20, 20, 20, 20, 20, 20, 20, 20),
    glow_fade = c(20, 20, 20, 20, 0.0001, 0.0001, 0.0001, 0.0001)
)

for (i in seq_len(nrow(df))) {
    new_img <- addBackgroundToHexagon(
        file.path(input_dir, df[i, "name"]),
        edge_fade = df[i, "edge_fade"],
        center_fade = df[i, "center_fade"],
        glow_fade = df[i, "glow_fade"],
        width = 1000,
        height = 400
    )
    image_write(new_img, file.path(output_dir, df[i, "name"]))
}
