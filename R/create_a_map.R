library(leaflet)
library(dplyr)
library(leaflet.extras)

create_a_map <- function(
    data.path = file.path("..", "data", "map_data.csv"),
    show_attractions = FALSE,
    show_transport = TRUE,
    show_accommodation = TRUE
){

  # read data
  places <- read.csv(data.path, stringsAsFactors = FALSE)

  # ensure correct types
  places$lat <- as.numeric(places$lat)
  places$lng <- as.numeric(places$lng)
  places$type <- as.character(places$type)
  places$name <- as.character(places$name)

  # allow missing link column
  if (!("link" %in% names(places))) places$link <- NA_character_

  places[["popup"]] <- ifelse(
    is.na(places[["link"]]) | places[["link"]] == "",
    places[["name"]],
    paste0(
      "<b>", places[["name"]], "</b><br>",
      "<a href='", places[["link"]], "' target='_blank'>More information</a>"
    )
  )

  # Add attractions color
  type_colors <- c(
    venue = "#1f5eff",
    transport = "#2f855a",
    accommodation = "#805ad5",
    attraction = "#e34a33"
  )

  # NEW: precompute filtered dfs once (and allow empty safely)
  transport_df     <- filter(places, type == "transport")
  accommodation_df <- filter(places, type == "accommodation")
  venue_df         <- filter(places, type == "venue")
  attraction_df    <- filter(places, type == "attraction")

  # NEW: only list groups that exist in the CSV
  overlay_groups <- c()
  if (nrow(venue_df) > 0) overlay_groups <- c(overlay_groups, "Venue")
  if (nrow(transport_df) > 0) overlay_groups <- c(overlay_groups, "Transport")
  if (nrow(accommodation_df) > 0) overlay_groups <- c(overlay_groups, "Accommodation")
  if (nrow(attraction_df) > 0) overlay_groups <- c(overlay_groups, "Attractions")

  map <- leaflet(places) |>
    addProviderTiles("OpenStreetMap.Mapnik")

  # ----------------------------
  # TRANSPORT (only if exists)
  # ----------------------------
  if (nrow(transport_df) > 0) {
    map <- map |>
      addCircleMarkers(
        data = transport_df,
        ~lng, ~lat,
        radius = 7,
        color = type_colors["transport"],
        fillColor = type_colors["transport"],
        fillOpacity = 0.85,
        stroke = FALSE,
        group = "Transport",
        popup = ~popup
      ) |>
      addLabelOnlyMarkers(
        data = transport_df,
        ~lng, ~lat,
        label = ~name,
        group = "Transport",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-size" = "12px",
            "font-weight" = "500",
            "color" = "#22543d",
            "background-color" = "rgba(255,255,255,0.8)",
            "padding" = "3px 5px",
            "border-radius" = "4px"
          )
        )
      )
  }

  # ----------------------------
  # ACCOMMODATION (only if exists)
  # ----------------------------
  if (nrow(accommodation_df) > 0) {
    map <- map |>
      addCircleMarkers(
        data = accommodation_df,
        ~lng, ~lat,
        radius = 7,
        color = type_colors["accommodation"],
        fillColor = type_colors["accommodation"],
        fillOpacity = 0.8,
        stroke = FALSE,
        group = "Accommodation",
        popup = ~popup
      ) |>
      addLabelOnlyMarkers(
        data = accommodation_df,
        ~lng, ~lat,
        label = ~name,
        group = "Accommodation",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-size" = "12px",
            "font-weight" = "500",
            "color" = "#44337a",
            "background-color" = "rgba(255,255,255,0.8)",
            "padding" = "3px 5px",
            "border-radius" = "4px"
          )
        )
      )
  }

  # ----------------------------
  # VENUE (HALO + CORE + LABEL) (only if exists)
  # ----------------------------
  if (nrow(venue_df) > 0) {
    map <- map |>
      addCircleMarkers(
        data = venue_df,
        ~lng, ~lat,
        radius = 26,
        color = type_colors["venue"],
        fillColor = type_colors["venue"],
        fillOpacity = 0.18,
        stroke = FALSE,
        group = "Venue"
      ) |>
      addCircleMarkers(
        data = venue_df,
        ~lng, ~lat,
        radius = 12,
        color = type_colors["venue"],
        fillColor = type_colors["venue"],
        fillOpacity = 1,
        stroke = FALSE,
        group = "Venue",
        popup = ~popup
      ) |>
      addLabelOnlyMarkers(
        data = venue_df,
        ~lng, ~lat,
        label = ~name,
        group = "Venue",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          offset = c(0, -18),
          textOnly = TRUE,
          style = list(
            "font-size" = "16px",
            "font-weight" = "900",
            "color" = "#1a365d",
            "background-color" = "rgba(255,255,255,0.95)",
            "padding" = "6px 8px",
            "border-radius" = "8px",
            "box-shadow" = "0 0 8px rgba(31,94,255,0.6)"
          )
        )
      )
  }

  # ----------------------------
  # ATTRACTIONS (only if exists)
  # ----------------------------
  if (nrow(attraction_df) > 0) {
    map <- map |>
      addCircleMarkers(
        data = attraction_df,
        ~lng, ~lat,
        radius = 7,
        color = "white",
        weight = 2,
        fillColor = type_colors["attraction"],
        fillOpacity = 0.9,
        stroke = TRUE,
        group = "Attractions",
        popup = ~popup
      ) |>
      addLabelOnlyMarkers(
        data = attraction_df,
        ~lng, ~lat,
        label = ~name,
        group = "Attractions",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-size" = "12px",
            "font-weight" = "500",
            "color" = "#9c2c2c",
            "background-color" = "rgba(255,255,255,0.85)",
            "padding" = "3px 5px",
            "border-radius" = "4px"
          )
        )
      )
  }

  # ----------------------------
  # LAYER CONTROL (only if any groups exist)
  # ----------------------------
  if (length(overlay_groups) > 0) {
    map <- map |>
      addLayersControl(
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )
  }

  # Conditionally hide (only if group exists)
  if (show_attractions == FALSE && "Attractions" %in% overlay_groups) {
    map <- map |> hideGroup("Attractions")
  }
  if (show_transport == FALSE && "Transport" %in% overlay_groups) {
    map <- map |> hideGroup("Transport")
  }
  if (show_accommodation == FALSE && "Accommodation" %in% overlay_groups) {
    map <- map |> hideGroup("Accommodation")
  }

  # Determine which types are currently visible (only types that exist)
  visible_types <- c()
  if (nrow(venue_df) > 0) visible_types <- c(visible_types, "venue")
  if (show_transport && nrow(transport_df) > 0) visible_types <- c(visible_types, "transport")
  if (show_accommodation && nrow(accommodation_df) > 0) visible_types <- c(visible_types, "accommodation")
  if (show_attractions && nrow(attraction_df) > 0) visible_types <- c(visible_types, "attraction")

  visible_places <- filter(places, type %in% visible_types)

  # safe fallback
  fit_df <- if (nrow(visible_places) > 0) visible_places else places

  if (nrow(fit_df) > 0) {
    map <- map |>
      fitBounds(
        lng1 = min(fit_df$lng, na.rm = TRUE),
        lat1 = min(fit_df$lat, na.rm = TRUE),
        lng2 = max(fit_df$lng, na.rm = TRUE),
        lat2 = max(fit_df$lat, na.rm = TRUE)
      )
  }

  # Add search bar
  map <- map |> addSearchOSM(
    options = searchOptions(
      position = "topleft",
      zoom = 16,
      autoCollapse = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  )

  return(map)
}
