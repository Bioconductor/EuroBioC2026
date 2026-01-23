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

  map <- leaflet(places) |>
    addProviderTiles("OpenStreetMap.Mapnik") |>

    # ----------------------------
  # TRANSPORT
  # ----------------------------
  addCircleMarkers(
    data = filter(places, type == "transport"),
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
      data = filter(places, type == "transport"),
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
    ) |>

    # ----------------------------
  # ACCOMMODATION
  # ----------------------------
  addCircleMarkers(
    data = filter(places, type == "accommodation"),
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
      data = filter(places, type == "accommodation"),
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
    ) |>

    # ----------------------------
  # VENUE (HALO)
  # ----------------------------
  addCircleMarkers(
    data = filter(places, type == "venue"),
    ~lng, ~lat,
    radius = 26,
    color = type_colors["venue"],
    fillColor = type_colors["venue"],
    fillOpacity = 0.18,
    stroke = FALSE,
    group = "Venue"
  ) |>

    # ----------------------------
  # VENUE (CORE)
  # ----------------------------
  addCircleMarkers(
    data = filter(places, type == "venue"),
    ~lng, ~lat,
    radius = 12,
    color = type_colors["venue"],
    fillColor = type_colors["venue"],
    fillOpacity = 1,
    stroke = FALSE,
    group = "Venue",
    popup = ~popup
  ) |>

    # ----------------------------
  # ATTRACTIONS (hidden by default)
  # ----------------------------
  addCircleMarkers(
    data = filter(places, type == "attraction"),
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
      data = filter(places, type == "attraction"),
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
    ) |>

    # ----------------------------
  # VENUE LABEL
  # ----------------------------
  addLabelOnlyMarkers(
    data = filter(places, type == "venue"),
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
  ) |>

    # ----------------------------
  # LAYER CONTROL
  # ----------------------------
  addLayersControl(
    overlayGroups = c("Venue", "Transport", "Accommodation", "Attractions"),
    options = layersControlOptions(collapsed = FALSE)
  )

  # Conditionally hide
  if (show_attractions == FALSE) {
    map <- map |> hideGroup("Attractions")
  }
  if (show_transport == FALSE) {
    map <- map |> hideGroup("Transport")
  }
  if (show_accommodation == FALSE) {
    map <- map |> hideGroup("Accommodation")
  }

  # Determine which types are currently visible
  visible_types <- c("venue")  # venue is always visible
  if (show_transport) visible_types <- c(visible_types, "transport")
  if (show_accommodation) visible_types <- c(visible_types, "accommodation")
  if (show_attractions) visible_types <- c(visible_types, "attraction")

  # Filter only the visible places
  visible_places <- filter(places, type %in% visible_types)

  # Then fit bounds only to these
  map <- map |>
    fitBounds(
      lng1 = min(visible_places$lng, na.rm = TRUE),
      lat1 = min(visible_places$lat, na.rm = TRUE),
      lng2 = max(visible_places$lng, na.rm = TRUE),
      lat2 = max(visible_places$lat, na.rm = TRUE)
    )

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
