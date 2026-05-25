library(dplyr)
library(leaflet)
library(leaflet.extras)
library(osrm)
library(sf)

create_a_map <- function(
  data.path = file.path("..", "data", "map_data.csv"),
  show_attractions = FALSE,
  show_transport = TRUE,
  show_accommodation = TRUE
) {
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
    transport_df <- filter(places, type == "transport")
    accommodation_df <- filter(places, type == "accommodation")
    venue_df <- filter(places, type == "venue")
    attraction_df <- filter(places, type == "attraction")

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

create_route_map <- function(data.path, osrm.profile = "car") {

  # -------------------------------------------------
  # LOAD DATA
  # -------------------------------------------------

  places <- read.csv(data.path, stringsAsFactors = FALSE)

  places$lat <- as.numeric(places$lat)
  places$lng <- as.numeric(places$lng)
  places$type <- paste0(
    toupper(substr(places$type, 1, 1)),
    tolower(substr(places$type, 2, nchar(places$type)))
  )
  places$name <- as.character(places$name)

  if (!("link" %in% names(places))) places$link <- NA_character_

  places[["popup"]] <- ifelse(
    is.na(places[["link"]]) | places[["link"]] == "",
    places[["name"]],
    paste0(
      "<b>", places[["name"]], "</b><br>",
      "<a href='", places[["link"]], "' target='_blank'>More information</a>"
    )
  )

  # -------------------------------------------------
  # SPLIT DATA
  # -------------------------------------------------

  route_points <- places |>
    filter(type == "Route") |>
    mutate(order_num = as.numeric(name)) |>
    arrange(order_num)

  place_points <- places |>
    filter(type != "Route")

  # -------------------------------------------------
  # MAP BASE
  # -------------------------------------------------

  map <- leaflet() |>
    addProviderTiles("OpenStreetMap.Mapnik")

  overlay_groups <- c()

  # -------------------------------------------------
  # PLACES (GROUPED BY TYPE)
  # -------------------------------------------------

  place_types <- unique(place_points$type)

  for (t in place_types) {

    df <- filter(place_points, type == t)

    overlay_groups <- c(overlay_groups, t)

    map <- map |>

      addCircleMarkers(
        data = df,
        lng = ~lng,
        lat = ~lat,
        radius = 6,
        color = "#1f5eff",
        fillColor = "#1f5eff",
        fillOpacity = 0.85,
        stroke = FALSE,
        popup = ~popup,
        group = t
      ) |>

      addLabelOnlyMarkers(
        data = df,
        lng = ~lng,
        lat = ~lat,
        label = ~name,
        group = t,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-size" = "11px",
            "font-weight" = "500",
            "background-color" = "rgba(255,255,255,0.85)",
            "padding" = "2px 4px",
            "border-radius" = "4px"
          )
        )
      )
  }

  # -------------------------------------------------
  # ROUTE (LINES + START + END TOGETHER)
  # -------------------------------------------------

  if (nrow(route_points) >= 1) {

    overlay_groups <- c(overlay_groups, "Route")

    if (nrow(route_points) >= 2) {
      # ROUTE LINES
      for (i in 1:(nrow(route_points) - 1)) {

        from <- route_points[i, ]
        to <- route_points[i + 1, ]

        route <- osrmRoute(
          src = data.frame(lon = from$lng, lat = from$lat),
          dst = data.frame(lon = to$lng, lat = to$lat),
          overview = "full",
          osrm.profile = osrm.profile
        )

        if (!is.null(route) && nrow(route) > 0) {
          map <- map |>
            addPolylines(
              data = route,
              color = "#ff5500",
              weight = 6,
              opacity = 0.9,
              group = "Route"
            )
        }
      }
    }



    # ROUTE ENDPOINTS (IN SAME LAYER)
    start_pt <- route_points[1, ]
    end_pt <- route_points[nrow(route_points), ]

    map <- map |>
      addCircleMarkers(
        lng = start_pt$lng,
        lat = start_pt$lat,
        radius = 11,
        color = "white",
        weight = 2,
        fillColor = "#2f855a",
        fillOpacity = 1,
        popup = paste0("START: ", start_pt$name),
        group = "Route"
      ) |>
      addLabelOnlyMarkers(
        lng = start_pt$lng,
        lat = start_pt$lat,
        label = "START",
        group = "Route",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-size" = "12px",
            "font-weight" = "900",
            "color" = "white",
            "background-color" = "#2f855a",
            "padding" = "4px 8px",
            "border-radius" = "6px"
          )
        )
      )

    if (nrow(route_points) >= 2) {
      map <- map |>
        addCircleMarkers(
          lng = end_pt$lng,
          lat = end_pt$lat,
          radius = 11,
          color = "white",
          weight = 2,
          fillColor = "#e53e3e",
          fillOpacity = 1,
          popup = paste0("END: ", end_pt$name),
          group = "Route"
        ) |>
        addLabelOnlyMarkers(
          lng = end_pt$lng,
          lat = end_pt$lat,
          label = "END",
          group = "Route",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            style = list(
              "font-size" = "12px",
              "font-weight" = "900",
              "color" = "white",
              "background-color" = "#e53e3e",
              "padding" = "4px 8px",
              "border-radius" = "6px"
            )
          )
        )
    }

  }

  # -------------------------------------------------
  # LAYER CONTROL (TOP RIGHT)
  # -------------------------------------------------

  if (length(overlay_groups) > 0) {
    map <- map |>
      addLayersControl(
        overlayGroups = overlay_groups,
        options = layersControlOptions(
          collapsed = FALSE,
          position = "topright"
        )
      )
  }

  # -------------------------------------------------
  # FIT BOUNDS
  # -------------------------------------------------

  map <- map |>
    fitBounds(
      lng1 = min(places$lng, na.rm = TRUE),
      lat1 = min(places$lat, na.rm = TRUE),
      lng2 = max(places$lng, na.rm = TRUE),
      lat2 = max(places$lat, na.rm = TRUE)
    )

  return(map)
}
