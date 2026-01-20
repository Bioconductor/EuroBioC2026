library(leaflet)
library(dplyr)

create_a_map <- function(show_attractions = FALSE, show_transport = TRUE, show_accommodation = TRUE){
    places <- data.frame(
        name = c(
            # Venue
            "BioCity Turku",
            # Transport
            "Kupittaa railway station",
            "Central railway station",
            "Bus station",
            # Accommodation
            "Sokos Hotel Kupittaa",
            "Bore",
            "Centro hotel",
            "Forenom Aparthotel",
            "Omena Hotel, Humalistonkatu",
            "Omena Hotel, Kauppiaskatu",
            "Scandic Hamburger Börs",
            "Scandic Julia",
            "Scandic Plaza",
            "Sokos Hotel Wiklund",
            # Attractions
            "Föri cable ferry",
            "Luostarinmäki",
            "Posankka",
            "Suomen Joutsen ship",
            "Turku Castle",
            "Turku Cathedral",
            "Ruissalo",
            "Paavo Nurmi statue",
            "Vähätori",
            "Old Great Square",
            "University Hill",
            "Market Hall",
            "Market Square"
        ),
        lat = c(
            # Venue
            60.449245,
            # Transport
            60.450278,
            60.456560,
            60.456720,
            # Accommodation
            60.45064568695374,
            60.43538198015936,
            60.45329874563878,
            60.450604033353564,
            60.451786058898904,
            60.45127812013253,
            60.45095241251385,
            60.45247822911028,
            60.45096175931625,
            60.452062101584694,
            # Attractions
            60.44125263669091,
            60.447718063010235,
            60.45909829087898,
            60.436423145140914,
            60.435430945366626,
            60.45248295806699,
            60.42811408205572,
            60.44797690400434,
            60.4512708499933,
            60.450695164471114,
            60.45451657365545,
            60.44971949569192,
            60.45174707388272
        ),
        lng = c(
            # Venue
            22.293052,
            # Transport
            22.296944,
            22.260760,
            22.266810,
            # Accommodation
            22.296439684431835,
            22.234050053746845,
            22.27139799977402,
            22.262739706488432,
            22.259048982360735,
            22.270035309937988,
            22.268656015104604,
            22.27238174210298,
            22.26161705373621,
            22.26919319450209,
            # Attractions
            22.247928011418203,
            22.276627523390086,
            22.289516330876715,
            22.237383032306006,
            22.228902947637483,
            22.278314462843667,
            22.14293591337591,
            22.27003446911901,
            22.273037982133197,
            22.275773247373095,
            22.284448914525427,
            22.266094817521928,
            22.26694352548474
        ),
        type = c(
            "venue",
            rep("transport", 3),
            rep("accommodation", 10),
            rep("attraction", 13)
        )
    )

    # Add attractions color
    type_colors <- c(
        venue = "#1f5eff",
        transport = "#2f855a",
        accommodation = "#805ad5",
        attraction = "#e34a33"   # red-orange for attractions
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
        group = "Transport"
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
        group = "Accommodation"
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
        group = "Venue"
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
        group = "Attractions"
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
            lng1 = min(visible_places$lng),
            lat1 = min(visible_places$lat),
            lng2 = max(visible_places$lng),
            lat2 = max(visible_places$lat)
        )

    return(map)
}
