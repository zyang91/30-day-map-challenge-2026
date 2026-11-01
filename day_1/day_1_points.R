# 30 Day Map Challenge 2026, Day 1: Points
# "My Philadelphia was two miles wide": every place I stopped at in the city,
# Sept. 27 – Oct. 30, 2026, from my personal TripLibre (HHT) travel-diary export.
#
# The raw export lives in data/ and is gitignored, so it never leaves this machine.
# Only the rendered PNG is meant to be shared, and it drops the places listed in
# `private_places` below (home, and anything else you would rather not publish).
#
# Basemap: City of Philadelphia open data (OpenDataPhilly), pulled from the
# City's ArcGIS FeatureServers and cached in data/basemap/.

library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(ggtext)
library(patchwork)
library(showtext)

sf_use_s2(FALSE)

# ---- paths ------------------------------------------------------------------

export_dir <- list.files("data", pattern = "^hht-export-", full.names = TRUE) |>
  sort() |>
  tail(1)
cache_dir <- file.path("data", "basemap")
out_file  <- file.path("day_1", "day_1_points.png")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

crs_pa  <- 2272 # NAD83 / Pennsylvania South (ftUS)
ft_mile <- 5280

# ---- fonts: Franklin for text, a Cheltenham-ish serif for headline and numbers --

font_add_google("Libre Franklin", "franklin")
font_add_google("Libre Franklin", "franklin_bold", regular.wt = 700)
font_add_google("Source Serif 4", "serif_reg")
font_add_google("Source Serif 4", "serif_bold", regular.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

# ---- palette ----------------------------------------------------------------

pal <- list(
  bg      = "#FFFFFF",
  land    = "#F3F1EC",
  water   = "#CFE0E8",
  water_t = "#6F98AD",
  park    = "#E1E9D7",
  road    = "#E2DED6",
  road_x  = "#D2CDC3",
  ring    = "#9C968B",
  ink     = "#121212",
  ink_2   = "#555555",
  ink_3   = "#8C8C8C"
)

group_cols <- c(
  "Work & study"     = "#2D6A9F",
  "Food & errands"   = "#E0892F",
  "Culture & sports" = "#B8324B",
  "Getting around"   = "#4F8A55"
)

# ---- basemap from OpenDataPhilly (cached) ------------------------------------

phl_services <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services"

# Page through an ArcGIS FeatureServer layer (max 2000 records per request).
fetch_layer <- function(name, service, layer = 0, fields = "objectid") {
  path <- file.path(cache_dir, paste0(name, ".gpkg"))
  if (file.exists(path)) return(st_read(path, quiet = TRUE))

  message("Downloading ", service, " …")
  pages  <- list()
  offset <- 0
  repeat {
    url <- paste0(
      phl_services, "/", service, "/FeatureServer/", layer, "/query?",
      "where=1%3D1&outFields=", fields, "&outSR=4326&f=geojson",
      "&resultRecordCount=2000&resultOffset=", offset
    )
    page <- st_read(url, quiet = TRUE)
    pages[[length(pages) + 1]] <- page
    if (nrow(page) < 2000) break
    offset <- offset + 2000
  }
  out <- bind_rows(pages)
  st_write(out, path, quiet = TRUE)
  out
}

city    <- fetch_layer("city_limits", "City_Limits") |> st_transform(crs_pa)
streets <- fetch_layer("streets", "Street_Centerline", fields = "class") |> st_transform(crs_pa)
water   <- fetch_layer("hydrology", "Hydrographic_Features_Poly", layer = 1) |> st_transform(crs_pa)
parks   <- fetch_layer("parks", "PPR_Properties") |> st_transform(crs_pa) |> st_make_valid()

# Keep the basemap quiet: arterials only, no small ponds or pocket parks.
# Street class: 1 expressway, 2 major arterial, 3 minor arterial, 4–5 collector/local.
water$area <- as.numeric(st_area(water))
parks$area <- as.numeric(st_area(parks))
roads    <- filter(streets, class %in% c(2, 3))
freeways <- filter(streets, class == 1)
water    <- filter(water, area > 1e5)
parks    <- filter(parks, area > 1e5)

# ---- places -----------------------------------------------------------------


# Tidy up names typed on the go.
display_names <- c(
  "33rd st trolley station & food truck" = "33rd St. trolley stop",
  "34 st station"                        = "34th St. station",
  "Weitzman office"                      = "Weitzman School complex",
  "walnut lucast subway station"         = "Walnut–Locust station",
  "Amtrak -30th St station"              = "30th Street Station",
  "Lincoln Financial Stadium"            = "Lincoln Financial Field",
  "philly airport"                       = "Airport",
  "NRG Station"                          = "NRG station",
  "hair cut-Chinatown"                   = "Chinatown barber",
  "whole food sale market"               = "Produce market",
  "Starbuck-Weitzman"                    = "Starbucks",
  "K+k restaurant"                       = "K+K restaurant",
  "Walnut theater"                       = "Walnut Street Theatre",
  "Penn museum"                          = "Penn Museum",
  "Buy more"                             = "Shopping",
  "class 1"                              = "Classroom",
  "teaching"                             = "Teaching",
  "University Architect"                 = "University Architect’s office"
)

category_group <- function(cat) {
  case_when(
    cat %in% c("work_school", "library")                         ~ "Work & study",
    cat %in% c("museum", "event_venue", "stadium", "recreation")  ~ "Culture & sports",
    cat %in% c("transit_station", "rail_station", "airport")     ~ "Getting around",
    TRUE                                                         ~ "Food & errands"
  )
}

places_raw <- read_csv(file.path(export_dir, "places.csv"), show_col_types = FALSE)
visits_raw <- read_csv(file.path(export_dir, "visits.csv"), show_col_types = FALSE)

visit_stats <- visits_raw |>
  filter(!is.na(place_id)) |>
  group_by(place_id) |>
  summarise(
    n_visits = n(),
    hours    = sum(duration_min, na.rm = TRUE) / 60,
    dates    = paste(unique(paste0(format(as.Date(arrival_utc), "%b. "),
                                       as.integer(format(as.Date(arrival_utc), "%d")))),
                     collapse = " & ")
  )

places <- places_raw |>
  mutate(name = trimws(name)) |>
  filter(is.na(merged_into), !is.na(name), !name %in% private_places,
         is.na(category) | category != "home") |>
  left_join(visit_stats, by = "place_id") |>
  mutate(
    n_visits = coalesce(n_visits, visit_count),
    hours    = coalesce(hours, 0),
    label    = coalesce(display_names[name], name),
    group    = factor(category_group(category), levels = names(group_cols))
  ) |>
  filter(n_visits > 0) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_pa) |>
  st_filter(city) |> # inside Philadelphia city limits only
  arrange(desc(n_visits))

places <- places |>
  mutate(x = st_coordinates(places)[, 1], y = st_coordinates(places)[, 2])


office <- filter(places, label == "Weitzman School complex")
places$miles <- as.numeric(st_distance(places, office)) / ft_mile

n_visits_total <- sum(places$n_visits)
n_within_2mi   <- sum(places$n_visits[places$miles <= 2])
share_2mi      <- round(100 * n_within_2mi / n_visits_total)

# Frame the map on the two-mile circle, nudged east so Center City fits.
cx <- office$x + 2600
cy <- office$y + 900
half_w <- 11800
half_h <- 10600
bb <- c(xmin = cx - half_w, xmax = cx + half_w, ymin = cy - half_h, ymax = cy + half_h)

in_frame <- with(places, x > bb["xmin"] & x < bb["xmax"] & y > bb["ymin"] & y < bb["ymax"])

rings <- st_sf(
  miles    = c(0.5, 1, 2),
  txt      = c("½ MILE", "1 MILE", "2 MILES"),
  geometry = st_buffer(st_geometry(office)[rep(1, 3)], c(0.5, 1, 2) * ft_mile)
)

# ---- places beyond the frame: arrows pinned to the map edge ----------------------

edge_t <- function(dx, dy, inset = 900) {
  tx <- ifelse(dx > 0, (bb["xmax"] - inset - cx) / dx, (bb["xmin"] + inset - cx) / dx)
  ty <- ifelse(dy > 0, (bb["ymax"] - inset - cy) / dy, (bb["ymin"] + inset - cy) / dy)
  pmin(tx, ty)
}

off_frame_labels <- c("Lincoln Financial Field", "Airport", "Temple University", "H-Mart")

beyond <- places |>
  st_drop_geometry() |>
  filter(!in_frame, label %in% off_frame_labels) |>
  mutate(
    t   = edge_t(x - cx, y - cy),
    ex  = cx + t * (x - cx),
    ey  = cy + t * (y - cy),
    ang = atan2(y - cy, x - cx),
    # Arrow tail sits a little inward from the tip, along the ray to the place.
    sx  = ex - 1100 * cos(ang),
    sy  = ey - 1100 * sin(ang),
    txt = paste0("<b>", label, "</b><br>", sprintf("%.1f", miles), " mi · ", dates)
  )

# ---- callouts: the handful of places that carry the story -------------------------

pl <- function(lbl) filter(st_drop_geometry(places), label == lbl)

callout <- function(lbl, body, dx, dy, hjust = 0, curvature = 0.2) {
  p <- pl(lbl)
  tibble(px = p$x, py = p$y, tx = p$x + dx, ty = p$y + dy,
         txt = paste0("<b>", lbl, "</b><br>", body), hjust = hjust, curvature = curvature)
}

fmt_h <- function(h) paste0(round(h), " hours")

callouts <- bind_rows(
  callout("Weitzman School complex",
          paste0(pl("Weitzman School complex")$n_visits, " visits and ",
                 fmt_h(pl("Weitzman School complex")$hours), ".<br>",
                 "No place but home got more of my time."),
          dx = -4600, dy = -3900, curvature = -0.25),
  callout("33rd St. trolley stop",
          paste0(pl("33rd St. trolley stop")$n_visits, " stops, and a food truck<br>on the corner"),
          dx = -2600, dy = 5400),
  callout("City Hall station",
          paste0(pl("City Hall station")$n_visits, " visits, at the spot where<br>the subway lines cross"),
          dx = -1200, dy = 4600, curvature = -0.2),
  callout("Academy of Music",
          paste0(pl("Academy of Music")$n_visits, " visits and ",
                 fmt_h(pl("Academy of Music")$hours), ".<br>My night out, most weeks."),
          dx = 1400, dy = -4800)
)

# ---- map ------------------------------------------------------------------------

to_ft <- function(lon, lat) {
  st_coordinates(st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_pa))
}
area_lbl <- function(lon, lat, txt, size = 2.6, colour = pal$ink_3, family = "franklin_bold",
                     fontface = "plain", angle = 0) {
  xy <- to_ft(lon, lat)
  annotate("text", x = xy[1], y = xy[2], label = txt, size = size, colour = colour,
           family = family, fontface = fontface, angle = angle, lineheight = 0.9)
}

p_map <- ggplot() +
  geom_sf(data = city, fill = pal$land, colour = NA) +
  geom_sf(data = parks, fill = pal$park, colour = NA) +
  geom_sf(data = roads, colour = pal$road, linewidth = 0.3) +
  geom_sf(data = freeways, colour = pal$road_x, linewidth = 0.55) +
  geom_sf(data = water, fill = pal$water, colour = NA) +
  # Distance rings around the office.
  geom_sf(data = rings, fill = NA, colour = pal$ring, linewidth = 0.35, linetype = "22") +
  geom_richtext(
    data = st_drop_geometry(rings), aes(x = office$x, y = office$y + miles * ft_mile, label = txt),
    family = "franklin_bold", size = 2.1, colour = pal$ring,
    fill = pal$land, label.colour = NA, label.padding = unit(1.5, "pt")
  ) +
  # Neighbourhoods and river, set quietly behind the data.
  area_lbl(-75.2135, 39.9580, "UNIVERSITY\nCITY") +
  area_lbl(-75.1655, 39.9575, "CENTER\nCITY") +
  area_lbl(-75.1812, 39.9478, "Schuylkill River", size = 2.5, colour = pal$water_t,
           family = "serif_reg", fontface = "italic", angle = 68) +
  # Points.
  geom_sf(data = places[in_frame, ], aes(size = n_visits, fill = group),
          shape = 21, colour = "white", stroke = 0.5, alpha = 0.95) +
  # Callouts.
  geom_curve(data = filter(callouts, curvature > 0),
             aes(x = tx, y = ty, xend = px, yend = py),
             curvature = 0.2, linewidth = 0.3, colour = pal$ink) +
  geom_curve(data = filter(callouts, curvature < 0),
             aes(x = tx, y = ty, xend = px, yend = py),
             curvature = -0.2, linewidth = 0.3, colour = pal$ink) +
  geom_richtext(
    data = callouts, aes(x = tx, y = ty, label = txt, hjust = hjust),
    family = "franklin", size = 2.9, colour = pal$ink, lineheight = 1.15,
    fill = alpha(pal$land, 0.85), label.colour = NA, vjust = 0.5,
    label.padding = unit(c(2, 3, 2, 3), "pt")
  ) +
  # Beyond the frame.
  geom_segment(data = beyond, aes(x = sx, y = sy, xend = ex, yend = ey),
               arrow = arrow(length = unit(4, "pt"), type = "closed"),
               linewidth = 0.4, colour = pal$ink_2) +
  geom_richtext(
    data = beyond,
    aes(x = sx - 250 * cos(ang), y = sy - 250 * sin(ang), label = txt,
        hjust = ifelse(abs(cos(ang)) > 0.7, ifelse(cos(ang) > 0, 1, 0), 0.5),
        vjust = ifelse(abs(cos(ang)) > 0.7, 0.5, ifelse(sin(ang) > 0, 1, 0))),
    family = "franklin", size = 2.5, colour = pal$ink_2, lineheight = 1.1,
    fill = NA, label.colour = NA, label.padding = unit(1, "pt")
  ) +
  scale_size_area(max_size = 13, limits = c(1, max(places$n_visits)),
                  guide = "none") +
  scale_fill_manual(values = group_cols, guide = "none") +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
           expand = FALSE, crs = crs_pa, datum = NA) +
  theme_void(base_family = "franklin") +
  theme(
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.margin       = margin(0, 0, 0, 0)
  )

# ---- side column: one big number, then where the hours went ------------------------

top_hours <- places |>
  st_drop_geometry() |>
  filter(hours >= 1) |>
  slice_max(hours, n = 8) |>
  mutate(label = factor(label, levels = rev(label)))

p_bars <- ggplot(top_hours, aes(x = hours, y = label, fill = group)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = paste0(round(hours), " h")), hjust = -0.2,
            family = "franklin", size = 2.7, colour = pal$ink_2) +
  scale_fill_manual(values = group_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = paste0(
      "<span style='font-family:serif_bold; font-size:64pt; color:", group_cols[["Work & study"]], "'>",
      share_2mi, "%</span><br>",
      "<span style='font-size:12pt'>of my ", n_visits_total, " stops in the city were within<br>",
      "two miles of the office.</span><br><br><br>",
      "<b>Where the hours went</b>"
    ),
    subtitle = "Time spent at each place, top eight"
  ) +
  theme_minimal(base_family = "franklin") +
  theme(
    panel.grid    = element_blank(),
    axis.text.x   = element_blank(),
    axis.title    = element_blank(),
    axis.text.y   = element_text(size = 8.5, colour = pal$ink, hjust = 1,
                                 margin = margin(r = 4)),
    plot.title    = element_markdown(family = "franklin", size = 11, colour = pal$ink,
                                     lineheight = 1.25, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 8.5, colour = pal$ink_2, margin = margin(b = 6)),
    plot.title.position = "plot",
    plot.margin   = margin(10, 0, 0, 0)
  )

# How to read the circles, in a sentence and three examples instead of a legend box.
size_key <- tibble(n = c(1, 5, 20), x = c(0.02, 0.36, 0.78),
                   txt = c("1 visit", "5 visits", "20 visits")) |>
  mutate(r = 0.083 * sqrt(n / 20)) # circle radius in key units, so labels hug each circle

p_key <- ggplot(size_key) +
  geom_point(aes(x = x, y = 0, size = n), shape = 21, fill = NA,
             colour = pal$ink_2, stroke = 0.45) +
  geom_text(aes(x = x + r + 0.03, y = 0, label = txt), hjust = 0,
            family = "franklin", size = 2.7, colour = pal$ink_2) +
  scale_size_area(max_size = 13, limits = c(1, max(places$n_visits)), guide = "none") +
  scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "On the map, circles grow with the number of visits.") +
  theme_void(base_family = "franklin") +
  theme(plot.title = element_text(size = 8.5, colour = pal$ink_2, margin = margin(b = 2)),
        plot.title.position = "plot",
        plot.margin = margin(18, 0, 6, 6))

p_side <- plot_spacer() / p_bars / wrap_elements(full = p_key) / plot_spacer() +
  plot_layout(heights = c(0.04, 1, 0.22, 0.08))

# ---- assemble -------------------------------------------------------------------

swatch <- function(g, txt = tolower(g)) {
  paste0("<b style='color:", group_cols[[g]], "'>", txt, "</b>")
}

final <- (p_map | plot_spacer() | p_side) +
  plot_layout(widths = c(2.15, 0.14, 1)) +
  plot_annotation(
    title    = "My Philadelphia was two miles wide",
    subtitle = paste0(
      "Over five weeks this fall, almost every place I stopped sat in a narrow band between Penn’s campus ",
      "and Center City,<br>whether I was there for ",
      swatch("Work & study"), ", ", swatch("Food & errands"), ", ",
      swatch("Culture & sports"), " or just ", swatch("Getting around"),
      ". The rest of the city was a game, a flight and a grocery run."
    ),
    caption  = paste0(
      "Each circle is a place I stopped between Sept. 27 and Oct. 30, 2026, sized by number of visits. ",
      "Rings measure straight-line distance from the office. Home is omitted.<br>",
      "Source: personal TripLibre travel diary (private) · Basemap: City of Philadelphia via OpenDataPhilly · ",
      "#30DayMapChallenge 2026, Day 1: Points"
    ),
    theme = theme(
      plot.background = element_rect(fill = pal$bg, colour = NA),
      plot.title    = element_text(family = "serif_bold", size = 30, colour = pal$ink,
                                   margin = margin(t = 4, b = 8)),
      plot.subtitle = element_markdown(family = "franklin", size = 11.5, colour = pal$ink_2,
                                       lineheight = 1.35, margin = margin(b = 16)),
      plot.caption  = element_markdown(family = "franklin", size = 7, colour = pal$ink_3,
                                       hjust = 0, lineheight = 1.4, margin = margin(t = 12)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      plot.margin   = margin(22, 26, 14, 26)
    )
  )

ggsave(out_file, final, width = 13.6, height = 9.0, dpi = 300, bg = pal$bg)
