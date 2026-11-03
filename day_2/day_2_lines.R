# 30 Day Map Challenge 2026, Day 2: Lines
# One day of my Atlanta trip, Oct. 2, 2026, drawn as the nine trips my
# TripLibre (HHT) travel diary recorded, colored by how I got there, with the
# same day laid out as a timeline underneath.
#
# The raw export lives in data/ and is gitignored. Only the rendered PNG is meant
# to be shared. The hotel is a public building, and I labeled it by neighborhood only.
#
# Basemap: OpenStreetMap via the Overpass API (osmdata), cached in data/basemap/.
# Claude Visualization, checked by Zhanchao Yang 

library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(ggtext)
library(patchwork)
library(showtext)
library(osmdata)
library(lubridate)

sf_use_s2(FALSE)



font_add_google("Libre Franklin", "franklin")
font_add_google("Libre Franklin", "franklin_bold", regular.wt = 700)
font_add_google("Source Serif 4", "serif_reg")
font_add_google("Source Serif 4", "serif_bold", regular.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)


pal <- list(
  bg      = "#FAF8F4",
  land    = "#F3F1EC",
  water   = "#CFE0E8",
  water_t = "#6F98AD",
  road    = "#E2DED6",
  road_x  = "#D2CDC3",
  stay    = "#E4E0D8",
  ink     = "#121212",
  ink_2   = "#555555",
  ink_3   = "#8C8C8C"
)

mode_cols <- c(
  "Taxi"    = "#E0892F",
  "MARTA"   = "#2D6A9F",
  "On foot" = "#4F8A55"
)


bbox_atl <- c(-84.505, 33.735, -84.295, 33.915)
set_overpass_url("https://overpass-api.de/api/interpreter")

fetch_osm <- function(name, key, values) {
  path <- file.path(cache_dir, paste0("atl_", name, ".gpkg"))
  if (file.exists(path)) return(st_read(path, quiet = TRUE))

  res <- opq(bbox_atl, timeout = 180) |>
    add_osm_feature(key = key, value = values) |>
    osmdata_sf()
  layers <- Filter(\(l) !is.null(l) && nrow(l) > 0,
                   res[c("osm_lines", "osm_polygons", "osm_multipolygons")])
  out <- bind_rows(lapply(layers, \(l) select(l, any_of(c("osm_id", key)))))
  st_write(out, path, quiet = TRUE)
  out
}

keep_type <- function(x, types) x[st_geometry_type(x) %in% types, ]

roads <- fetch_osm("roads", "highway", c("motorway", "trunk", "primary", "secondary")) |>
  keep_type(c("LINESTRING", "MULTILINESTRING")) |>
  st_transform(crs_atl)
water <- fetch_osm("water", "natural", "water") |>
  keep_type(c("POLYGON", "MULTIPOLYGON")) |>
  st_transform(crs_atl) |>
  st_make_valid()
rivers <- fetch_osm("rivers", "waterway", "river") |>
  keep_type(c("LINESTRING", "MULTILINESTRING")) |>
  st_transform(crs_atl)

water <- water[as.numeric(st_area(water)) > 2e4, ]
freeways <- filter(roads, highway %in% c("motorway", "trunk"))
roads    <- filter(roads, !highway %in% c("motorway", "trunk"))



# Google encoded polyline (precision 1e-5) -> matrix of lon/lat.
decode_polyline <- function(enc) {
  b <- utf8ToInt(enc) - 63L
  vals <- integer(0)
  res <- 0L
  shift <- 0L
  for (x in b) {
    res <- bitwOr(res, bitwShiftL(bitwAnd(x, 31L), shift))
    shift <- shift + 5L
    if (x < 32L) {
      vals <- c(vals, if (bitwAnd(res, 1L)) -bitwShiftR(res, 1L) - 1L else bitwShiftR(res, 1L))
      res <- 0L
      shift <- 0L
    }
  }
  lat <- cumsum(vals[c(TRUE, FALSE)]) / 1e5
  lon <- cumsum(vals[c(FALSE, TRUE)]) / 1e5
  cbind(lon, lat)
}

proj <- function(lonlat) sf_project("EPSG:4326", paste0("EPSG:", crs_atl), lonlat)

# GPS routes jitter at the ends of a trip and sometimes jump out and back.
# Pin each leg to where it really started and ended, drop vertices where the
# path doubles back on itself (a turn sharper than `max_turn` degrees), then
# round the corners with a few passes of Chaikin smoothing.
clean_path <- function(m, start, end, max_turn = 125, min_step = 40) {
  m <- rbind(start, m, end)
  step <- c(Inf, sqrt(rowSums(diff(m)^2)))
  m <- m[step > min_step | seq_len(nrow(m)) == nrow(m), , drop = FALSE]
  m[1, ] <- start
  repeat {
    n <- nrow(m)
    if (n < 3) break
    a <- m[2:(n - 1), , drop = FALSE] - m[1:(n - 2), , drop = FALSE]
    b <- m[3:n, , drop = FALSE] - m[2:(n - 1), , drop = FALSE]
    turn <- abs(atan2(a[, 1] * b[, 2] - a[, 2] * b[, 1], a[, 1] * b[, 1] + a[, 2] * b[, 2]))
    if (max(turn) * 180 / pi < max_turn) break
    m <- m[-(which.max(turn) + 1), , drop = FALSE]
  }
  chaikin(m)
}

chaikin <- function(m, passes = 3) {
  for (i in seq_len(passes)) {
    n <- nrow(m)
    if (n < 3) break
    p <- m[-n, , drop = FALSE]
    q <- m[-1, , drop = FALSE]
    cuts <- matrix(t(cbind(0.75 * p + 0.25 * q, 0.25 * p + 0.75 * q)), ncol = 2, byrow = TRUE)
    m <- rbind(m[1, ], cuts, m[n, ])
  }
  m
}

trips <- read_csv(file.path(export_dir, "trips.csv"), show_col_types = FALSE) |>
  filter(person_day_id == the_day) |>
  arrange(trip_seq)
segments <- read_csv(file.path(export_dir, "trip_segments.csv"), show_col_types = FALSE) |>
  filter(trip_id %in% trips$trip_id, !is.na(route_polyline))
visits <- read_csv(file.path(export_dir, "visits.csv"), show_col_types = FALSE)

# The trip-level mode is my own correction; segment modes are still the phone's
# guesses (it called the MARTA ride "car", and a 15 mph leg "walk"), so trust
# walking segments only at walking speed and give everything else the trip's mode.
mode_label <- c(taxi = "Taxi", subway = "MARTA", walk = "On foot")

legs <- segments |>
  left_join(select(trips, trip_id, trip_seq, trip_mode = mode,
                   origin_lon, origin_lat, destination_lon, destination_lat), by = "trip_id") |>
  arrange(trip_seq, seq) |>
  mutate(
    kmh  = distance_m / 1000 / ((end_ts - start_ts) / 3600),
    mode = ifelse(mode == "walk" & kmh < 7, "On foot", mode_label[trip_mode]),
    mode = factor(mode, levels = names(mode_cols)),
    raw  = lapply(route_polyline, \(p) proj(decode_polyline(p)))
  ) |>
  # Back-to-back legs with the same mode become one leg, so a jump at the seam
  # gets cleaned like any other.
  group_by(trip_id) |>
  mutate(run = cumsum(mode != lag(mode, default = first(mode)))) |>
  group_by(trip_id, trip_seq, run, mode, origin_lon, origin_lat, destination_lon, destination_lat) |>
  summarise(start_utc = first(start_utc), end_utc = last(end_utc),
            distance_m = sum(distance_m), raw = list(do.call(rbind, raw)), .groups = "drop") |>
  rename(seq = run) |>
  group_by(trip_id) |>
  mutate(
    # A leg starts at the trip origin or where the previous leg ended, and ends
    # at the trip destination or where the next leg starts.
    start = ifelse(seq == min(seq),
                   lapply(row_number(), \(i) proj(cbind(origin_lon[i], origin_lat[i]))),
                   lapply(lag(raw), \(m) m[nrow(m), , drop = FALSE])),
    end   = ifelse(seq == max(seq),
                   lapply(row_number(), \(i) proj(cbind(destination_lon[i], destination_lat[i]))),
                   lapply(lead(raw), \(m) m[1, , drop = FALSE]))
  ) |>
  ungroup() |>
  mutate(geometry = st_sfc(
    Map(\(m, s, e) st_linestring(clean_path(m, s, e)), raw, start, end),
    crs = crs_atl
  )) |>
  select(-raw, -start, -end) |>
  st_as_sf()

total_miles <- sum(trips$distance_m) / m_mile


to_xy <- function(lon, lat) proj(cbind(lon, lat))

# Hand-picked stops for the map; coordinates are the diary's own place records.
stops <- tribble(
  ~key,      ~label,                    ~lon,       ~lat,
  "hotel",   "Hotel",                   -84.368564, 33.844717,
  "emory",   "Emory University",        -84.325251, 33.788306,
  "coke",    "World of Coca-Cola",      -84.392753, 33.763237,
  "arena",   "State Farm Arena",        -84.396345, 33.756423,
  "five",    "Five Points",             -84.391706, 33.753904,
  "lind",    "Lindbergh Center",        -84.369444, 33.823132,
  "battery", "The Battery Atlanta",     -84.471340, 33.888729
)
stops[c("x", "y")] <- to_xy(stops$lon, stops$lat)

st <- function(k) filter(stops, key == k)

numbered <- filter(stops, key %in% c("emory", "coke", "arena", "battery")) |>
  mutate(n = c(1, 2, 3, 4))
stations <- filter(stops, key %in% c("five", "lind"))


cx <- mean(range(stops$x)) - 300
cy <- mean(range(stops$y)) + 200
half <- 9800
bb <- c(xmin = cx - half, xmax = cx + half, ymin = cy - half * 0.9, ymax = cy + half * 0.9)


area_lbl <- function(lon, lat, txt, size = 2.6, colour = pal$ink_3, family = "franklin_bold",
                     fontface = "plain", angle = 0) {
  xy <- to_xy(lon, lat)
  annotate("text", x = xy[1], y = xy[2], label = txt, size = size, colour = colour,
           family = family, fontface = fontface, angle = angle, lineheight = 0.9)
}

note <- function(k, body, dx, dy, hjust = 0, curvature = 0.2) {
  s <- st(k)
  tibble(px = s$x, py = s$y, tx = s$x + dx, ty = s$y + dy, txt = body,
         hjust = hjust, curvature = curvature)
}

notes <- bind_rows(
  note("emory", "<b>1 Emory University</b><br>An hour walking the campus", dx = -600, dy = 2900,
       hjust = 0.5, curvature = 0),
  note("coke", "<b>2 World of Coca-Cola</b><br>An hour and a half inside", dx = 3300, dy = 1300,
       curvature = -0.2),
  note("arena", "<b>3 State Farm Arena</b><br>A 40-minute walk, then<br>MARTA back north",
       dx = -1900, dy = -700, hjust = 1, curvature = -0.2),
  note("battery", "<b>4 The Battery Atlanta</b><br>A stroll, then dinner<br>next door",
       dx = -700, dy = -1900, hjust = 0.5, curvature = 0),
  note("hotel", "<b>Hotel in Buckhead</b><br>Home base: out and back<br>twice in one day",
       dx = 2600, dy = 2100, curvature = 0.2)
)

p_map <- ggplot() +
  geom_sf(data = rivers, colour = pal$water, linewidth = 1.4) +
  geom_sf(data = water, fill = pal$water, colour = NA) +
  geom_sf(data = roads, colour = pal$road, linewidth = 0.3) +
  geom_sf(data = freeways, colour = pal$road_x, linewidth = 0.6) +
  area_lbl(-84.378, 33.866, "BUCKHEAD") +
  area_lbl(-84.407, 33.785, "MIDTOWN") +
  area_lbl(-84.362, 33.759, "DOWNTOWN") +
  area_lbl(-84.488, 33.850, "CUMBERLAND") +
  area_lbl(-84.452, 33.842, "Chattahoochee\nRiver", size = 2.5, colour = pal$water_t,
           family = "serif_reg", fontface = "italic") +
  # Routes: a casing in the page colour so overlapping legs stay readable.
  geom_sf(data = legs, colour = pal$bg, linewidth = 2.6, lineend = "round") +
  geom_sf(data = legs, aes(colour = mode), linewidth = 1.3, lineend = "round",
          linejoin = "round") +
  # Stops.
  geom_point(data = stations, aes(x, y), shape = 21, size = 2, fill = "white",
             colour = mode_cols[["MARTA"]], stroke = 0.9) +
  geom_text(data = stations, aes(x + ifelse(key == "five", 350, -350), y,
                                 label = paste(label, "station"), hjust = ifelse(key == "five", 0, 1)),
            family = "franklin", fontface = "italic",
            size = 2.4, colour = mode_cols[["MARTA"]]) +
  geom_point(data = st("hotel"), aes(x, y), shape = 22, size = 3.4, fill = pal$ink,
             colour = "white", stroke = 0.7) +
  geom_point(data = numbered, aes(x, y), shape = 21, size = 4.6, fill = pal$ink,
             colour = "white", stroke = 0.7) +
  geom_text(data = numbered, aes(x, y, label = n), family = "franklin_bold",
            size = 2.4, colour = "white") +
  # Notes.
  geom_curve(data = filter(notes, curvature > 0), aes(x = tx, y = ty, xend = px, yend = py),
             curvature = 0.2, linewidth = 0.3, colour = pal$ink) +
  geom_curve(data = filter(notes, curvature < 0), aes(x = tx, y = ty, xend = px, yend = py),
             curvature = -0.2, linewidth = 0.3, colour = pal$ink) +
  geom_segment(data = filter(notes, curvature == 0), aes(x = tx, y = ty, xend = px, yend = py),
               linewidth = 0.3, colour = pal$ink) +
  geom_richtext(
    data = notes, aes(x = tx, y = ty, label = txt, hjust = hjust),
    family = "franklin", size = 2.9, colour = pal$ink, lineheight = 1.15,
    fill = alpha(pal$bg, 0.9), label.colour = NA, vjust = 0.5,
    label.padding = unit(c(2, 3, 2, 3), "pt")
  ) +
  scale_colour_manual(values = mode_cols, guide = "none") +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
           expand = FALSE, crs = crs_atl, datum = NA) +
  theme_void(base_family = "franklin") +
  theme(
    # No frame: the map sits straight on the page.
    panel.background = element_rect(fill = pal$bg, colour = NA),
    plot.margin      = margin(0, 0, 0, 0)
  )


day_start <- 9
day_end   <- 19.5

clock <- function(t) {
  t <- with_tz(ymd_hms(t), "America/New_York")
  pmin(pmax(hour(t) + minute(t) / 60 + second(t) / 3600, day_start), day_end)
}

stay_label <- c(
  "Embassy suite-Buckhead" = "Hotel",
  "Emory"                  = "<b>1</b> Emory",
  "World of Coca-cola"     = "<b>2</b> World of Coca-Cola",
  "State Farm Arena"       = "<b>3</b>",
  "Battery Park"           = "<b>4</b>",
  "Alex Resturant"         = "Dinner"
)

stays <- visits |>
  mutate(from = clock(arrival_utc), to = clock(departure_utc)) |>
  filter(to > from, place_name %in% names(stay_label)) |>
  mutate(
    label = stay_label[place_name],
    mins  = round((to - from) * 60),
    dur   = ifelse(mins >= 60, sprintf("%d hr %d min", mins %/% 60, mins %% 60), paste(mins, "min")),
    txt   = ifelse(mins >= 45 & label != "Hotel" | (label == "Hotel" & mins > 100),
                   paste0(ifelse(label == "Hotel", "Back at the hotel", label),
                          "<br><span style='color:", pal$ink_2, "'>", dur, "</span>"),
                   label)
  ) |>
  filter(!(label == "Hotel" & mins < 100))

moves <- legs |>
  st_drop_geometry() |>
  mutate(from = clock(start_utc), to = clock(end_utc))

hours <- tibble(h = 9:19) |>
  mutate(txt = case_when(h == 9 ~ "9 a.m.", h == 12 ~ "Noon", h == 13 ~ "1 p.m.",
                         h > 12 ~ as.character(h - 12), TRUE ~ as.character(h)))

p_time <- ggplot() +
  geom_segment(data = hours, aes(x = h, xend = h, y = -0.25, yend = 1.0),
               colour = pal$road_x, linewidth = 0.25) +
  geom_text(data = hours, aes(x = h, y = -0.55, label = txt), family = "franklin",
            size = 2.6, colour = pal$ink_2) +
  annotate("rect", xmin = day_start, xmax = day_end, ymin = 0, ymax = 0.5,
           fill = pal$stay, colour = NA, alpha = 0.45) +
  geom_rect(data = stays, aes(xmin = from, xmax = to, ymin = 0, ymax = 0.5),
            fill = pal$stay, colour = NA) +
  geom_rect(data = moves, aes(xmin = from, xmax = to, ymin = 0, ymax = 0.5, fill = mode),
            colour = NA) +
  geom_richtext(data = stays, aes(x = (from + to) / 2, y = 0.62, label = txt),
                vjust = 0, family = "franklin", size = 2.6, colour = pal$ink, lineheight = 1.1,
                fill = NA, label.colour = NA, label.padding = unit(0, "pt")) +
  annotate("text", x = day_start, y = 0.25, label = "Left the\nhotel ", hjust = 1,
           family = "franklin", size = 2.4, colour = pal$ink_2, lineheight = 0.95) +
  scale_fill_manual(values = mode_cols, guide = "none") +
  scale_x_continuous(limits = c(day_start - 0.6, day_end + 0.1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.75, 1.25), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "<b>The same day, hour by hour.</b> <span style='color:#555555'>Gray is time spent at a stop; color is time on the move.</span>") +
  theme_void(base_family = "franklin") +
  theme(
    plot.title = element_markdown(size = 10, colour = pal$ink, margin = margin(b = 4)),
    plot.title.position = "plot",
    plot.margin = margin(14, 0, 0, 0)
  )


swatch <- function(m, txt = tolower(m)) {
  paste0("<b style='color:", mode_cols[[m]], "'>", txt, "</b>")
}

final <- (p_map / p_time) +
  plot_layout(heights = c(1, 0.2)) +
  plot_annotation(
    title    = paste0(
      "<span style='font-family:franklin_bold; font-size:10pt; color:", mode_cols[["Taxi"]],
      "'>A TRAVEL DIARY · ATLANTA</span><br>",
      "Nine trips, one day in Atlanta"
    ),
    subtitle = paste0(
      "On Friday, Oct. 2, I covered ", round(total_miles), " miles from Emory to downtown to ",
      "Cobb County, mostly by ", swatch("Taxi"), ",<br>with a ride on ", swatch("MARTA", "MARTA"),
      " and an afternoon ", swatch("On foot", "on foot"), ", and still made it back to the ",
      "hotel twice."
    ),
    caption  = paste0(
      "Routes recorded by my phone, pinned to each stop and smoothed: GPS jumps that double back ",
      "were removed, and legs with no signal (the MARTA tunnel) are drawn straight.<br>",
      "Modes are my own corrections. Source: personal TripLibre travel diary (private) · ",
      "Basemap: © OpenStreetMap contributors · #30DayMapChallenge 2026, Day 2: Lines"
    ),
    theme = theme(
      plot.background = element_rect(fill = pal$bg, colour = NA),
      plot.title    = element_markdown(family = "serif_bold", size = 30, colour = pal$ink,
                                       lineheight = 1.25, margin = margin(t = 4, b = 8)),
      plot.subtitle = element_markdown(family = "franklin", size = 11.5, colour = pal$ink_2,
                                       lineheight = 1.35, margin = margin(b = 10)),
      plot.caption  = element_markdown(family = "franklin", size = 7, colour = pal$ink_3,
                                       hjust = 0, lineheight = 1.4, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      plot.margin   = margin(24, 30, 16, 30)
    )
  )

ggsave(out_file, final, width = 10.5, height = 13, dpi = 300, bg = pal$bg)
