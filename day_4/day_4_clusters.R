# 30 Day Map Challenge 2026, Day 4: Clusters
# "A Field Guide to the Habitats of Homo deadlinus": my out-of-home stops in Philadelphia,
# Sept. 27 – Oct. 30, 2026 (TripLibre / HHT export), clustered into "habitats"
# and written up like a nature guide. The jokes are the wrapper; every number
# in them comes from the diary.
#
# Method: single-linkage clustering of every non-home stop, cut at 400 m, so
# stops chain into the same cluster when each is within 400 m of another one.
# A cluster with two or more stops is a "habitat"; its outline is the union of
# 250 m circles around its stops, rounded off and drawn with a wobbly pen.
#
# Privacy: home stops are dropped before clustering and never drawn. The raw
# export lives in data/ and is gitignored; only the rendered PNG is shared.
#
# Basemap: City of Philadelphia (OpenDataPhilly), cached in data/basemap/ by Day 1.
# Claude visualization, checked by Zhanchao Yang

library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(ggtext)
library(patchwork)
library(showtext)

sf_use_s2(FALSE)


crs_pa  <- 2272  # NAD83 / Pennsylvania South (ftUS)
ft_m    <- 3.28084
cut_m   <- 400
blob_m  <- 250

# ---- fonts: field-guide serif, typewriter labels, a pencil hand -------------------

font_add_google("Fraunces", "fraunces")
font_add_google("Fraunces", "fraunces_bold", regular.wt = 800)
font_add_google("Courier Prime", "typewriter")
font_add_google("Courier Prime", "typewriter_bold", regular.wt = 700)
font_add_google("Caveat", "pencil")
font_add_google("Caveat", "pencil_bold", regular.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

# ---- palette: an old naturalist's notebook ----------------------------------------

pal <- list(
  paper  = "#F3ECDA",
  land   = "#EDE4CC",
  water  = "#C9DCD6",
  water_t = "#5E8C82",
  road   = "#DCCFAF",
  road_x = "#CDBD98",
  ink    = "#3A2E22",
  ink_2  = "#6B5B48",
  ink_3  = "#9C8B72",
  red    = "#B5452F"
)

hab_cols <- c("#7FA36B", "#D98F4E", "#C9A23A", "#6E95B8", "#B9739B", "#8C7AB8",
              "#7FA36B", "#D98F4E", "#6E95B8", "#B9739B")

# ---- stops and clusters ------------------------------------------------------------

visits <- read_csv(file.path(export_dir, "visits.csv"), show_col_types = FALSE) |>
  filter(is.na(place_category) | place_category != "home") |>
  mutate(local = as.POSIXct(arrival_utc, tz = "UTC") |> format(tz = "America/New_York", "%Y-%m-%d"))

n_home <- read_csv(file.path(export_dir, "visits.csv"), show_col_types = FALSE) |>
  filter(place_category == "home") |>
  nrow()

city <- st_read(file.path(cache_dir, "city_limits.gpkg"), quiet = TRUE) |> st_transform(crs_pa)

# Philadelphia only: keep stops inside city limits.
stops <- st_as_sf(visits, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
stops <- stops[lengths(st_intersects(st_transform(stops, crs_pa), city)) > 0, ]

sf_use_s2(TRUE)
d  <- units::drop_units(st_distance(stops)) # great-circle metres (s2)
sf_use_s2(FALSE)
hc <- hclust(as.dist(d), method = "single")
stops$cl <- cutree(hc, h = cut_m)

clusters <- stops |>
  st_drop_geometry() |>
  group_by(cl) |>
  summarise(
    n     = n(),
    hours = sum(duration_min, na.rm = TRUE) / 60,
    med   = median(duration_min, na.rm = TRUE),
    lat   = mean(lat),
    lon   = mean(lon),
    days  = n_distinct(local),
    places = paste(unique(place_name), collapse = " | ")
  ) |>
  mutate(region = "Philadelphia")

# The cluster that holds a given (uniquely named) place.
cs <- function(place) clusters[clusters$cl == stops$cl[match(place, stops$place_name)], ]

n_stops    <- nrow(stops)
n_habitats <- sum(clusters$n >= 2)
n_loners   <- sum(clusters$n == 1)

# ---- a wobbly pen ------------------------------------------------------------------

# Union of circles around each stop, rounded, with holes filled, then the
# outline is nudged in and out along a smooth random wave so it looks inked.
wobble_ring <- function(xy, amp, seed) {
  set.seed(seed)
  n  <- nrow(xy)
  t  <- seq(0, 2 * pi, length.out = n)
  k  <- sample(3:6, 2)
  ph <- runif(3, 0, 2 * pi)
  off <- amp * (sin(k[1] * t + ph[1]) + 0.5 * sin(k[2] * t + ph[2]) + 0.25 * sin(11 * t + ph[3]))
  # outward normals from neighbouring points (ring is closed)
  nx <- c(xy[2:n, 2] - xy[1:(n - 1), 2], xy[2, 2] - xy[n - 1, 2])
  ny <- -c(xy[2:n, 1] - xy[1:(n - 1), 1], xy[2, 1] - xy[n - 1, 1])
  len <- sqrt(nx^2 + ny^2)
  len[len == 0] <- 1
  out <- cbind(xy[, 1] + off * nx / len, xy[, 2] + off * ny / len)
  out[n, ] <- out[1, ]
  out
}

blob <- function(pts, r, amp, seed, crs) {
  g <- st_union(st_buffer(st_geometry(pts), r, nQuadSegs = 16))
  g <- st_buffer(st_buffer(g, r * 0.6), -r * 0.6) # round the waist between circles
  g <- st_cast(g, "POLYGON")
  g <- g[which.max(st_area(g))]
  ring <- st_coordinates(st_segmentize(st_exterior_ring(g), r / 8))[, 1:2]
  st_sfc(st_polygon(list(wobble_ring(ring, amp, seed))), crs = crs)
}

st_exterior_ring <- function(g) st_cast(g, "LINESTRING")[1]

make_habitats <- function(region_name, crs, r, amp) {
  ids <- clusters$cl[clusters$region == region_name & clusters$n >= 2]
  pts <- st_transform(stops, crs)
  geoms <- lapply(seq_along(ids), function(i) {
    blob(pts[pts$cl == ids[i], ], r, amp, seed = 40 + ids[i], crs = crs)
  })
  st_sf(cl = ids, geometry = do.call(c, geoms))
}

# ---- Philadelphia ------------------------------------------------------------------

streets <- st_read(file.path(cache_dir, "streets.gpkg"), quiet = TRUE) |> st_transform(crs_pa)
water   <- st_read(file.path(cache_dir, "hydrology.gpkg"), quiet = TRUE) |> st_transform(crs_pa)
parks   <- st_read(file.path(cache_dir, "parks.gpkg"), quiet = TRUE) |> st_transform(crs_pa)

water <- water[as.numeric(st_area(water)) > 1e5, ]
parks <- parks[as.numeric(st_area(parks)) > 2e5, ]
art   <- filter(streets, class %in% c(1, 2, 3))

phl_hab <- make_habitats("Philadelphia", crs_pa, blob_m * ft_m, amp = 70)
phl_pts <- st_transform(stops, crs_pa) |> filter(cl %in% clusters$cl[clusters$region == "Philadelphia"])
phl_pts[c("x", "y")] <- st_coordinates(phl_pts)

# Frame: University City to Fishtown-ish, enough for the inner habitats.
bb <- c(xmin = 2680000, xmax = 2699000, ymin = 229300, ymax = 243600)

# ---- the habitats, in field-guide voice --------------------------------------------

fmt_min <- function(m) if (m >= 60) sprintf("%.1f hr", m / 60) else sprintf("~%d min", round(m))

core <- cs("Weitzman office"); cc <- cs("City Hall station"); food <- cs("BOA")
amtk <- cs("Amtrak -30th St station"); chin <- cs("hair cut-Chinatown"); air <- cs("philly airport")

guide <- tribble(
  ~key,   ~name,                       ~blurb,
  "core", "The Natural Habitat",       sprintf("%d sightings, %d hours. Basically lives here.", core$n, round(core$hours)),
  "cc",   "The Commuter Corridor",     sprintf("%d sightings, %s each. Always passing through.", cc$n, fmt_min(cc$med)),
  "food", "The Foraging Grounds",      sprintf("%d sightings, %s each. Grabs groceries, flees.", food$n, fmt_min(food$med)),
  "amtk", "The Train-Station Outpost", sprintf("%d sightings near 30th Street Station.", amtk$n),
  "chin", "The Grooming Station",      "A haircut, then noodles. Same afternoon.",
  "air",  "The Launch Pad",            sprintf("%d sightings. Leaves town, comes back.", air$n)
) |>
  mutate(cl = c(core$cl, cc$cl, food$cl, amtk$cl, chin$cl, air$cl), no = row_number())

phl_hab <- phl_hab |>
  left_join(select(guide, cl, no, name), by = "cl") |>
  mutate(col = hab_cols[no])

# Number badge + pencilled name beside each habitat, on a hand-picked side.
side <- c(core = "below", cc = "below", food = "above", amtk = "above", chin = "below", air = "none")
badges <- phl_hab |>
  filter(!is.na(no)) |>
  mutate(
    key  = guide$key[no],
    xmin = sapply(geometry, \(g) min(st_coordinates(g)[, 1])),
    xmax = sapply(geometry, \(g) max(st_coordinates(g)[, 1])),
    ymin = sapply(geometry, \(g) min(st_coordinates(g)[, 2])),
    ymax = sapply(geometry, \(g) max(st_coordinates(g)[, 2])),
    s    = side[key],
    x    = case_when(s == "right" ~ xmax + 450, TRUE ~ (xmin + xmax) / 2 - 1200),
    y    = case_when(s == "above" ~ ymax + 450, s == "below" ~ ymin - 450, TRUE ~ (ymin + ymax) / 2)
  ) |>
  filter(s != "none") |>
  st_drop_geometry()

in_frame <- function(x, y) x > bb["xmin"] & x < bb["xmax"] & y > bb["ymin"] & y < bb["ymax"]

area_lbl <- function(x, y, txt, size = 2.5) {
  annotate("text", x = x, y = y, label = txt, family = "typewriter", size = size,
           colour = pal$ink_3, lineheight = 0.9)
}

p_map <- ggplot() +
  geom_sf(data = city, fill = pal$land, colour = NA) +
  geom_sf(data = parks, fill = "#DDE1C1", colour = NA) +
  geom_sf(data = water, fill = pal$water, colour = NA) +
  geom_sf(data = art, colour = pal$road, linewidth = 0.25) +
  # habitats: wash of colour, then two pen passes slightly apart
  geom_sf(data = phl_hab, aes(fill = col), colour = NA, alpha = 0.38) +
  geom_sf(data = phl_hab, aes(colour = col), fill = NA, linewidth = 0.7) +
  geom_sf(data = st_buffer(phl_hab, 35), aes(colour = col), fill = NA, linewidth = 0.25, alpha = 0.6) +
  # every sighting is a little pencil cross
  geom_point(data = filter(phl_pts, in_frame(x, y)), aes(x, y), shape = 4, size = 1.1,
             stroke = 0.45, colour = pal$ink) +
  geom_point(data = badges, aes(x, y), shape = 21, size = 6,
             fill = pal$paper, colour = pal$ink, stroke = 0.6) +
  geom_text(data = badges, aes(x, y, label = no),
            family = "fraunces_bold", size = 3, colour = pal$ink) +
  geom_text(data = badges, aes(x + 420, y, label = name),
            hjust = 0, family = "pencil_bold", size = 4.6, colour = pal$ink) +
  area_lbl(2681600, 238600, "UNIVERSITY\nCITY") +
  area_lbl(2691800, 239300, "CENTER\nCITY") +
  annotate("text", x = 2689700, y = 233000, label = "Schuylkill River", angle = 62,
           family = "fraunces", fontface = "italic", size = 2.6, colour = pal$water_t) +
  # the airport is off the map: an arrow to it
  annotate("segment", x = 2682800, y = 231900, xend = 2681200, yend = 230500,
           arrow = arrow(length = unit(5, "pt"), type = "closed"), colour = pal$ink, linewidth = 0.4) +
  annotate("text", x = 2683000, y = 232200, hjust = 0, vjust = 0, family = "pencil_bold",
           size = 4, colour = pal$ink, label = "6  The Launch Pad\n    (airport, 5 mi that way)", lineheight = 0.85) +
  annotate("text", x = 2689500, y = 243000, hjust = 0.5, family = "pencil", size = 3.8,
           colour = pal$ink_2, label = "Note: nest not shown, to protect the specimen.") +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], expand = FALSE,
           crs = crs_pa, datum = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = pal$paper, colour = NA),
        panel.border = element_rect(fill = NA, colour = pal$ink, linewidth = 0.6),
        plot.margin = margin(0, 0, 0, 0))

# ---- field notes under the map: a 4 x 2 grid -----------------------------------------

notes <- tibble(
  head = c(paste0("<span style='color:", hab_cols[guide$no], "'>", guide$no, ".</span> ", guide$name),
           "Solitary sightings"),
  body = c(guide$blurb, sprintf("%d stops never formed a habitat. The specimen wanders.", n_loners))
) |>
  mutate(
    i   = row_number() - 1,
    x   = (i %% 4) / 4,
    y   = 1 - (i %/% 4) * 0.55,
    txt = paste0("<span style='font-family:fraunces_bold; font-size:11pt'>", head, "</span><br>",
                 "<span style='font-size:8.3pt; color:", pal$ink_2, "'>",
                 gsub("\n", "<br>", stringr::str_wrap(body, 34)), "</span>")
  )

p_notes <- ggplot(notes) +
  geom_richtext(aes(x, y, label = txt), hjust = 0, vjust = 1, family = "typewriter",
                size = 3, colour = pal$ink, lineheight = 1.2, fill = NA, label.colour = NA,
                label.padding = unit(0, "pt")) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = "FIELD NOTES") +
  theme_void() +
  theme(plot.title = element_text(family = "typewriter_bold", size = 9, colour = pal$ink,
                                  margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.margin = margin(16, 0, 0, 0))

# ---- assemble -----------------------------------------------------------------------

final <- (p_map / p_notes) +
  plot_layout(heights = c(1, 0.2)) +
  plot_annotation(
    title = paste0(
      "<span style='font-family:typewriter_bold; font-size:9pt; color:", pal$red,
      "'>A FIELD GUIDE · PHILADELPHIA</span><br>",
      "The Habitats of <i>Homo deadlinus</i>"
    ),
    subtitle = paste0(
      "Each <b>×</b> is one of ", nrow(stops), " sightings away from home, Sept. 27 – Oct. 30, 2026.<br>",
      "Sightings within ", cut_m, " m of each other form a habitat."
    ),
    caption = paste0(
      "Single-linkage clustering of non-home stops inside city limits, cut at ", cut_m,
      " m; habitats are clusters of 2+ stops, drawn as merged ", blob_m, " m circles.<br>Home is never mapped. ",
      "Source: personal TripLibre travel diary (private) · Basemap: City of Philadelphia via OpenDataPhilly<br>",
      "#30DayMapChallenge 2026, Day 4: Clusters · Claude visualization, checked by Zhanchao Yang"
    ),
    theme = theme(
      plot.background = element_rect(fill = pal$paper, colour = NA),
      plot.title    = element_markdown(family = "fraunces_bold", size = 30, colour = pal$ink,
                                       lineheight = 1.3, margin = margin(t = 2, b = 8)),
      plot.subtitle = element_markdown(family = "fraunces", size = 12, colour = pal$ink_2,
                                       lineheight = 1.4, margin = margin(b = 16)),
      plot.caption  = element_markdown(family = "typewriter", size = 6.3, colour = pal$ink_3,
                                       hjust = 0, lineheight = 1.45, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      plot.margin = margin(24, 28, 14, 28)
    )
  ) &
  theme(plot.background = element_rect(fill = pal$paper, colour = NA))

ggsave(out_file, final, width = 11, height = 12.4, dpi = 300, bg = pal$paper)
