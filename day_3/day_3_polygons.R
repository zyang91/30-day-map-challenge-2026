# 30 Day Map Challenge 2026, Day 3: Polygons
# "My Philadelphia, by the hour": how big is my activity space? It depends on
# whether you count places or time. Each polygon is the convex hull of the
# smallest set of places that holds a given share of the hours I spent away
# from home in the city, Sept. 27 – Oct. 30, 2026 (TripLibre / HHT export).
#
# Method: rank places by total dwell time, take the top places until their
# cumulative hours reach 50 / 75 / 90 / 100 % of all out-of-home hours, draw the
# convex hull of that set, and pad every hull by the same 600 ft so one- and
# two-place sets still have an area. Areas are reported for the padded hulls.
#
# The raw export lives in data/ and is gitignored. Home is never used, so no
# hull vertex sits at home. Only the rendered PNG is meant to be shared.
#
# Basemap: City of Philadelphia open data (OpenDataPhilly), cached in
# data/basemap/ by day_1/day_1_points.R.
# Claude visualization, checked by Zhanchao Yang

library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(ggtext)
library(patchwork)
library(showtext)

sf_use_s2(FALSE)


crs_pa  <- 2272 # NAD83 / Pennsylvania South (ftUS)
sqft_sqmi <- 5280^2
pad_ft  <- 600


font_add_google("JetBrains Mono", "jbmono")
font_add_google("JetBrains Mono", "jbmono_bold", regular.wt = 700)
font_add_google("Space Grotesk", "grotesk")
font_add_google("Space Grotesk", "grotesk_bold", regular.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

# ---- palette: a dark instrument panel ---------------------------------------------

pal <- list(
  bg     = "#070B16",
  land   = "#0C1324",
  street = "#1A2540",
  art    = "#24345A",
  water  = "#0A1A2E",
  city   = "#3E5C8A",
  grid   = "#141E36",
  ink    = "#E8EEF9",
  ink_2  = "#8C9BB8",
  ink_3  = "#55647F"
)

levels_tbl <- tibble(
  share = c(1, 0.9, 0.75, 0.5),
  key   = c("100", "90", "75", "50"),
  col   = c("#7A5CFF", "#3D8BFF", "#19D3FF", "#7CFFC4")
)

# ---- basemap ----------------------------------------------------------------------

city    <- st_read(file.path(cache_dir, "city_limits.gpkg"), quiet = TRUE) |> st_transform(crs_pa)
streets <- st_read(file.path(cache_dir, "streets.gpkg"), quiet = TRUE) |> st_transform(crs_pa)
water   <- st_read(file.path(cache_dir, "hydrology.gpkg"), quiet = TRUE) |> st_transform(crs_pa)

water <- water[as.numeric(st_area(water)) > 2e5, ]
art   <- filter(streets, class %in% c(1, 2))
local <- filter(streets, class %in% c(3, 4, 5))

city_sqmi <- as.numeric(st_area(st_union(city))) / sqft_sqmi

# ---- places and hours -------------------------------------------------------------

places_raw <- read_csv(file.path(export_dir, "places.csv"), show_col_types = FALSE)
visits_raw <- read_csv(file.path(export_dir, "visits.csv"), show_col_types = FALSE)

# A merged place points at the place it was folded into.
canon <- with(places_raw, setNames(coalesce(merged_into, place_id), place_id))

places <- visits_raw |>
  filter(is.na(place_category) | place_category != "home") |>
  mutate(place_id = unname(canon[place_id])) |>
  group_by(place_id) |>
  summarise(hours = sum(duration_min, na.rm = TRUE) / 60, n_visits = n()) |>
  inner_join(select(places_raw, place_id, name, category, lat, lon), by = "place_id") |>
  filter(is.na(category) | category != "home") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_pa) |>
  st_filter(city) |>
  arrange(desc(hours)) |>
  mutate(cum_share = cumsum(hours) / sum(hours))

total_hours <- sum(places$hours)

# Smallest top-ranked set that reaches each share of the hours.
hull_for <- function(share) {
  k   <- which(places$cum_share >= share - 1e-9)[1]
  set <- places[seq_len(k), ]
  st_sf(
    share    = share,
    n_places = k,
    geometry = st_buffer(st_convex_hull(st_union(set)), pad_ft, nQuadSegs = 24)
  )
}

hulls <- bind_rows(lapply(levels_tbl$share, hull_for)) |>
  left_join(levels_tbl, by = "share") |>
  mutate(
    sqmi     = as.numeric(st_area(geometry)) / sqft_sqmi,
    pct_city = 100 * sqmi / city_sqmi
  )

h <- function(s, col) hulls[[col]][hulls$share == s]
shrink <- h(1, "sqmi") / h(0.5, "sqmi")

# ---- a little neon: stack wide, faint strokes under a crisp one ------------------

glow_path <- function(data, colour, width = 0.5, layers = 6, spread = 1.0, alpha = 0.06) {
  c(
    lapply(rev(seq_len(layers)), function(i) {
      geom_sf(data = data, fill = NA, colour = alpha(colour, alpha),
              linewidth = width + i * spread, lineend = "round", linejoin = "round")
    }),
    list(geom_sf(data = data, fill = NA, colour = colour, linewidth = width,
                 linejoin = "round"))
  )
}

glow_points <- function(data, colour = "#FFFFFF", size_col = "sz") {
  pts <- st_drop_geometry(data)
  pts$x <- st_coordinates(data)[, 1]
  pts$y <- st_coordinates(data)[, 2]
  c(
    lapply(c(4, 3, 2), function(m) {
      geom_point(data = pts, aes(x, y, size = .data[[size_col]] * m),
                 colour = alpha(colour, 0.05), stroke = 0)
    }),
    list(geom_point(data = pts, aes(x, y, size = .data[[size_col]]),
                    colour = colour, stroke = 0))
  )
}

places$sz <- 0.5 + 1.6 * sqrt(places$hours / max(places$hours))

# ---- frame & labels ---------------------------------------------------------------

bb0 <- st_bbox(city)
pad <- 2500
bb  <- c(xmin = bb0[["xmin"]] - pad, xmax = bb0[["xmax"]] + pad,
         ymin = bb0[["ymin"]] - pad, ymax = bb0[["ymax"]] + pad)

# Zoom window on the core, where the two smallest polygons live.
zb <- c(xmin = 2681200, xmax = 2696200, ymin = 231400, ymax = 240400)
zoom_box <- st_as_sfc(st_bbox(zb, crs = st_crs(crs_pa)))

to_xy <- function(lon, lat) {
  st_coordinates(st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), crs_pa))[1, ]
}

top_names <- c(
  "Weitzman office"           = "PENN",
  "Academy of Music"          = "ACADEMY OF MUSIC",
  "class 1"                   = "CLASSROOM",
  "philly airport"            = "AIRPORT",
  "USCIS"                     = "ERRAND",
  "Lincoln Financial Stadium" = "STADIUM",
  "Buy more"                  = "SHOPPING RUN"
)
named <- places |>
  filter(name %in% names(top_names)) |>
  mutate(lab = top_names[name],
         x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

lbl_txt <- function(s) {
  sprintf("%s%% OF HOURS · %s MI²", h(s, "key"), formatC(h(s, "sqmi"), format = "f", digits = 1))
}

# City-wide labels for the two big polygons, placed by hand along their edges.
big_lbl <- bind_rows(
  tibble(share = 1,   xy = list(to_xy(-75.085, 40.075)), angle = 52),
  tibble(share = 0.9, xy = list(to_xy(-75.225, 39.925)), angle = 58)
) |>
  mutate(x = sapply(xy, `[`, 1), y = sapply(xy, `[`, 2),
         txt = sapply(share, lbl_txt), col = sapply(share, h, col = "col"))

# ---- map layers shared by the city view and the zoom ------------------------------

hull_layers <- function(shares, width = 0.45, layers = 6, spread = 1) {
  unlist(lapply(shares, function(s) {
    hh <- hulls[hulls$share == s, ]
    c(list(geom_sf(data = hh, fill = alpha(hh$col, 0.07), colour = NA)),
      glow_path(hh, hh$col, width = width, layers = layers, spread = spread))
  }), recursive = FALSE)
}

base_layers <- function(street_w = 1) {
  list(
    geom_sf(data = city, fill = pal$land, colour = NA),
    geom_sf(data = water, fill = pal$water, colour = NA),
    geom_sf(data = local, colour = pal$street, linewidth = 0.12 * street_w),
    geom_sf(data = art, colour = pal$art, linewidth = 0.3 * street_w)
  )
}

map_theme <- theme_void(base_family = "jbmono") +
  theme(
    panel.background  = element_rect(fill = pal$bg, colour = NA),
    plot.background   = element_rect(fill = pal$bg, colour = NA),
    panel.grid.major  = element_line(colour = pal$grid, linewidth = 0.25),
    panel.border      = element_rect(fill = NA, colour = pal$ink_3, linewidth = 0.3),
    axis.text         = element_text(family = "jbmono", size = 6, colour = pal$ink_3),
    axis.text.x       = element_text(margin = margin(t = 4)),
    axis.text.y       = element_text(margin = margin(r = 4), hjust = 1),
    axis.ticks        = element_line(colour = pal$ink_3, linewidth = 0.25),
    axis.ticks.length = unit(3, "pt"),
    plot.margin       = margin(0, 0, 0, 0)
  )

# ---- city map ---------------------------------------------------------------------

p_map <- ggplot() +
  base_layers() +
  glow_path(city, pal$city, width = 0.3, layers = 4, spread = 0.8, alpha = 0.08) +
  hull_layers(levels_tbl$share) +
  glow_points(places) +
  geom_sf(data = zoom_box, fill = NA, colour = pal$ink, linewidth = 0.3, linetype = "22") +
  annotate("text", x = zb[["xmin"]], y = zb[["ymax"]] + 700, label = "ZOOM →", hjust = 0,
           family = "jbmono_bold", size = 2.2, colour = pal$ink) +
  geom_text(data = filter(named, !lab %in% c("PENN", "CLASSROOM", "ACADEMY OF MUSIC")),
            aes(x + 700, y, label = lab), hjust = 0, family = "jbmono", size = 2.2,
            colour = pal$ink_2) +
  geom_text(data = big_lbl, aes(x, y, label = txt, colour = col, angle = angle),
            family = "jbmono_bold", size = 2.4) +
  scale_colour_identity() +
  scale_size_identity() +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], expand = FALSE,
           crs = crs_pa, datum = 4326) +
  scale_x_continuous(breaks = seq(-75.25, -74.95, by = 0.05)) +
  scale_y_continuous(breaks = seq(39.85, 40.15, by = 0.05)) +
  map_theme

# ---- zoom map ---------------------------------------------------------------------

hb <- function(s) st_bbox(hulls$geometry[hulls$share == s])
small_lbl <- bind_rows(
  tibble(share = 0.75, x = mean(hb(0.75)[c(1, 3)]), y = hb(0.75)[["ymax"]] + 450),
  tibble(share = 0.5,  x = mean(hb(0.75)[c(1, 3)]), y = hb(0.75)[["ymin"]] - 450)
) |>
  mutate(txt = sapply(share, lbl_txt), col = sapply(share, h, col = "col"))

zoom_named <- filter(named, lab %in% c("PENN", "CLASSROOM", "ACADEMY OF MUSIC")) |>
  mutate(dx = c(PENN = -600, CLASSROOM = 0, `ACADEMY OF MUSIC` = 0)[lab],
         dy = c(PENN = -900, CLASSROOM = 900, `ACADEMY OF MUSIC` = -900)[lab])

p_zoom <- ggplot() +
  base_layers(street_w = 1.6) +
  hull_layers(levels_tbl$share, width = 0.6, layers = 7, spread = 1.3) +
  glow_points(mutate(places, sz = sz * 1.5)) +
  geom_text(data = zoom_named, aes(x + dx, y + dy, label = lab), family = "jbmono",
            size = 2.2, colour = pal$ink) +
  geom_text(data = small_lbl, aes(x, y, label = txt, colour = col),
            family = "jbmono_bold", size = 2.4) +
  annotate("text", x = 2683000, y = 239200, label = "UNIVERSITY\nCITY", family = "jbmono",
           size = 2, colour = pal$ink_3, lineheight = 0.9) +
  annotate("text", x = 2693300, y = 238900, label = "CENTER\nCITY", family = "jbmono",
           size = 2, colour = pal$ink_3, lineheight = 0.9) +
  scale_colour_identity() +
  scale_size_identity() +
  coord_sf(xlim = zb[c("xmin", "xmax")], ylim = zb[c("ymin", "ymax")], expand = FALSE,
           crs = crs_pa, datum = NA) +
  labs(title = "ZOOM · UNIVERSITY CITY TO CENTER CITY") +
  map_theme +
  theme(panel.border = element_rect(fill = NA, colour = pal$ink, linewidth = 0.3,
                                    linetype = "22"),
        plot.title = element_text(family = "jbmono_bold", size = 7, colour = pal$ink,
                                  margin = margin(b = 4)))

# ---- side panel: the readout -------------------------------------------------------

readout <- hulls |>
  st_drop_geometry() |>
  mutate(
    yy    = match(key, rev(levels_tbl$key)),
    left  = paste0(key, "% OF HOURS"),
    right = sprintf("%s mi² · %d place%s", formatC(sqmi, format = "f", digits = 1),
                    n_places, ifelse(n_places == 1, "", "s"))
  )

p_bars <- ggplot(readout) +
  geom_rect(aes(xmin = 0, xmax = city_sqmi, ymin = yy - 0.18, ymax = yy + 0.18),
            fill = "#111A30") +
  geom_rect(aes(xmin = 0, xmax = pmax(sqmi, 0.6), ymin = yy - 0.18, ymax = yy + 0.18,
                fill = col)) +
  geom_text(aes(x = 0, y = yy + 0.36, label = left, colour = col), hjust = 0,
            family = "jbmono_bold", size = 2.4) +
  geom_text(aes(x = city_sqmi, y = yy + 0.36, label = right), hjust = 1,
            family = "jbmono", size = 2.3, colour = pal$ink_2) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.7, 4.6), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "AREA OF EACH POLYGON",
    subtitle = sprintf("Full bar = all of Philadelphia, %d mi²", round(city_sqmi))
  ) +
  theme_void(base_family = "jbmono") +
  theme(
    plot.background = element_rect(fill = pal$bg, colour = NA),
    plot.title    = element_text(family = "jbmono_bold", size = 7, colour = pal$ink,
                                 margin = margin(b = 2)),
    plot.subtitle = element_text(family = "jbmono", size = 6.5, colour = pal$ink_3,
                                 margin = margin(b = 2)),
    plot.title.position = "plot",
    plot.margin = margin(4, 0, 0, 0)
  )

hero <- ggplot() +
  annotate("text", x = 0, y = 0.98, hjust = 0, vjust = 1, family = "grotesk_bold",
           size = 26, colour = levels_tbl$col[4], label = paste0("×", round(shrink))) +
  annotate("richtext", x = 0.47, y = 0.92, hjust = 0, vjust = 1, family = "grotesk",
           size = 3.5, colour = pal$ink, lineheight = 1.3, fill = NA, label.colour = NA,
           label.padding = unit(0, "pt"),
           label = paste0(
             "Wrap every place I stopped and my<br>Philadelphia spans <b>",
             round(h(1, "pct_city")), "%</b> of the city.<br>",
             "But half my hours fit in a polygon<br><b style='color:", levels_tbl$col[4], "'>",
             round(shrink), " times smaller</b>, spanning just<br>",
             "two spots on Penn’s campus and<br>the Academy of Music."
           )) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = pal$bg, colour = NA))

p_side <- hero / p_zoom / p_bars +
  plot_layout(heights = c(0.45, 1.05, 0.62))

# ---- assemble ---------------------------------------------------------------------

final <- (p_map | plot_spacer() | p_side) +
  plot_layout(widths = c(1.3, 0.06, 1)) +
  plot_annotation(
    title = paste0(
      "<span style='font-family:jbmono_bold; font-size:9pt; color:", levels_tbl$col[3],
      "'>ACTIVITY SPACE · PHILADELPHIA · SEPT. 27 – OCT. 30, 2026</span><br>",
      "How big is my Philadelphia? Depends what you count."
    ),
    subtitle = paste0(
      "Rank the places I stopped by hours spent there. Add them one by one until they hold ",
      "<b style='color:", levels_tbl$col[4], "'>50%</b>, ",
      "<b style='color:", levels_tbl$col[3], "'>75%</b>, ",
      "<b style='color:", levels_tbl$col[2], "'>90%</b> or ",
      "<b style='color:", levels_tbl$col[1], "'>all</b> of my ",
      round(total_hours), " out-of-home hours,<br>then wrap each set in a polygon. ",
      "Dots grow with hours spent."
    ),
    caption = paste0(
      "Polygons: convex hulls of the fewest places holding each share of out-of-home hours inside city limits, ",
      "padded 600 ft. Home is excluded and never a vertex.<br>",
      "Source: personal TripLibre travel diary (private) · Basemap: City of Philadelphia via OpenDataPhilly · ",
      "#30DayMapChallenge 2026, Day 3: Polygons · Claude visualization, checked by Zhanchao Yang"
    ),
    theme = theme(
      plot.background = element_rect(fill = pal$bg, colour = NA),
      plot.title    = element_markdown(family = "grotesk_bold", size = 26, colour = pal$ink,
                                       lineheight = 1.35, margin = margin(t = 2, b = 8)),
      plot.subtitle = element_markdown(family = "grotesk", size = 11, colour = pal$ink_2,
                                       lineheight = 1.4, margin = margin(b = 16)),
      plot.caption  = element_markdown(family = "jbmono", size = 6, colour = pal$ink_3,
                                       hjust = 0, lineheight = 1.5, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      plot.margin = margin(24, 28, 14, 28)
    )
  ) &
  theme(plot.background = element_rect(fill = pal$bg, colour = NA))

ggsave(out_file, final, width = 13.6, height = 10, dpi = 300, bg = pal$bg)
