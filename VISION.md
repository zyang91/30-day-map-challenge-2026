## Vision: Mapping My Life — 30 Cartographic Experiments with TripLibre

**2026 concept:** Use personal travel diary data recorded by [TripLibre](https://hhtdiary.org/) as the primary dataset for this year's [#30DayMapChallenge](https://30daymapchallenge.com/2026/). Rather than hunting for a new dataset every day, explore how one individual's everyday mobility can become cartography, spatial analysis, and art.

> **30 themes, one life, many ways to see movement.**

The goal is **not** to finish all 30 prompts. Participate selectively when the official theme fits the available data and there is a compelling question or visual idea. Prioritize creativity, defensible interpretation, privacy, and enjoyment over daily streaks.

## Targets

- **Stretch goal:** around **10–13 completed maps**, with no obligation to reach that number.
- **Quality goal:** at least **5 distinctive maps** that communicate an interesting finding or perspective on everyday mobility.
- **Data goal:** use **real TripLibre exports** (GPS/trips/visits/places/timestamps/modes where available) as the analytical foundation; supplementary basemaps or contextual OSM data are fine.
- **Product goal:** demonstrate what a local-first personal mobility diary makes possible, without turning the challenge into an ad campaign.
- **Learning goal:** identify useful new analyses, export needs, and data-quality limitations to inform TripLibre development.

## Candidate days — prioritized (not commitments)

| Date | Official theme | Proposed idea |
| --- | --- | --- |
| Nov 1 | Points | **My Places** — visited locations scaled by frequency or dwell time |
| Nov 2 | Lines | **Every Trip I Took** — colored GPS trajectories by mode |
| Nov 3 | Polygons | **My Activity Space** — concave/convex hulls or other defensible activity-space geometry |
| Nov 4 | Clusters | **Where My Life Happens** — spatial clusters of repeated visits |
| Nov 7 | 10 minute map | **10-minute export-to-map** — actually time-box the entire mapping task |
| Nov 10 | Prompting only | **Map by Conversation** — Claude generates a map end-to-end using prompts and actual TripLibre data |
| Nov 13 | Interactions | **My OD Network** — movement flows between regularly visited places |
| Nov 17 | Light & dark | **Two Cities, One Life** — contrasting daytime and nighttime mobility |
| Nov 18 | NULL | **Where the Diary Goes Silent** — verified GPS gaps, missing records, or uncertainty (do not conflate intentional sampling with data loss) |
| Nov 20 | Hexagons | **Life in Hexagons** — aggregated visit, dwell, or movement density |
| Nov 24 | Network | **My Personal Transport Network** — repeatedly traveled links and trips |
| Nov 25 | Is this a map? | **A Portrait Made of Movement** — personal GPS data as abstract cartographic art |
| Nov 29 | Raster | **My Mobility Surface** — a continuous activity/travel intensity surface |

### Optional, only when the theme and data genuinely fit

- **Nov 6 Vintage:** historical-map aesthetics for a modern mobility record (clearly labeled as a style, not historical data).
- **Nov 9 Urban-rural:** only if trips actually span urban and rural environments.
- **Nov 14 Borgesian map:** exceptionally detailed real journey rather than invented coverage.
- **Nov 15 Inside out:** invert observed vs. unvisited space, with explicit caveats.
- **Nov 21 OpenStreetMap:** overlay actual trips with OSM streets, POIs, or transit infrastructure.
- **Nov 22 Projections:** worthwhile only for journeys at a scale where choice of projection tells a story.
- **Nov 23 Senses: Taste:** only where genuine restaurant/food place records are reliable; do not claim GPS observes taste.
- **Nov 26 Water:** only if actual movement along/across water makes a useful story.
- **Nov 27 New tool:** try a genuinely new cartographic tool.
- **Nov 28 Senses: Feeling:** only with genuine annotations or explicitly framed subjective artistic interpretation.
- **Nov 30 Pen & paper:** a real analog sketch, if desired, rather than an AI simulation of handwriting.

### Likely skips under the one-dataset rule

- **Nov 5 Sight, Nov 11 Sound, Nov 19 Smell:** GPS alone does not measure these senses.
- **Nov 8 Utopia:** speculative future geography is a poor fit for the real-record premise.
- **Nov 12 Power:** personal mobility data alone do not establish electrical, institutional, or political power.
- **Nov 16 Collaborative map:** requires an actual human collaborator; Claude does not qualify.

These are initial editorial judgments, not rigid prohibitions. Reconsider only if a genuinely data-grounded idea emerges.

## Creative and research principles

1. **Data first, theme second.** Do not force a narrative or invent measurements merely to complete a prompt.
2. **Map something different each time.** Rotate among location, trajectory, activity space, networks, time, uncertainty, and abstraction; avoid 13 cosmetic variants of the same heatmap.
3. **Respect measurement limits.** Distinguish recorded points from inferred visits, observed trips from snapped routes, and real gaps from normal sampling behavior.
4. **Protect personal privacy.** Never publish raw identifying GPS exports. Mask/remove home and other sensitive locations, avoid revealing repeatable routines, and consider aggregation, generalization, temporal coarsening, or selected non-sensitive trips. Inspect every exported artifact, map, annotation, and metadata before publishing.
5. **Reproducibility where practical.** Save map-specific scripts, assumptions, transformations, image output, and a short caption. Public repository should contain only privacy-safe artifacts.
6. **Transparency about AI.** Credit Claude for implementation, retain human responsibility for concepts, interpretations, and final review.

## Proposed lightweight workflow

- [ ] Confirm export schema, available history, GPS quality, and safe data handling.
- [ ] Build one reusable TripLibre export → cleaned spatial data → visualization workflow.
- [ ] Keep raw location data private and define a privacy-safe publication process.
- [ ] For a selected official theme: propose a one-sentence research/creative question.
- [ ] Have Claude produce the map and a reproducible script, plus a concise English caption.
- [ ] Human review: check spatial accuracy, legend, visual quality, honest interpretation, privacy, and AI disclosure.
- [ ] Publish on the corresponding November day with **#30DayMapChallenge**, only if ready.
- [ ] Document interesting limitations and potential TripLibre improvements as separate issues when useful.

## Success criterion

A cohesive, enjoyable mini-atlas of one person's mobility, **not a 30/30 completion badge**. The best outcome is a set of maps that make people ask: **“What could I learn about my own everyday geography?”**

_Official theme source: https://30daymapchallenge.com/2026/_
