# MERGE THREE HITTER DATA FILES TO CREATE A JSON FOR PLAYRGUESSR GAME
# HITTER-DASHBOARD
# HITTER-STANDARD
# FIELDING

library(tidyverse)
library(writexl)
library(jsonlite)
path <- "E:/Andrew/AndrewDocs/Baseball/PlayrGuessr/"

# ── Load files ────────────────────────────────────────────────────────────────
fielding  <- read_csv(paste0(path, "fielding.csv"),  locale = locale(encoding = "UTF-8"))
standard  <- read_csv(paste0(path, "hitters-standard.csv"),  locale = locale(encoding = "UTF-8"))
dashboard <- read_csv(paste0(path, "hitters-dashboard.csv"), locale = locale(encoding = "UTF-8"))

# ── Identify pitchers to exclude ──────────────────────────────────────────────
pitchers_only <- fielding |>
  group_by(PlayerId) |>
  summarise(all_pitcher = all(Pos == "P")) |>
  filter(all_pitcher) |>
  pull(PlayerId)

# ── Fielding: aggregate to player-season-team level ───────────────────────────
fielding_agg <- fielding |>
  filter(Pos != "P") |>
  group_by(PlayerId, Season, Team) |>
  arrange(desc(G), .by_group = TRUE) |>
  summarise(
    Pos_str = paste0(Pos, "(", G, ")", collapse = " "),
    .groups = "drop"
  )

# ── Dashboard: keep only what we need ────────────────────────────────────────
dashboard_slim <- dashboard |>
  select(PlayerId, Season, Team, OBP, SLG, WAR, wRC_plus = `wRC+`)

# ── Join standard + dashboard ─────────────────────────────────────────────────
batting <- standard |>
  left_join(dashboard_slim, by = c("PlayerId", "Season", "Team"))

# ── Join in fielding position string ──────────────────────────────────────────
batting <- batting |>
  left_join(fielding_agg, by = c("PlayerId", "Season", "Team"))

# ── Build final dataframe ─────────────────────────────────────────────────────
final <- batting |>
  mutate(OPS = OBP + SLG) |>
  filter(!PlayerId %in% pitchers_only) |>    # ← here, just before select()
  select(
    Season, Name, NameASCII, Team, PlayerId,
    G, PA, AB,
    H, `1B`, `2B`, `3B`, HR,
    R, RBI,
    BB, IBB, SO, HBP, SF, SH, GDP,
    SB, CS,
    AVG, OBP, SLG, OPS, WAR, wRC_plus,
    Pos_str
  )

# ── Quick sanity check ────────────────────────────────────────────────────────
glimpse(final)
cat("Rows:", nrow(final), "\n")
cat("Players:", n_distinct(final$PlayerId), "\n")

# Check accented names
final |> filter(grepl("ñ|á|é|í|ó|ú", Name)) |> distinct(Name) |> print(n = 20)

# ── Export ────────────────────────────────────────────────────────────────────
write_csv(final, paste0(path, "players-final.csv"))
write_xlsx(final, paste0(path, "players-final.xlsx"))


# ── Nest seasons by player ────────────────────────────────────────────────────
players_nested <- final |>
  group_by(PlayerId, Name, NameASCII) |>
  summarise(
    career_start = min(Season),
    career_end   = max(Season),
    seasons      = list(pick(everything())),
    .groups      = "drop"
  )

# ── Flag duplicate NameASCII values and build DisplayName ─────────────────────
duplicated_names <- players_nested |>
  count(NameASCII) |>
  filter(n > 1) |>
  pull(NameASCII)

players_nested <- players_nested |>
  mutate(
    DisplayName = if_else(
      NameASCII %in% duplicated_names,
      paste0(Name, " (", career_start, "-", career_end, ")"),
      Name
    )
  )
 
# ── Export to JSON ────────────────────────────────────────────────────────────
players_json <- players_nested |>
  select(PlayerId, Name, NameASCII, DisplayName, career_start, career_end, seasons)

write_json(players_json, paste0(path, "players.json"), auto_unbox = TRUE, pretty = FALSE)

cat("JSON written:", nrow(players_json), "players\n")