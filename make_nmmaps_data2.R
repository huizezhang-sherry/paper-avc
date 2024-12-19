library(tidyverse)

meta <- read_csv("~/projects/nmmaps1987_2005/meta/nmmaps_cities.csv")
cities <- meta |>
  select(city, pop) |>
  group_by(city) |>
  summarize(pop = sum(pop)) |>
  arrange(desc(pop)) |>
  slice(1:38) |>
  pull(city) |>
  c("balt", "bost")

# cities0 <- c("ny", "det", "chic", "la", "balt",
#             "seat", "pitt", "bost", "denv", "phoe")
# all(cities0 %in% cities)

for(city in cities) {
  message(city)
  dat0 <- readRDS(sprintf("~/projects/nmmaps1987_2005/rds/%s.rds",
                          city)) |>
    as_tibble()

  dat <- dat0 |>
    filter(date >= "1992-01-01" & date <= "2000-12-31") |>
    mutate(pm10 = pm10tmean + pm10mtrend) |>
    rename(mortality = death) |>
    select(mortality, date, pm10, tmpd, dptp) |>
    group_by(date) |>
    summarize(mortality = sum(mortality),
              pm10 = mean(pm10),
              temp = mean(tmpd),
              dewpt = mean(dptp),
              .groups = "drop") |>
    mutate(season = factor(quarter(date),
                           labels = c("winter", "spring", "summer", "fall")))

  write_csv(dat, file = sprintf("data/pm10-%s.csv", city))
}
