#' # Reproducible script for *Inside Out: Externalizing Assumptions in Data Analysis as Validation Checks*
## ----setup, include=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, messages = FALSE)
#remotes::install_github("huizezhang-sherry/adtoolbox")
library(tidyverse)
library(patchwork)
library(MASS)
library(ggh4x)
library(adtoolbox)
library(broom)
library(knitr)
library(rpart)
library(infotheo)
library(patchwork)
library(readr)
library(kableExtra)
library(scales)
library(qqplotr)
pm10 <- read_csv(here::here("data", "pm10.csv")) |>
  filter(!is.na(pm10)) |>
  mutate(season = as.factor(season))


## ----------------------------------------------------------------------------------------
#| fig-height: 3
#| label: fig-step-count
#| fig-cap: Beeswarm plot of average step counts across 300 simulated 30-day periods. Each point represents the average step count from one simulation. Similar to a boxplot or violin plot, the beeswarm plot also displays the distribution of each individual data point. The orange points indicate instances where the average step count fails outside the [8,500, 9,500] interval, representing an unexpected outcome in this scenario.
set.seed(1234)
step_count <- tibble(id = 1:300) |>
  rowwise() |>
  mutate(
    small_n = rpois(1, 8),
    large_n = rpois(1, 8),
    norm_sd = rlogis(1, 800, scale = 100),
    norm_n = 30 - small_n - large_n,
    step = list(round(c(rnorm(small_n, 6000, 200),
                        rnorm(large_n, 12000, 200),
                        rnorm(norm_n, 9000, 300)
    ), 0)),
    mean = mean(step, na.rm = TRUE),
    unexpect = ifelse(between(mean, 8500, 9500), 0, 1),
    `q(step, 0.6) > 10000` = ifelse(quantile(step, 0.6, na.rm = TRUE) > 10000, 1, 0),
    `q(step, 0.4) < 8000` = ifelse(quantile(step, 0.4, na.rm = TRUE) < 8000, 1, 0),
    `sd(step) > 2500` = ifelse(sd(step) > 2500, 1, 0),
  ) |>
  ungroup()

step_count |>
  ggplot(aes(x = mean, y = 1, color = as.factor(unexpect))) +
  ggbeeswarm::geom_quasirandom() +
  scale_y_continuous(breaks = seq(0, 13, 1)) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Average number of steps") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank(), axis.text.y = element_blank())

## ----------------------------------------------------------------------------------------
#| label: fig-logic-reg
#| fig.width: 8
#| fig.heigh: 2
#| fig-cap: Logic regression model fitted to the three checks. The model suggests the rule (quantile(step, 0.6) > 10,000 OR quantile(step, 0.4) < 8,000) AND (NOT sd(step) > 2,500). The NOT operator applied to `sd(step) > 2,500` is colored with a black background to distinguish it from other checks.
fit <- step_count |> fit_logic_reg(unexpect, `q(step, 0.6) > 10000`:`sd(step) > 2500`, seed = 1, nleaves = 3)
plot_logicreg(fit)


## ----------------------------------------------------------------------------------------
fit_regtree <- rpart(unexpect ~ ., data = step_count |> dplyr::select(unexpect:`sd(step) > 2500`), method = "class",
                     control = rpart.control(cp = 0.01))
pred_vec <- predict(fit_regtree) |> as_tibble() |> mutate(.fitted = ifelse(`0` > `1`, 0, 1)) |> dplyr::select(.fitted)
regtree_df <- step_count |> dplyr::select(unexpect) |> bind_cols(pred_vec) |>
  calc_miscla_rate(unexpect, .fitted)

sum_entropy <- lapply(as.list(step_count[,c(9:11)]),  function(x){infotheo::entropy(x, method = "emp")}) |> unlist() |> sum()
multi_info <- infotheo::multiinformation(step_count[,c(9:11)])
joint_entropy <- sum_entropy - multi_info
ratio <- joint_entropy / sum_entropy

regtree_df <- regtree_df |>
  mutate(independence =  (ratio - 1/3) / (1 - 1/3)) |>
  calc_metrics(metrics = c("harmonic", "arithmetic"))

sum_entropy <- lapply(as.list(step_count[,c(9,11)]),  function(x){infotheo::entropy(x, method = "emp")}) |> unlist() |> sum()
multi_info <- infotheo::multiinformation(step_count[,c(9,11)])
joint_entropy <- sum_entropy - multi_info
ratio <-  joint_entropy / sum_entropy

another <- tibble(.fitted = as.vector(step_count[,9] & !step_count[,11]) |> as.numeric(),
                  unexpect = step_count$unexpect) |>
  calc_miscla_rate(unexpect, .fitted) |>
  mutate(independence = (ratio - 1/2) / (1 - 1/2)) |>
  calc_metrics(metrics = c("harmonic", "arithmetic")) |>
  mutate(id = 6)


## ----------------------------------------------------------------------------------------
#| label: tbl-logic-reg
#| tbl-cap: "Precision, recall, and independence calculated for each individual check and the logic regression check rule. The harmonic and arithmetic means of the three metrics are included to evaluate the quality of the checks in  diagnosing unexpected step counts (more than five days with fewer than 8,000 steps)."
list(tibble(.fitted = step_count$`q(step, 0.6) > 10000`, unexpect = step_count$unexpect),
     tibble(.fitted = step_count$`q(step, 0.4) < 8000`, unexpect = step_count$unexpect),
     tibble(.fitted = step_count$`sd(step) > 2500`, unexpect = step_count$unexpect)
) |>
  map_dfr(~.x |> calc_miscla_rate(unexpect, .fitted) |>
            mutate(independence = 1) |>
            calc_metrics(metrics = c("harmonic", "arithmetic"))) |>
  mutate(checks = c("Check 1: q(step, 0.6) > 10000", "Check 2: q(step, 0.4) < 8000", "Check 3: sd(step) > 2500")) |>
  bind_rows(augment(fit) |>
              calc_miscla_rate(.fitted, unexpect) |>
              calc_independence() |>
              calc_metrics(metrics = c("harmonic", "arithmetic")) |>
              mutate(checks = "Logic regression: (check 1 OR check 2) AND (not check 3)")) |>
  bind_rows(another  |>
              mutate(checks = "Comparison: (check 1) AND (not check 3)")) |>
  bind_rows(regtree_df |>
              mutate(checks = "Regression tree")) |>
  dplyr::select(checks, precision:arithmetic) |>
  dplyr::rename(`indep.` = independence) |>
  kable(digits = 3, escape = FALSE, format = "html", booktab = TRUE,
        col.names = c("Checks", "Precision", "Recall", "Independence", "Harmonic", "Arithmetic")) |>
  kableExtra::kable_styling(latex_options="scale_down") |>
  column_spec(1, width = '19em')


## ----------------------------------------------------------------------------------------
#| label: fig-dist-fit
#| fig-cap: "QQ-plot of the goodness-of-fit of selected distributions used to generate simulated data for mortality, PM10, and temperature. Left: Mortality as a negative binomial distribution (size = 74, mean = 183), Middle: PM10 is modeled using a rescaled beta distribution with shape parameters 4.21 and 11.67, multiplied by 10 to match the observed range. Right: Temperature is modeled using a Weibull distribution (shape = 3.8, scale = 61)"
#| fig.height: 4
#| fig.width: 10
aa2 <- fitdistrplus::fitdist(pm10$mortality, "nbinom")
p1 <- tibble(fitted = rnbinom(558, size = aa2$estimate[1], mu = aa2$estimate[2]),
       observed = pm10$mortality) %>%
  ggplot(aes(sample = fitted)) +
  stat_qq_band(fill = "grey80") +
  stat_qq_line(color = "grey30") +
  stat_qq_point(size = 0.3) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  ggtitle("Mortality")


aa6 <- fitdistrplus::fitdist(pm10$pm10[pm10$pm10 < 90]/100, "beta")
p2 <- tibble(fitted = rbeta(557, shape1 = aa6$estimate[1], shape2 = aa6$estimate[2]) * 100,
       observed = pm10$pm10[pm10$pm10 < 90]) %>%
  ggplot(aes(sample = fitted)) +
  stat_qq_band(fill = "grey80") +
  stat_qq_line(color = "grey30") +
  stat_qq_point(size = 0.3) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  ggtitle("PM10")


aa4 <- fitdistrplus::fitdist(pm10$temp, "weibull")
p3 <- tibble(fitted= rweibull(558, aa4$estimate[1], aa4$estimate[2]),
       observed= pm10$temp) %>%
  ggplot(aes(sample = fitted)) +
  stat_qq_band(fill = "grey80") +
  stat_qq_line(color = "grey30") +
  stat_qq_point(size = 0.3) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  ggtitle("Temperature")

p1 | p2 | p3


## ----------------------------------------------------------------------------------------
# cor(pm10$pm10, pm10$mortality)
# cor(pm10$temp, pm10$mortality)
# cor(pm10$temp, pm10$pm10)
corr_grid <- expand.grid(#seq(0.01, 0.2, by = 0.02),
                         seq(-0.1, 0.1, by = 0.02),
                         seq(0.1, 0.5, by = 0.05),
                         seq(0.25, 0.45, by = 0.05))

gen_corr_mtx <- function(r1, r2, r3) {
  # correlation between mortality and pm10 are negative - r1
  # correlation between mortality and temp are negative - r2
  cor_matrix <- matrix(c(1, r1, -r2,
                         r1, 1, r3,
                         -r2, r3, 1), nrow = 3, byrow = TRUE)
  if (all(eigen(cor_matrix)$values > 0)) return(cor_matrix)
}

corr_mtx <- lapply(1:nrow(corr_grid), function(i) {
  gen_corr_mtx(corr_grid[i, 1], corr_grid[i, 2], corr_grid[i, 3]) })
corr_mtx <- corr_mtx[map_lgl(corr_mtx, ~!is.null(.x))]
sample_size <- c(500, 3000)
outlier <- c(TRUE, FALSE)

generate_data <- function(n, mtx, seed = 123, outlier = FALSE) {
  mu <- c(0, 0, 0)
  set.seed(seed)
  data <- mvrnorm(n, mu, mtx, empirical = TRUE)
  U <- pnorm(data, mean = 0, sd = 1)

  if (!outlier) {
    tibble(mortality = qnbinom(U[,1], size = 74, mu = 183),
           pm10 = qbeta(U[,2], shape1 = 4.21, shape2 = 11.67) * 100,
           temp = qweibull(U[,3], shape = 3.8, scale = 61))
  } else{
    pm10_vec <- qbeta(U[,2], shape1 = 4.21, shape2 = 11.67) * 100
    tibble(
      mortality = c(qnbinom(U[,1], size = 74, mu = 183)[-1],
                    rnorm(n = 1, mean = 300, sd = 10)),
      pm10 = c(pm10_vec[-1], rnorm(n = 1, mean = 100, sd = 10)),
      temp = qweibull(U[,3], shape = 3.8, scale = 61)
    )
  }

}

res <- tibble(corr_mtx = corr_mtx) |>
  mutate(id = row_number()) |>
  crossing(sample_size, outlier) |>
  rowwise() |>
  mutate(data = list(generate_data(n = sample_size, mtx = corr_mtx, outlier = outlier)),
         fit = list(summary(glm(mortality ~ pm10 + temp, family = "poisson", data))$coefficients))

pm10_df <- res |>
  mutate(
    p_value = fit[2,4],
    coef = fit[2,1],
    mor_pm10_correlation = corr_mtx[1, 2],
    mor_temp_correlation = corr_mtx[1, 3],
    temp_pm10_yz_correlation = corr_mtx[2, 3],
    unexpect = ifelse(between(coef, 0, 0.005), 0, 1),
    `cor(m, PM10) < -0.05` = ifelse(mor_pm10_correlation < -0.05, 1, 0),
    `cor(m, PM10) < -0.03` = ifelse(mor_pm10_correlation < -0.03, 1, 0),
    `cor(m, PM10) > 0.03` = ifelse(mor_pm10_correlation > 0.03, 1, 0),
    `cor(m, PM10) > 0.05` = ifelse(mor_pm10_correlation > 0.05, 1, 0),
    `cor(m, tmp) > -0.3` = ifelse(mor_temp_correlation > -0.3, 1, 0),
    `cor(m, tmp) > -0.35` = ifelse(mor_temp_correlation > -0.35, 1, 0),
    `cor(m, tmp) > -0.4` = ifelse(mor_temp_correlation > -0.4, 1, 0),
    `cor(m, tmp) > -0.45` = ifelse(mor_temp_correlation > -0.45, 1, 0)
  ) |>
  ungroup()

pm10_df <- pm10_df |>
  rowwise() |>
  mutate(PM10_outlier = ifelse(any(scale(data$pm10) > 4), 1, 0),
         mortality_outlier = ifelse(any(scale(data$mortality) > 4), 1, 0)
  ) |>
  ungroup()

unexpect_df <- tibble(
  unexpect = c(0, 1),
  n = pm10_df$unexpect |> table() |> as.numeric() |> rev(),
  wgt = n/sum(n))
wgt_vec <- pm10_df |> dplyr::select(unexpect) |> left_join(unexpect_df) |> pull(wgt)


## ----------------------------------------------------------------------------------------
#| fig.width: 12
#| fig.height: 6
fit <- pm10_df |> fit_logic_reg(unexpect, `cor(m, PM10) < -0.05`:mortality_outlier,
                                seed = 7, nleaves = 4, wgt = wgt_vec)
plot_logicreg(fit)


## ----------------------------------------------------------------------------------------
#| label: tbl-linear-reg
#| tbl-cap: "Precision, recall, and independence metrics derived from the logic regression model and each individual check, along with harmonic and arithmetic means."
df <- list(
  tibble(.fitted = pm10_df$`cor(m, PM10) < -0.03`,
         unexpect = pm10_df$unexpect),
  tibble(.fitted = pm10_df$`cor(m, tmp) > -0.3`,
         unexpect = pm10_df$unexpect),
  tibble(.fitted = pm10_df$mortality_outlier,
         unexpect = pm10_df$unexpect),
  tibble(.fitted = pm10_df$PM10_outlier,
         unexpect = pm10_df$unexpect)) |>
  map_dfr(~.x |> calc_miscla_rate(.fitted, unexpect) |>
            mutate(independence = 1) |>
            calc_metrics(metrics = c("harmonic", "arithmetic")), .id = "id") |>
  mutate(id = as.numeric(id))

reg <- fit |> augment() |>
  calc_miscla_rate(unexpect, .fitted) |>
  calc_independence() |>
  calc_metrics(metrics = c("harmonic", "arithmetic")) |>
  mutate(id = 5)

df <- df |> bind_rows(reg)

tbl <- tibble(check = c("Check 1: cor(m, PM10) < -- 0.03", "Check 2: cor(m, tmp) > -- 0.35",
                        "Check 3: mortality outlier", "Check 4: PM10 outlier",
                        "Logic regression: (check 1) AND ((check 2) OR (check 3 AND check 4))")) |>
  bind_cols(df |> dplyr::select(-id)) |> rename(`indep.` = independence)

tbl |>
  kable(digits = 3, escape = FALSE, format = "html", booktab = TRUE,
        col.names = c("Checks", "Precision", "Recall", "Independence", "Harmonic", "Arithmetic")) |>
  kableExtra::kable_styling(latex_options="scale_down") |>
  column_spec(1, width = '18em')

# ---------------------------------------------------------------------------
#| label: tbl-all-cities
#| tbl-cap: "Results from full analysis of 40 cities from the NMMAPS air pollution and mortality study. In the table, a T indicates that a check failed and an F indicates that a check passed."
pm10 <- read_csv("data/pm10.csv", col_types = cols()) |> filter(!is.na(pm10)) |> filter(!is.na(temp))
files <- list.files(path = "data", pattern = "csv", full.names = TRUE)[-1]
pred_res <- files |> map_dfr(function(raw){
  data <- read_csv(raw, col_types = cols()) |> filter(!is.na(pm10)) |> filter(!is.na(temp))
  res_df <- glm(mortality ~ pm10 + temp, family = "poisson", data = data)$coefficients |>
    as_tibble_row()

  res_df |>
    mutate(
      cor_m_tmp = cor(data$mortality, data$temp),
      cor_m_pm10 = cor(data$mortality, data$pm10),
      cor_tmp_pm10 = cor(data$temp, data$pm10),
      unexpect = !between(pm10, 0, 0.005),
      mort_outlier = any(scale(data$mortality) > 4),
      pm10_outlier = any(scale(data$pm10) > 4),
      lgl_cor_m_tmp = cor(data$mortality, data$temp) > -0.3,
      lgl_cor_m_pm10 = cor(data$mortality, data$pm10) < -0.03,
      .pred = ((mort_outlier &  pm10_outlier) | (lgl_cor_m_tmp)) & (lgl_cor_m_pm10),
      ss = nrow(data)
    )
})
city_name_map <- read_csv(here::here("data/city_name_map.csv"),col_types = cols())


df <- tibble(city = str_remove(files, "data/pm10-") |> str_remove(".csv")) |> bind_cols(pred_res) |>
  left_join(city_name_map) |>
  filter(city != "data/pm10") |>
  dplyr::select(cityname, pm10, unexpect, .pred,
                cor_m_tmp, lgl_cor_m_tmp,
                cor_m_pm10, lgl_cor_m_pm10,
                mort_outlier, pm10_outlier)

df |>
  mutate(pm10 = format(round(pm10, 4), nsmall = 4),
         cor_m_tmp = format(round(cor_m_tmp, 2), nsmall = 2),
         cor_m_pm10 = format(round(cor_m_pm10, 2), nsmall = 2)) |>
  mutate(across(c(unexpect, .pred, lgl_cor_m_pm10, lgl_cor_m_tmp, mort_outlier, pm10_outlier),
                ~ifelse(.x, "T", "F"))) |>
  mutate(cor_m_tmp = paste0(cor_m_tmp, " (", lgl_cor_m_tmp, ")"),
         cor_m_pm10 = paste0(cor_m_pm10, " (", lgl_cor_m_pm10, ")")) |>
  dplyr::select(-lgl_cor_m_tmp, -lgl_cor_m_pm10) |>
  kable(format = "html", booktabs = TRUE, escape = F,
        align = "lrccrrcc")


## ----------------------------------------------------------------------------------------
#| label: tbl-accuracy
#| tbl-cap: "Summary of the observed and the predicted unexpected PM10 coefficient results from the 40 NMMAPS cities using the logic regression model."
df |>
  dplyr::select(unexpect, .pred) |>
  table() |>
  as_tibble() |>
  rename(Observed = unexpect) |>
  pivot_wider(names_from = .pred, values_from = n) |>
  kable(format = "html",
        booktabs = TRUE) |>
   add_header_above(c("", "Predicted"=2),
                    escape = FALSE)
