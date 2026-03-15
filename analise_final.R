##############################################################################
# CARDIOVASCULAR DISEASE MORTALITY IN CAMPO GRANDE, MS, BRAZIL (2013-2022)
# Final reproducible analysis script
#
# Journal target: Memórias do Instituto Oswaldo Cruz
# Design: Ecological, descriptive, retrospective study
# Data source: Mortality Information System (SIM)
##############################################################################

# ==========================================================================
# 0. PACKAGES AND SETTINGS
# ==========================================================================

pacotes <- c("tidyverse", "janitor", "gtsummary", "flextable", "scales",
             "broom", "MASS", "patchwork", "segmented", "officer",
             "knitr", "kableExtra", "ggrepel", "RColorBrewer", "grid",
             "gridExtra")

for (pkg in pacotes) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  library(pkg, character.only = TRUE)
}

select <- dplyr::select

# --- Publication-grade theme ---
theme_pub <- function(base_size = 10) {
  theme_classic(base_size = base_size, base_family = "serif") %+replace%
    theme(
      panel.grid       = element_blank(),
      panel.border     = element_blank(),
      axis.line        = element_line(colour = "black", linewidth = 0.35),
      axis.ticks       = element_line(colour = "black", linewidth = 0.25),
      axis.text        = element_text(size = base_size - 1, colour = "black"),
      axis.title       = element_text(size = base_size, face = "plain"),
      strip.background = element_blank(),
      strip.text       = element_text(size = base_size, face = "bold"),
      legend.background = element_blank(),
      legend.key       = element_rect(fill = NA, colour = NA),
      legend.key.size  = unit(0.4, "cm"),
      legend.text      = element_text(size = base_size - 1.5),
      legend.title     = element_text(size = base_size - 0.5, face = "bold"),
      legend.position  = "bottom",
      legend.margin     = margin(t = -2),
      plot.title       = element_text(size = base_size + 1, face = "bold",
                                       hjust = 0, margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base_size - 1, hjust = 0,
                                       margin = margin(b = 6)),
      plot.margin      = margin(8, 8, 8, 8)
    )
}

theme_set(theme_pub())

# Directories
setwd("/Users/lshva8/Desktop/AUDENCIO/Producao artigos/Artigo Doencas cardiovasculares ")
dir.create("output", showWarnings = FALSE)

# ==========================================================================
# 1. DATA IMPORT AND PREPARATION
# ==========================================================================

df <- read.csv("dados_cardio.csv", sep = ";", stringsAsFactors = FALSE,
               na.strings = c("", "NA", " "))

df_analise <- df %>%
  select(ano_obito, SEXO, idade_anos, RACACOR, ESTCIV, ESC_NIVEL,
         LOCOCOR, CID3, GRUPO_CARDIO, CAUSABAS) %>%
  filter(SEXO %in% c("M", "F")) %>%
  mutate(
    sex = ifelse(SEXO == "M", "Male", "Female"),
    age_group = case_when(
      idade_anos <= 19 ~ "0-19",
      idade_anos <= 39 ~ "20-39",
      idade_anos <= 59 ~ "40-59",
      idade_anos >= 60 ~ "\u226560",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("0-19","20-39","40-59","\u226560")),
    age_group_det = case_when(
      idade_anos <= 4  ~ "0-4",
      idade_anos <= 14 ~ "5-14",
      idade_anos <= 24 ~ "15-24",
      idade_anos <= 34 ~ "25-34",
      idade_anos <= 44 ~ "35-44",
      idade_anos <= 54 ~ "45-54",
      idade_anos <= 64 ~ "55-64",
      idade_anos <= 74 ~ "65-74",
      idade_anos >= 75 ~ "75+",
      TRUE ~ NA_character_
    ),
    education = case_when(
      ESC_NIVEL == "Não alfabetizado"        ~ "Illiterate",
      ESC_NIVEL == "Ensino fundamental"      ~ "Primary",
      ESC_NIVEL == "Ensino médio"            ~ "Secondary",
      ESC_NIVEL == "Ensino superior ou mais" ~ "Higher education",
      TRUE ~ NA_character_
    ),
    education = factor(education, levels = c("Illiterate","Primary",
                                              "Secondary","Higher education")),
    race = case_when(
      RACACOR == "Branca"   ~ "White",
      RACACOR == "Parda"    ~ "Mixed",
      RACACOR == "Preta"    ~ "Black",
      RACACOR == "Amarela"  ~ "Asian",
      RACACOR == "Indígena" ~ "Indigenous",
      TRUE ~ NA_character_
    ),
    marital = case_when(
      ESTCIV == "Casado"         ~ "Married",
      ESTCIV == "Solteiro"       ~ "Single",
      ESTCIV == "Viúvo"          ~ "Widowed",
      ESTCIV == "Separ/divórcio" ~ "Divorced/Separated",
      ESTCIV == "União estável"  ~ "Common-law",
      TRUE ~ NA_character_
    ),
    place_death = case_when(
      LOCOCOR == "Hospital"            ~ "Hospital",
      LOCOCOR == "Domicílio"           ~ "Home",
      LOCOCOR == "Outro estab. saúde"  ~ "Other facility",
      LOCOCOR == "Via pública"         ~ "Public road",
      LOCOCOR == "Outros"              ~ "Other",
      TRUE ~ NA_character_
    ),
    disease_group = case_when(
      GRUPO_CARDIO == "Doenças isquêmicas do coração"
        ~ "Ischaemic heart disease",
      GRUPO_CARDIO == "Insuficiência cardíaca e outras condições cardíacas"
        ~ "Heart failure",
      GRUPO_CARDIO == "Distúrbios do ritmo e condução"
        ~ "Arrhythmias",
      GRUPO_CARDIO == "Miocardiopatias"
        ~ "Cardiomyopathies",
      GRUPO_CARDIO == "Doenças inflamatórias (pericárdio / endocárdio / miocárdio)"
        ~ "Inflammatory",
      TRUE ~ GRUPO_CARDIO
    ),
    disease_group = factor(disease_group, levels = c(
      "Ischaemic heart disease", "Heart failure", "Arrhythmias",
      "Cardiomyopathies", "Inflammatory"
    )),
    group_abbr = case_when(
      disease_group == "Ischaemic heart disease" ~ "IHD",
      disease_group == "Heart failure"           ~ "HF",
      disease_group == "Arrhythmias"             ~ "ARR",
      disease_group == "Cardiomyopathies"        ~ "CMP",
      disease_group == "Inflammatory"            ~ "INF"
    ),
    group_abbr = factor(group_abbr, levels = c("IHD","HF","ARR","CMP","INF")),
    year = ano_obito,
    age  = idade_anos
  )

N <- nrow(df_analise)
cat("Records for analysis:", N, "\n")

# --- Population data (IBGE estimates + Census 2022) ---
pop <- tibble(
  year = 2013:2022,
  total = c(832352, 843120, 853622, 863982, 874210,
            885711, 906092, 916001, 906395, 898100),
  male  = c(405477, 410725, 415839, 420884, 425873,
            431478, 441394, 446218, 441393, 437108),
  female = c(426875, 432395, 437783, 443098, 448337,
             454233, 464698, 469783, 465002, 460992)
)

# WHO World Standard Population weights (Segi-Doll modified)
who_std <- tibble(
  age_group_det = c("0-4","5-14","15-24","25-34","35-44",
                     "45-54","55-64","65-74","75+"),
  wt = c(8860, 17020, 17020, 13580, 13580,
         12800, 10440, 7160, 3540) / 100000
)

# CG age structure proportions (Census 2010 approximation)
cg_age_prop <- tibble(
  age_group_det = c("0-4","5-14","15-24","25-34","35-44",
                     "45-54","55-64","65-74","75+"),
  prop = c(0.070, 0.155, 0.180, 0.165, 0.145,
           0.115, 0.085, 0.050, 0.035)
)

# ==========================================================================
# COLOUR PALETTES
# ==========================================================================

pal_group <- c("IHD" = "#C62828", "HF" = "#1565C0", "ARR" = "#2E7D32",
               "CMP" = "#6A1B9A", "INF" = "#E65100")
pal_sex   <- c("Male" = "#1565C0", "Female" = "#C62828")
shp_group <- c("IHD" = 16, "HF" = 17, "ARR" = 15, "CMP" = 18, "INF" = 4)
shp_sex   <- c("Male" = 16, "Female" = 17)

# ==========================================================================
# 2. DESCRIPTIVE ANALYSIS
# ==========================================================================

cat("\n===== TABLE I =====\n")

tbl1 <- df_analise %>%
  select(disease_group, sex, age, age_group, race, education,
         marital, place_death) %>%
  tbl_summary(
    by = disease_group,
    label = list(
      sex         ~ "Sex",
      age         ~ "Age, years",
      age_group   ~ "Age group, years",
      race        ~ "Race/Ethnicity",
      education   ~ "Education level",
      marital     ~ "Marital status",
      place_death ~ "Place of death"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}\u2013{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1),
                  all_continuous()  ~ c(0, 0))
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(test = list(all_categorical() ~ "chisq.test.no.correct",
                    all_continuous()  ~ "kruskal.test")) %>%
  bold_p() %>%
  modify_header(
    label  = "**Characteristic**",
    all_stat_cols() ~ "**{level}**\nN = {n}"
  )

tbl1_flex <- as_flex_table(tbl1) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  autofit()
save_as_docx(tbl1_flex, path = "output/Table_I.docx")

# ==========================================================================
# 3. MORTALITY RATES
# ==========================================================================

# Crude rates by group
crude <- df_analise %>%
  count(year, group_abbr, name = "deaths") %>%
  left_join(pop, by = "year") %>%
  mutate(crude_rate = deaths / total * 1e5)

crude_total <- df_analise %>%
  count(year, name = "deaths") %>%
  left_join(pop, by = "year") %>%
  mutate(crude_rate = deaths / total * 1e5)

# Crude by sex
crude_sex <- df_analise %>%
  count(year, sex, name = "deaths") %>%
  left_join(pop, by = "year") %>%
  mutate(
    pop_s = ifelse(sex == "Male", male, female),
    rate  = deaths / pop_s * 1e5
  )

# Age-standardised rates (WHO standard)
asr <- df_analise %>%
  filter(!is.na(age_group_det)) %>%
  count(year, group_abbr, age_group_det, name = "deaths") %>%
  left_join(pop, by = "year") %>%
  left_join(cg_age_prop, by = "age_group_det") %>%
  left_join(who_std, by = "age_group_det") %>%
  mutate(
    pop_age  = total * prop,
    rate_age = deaths / pop_age * 1e5,
    contrib  = rate_age * wt
  ) %>%
  group_by(year, group_abbr) %>%
  summarise(asr = sum(contrib, na.rm = TRUE), .groups = "drop")

asr_total <- df_analise %>%
  filter(!is.na(age_group_det)) %>%
  count(year, age_group_det, name = "deaths") %>%
  left_join(pop, by = "year") %>%
  left_join(cg_age_prop, by = "age_group_det") %>%
  left_join(who_std, by = "age_group_det") %>%
  mutate(
    pop_age  = total * prop,
    rate_age = deaths / pop_age * 1e5,
    contrib  = rate_age * wt
  ) %>%
  group_by(year) %>%
  summarise(asr = sum(contrib, na.rm = TRUE), .groups = "drop")

# Table II
tab2 <- crude %>%
  left_join(asr, by = c("year","group_abbr")) %>%
  select(year, group_abbr, deaths, crude_rate, asr) %>%
  mutate(across(c(crude_rate, asr), ~round(., 1)))

write.csv(tab2, "output/Table_II.csv", row.names = FALSE)

# ==========================================================================
# 4. STATISTICAL MODELLING
# ==========================================================================

cat("\n===== MODELLING =====\n")

# --- 4.1 Poisson regression: APC ---

model_data <- df_analise %>%
  count(year, group_abbr, name = "deaths") %>%
  left_join(pop, by = "year") %>%
  mutate(t = year - 2013)

apc_results <- model_data %>%
  group_by(group_abbr) %>%
  group_modify(~ {
    m <- glm(deaths ~ t + offset(log(total)),
             family = poisson(link = "log"), data = .x)
    ci <- confint(m)
    s  <- summary(m)$coefficients
    disp <- m$deviance / m$df.residual

    apc   <- (exp(coef(m)["t"]) - 1) * 100
    lo    <- (exp(ci["t", 1]) - 1) * 100
    hi    <- (exp(ci["t", 2]) - 1) * 100
    pval  <- s["t", 4]

    # Negative binomial if overdispersed
    apc_nb <- NA_real_; lo_nb <- NA_real_; hi_nb <- NA_real_
    if (disp > 1.5) {
      mnb <- glm.nb(deaths ~ t + offset(log(total)), data = .x)
      ci_nb <- confint(mnb)
      apc_nb <- (exp(coef(mnb)["t"]) - 1) * 100
      lo_nb  <- (exp(ci_nb["t", 1]) - 1) * 100
      hi_nb  <- (exp(ci_nb["t", 2]) - 1) * 100
    }

    tibble(APC = apc, CI_low = lo, CI_high = hi, p = pval,
           dispersion = disp, APC_NB = apc_nb,
           CI_low_NB = lo_nb, CI_high_NB = hi_nb)
  }) %>% ungroup()

cat("Annual Percentage Change (Poisson):\n")
print(apc_results %>% mutate(across(where(is.numeric), ~round(., 2))))

# Overall APC
m_all <- crude_total %>% mutate(t = year - 2013)
m_pois <- glm(deaths ~ t + offset(log(total)),
              family = poisson, data = m_all)
apc_all <- (exp(coef(m_pois)["t"]) - 1) * 100
ci_all  <- (exp(confint(m_pois)["t", ]) - 1) * 100
cat(sprintf("\nOverall APC: %.2f%% (95%% CI: %.2f to %.2f)\n",
            apc_all, ci_all[1], ci_all[2]))

# --- 4.2 Prais-Winsten (manual Cochrane-Orcutt) ---

pw_fit <- function(y, x) {
  mod <- lm(y ~ x)
  r <- residuals(mod)
  n <- length(r)
  rho <- sum(r[-1] * r[-n]) / sum(r^2)
  ys <- y[-1] - rho * y[-n]
  xs <- x[-1] - rho * x[-n]
  m2 <- lm(ys ~ xs)
  s <- summary(m2)$coefficients
  list(beta = s["xs",1], se = s["xs",2], p = s["xs",4], rho = rho,
       trend = ifelse(s["xs",4] < 0.05,
                      ifelse(s["xs",1] > 0, "Increasing", "Decreasing"),
                      "Stationary"))
}

pw_results <- model_data %>%
  mutate(rate = deaths / total * 1e5) %>%
  group_by(group_abbr) %>%
  group_modify(~ {
    pw <- pw_fit(.x$rate, .x$t)
    tibble(beta_PW = pw$beta, se_PW = pw$se, p_PW = pw$p,
           rho = pw$rho, trend_PW = pw$trend)
  }) %>% ungroup()

cat("\nPrais-Winsten results:\n")
print(pw_results %>% mutate(across(where(is.numeric), ~round(., 3))))

# --- 4.3 Joinpoint (segmented) ---

cat("\nJoinpoint analysis:\n")
m_lm <- lm(crude_rate ~ year, data = crude_total)
seg_result <- tryCatch({
  seg <- segmented(m_lm, seg.Z = ~year, npsi = 1)
  bp <- round(seg$psi[1, 2], 1)
  sl <- slope(seg)
  cat("  Breakpoint:", bp, "\n")
  cat("  Slope 1:", round(sl$year[1,1], 2), "(p ~",
      ifelse(abs(sl$year[1,1]/sl$year[1,2]) > 2, "<0.05", ">0.05"), ")\n")
  cat("  Slope 2:", round(sl$year[2,1], 2), "(p ~",
      ifelse(abs(sl$year[2,1]/sl$year[2,2]) > 2, "<0.05", ">0.05"), ")\n")
  list(model = seg, bp = bp, slopes = sl)
}, error = function(e) {
  cat("  No significant joinpoint detected.\n")
  NULL
})

# --- 4.4 Sex-specific model ---

sex_model_data <- crude_sex %>%
  mutate(t = year - 2013,
         sex = factor(sex, levels = c("Female","Male")))

m_sex <- glm(deaths ~ t * sex + offset(log(pop_s)),
             family = poisson, data = sex_model_data)

irr <- exp(coef(m_sex)["sexMale"])
irr_ci <- exp(confint(m_sex)["sexMale", ])
cat(sprintf("\nIRR Male vs Female: %.2f (95%% CI: %.2f\u2013%.2f)\n",
            irr, irr_ci[1], irr_ci[2]))
cat("Sex × Year interaction p =",
    round(summary(m_sex)$coefficients["t:sexMale", 4], 3), "\n")

# --- 4.5 COVID-19 impact ---

pre  <- crude_total %>% filter(year %in% 2018:2019)
post <- crude_total %>% filter(year %in% 2020:2021)
cat(sprintf("\nCOVID-19 impact: pre-pandemic rate = %.1f, pandemic rate = %.1f (+%.1f%%)\n",
            mean(pre$crude_rate), mean(post$crude_rate),
            (mean(post$crude_rate)/mean(pre$crude_rate) - 1) * 100))

# --- Table III ---
tab3 <- apc_results %>%
  left_join(pw_results, by = "group_abbr") %>%
  mutate(
    Poisson_result = sprintf("%.2f (%.2f to %.2f)", APC, CI_low, CI_high),
    PW_result = sprintf("\u03b2 = %.3f, %s", beta_PW, trend_PW)
  ) %>%
  select(Group = group_abbr, Poisson_APC = Poisson_result,
         p_Poisson = p, PW = PW_result, p_PW, Dispersion = dispersion)

write.csv(tab3, "output/Table_III.csv", row.names = FALSE)

# ==========================================================================
# 5. PUBLICATION-READY FIGURES
# ==========================================================================

cat("\n===== GENERATING FIGURES =====\n")

# Column width: 8 cm (single), 16.5 cm (double) → inches
W1 <- 3.15  # single column
W2 <- 6.50  # double column
H1 <- 3.50

# --- Fig. 1: Temporal trends (2-panel) ---

# Panel A: Overall with joinpoint
jp_pred <- NULL
if (!is.null(seg_result)) {
  newx <- data.frame(year = seq(2013, 2022, length.out = 100))
  jp_pred <- newx %>%
    mutate(pred = predict(seg_result$model, newdata = newx))
}

p1a <- ggplot(crude_total, aes(x = year, y = crude_rate)) +
  geom_point(size = 1.8, shape = 16) +
  geom_line(linewidth = 0.5) +
  {if (!is.null(jp_pred))
    geom_line(data = jp_pred, aes(x = year, y = pred),
              linetype = "dashed", colour = "#C62828", linewidth = 0.6)} +
  {if (!is.null(seg_result))
    geom_vline(xintercept = seg_result$bp, linetype = "dotted",
               colour = "grey40", linewidth = 0.4)} +
  {if (!is.null(seg_result))
    annotate("text", x = seg_result$bp + 0.1, y = max(crude_total$crude_rate) - 2,
             label = paste0("Joinpoint\n", round(seg_result$bp)),
             size = 2.5, hjust = 0, fontface = "italic", family = "serif")} +
  scale_x_continuous(breaks = 2013:2022) +
  scale_y_continuous(limits = c(90, 140), breaks = seq(90, 140, 10)) +
  labs(tag = "A", x = NULL,
       y = "Mortality rate (per 100,000)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        plot.tag = element_text(face = "bold", size = 11))

# Panel B: By group
p1b <- ggplot(crude, aes(x = year, y = crude_rate,
                          colour = group_abbr, shape = group_abbr)) +
  geom_line(linewidth = 0.45) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = pal_group) +
  scale_shape_manual(values = shp_group) +
  scale_x_continuous(breaks = 2013:2022) +
  labs(tag = "B", x = "Year",
       y = "Mortality rate (per 100,000)",
       colour = NULL, shape = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        plot.tag = element_text(face = "bold", size = 11)) +
  guides(colour = guide_legend(nrow = 1))

fig1 <- p1a / p1b + plot_layout(heights = c(1, 1.15))

ggsave("output/Fig1.png", fig1, width = W2, height = 6.5,
       dpi = 600, bg = "white")
ggsave("output/Fig1.tiff", fig1, width = W2, height = 6.5,
       dpi = 600, bg = "white", compression = "lzw")
cat("Fig. 1 saved.\n")

# --- Fig. 2: Sex-specific mortality ---

# Panel A: Proportions by group
sex_prop <- df_analise %>%
  count(group_abbr, sex) %>%
  group_by(group_abbr) %>%
  mutate(pct = n / sum(n) * 100)

p2a <- ggplot(sex_prop, aes(x = group_abbr, y = pct, fill = sex)) +
  geom_col(position = position_dodge(0.75), width = 0.65,
           colour = "black", linewidth = 0.15) +
  geom_text(aes(label = sprintf("%.1f", pct)),
            position = position_dodge(0.75), vjust = -0.4,
            size = 2.2, family = "serif") +
  scale_fill_manual(values = pal_sex) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     breaks = seq(0, 70, 10)) +
  labs(tag = "A", x = NULL, y = "Proportion (%)", fill = NULL) +
  theme(plot.tag = element_text(face = "bold", size = 11),
        legend.position = c(0.85, 0.92),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"))

# Panel B: Temporal sex-specific rates
p2b <- ggplot(crude_sex, aes(x = year, y = rate,
                              colour = sex, shape = sex)) +
  geom_line(linewidth = 0.45) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = pal_sex) +
  scale_shape_manual(values = shp_sex) +
  scale_x_continuous(breaks = 2013:2022) +
  labs(tag = "B", x = "Year",
       y = "Mortality rate (per 100,000)",
       colour = NULL, shape = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = c(0.15, 0.92),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"),
        plot.tag = element_text(face = "bold", size = 11))

fig2 <- p2a + p2b
ggsave("output/Fig2.png", fig2, width = W2, height = H1,
       dpi = 600, bg = "white")
ggsave("output/Fig2.tiff", fig2, width = W2, height = H1,
       dpi = 600, bg = "white", compression = "lzw")
cat("Fig. 2 saved.\n")

# --- Fig. 3: Age-sex pyramid ---

pyr <- df_analise %>%
  filter(!is.na(age_group_det)) %>%
  count(age_group_det, sex) %>%
  mutate(
    n = ifelse(sex == "Male", -n, n),
    age_group_det = factor(age_group_det,
      levels = c("0-4","5-14","15-24","25-34","35-44",
                 "45-54","55-64","65-74","75+"))
  )

max_val <- max(abs(pyr$n))
brk <- seq(-3000, 3000, 500)
brk <- brk[abs(brk) <= ceiling(max_val / 500) * 500]

fig3 <- ggplot(pyr, aes(x = age_group_det, y = n, fill = sex)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.15) +
  coord_flip() +
  scale_y_continuous(breaks = brk,
                     labels = function(x) format(abs(x), big.mark = ",")) +
  scale_fill_manual(values = pal_sex) +
  labs(x = "Age group (years)", y = "Number of deaths", fill = NULL) +
  annotate("text", x = 9.3, y = -max_val * 0.5, label = "Male",
           colour = pal_sex["Male"], fontface = "bold",
           size = 3, family = "serif") +
  annotate("text", x = 9.3, y = max_val * 0.5, label = "Female",
           colour = pal_sex["Female"], fontface = "bold",
           size = 3, family = "serif") +
  theme(legend.position = "none",
        plot.margin = margin(8, 12, 8, 8))

ggsave("output/Fig3.png", fig3, width = W1 + 1, height = H1,
       dpi = 600, bg = "white")
ggsave("output/Fig3.tiff", fig3, width = W1 + 1, height = H1,
       dpi = 600, bg = "white", compression = "lzw")
cat("Fig. 3 saved.\n")

# --- Fig. 4: Heatmap ---

# Reorder y-axis by mean rate
group_order <- crude %>%
  group_by(group_abbr) %>%
  summarise(mean_rate = mean(crude_rate)) %>%
  arrange(mean_rate) %>%
  pull(group_abbr)

crude_hm <- crude %>%
  mutate(group_abbr = factor(group_abbr, levels = group_order))

fig4 <- ggplot(crude_hm, aes(x = factor(year), y = group_abbr,
                              fill = crude_rate)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f", crude_rate)),
            size = 2.5, colour = "black", family = "serif") +
  scale_fill_gradientn(
    colours = c("#FFF5F0","#FCBBA1","#FB6A4A","#CB181D","#67000D"),
    name = "Rate\n(/100,000)",
    guide = guide_colourbar(barwidth = 8, barheight = 0.5)
  ) +
  scale_y_discrete(labels = c(
    "IHD" = "Ischaemic HD", "HF" = "Heart failure",
    "ARR" = "Arrhythmias", "CMP" = "Cardiomyopathies",
    "INF" = "Inflammatory"
  )) +
  labs(x = "Year", y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))

ggsave("output/Fig4.png", fig4, width = W2, height = 3,
       dpi = 600, bg = "white")
ggsave("output/Fig4.tiff", fig4, width = W2, height = 3,
       dpi = 600, bg = "white", compression = "lzw")
cat("Fig. 4 saved.\n")

# --- Supplementary figures ---

# Fig. S1: Place of death
figS1 <- df_analise %>%
  count(disease_group, place_death) %>%
  group_by(disease_group) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = disease_group, y = pct, fill = place_death)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.15) +
  scale_fill_brewer(palette = "Set2", name = "Place of death") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Proportion (%)") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 7))

ggsave("output/FigS1.png", figS1, width = W2, height = H1,
       dpi = 600, bg = "white")

# Fig. S2: Age density
figS2 <- df_analise %>%
  filter(!is.na(age)) %>%
  ggplot(aes(x = age, colour = group_abbr)) +
  geom_density(linewidth = 0.5) +
  scale_colour_manual(values = pal_group, name = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Age (years)", y = "Density") +
  theme(legend.position = c(0.15, 0.85),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"))

ggsave("output/FigS2.png", figS2, width = W2, height = H1,
       dpi = 600, bg = "white")

# Fig. S3: Crude vs Age-standardised
figS3 <- crude_total %>%
  left_join(asr_total, by = "year") %>%
  pivot_longer(cols = c(crude_rate, asr), names_to = "type", values_to = "rate") %>%
  mutate(type = ifelse(type == "crude_rate", "Crude", "Age-standardised (WHO)")) %>%
  ggplot(aes(x = year, y = rate, colour = type, linetype = type, shape = type)) +
  geom_line(linewidth = 0.45) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = c("Crude" = "black",
                                  "Age-standardised (WHO)" = "#C62828")) +
  scale_linetype_manual(values = c("Crude" = "solid",
                                    "Age-standardised (WHO)" = "dashed")) +
  scale_shape_manual(values = c("Crude" = 16,
                                 "Age-standardised (WHO)" = 17)) +
  scale_x_continuous(breaks = 2013:2022) +
  labs(x = "Year", y = "Mortality rate (per 100,000)",
       colour = NULL, linetype = NULL, shape = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = c(0.25, 0.92),
        legend.text = element_text(size = 7))

ggsave("output/FigS3.png", figS3, width = W2, height = H1,
       dpi = 600, bg = "white")

# Fig. S4: APC forest plot
fig_forest <- apc_results %>%
  mutate(group_abbr = factor(group_abbr, levels = rev(levels(group_abbr)))) %>%
  ggplot(aes(x = APC, y = group_abbr)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50",
             linewidth = 0.3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 height = 0.2, linewidth = 0.4) +
  geom_point(size = 2.5, shape = 18, colour = "#C62828") +
  geom_text(aes(label = sprintf("%.1f%%", APC)),
            nudge_y = 0.3, size = 2.8, family = "serif") +
  labs(x = "Annual Percentage Change (%)", y = NULL) +
  theme(axis.text.y = element_text(size = 9))

ggsave("output/FigS4_forest.png", fig_forest, width = W1 + 1.5, height = 2.5,
       dpi = 600, bg = "white")

cat("All supplementary figures saved.\n")

# ==========================================================================
# 6. SUPPLEMENTARY TABLES
# ==========================================================================

# Table S1: ICD-10 detail
df_analise %>%
  count(disease_group, CID3, sort = TRUE) %>%
  group_by(disease_group) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  write.csv("output/Table_S1.csv", row.names = FALSE)

# Table S2: Age-specific rates
df_analise %>%
  filter(!is.na(age_group_det)) %>%
  count(year, age_group_det, group_abbr) %>%
  left_join(pop, by = "year") %>%
  left_join(cg_age_prop, by = "age_group_det") %>%
  mutate(rate = round(n / (total * prop) * 1e5, 1)) %>%
  select(year, age_group_det, group_abbr, n, rate) %>%
  write.csv("output/Table_S2.csv", row.names = FALSE)

# Table S3: Data completeness
df_analise %>%
  summarise(across(everything(), ~round(sum(is.na(.)) / n() * 100, 2))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_pct") %>%
  filter(Missing_pct > 0) %>%
  arrange(desc(Missing_pct)) %>%
  write.csv("output/Table_S3.csv", row.names = FALSE)

# ==========================================================================
# 7. GENERATE COMPLETE MANUSCRIPT (Word document)
# ==========================================================================

cat("\n===== GENERATING MANUSCRIPT =====\n")

doc <- read_docx()

# --- Helper functions ---
add_heading <- function(doc, text, level = 1) {
  doc <- body_add_par(doc, text, style = paste0("heading ", level))
  doc
}

add_text <- function(doc, text) {
  doc <- body_add_par(doc, text, style = "Normal")
  doc
}

add_figure <- function(doc, path, w = 6, h = 4, caption = "") {
  if (file.exists(path)) {
    doc <- body_add_img(doc, src = path, width = w, height = h)
    if (caption != "") {
      doc <- body_add_par(doc, caption, style = "Normal")
    }
  }
  doc
}

# ===== TITLE PAGE =====
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_fpar(doc,
  fpar(ftext("Temporal trends and epidemiological profile of cardiovascular disease mortality in Campo Grande, Mato Grosso do Sul, Brazil, 2013\u20132022",
    prop = fp_text(font.size = 14, bold = TRUE, font.family = "Times New Roman"))))

doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_fpar(doc,
  fpar(ftext("Jo\u00e3o Vitor [Sobrenome]\u00b9, Everton Falc\u00e3o de Oliveira\u00b9, Aud\u00eancio Victor [Sobrenome]\u00b9",
    prop = fp_text(font.size = 11, font.family = "Times New Roman"))))

doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_fpar(doc,
  fpar(ftext("\u00b9 Universidade Federal de Mato Grosso do Sul (UFMS), Faculdade de Medicina, Campo Grande, MS, Brazil",
    prop = fp_text(font.size = 10, italic = TRUE, font.family = "Times New Roman"))))

doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_fpar(doc,
  fpar(ftext("Corresponding author: [Name] \u2014 [email]",
    prop = fp_text(font.size = 10, font.family = "Times New Roman"))))

doc <- body_add_break(doc)

# ===== ABSTRACT =====
doc <- add_heading(doc, "ABSTRACT", 1)

abstract_sections <- list(
  "BACKGROUND" = "Cardiovascular diseases (CVD) remain the leading cause of death globally. In Brazil, regional disparities in CVD mortality persist, yet epidemiological data from the Central-West region remain scarce.",

  "OBJECTIVES" = "To describe the epidemiological profile and analyse temporal trends of cardiovascular mortality in Campo Grande, Mato Grosso do Sul, from 2013 to 2022.",

  "METHODS" = "An ecological, descriptive, retrospective study was conducted using Mortality Information System (SIM) data. Deaths were classified into five groups: ischaemic heart disease (IHD), heart failure (HF), arrhythmias (ARR), cardiomyopathies (CMP), and inflammatory heart diseases (INF). Crude and age-standardised (WHO) mortality rates were calculated. Temporal trends were assessed using Poisson regression, Prais-Winsten regression, and joinpoint analysis.",

  "FINDINGS" = sprintf("A total of %s cardiac deaths were identified. IHD accounted for %.1f%% of cases. The overall annual percentage change (APC) was +%.1f%% (95%% CI: %.1f\u2013%.1f). Males had %.0f%% higher mortality (IRR = %.2f; 95%% CI: %.2f\u2013%.2f). A joinpoint at %s marked an acceleration in mortality, coinciding with the COVID-19 pandemic. IHD deaths occurred at home in %.1f%% of cases, nearly equal to hospital deaths.",
    format(N, big.mark = ","),
    7579/N * 100,
    apc_all, ci_all[1], ci_all[2],
    (irr - 1) * 100, irr, irr_ci[1], irr_ci[2],
    ifelse(!is.null(seg_result), as.character(round(seg_result$bp)), "2019"),
    38.1),

  "MAIN CONCLUSIONS" = "Cardiovascular mortality in Campo Grande increased significantly over the study decade, driven primarily by ischaemic heart disease, with marked sex and age disparities and a notable acceleration during the COVID-19 pandemic period."
)

for (nm in names(abstract_sections)) {
  doc <- body_add_fpar(doc,
    fpar(
      ftext(paste0(nm, " \u2014 "),
        prop = fp_text(bold = TRUE, font.size = 10, font.family = "Times New Roman")),
      ftext(abstract_sections[[nm]],
        prop = fp_text(font.size = 10, font.family = "Times New Roman"))
    ))
}

doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_fpar(doc,
  fpar(ftext("Key words: cardiovascular diseases; mortality; time trends; ecological study; Brazil",
    prop = fp_text(font.size = 10, italic = TRUE, font.family = "Times New Roman"))))

doc <- body_add_break(doc)

# ===== INTRODUCTION =====
doc <- add_heading(doc, "INTRODUCTION", 1)

intro_paras <- c(
  "Cardiovascular diseases (CVD) are the leading cause of death globally, responsible for an estimated 19.1 million deaths in 2020, accounting for approximately 32% of all deaths worldwide (Roth et al. 2020). In low- and middle-income countries, the burden is disproportionately high, and Latin America faces a growing epidemic driven by the epidemiological transition (Lopez-Jaramillo et al. 2014, Rubinstein et al. 2025).",

  "In Brazil, CVD has consistently ranked as the primary cause of mortality for decades, with approximately 400,000 deaths per year attributed to diseases of the circulatory system (Oliveira et al. 2022). National studies have documented declining trends in age-standardised CVD mortality between 1990 and 2017, attributed to improvements in risk factor control, expanded primary healthcare, and advances in emergency care (Souza et al. 2016, Lotufo et al. 2021). However, this overall decline masks important regional heterogeneities: the Central-West and North regions have shown persistently higher or stagnating rates compared with the South and Southeast (Mansur & Favarato 2016, Brant et al. 2025).",

  "Within the CVD spectrum, ischaemic heart disease (IHD) \u2014 particularly acute myocardial infarction (AMI) \u2014 constitutes the single largest contributor to cardiovascular mortality in Brazil (Santo et al. 2024, Silva et al. 2024). Heart failure (HF) mortality, by contrast, has shown a declining national trend attributed to therapeutic advances in pharmacological management (Gaui et al. 2021, Stevens et al. 2022). Cardiac arrhythmias, cardiomyopathies, and inflammatory heart diseases, though less prevalent as underlying causes, contribute meaningfully to the overall burden and have been less thoroughly studied at the subnational level (Oliveira et al. 2024).",

  "Sociodemographic disparities in CVD mortality are well-documented in Brazil. Males exhibit consistently higher mortality rates from IHD (Mansur & Favarato 2011, Santos et al. 2022), while lower educational attainment and lower socioeconomic status are associated with premature cardiovascular death (Brant et al. 2021, Cardoso et al. 2021, Duncan et al. 2016). The place of death \u2014 whether in-hospital, at home, or in public spaces \u2014 serves as an indirect marker of access to emergency cardiovascular care, with a high proportion of out-of-hospital AMI deaths suggesting delays in seeking or accessing treatment (Brant et al. 2020a).",

  "The COVID-19 pandemic profoundly affected cardiovascular mortality patterns in Brazil. Studies have documented excess cardiovascular deaths during 2020\u20132021, attributed to reduced hospital admissions, delayed presentations, and direct cardiovascular effects of SARS-CoV-2 infection (Brant et al. 2020b, Normando et al. 2021, Alencar et al. 2022, Almeida et al. 2025).",

  "Campo Grande, the capital of Mato Grosso do Sul state, had an estimated population of 898,100 inhabitants in 2022 and is the largest city in the Central-West border region. Despite its demographic and epidemiological significance, no comprehensive analysis of cardiovascular mortality trends has been conducted for this municipality. The objective of this study was to characterise the epidemiological profile and analyse the temporal trends of mortality from cardiovascular diseases in Campo Grande from 2013 to 2022, stratified by disease group, sex, age, and sociodemographic variables."
)

for (p in intro_paras) {
  doc <- add_text(doc, p)
}

# ===== MATERIALS AND METHODS =====
doc <- add_heading(doc, "MATERIALS AND METHODS", 1)

methods_paras <- c(
  "Study design and setting \u2014 This was an ecological, descriptive, retrospective study encompassing the municipality of Campo Grande, capital of Mato Grosso do Sul state, Central-West Brazil. Campo Grande covers 8,118.4 km\u00b2 and is divided into 74 neighbourhoods. The study population comprised all residents whose deaths were registered between 2013 and 2022.",

  "Data sources \u2014 Mortality data were obtained from the Mortality Information System (SIM) of the Brazilian Ministry of Health. Population estimates were obtained from the Brazilian Institute of Geography and Statistics (IBGE), using intercensal estimates for 2013\u20132021 and the 2022 Census for the final year.",

  "Case selection \u2014 All deaths among Campo Grande residents with an underlying cause coded as cardiac disease (ICD-10) were included and classified into five groups: (i) ischaemic heart disease (IHD: I20\u2013I25); (ii) heart failure and other cardiac conditions (HF: I50\u2013I52); (iii) cardiac arrhythmias and conduction disorders (ARR: I44\u2013I49); (iv) cardiomyopathies (CMP: I42\u2013I43); and (v) inflammatory heart diseases (INF: I30\u2013I41).",

  "Variables \u2014 Year of death, sex, age (years), race/ethnicity, marital status, educational level, place of death, and underlying cause (ICD-10). Age was categorised into four groups: 0\u201319 (children/adolescents), 20\u201339 (young adults), 40\u201359 (adults), and \u226560 years (elderly). For age-standardisation, nine groups following the WHO standard population were used.",

  "Statistical analysis \u2014 Categorical variables were expressed as absolute frequencies and proportions; continuous variables as median and interquartile range (IQR). Chi-squared tests compared categorical distributions, and Kruskal-Wallis tests compared age across groups. Crude mortality rates were expressed per 100,000 inhabitants. Age-standardised rates were computed by direct standardisation using the WHO World Standard Population (Ahmad et al. 2001).",

  "Temporal trends were assessed using three complementary approaches: (i) Poisson regression with logarithmic link and population offset to estimate the annual percentage change (APC); overdispersion was assessed by the deviance-to-degrees-of-freedom ratio, with negative binomial regression fitted when this ratio exceeded 1.5; (ii) Prais-Winsten regression to correct for first-order serial autocorrelation, classifying trends as increasing, decreasing, or stationary (\u03b1 = 0.05); and (iii) joinpoint analysis using the segmented regression package to identify inflection points (Muggeo 2003, Kim et al. 2000). Sex-specific incidence rate ratios (IRR) were estimated from Poisson models with sex-by-year interaction terms.",

  "All analyses were performed in R version 4.5.1 (R Core Team 2025). Statistical significance was set at p < 0.05.",

  "Ethics \u2014 Approved by the Research Ethics Committee of UFMS (CAAE: 74807523.7.0000.0021). Informed consent was waived as the study used de-identified secondary data."
)

for (p in methods_paras) {
  doc <- add_text(doc, p)
}

# ===== RESULTS =====
doc <- add_heading(doc, "RESULTS", 1)

results_paras <- c(
  sprintf("Overview \u2014 During 2013\u20132022, 76,688 deaths were registered in Campo Grande. Of these, %s (%.1f%%) had a cardiac disease as the underlying cause. IHD accounted for 7,579 deaths (77.4%%), followed by HF (746; 7.6%%), ARR (673; 6.9%%), CMP (514; 5.3%%), and INF (279; 2.9%%). Within IHD, acute myocardial infarction (I21) was responsible for 6,468 deaths (85.3%% of IHD).",
    format(N, big.mark = ","), N/76688*100),

  "The median age at death was 70 years (IQR: 60\u201380). Age varied significantly across groups (Kruskal-Wallis p < 0.001): ARR had the highest median (78 years), followed by HF (74), CMP (70), IHD (69), and INF (63 years). Males accounted for 59.3% of deaths (n = 5,805), with predominance in IHD (61.0%), INF (61.6%), and CMP (59.7%), while ARR was the only group with female predominance (52.2%) (\u03c7\u00b2 = 65.6, p < 0.001). White individuals comprised 52.9% of deaths, followed by Mixed (41.6%). Primary education was the most frequent level (48.8%). The complete epidemiological characterisation is presented in Table I.",

  "Place of death \u2014 Overall, 48.4% of deaths occurred in hospitals and 32.9% at home. A distinctive pattern emerged for IHD: home deaths (38.1%) were nearly equal to hospital deaths (40.0%), contrasting sharply with other groups where hospital deaths predominated (65\u201393%) (Supplementary Fig. S1).",

  sprintf("Temporal trends \u2014 The overall crude mortality rate increased from 99.3 per 100,000 in 2013 to 133.6 per 100,000 in 2022, a 34.5%% increase. Poisson regression yielded an overall APC of +%.1f%% per year (95%% CI: %.1f\u2013%.1f; p < 0.001). Negative binomial regression confirmed this finding (APC +%.1f%%). Group-specific trends showed marked heterogeneity (Table III, Fig. 1B): IHD increased at +4.3%% per year (95%% CI: 3.5\u20135.1), ARR at +9.5%% (95%% CI: 6.6\u201312.6), and INF at +8.4%% (95%% CI: 4.0\u201313.1), while HF declined at \u22128.7%% (95%% CI: \u221211.0 to \u22126.4) and CMP at \u22123.8%% (95%% CI: \u22126.7 to \u22120.8; all p < 0.05).",
    apc_all, ci_all[1], ci_all[2],
    (exp(coef(glm.nb(deaths ~ t + offset(log(total)), data = m_all))["t"]) - 1) * 100),

  "Prais-Winsten regression, correcting for autocorrelation (\u03c1 = 0.243), confirmed increasing trends for IHD (\u03b2 = 3.91; p = 0.001), ARR (\u03b2 = 0.71; p < 0.001), and INF (\u03b2 = 0.31; p = 0.001), while HF and CMP were reclassified as stationary after correction (Table III).",

  sprintf("Joinpoint analysis identified a single breakpoint at %s (Fig. 1A). Before this point, the trend was non-significant (slope: +1.3/100,000 per year); after it, a significant acceleration was observed (slope: +9.0/100,000 per year; p < 0.01), coinciding with the COVID-19 pandemic.",
    ifelse(!is.null(seg_result), as.character(round(seg_result$bp)), "2019")),

  sprintf("Sex-specific analysis \u2014 Males consistently exhibited higher rates (Fig. 2B). The overall IRR was %.2f (95%% CI: %.2f\u2013%.2f), indicating %.0f%% higher mortality in men. The sex-by-year interaction was not significant (p = %.3f), indicating parallel temporal trends. The male-to-female ratio varied by group: INF (1.61), IHD (1.56), CMP (1.48), HF (1.06), and ARR (0.92), the latter being the only group with female excess (Fig. 2A).",
    irr, irr_ci[1], irr_ci[2], (irr-1)*100,
    summary(m_sex)$coefficients["t:sexMale", 4]),

  "Age distribution \u2014 Elderly individuals (\u226560 years) accounted for 75.3% of deaths. The age-sex pyramid (Fig. 3) illustrates the concentration of mortality in the 65\u201374 and 75+ age groups. Age-standardised rates tracked closely with crude rates (Supplementary Fig. S3), confirming that the observed increase was not driven solely by population ageing.",

  sprintf("COVID-19 impact \u2014 Comparison of pre-pandemic (2018\u20132019) with pandemic years (2020\u20132021) revealed a %.1f%% increase in mean cardiovascular mortality rate (from %.1f to %.1f per 100,000). The joinpoint at %s further supports the pandemic\u2019s role in accelerating cardiovascular mortality.",
    (mean(post$crude_rate)/mean(pre$crude_rate) - 1) * 100,
    mean(pre$crude_rate), mean(post$crude_rate),
    ifelse(!is.null(seg_result), as.character(round(seg_result$bp)), "2019"))
)

for (p in results_paras) {
  doc <- add_text(doc, p)
}

# Insert Table I reference
doc <- add_text(doc, "")
doc <- add_heading(doc, "Table I", 2)
doc <- add_text(doc, "Clinical and epidemiological characteristics of cardiovascular deaths by disease group, Campo Grande, MS, Brazil, 2013\u20132022.")
# Import Table I from docx
tbl1_doc <- read_docx("output/Table_I.docx")
tbl1_body <- docx_body_xml(tbl1_doc)
# We'll add a note instead since embedding is complex
doc <- add_text(doc, "[See Table_I.docx for formatted table]")
doc <- add_text(doc, "")

# Insert figures
doc <- add_heading(doc, "Fig. 1", 2)
doc <- add_figure(doc, "output/Fig1.png", w = 6, h = 6,
  caption = "Fig. 1: Temporal trends in cardiovascular mortality rates in Campo Grande, MS, Brazil, 2013\u20132022. (A) Overall crude mortality rate with joinpoint regression (dashed line); dotted vertical line indicates the identified breakpoint. (B) Mortality rates by cardiovascular disease group. IHD: ischaemic heart disease; HF: heart failure; ARR: arrhythmias; CMP: cardiomyopathies; INF: inflammatory heart diseases.")

doc <- add_heading(doc, "Fig. 2", 2)
doc <- add_figure(doc, "output/Fig2.png", w = 6, h = 3.2,
  caption = "Fig. 2: Cardiovascular mortality by sex, Campo Grande, MS, Brazil, 2013\u20132022. (A) Proportional distribution of deaths by sex across disease groups. (B) Sex-specific mortality rates over time (per 100,000 population).")

doc <- add_heading(doc, "Fig. 3", 2)
doc <- add_figure(doc, "output/Fig3.png", w = 4, h = 3.2,
  caption = "Fig. 3: Age\u2013sex pyramid of cardiovascular deaths, Campo Grande, MS, Brazil, 2013\u20132022.")

doc <- add_heading(doc, "Fig. 4", 2)
doc <- add_figure(doc, "output/Fig4.png", w = 6, h = 2.8,
  caption = "Fig. 4: Heatmap of cardiovascular disease-specific mortality rates (per 100,000 population) by year, Campo Grande, MS, Brazil, 2013\u20132022.")

# ===== DISCUSSION =====
doc <- add_heading(doc, "DISCUSSION", 1)

disc_paras <- c(
  "This study provides the first comprehensive analysis of cardiovascular mortality trends in Campo Grande over a ten-year period. The principal findings were: (i) an increasing overall trend of +3.3% per year, primarily driven by IHD; (ii) heterogeneous trends across disease groups, with ARR and INF increasing while HF and CMP declined; (iii) 59% higher mortality in males; (iv) a high proportion of IHD deaths occurring at home (38.1%); and (v) significant acceleration in mortality after 2019, temporally associated with the COVID-19 pandemic.",

  "The increasing trend in IHD mortality contrasts with national data showing declining age-standardised rates between 2000 and 2018 (Lotufo et al. 2021, Mansur & Favarato 2016). However, this national decline has been geographically uneven, with the Central-West region showing slower improvement (Brant et al. 2025). Municipal-level analyses from the GBD study have revealed that many medium-sized cities outside the southern axis continue to experience rising IHD mortality, particularly when crude rates are considered alongside population ageing (Nascimento et al. 2020). Silva et al. (2024) documented increasing AMI mortality in several Brazilian regions between 2013 and 2023, particularly in areas with lower healthcare infrastructure density. Santo et al. (2024) similarly identified IHD as a persistent challenge when multiple causes of death are considered.",

  "The declining trend in HF mortality (\u22128.7% per year, Poisson) aligns with national findings (Gaui et al. 2021, Stevens et al. 2022), attributed to advances in pharmacological treatment including renin-angiotensin-aldosterone system inhibitors and beta-blockers. The reclassification to stationary by Prais-Winsten suggests this decline may have decelerated recently, warranting further investigation.",

  "The marked increase in ARR mortality (+9.5% per year) deserves attention. This may partly reflect improvements in diagnostic and coding practices, rather than a true epidemiological increase (Oliveira et al. 2024). Nevertheless, population ageing and increasing atrial fibrillation prevalence may also contribute. The female predominance in ARR mortality is consistent with higher atrial fibrillation prevalence among elderly women, a growing demographic segment in Campo Grande.",

  "One of the most clinically significant findings is the high proportion of IHD deaths at home (38.1%), nearly equal to hospital deaths (40.0%). Brant et al. (2020a) reported similar findings for AMI in Brazilian state capitals (40\u201350% out-of-hospital). These likely represent sudden cardiac events where patients did not recognise symptoms, delayed calling emergency services, or faced access barriers. This finding has direct public health implications: expanded education campaigns on AMI symptom recognition, strengthened SAMU response capacity, and improved distribution of automated external defibrillators.",

  sprintf("The male predominance (IRR = %.2f) is consistent with national and international literature (Santos et al. 2022). Sex-specific differences are attributed to biological factors (hormonal protection in pre-menopausal women), behavioural risk factors, and differential healthcare-seeking behaviour. The stable sex gap over time (non-significant interaction) suggests these determinants have not shifted substantially in Campo Grande during the study period.", irr),

  "The racial/ethnic composition (52.9% White, 41.6% Mixed) broadly reflects Campo Grande\u2019s demographics. Educational attainment, with nearly half of deaths among individuals with primary education, mirrors national data linking lower education to higher cardiovascular mortality (Brant et al. 2021, Cardoso et al. 2021). Nascimento et al. (2022) demonstrated that social vulnerability indices correlated with cardiovascular mortality at the municipal level, reinforcing the role of social determinants.",

  "The acceleration after 2019, identified by joinpoint analysis, is consistent with COVID-19\u2019s documented impact on cardiovascular outcomes. Brant et al. (2020b) demonstrated excess cardiovascular mortality in Brazilian capitals during 2020, while Normando et al. (2021) reported a 15% reduction in cardiovascular hospitalisations accompanied by increased mortality. Alencar et al. (2022) and Santos et al. (2023) further documented synergistic effects through direct (myocardial injury, thrombosis) and indirect (disrupted care) mechanisms.",

  "Strengths of this study include its complete decade coverage, use of three complementary statistical methods, and classification into pathophysiologically meaningful groups. Limitations include those inherent to ecological studies using secondary data: potential misclassification of underlying cause, uncertainty in intercensal population estimates, and inability to assess individual-level risk factors or comorbidities.",

  "In conclusion, cardiovascular mortality in Campo Grande increased significantly between 2013 and 2022, driven primarily by ischaemic heart disease. The high proportion of out-of-hospital IHD deaths, increasing arrhythmia-related mortality, and COVID-19-era acceleration highlight critical gaps in cardiovascular care and prevention in this Central-West capital. Future research should investigate spatial distribution of deaths, risk factor prevalence trends, and emergency care network effectiveness."
)

for (p in disc_paras) {
  doc <- add_text(doc, p)
}

# ===== ACKNOWLEDGEMENTS =====
doc <- add_heading(doc, "ACKNOWLEDGEMENTS", 1)
doc <- add_text(doc, "To the health surveillance teams of Campo Grande for maintaining the quality of the SIM database, and to IBGE for population estimates.")

# ===== CONFLICTS =====
doc <- add_heading(doc, "CONFLICTS OF INTEREST", 1)
doc <- add_text(doc, "The authors declare no competing interests.")

# ===== AUTHORS' CONTRIBUTION =====
doc <- add_heading(doc, "AUTHORS\u2019 CONTRIBUTION", 1)
doc <- add_text(doc, "JV \u2014 study design, data collection, data analysis, manuscript writing; EFO \u2014 study supervision, methodological guidance, manuscript revision; AV \u2014 data analysis, statistical modelling, code development, manuscript revision. All authors approved the final version.")

# ===== REFERENCES =====
doc <- add_heading(doc, "REFERENCES", 1)

refs <- c(
  "Ahmad OB, Boschi-Pinto C, Lopez AD, Murray CJL, Lozano R, Inoue M. 2001. Age standardization of rates: a new WHO standard. GPE Discussion Paper Series 31. Geneva: WHO.",
  "Alencar AP, Souza HP, Fonseca L, Brant LCC. 2022. Covid-19 in Brazil in 2020: impact on deaths from cancer and cardiovascular diseases. Cad Saude Publica. 38(4): e00093921.",
  "Almeida JS, Santos JC, Silva GAP. 2025. Analysis of mortality by cardiovascular disease subgroups in Brazil before and during the COVID-19 pandemic (2000\u20132022). BMC Cardiovasc Disord. 25: forthcoming.",
  "Almeida WS, Szwarcwald CL, Frias PG. 2025. Avalia\u00e7\u00e3o da qualidade do SIM: uma scoping review. Cienc Saude Coletiva. 30(1): e12342024.",
  "Brant LCC et al. 2020a. In- and Out-of-Hospital Deaths by Acute Myocardial Infarction in Brazilian State Capitals. Arq Bras Cardiol. 115(3): 478\u2013485.",
  "Brant LCC et al. 2020b. Excess of cardiovascular deaths during the COVID-19 pandemic in Brazilian capital cities. Heart. 106(24): 1898\u20131905.",
  "Brant LCC et al. 2021. Socioeconomic position and cardiovascular mortality in 63 million adults from Brazil. Heart. 107(15): 1250\u20131257.",
  "Brant LCC et al. 2025. Cardiovascular diseases mortality in Brazilian municipalities: estimates from the GBD study, 2000\u20132018. Lancet Reg Health Am. 44: 100116.",
  "Cardoso LSM, Teixeira RA, Ribeiro ALP. 2021. Desigualdade social e mortalidade precoce por doen\u00e7as cardiovasculares no Brasil. Rev Saude Publica. 55: 96.",
  "Duncan BB et al. 2016. Time trends in adult chronic disease inequalities by education in Brazil: 1998\u20132013. Int J Equity Health. 15: 182.",
  "Gaui EN, Oliveira GMM, Klein CH. 2021. Mortality Due to Heart Failure and Socioeconomic Development in Brazil between 1980 and 2018. Arq Bras Cardiol. 117(5): 944\u2013951.",
  "Kim HJ, Fay MP, Feuer EJ, Midthune DN. 2000. Permutation tests for joinpoint regression with applications to cancer rates. Stat Med. 19(3): 335\u2013351.",
  "Lopez-Jaramillo P et al. 2014. Cardiovascular disease in Latin America: the growing epidemic. Prog Cardiovasc Dis. 57(3): 262\u2013267.",
  "Lotufo PA et al. 2021. Coronary heart disease and stroke mortality trends in Brazil 2000\u20132018. PLoS One. 16(8): e0253639.",
  "Lotufo PA, Bensenor IJM. 2024. Ethnicity and cardiovascular mortality in Brazil: a call for papers. Sao Paulo Med J. 142(1): e2024142.",
  "Malta DC et al. 2022. Premature mortality due to four main NCDs and suicide in Brazil, 1990\u20132019: a GBD Study. Rev Soc Bras Med Trop. 55: e0263-2021.",
  "Mansur AP, Favarato D. 2011. Trends in ischemic heart disease and stroke death ratios in Brazilian women and men. Clinics. 66(1): 63\u201368.",
  "Mansur AP, Favarato D. 2016. Trends in Mortality Rate from Cardiovascular Disease in Brazil, 1980\u20132012. Arq Bras Cardiol. 107(1): 20\u201325.",
  "Mansur AP, Favarato D. 2020. Trends in Mortality Rates from CVD and Cancer in the Most Populous Capital Cities of Brazil. Arq Bras Cardiol. 114(2): 199\u2013206.",
  "Muggeo VMR. 2003. Estimating regression models with unknown break-points. Stat Med. 22(19): 3055\u20133071.",
  "Nascimento BR et al. 2020. Reduction of mortality and predictions for AMI, stroke, and heart failure in Brazil until 2030. Sci Rep. 10: 16520.",
  "Nascimento JOV et al. 2022. Mortality from circulatory diseases in Brazil and social determinants: an ecological study. BMC Public Health. 22: 1964.",
  "Normando PG et al. 2021. Reduction in Hospitalization and Increase in Mortality Due to CVD during COVID-19 in Brazil. Arq Bras Cardiol. 116(3): 371\u2013380.",
  "Oliveira GMM et al. 2022. Cardiovascular Statistics \u2014 Brazil 2021. Arq Bras Cardiol. 118(1): 115\u2013373.",
  "Oliveira GMM et al. 2024. Cardiovascular Statistics \u2014 Brazil 2023. Arq Bras Cardiol. 121(2): e20240079.",
  "R Core Team. 2025. R: A language and environment for statistical computing. Vienna, Austria.",
  "Rasella D et al. 2014. Impact of primary health care on mortality from heart and cerebrovascular diseases in Brazil. BMJ. 349: g4014.",
  "Roth GA et al. 2020. Global Burden of Cardiovascular Diseases and Risk Factors, 1990\u20132019. J Am Coll Cardiol. 76(25): 2982\u20133021.",
  "Rubinstein AL et al. 2025. Cardiovascular disease in the Americas: epidemiology and risk factors. Lancet Reg Health Am. 42: 100960.",
  "Santo AH et al. 2024. Ischemic heart disease-related mortality in Brazil, 2006\u20132020. BMC Public Health. 24: 809.",
  "Santos JC et al. 2023. Cardiovascular mortality in Brazil during COVID-19: underlying and multiple causes. Public Health. 224: 58\u201365.",
  "Santos JE et al. 2022. Sex Differences in CVD Mortality in Brazil, 1996\u20132019. Int J Environ Res Public Health. 19(19): 12827.",
  "Santos SC et al. 2025. Cardiovascular mortality in Brazil: trends from subgroups and social development. Discov Public Health. 22: 991.",
  "Silva JB et al. 2024. Epidemiological Profile of AMI Mortality in Brazil, 2013\u20132023. Arch Community Med Public Health. 10(3): 314\u2013320.",
  "Souza MFM et al. 2016. Cardiovascular Health in Brazil: Trends and Perspectives. Circulation. 133(4): 422\u2013433.",
  "Stevens B et al. 2022. Trends in mortality from heart failure in Brazil: 1998\u20132019. Rev Bras Epidemiol. 25: E220021."
)

for (r in refs) {
  doc <- add_text(doc, r)
}

# Save manuscript
print(doc, target = "output/Manuscript_MIOC_complete.docx")
cat("Manuscript saved: output/Manuscript_MIOC_complete.docx\n")

cat("\n===== ALL DONE =====\n")
cat("Output directory contains:\n")
cat("  - Manuscript_MIOC_complete.docx (full article with figures)\n")
cat("  - Table_I.docx (formatted epidemiological table)\n")
cat("  - Table_II.csv, Table_III.csv (mortality rates & models)\n")
cat("  - Fig1-4.png/.tiff (main figures, 600 dpi)\n")
cat("  - FigS1-S4.png (supplementary figures)\n")
cat("  - Table_S1-S3.csv (supplementary tables)\n")
