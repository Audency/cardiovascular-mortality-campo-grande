##############################################################################
# ANÁLISE EPIDEMIOLÓGICA - MORTALIDADE POR DOENÇAS CARDIOVASCULARES
# Campo Grande, Mato Grosso do Sul, Brasil (2013-2022)
#
# Estudo ecológico descritivo e retrospectivo
# Fonte: Sistema de Informações sobre Mortalidade (SIM)
# Revista: Memórias do Instituto Oswaldo Cruz
##############################################################################

# ==========================================================================
# 0. PACOTES E CONFIGURAÇÕES
# ==========================================================================

pacotes <- c("tidyverse", "janitor", "gtsummary", "flextable", "scales",
             "broom", "MASS", "patchwork", "RColorBrewer",
             "knitr", "kableExtra", "segmented", "prais")

for (pkg in pacotes) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Resolver conflito MASS::select vs dplyr::select
select <- dplyr::select

# Tema limpo para publicação — sem grades, sem linhas desnecessárias
theme_pub <- theme_classic(base_size = 11, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.4),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9, colour = "black")
  )

theme_set(theme_pub)

# Diretório de trabalho
setwd("/Users/lshva8/Desktop/AUDENCIO/Producao artigos/Artigo Doencas cardiovasculares ")

# Criar pastas
dir.create("figuras", showWarnings = FALSE)
dir.create("tabelas", showWarnings = FALSE)

# ==========================================================================
# 1. IMPORTAÇÃO E LIMPEZA DOS DADOS
# ==========================================================================

df <- read.csv("dados_cardio.csv", sep = ";", stringsAsFactors = FALSE,
               na.strings = c("", "NA", " "))

cat("Dimensões originais:", dim(df), "\n")

df_analise <- df %>%
  select(
    ano_obito, SEXO, idade_anos, RACACOR, ESTCIV, ESC_NIVEL,
    LOCOCOR, CID3, GRUPO_CARDIO, CAUSABAS, DTOBITO
  ) %>%
  mutate(
    faixa_etaria = case_when(
      idade_anos >= 0  & idade_anos <= 19 ~ "0-19",
      idade_anos >= 20 & idade_anos <= 39 ~ "20-39",
      idade_anos >= 40 & idade_anos <= 59 ~ "40-59",
      idade_anos >= 60                    ~ "≥60",
      TRUE ~ NA_character_
    ),
    faixa_etaria = factor(faixa_etaria,
                          levels = c("0-19", "20-39", "40-59", "≥60")),
    faixa_det = case_when(
      idade_anos >= 0  & idade_anos <= 4  ~ "0-4",
      idade_anos >= 5  & idade_anos <= 14 ~ "5-14",
      idade_anos >= 15 & idade_anos <= 24 ~ "15-24",
      idade_anos >= 25 & idade_anos <= 34 ~ "25-34",
      idade_anos >= 35 & idade_anos <= 44 ~ "35-44",
      idade_anos >= 45 & idade_anos <= 54 ~ "45-54",
      idade_anos >= 55 & idade_anos <= 64 ~ "55-64",
      idade_anos >= 65 & idade_anos <= 74 ~ "65-74",
      idade_anos >= 75                    ~ "75+",
      TRUE ~ NA_character_
    ),
    SEXO = case_when(
      SEXO == "M" ~ "Masculino",
      SEXO == "F" ~ "Feminino",
      TRUE ~ "Indeterminado"
    ),
    ESC_NIVEL = factor(ESC_NIVEL, levels = c(
      "Não alfabetizado", "Ensino fundamental",
      "Ensino médio", "Ensino superior ou mais"
    )),
    grupo_curto = case_when(
      GRUPO_CARDIO == "Doenças isquêmicas do coração" ~ "IHD",
      GRUPO_CARDIO == "Insuficiência cardíaca e outras condições cardíacas" ~ "HF",
      GRUPO_CARDIO == "Distúrbios do ritmo e condução" ~ "ARR",
      GRUPO_CARDIO == "Miocardiopatias" ~ "CMP",
      GRUPO_CARDIO == "Doenças inflamatórias (pericárdio / endocárdio / miocárdio)" ~ "INF",
      TRUE ~ GRUPO_CARDIO
    ),
    grupo_curto = factor(grupo_curto, levels = c("IHD", "HF", "ARR", "CMP", "INF")),
    grupo_label = case_when(
      grupo_curto == "IHD" ~ "Ischaemic heart disease",
      grupo_curto == "HF"  ~ "Heart failure",
      grupo_curto == "ARR" ~ "Arrhythmias",
      grupo_curto == "CMP" ~ "Cardiomyopathies",
      grupo_curto == "INF" ~ "Inflammatory",
      TRUE ~ as.character(grupo_curto)
    ),
    grupo_label = factor(grupo_label, levels = c(
      "Ischaemic heart disease", "Heart failure", "Arrhythmias",
      "Cardiomyopathies", "Inflammatory"
    ))
  )

# Excluir sexo indeterminado (n=1)
df_analise <- df_analise %>% filter(SEXO != "Indeterminado")
cat("Registros para análise:", nrow(df_analise), "\n")

# --- Populações estimadas de Campo Grande (IBGE/SIDRA) ---
pop_cg <- tibble(
  ano_obito = 2013:2022,
  pop_total = c(832352, 843120, 853622, 863982, 874210, 885711,
                906092, 916001, 906395, 898100),
  pop_masc  = c(405477, 410725, 415839, 420884, 425873, 431478,
                441394, 446218, 441393, 437108),
  pop_fem   = c(426875, 432395, 437783, 443098, 448337, 454233,
                464698, 469783, 465002, 460992)
)

# População padrão da OMS (Segi-Doll modificada) para padronização direta
pop_padrao_oms <- tibble(
  faixa_det = c("0-4", "5-14", "15-24", "25-34", "35-44",
                "45-54", "55-64", "65-74", "75+"),
  peso_oms = c(8860, 17020, 17020, 13580, 13580,
               12800, 10440, 7160, 3540) / 100000
)

# Proporções populacionais por faixa (Censo 2010, Campo Grande)
prop_faixa_cg <- tibble(
  faixa_det = c("0-4", "5-14", "15-24", "25-34", "35-44",
                "45-54", "55-64", "65-74", "75+"),
  prop_pop = c(0.070, 0.155, 0.180, 0.165, 0.145,
               0.115, 0.085, 0.050, 0.035)
)

# ==========================================================================
# 2. ANÁLISE DESCRITIVA
# ==========================================================================

cat("\n========== TABELA I: Características por grupo ==========\n\n")

# Tabela 1 com gtsummary (estilo MIOC: algarismos romanos)
tab1_gts <- df_analise %>%
  filter(!is.na(GRUPO_CARDIO)) %>%
  select(grupo_label, SEXO, faixa_etaria, RACACOR, ESC_NIVEL,
         ESTCIV, LOCOCOR, idade_anos) %>%
  tbl_summary(
    by = grupo_label,
    label = list(
      SEXO ~ "Sex",
      faixa_etaria ~ "Age group (years)",
      RACACOR ~ "Race/Ethnicity",
      ESC_NIVEL ~ "Education level",
      ESTCIV ~ "Marital status",
      LOCOCOR ~ "Place of death",
      idade_anos ~ "Age (years)"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",
    digits = list(all_categorical() ~ c(0, 1),
                  all_continuous() ~ c(0, 0))
  ) %>%
  add_overall() %>%
  add_p(test = list(all_categorical() ~ "chisq.test",
                    all_continuous() ~ "kruskal.test")) %>%
  bold_p() %>%
  modify_header(label = "**Variable**")

tab1_flex <- as_flex_table(tab1_gts)
save_as_docx(tab1_flex, path = "tabelas/Table_I.docx")
cat("Table I salva.\n")

# --- Tabela II: Mortalidade bruta e ajustada por idade (OMS) ---

# Mortalidade bruta
mort_bruta <- df_analise %>%
  count(ano_obito, grupo_curto, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  mutate(taxa_bruta = (obitos / pop_total) * 100000)

# Mortalidade bruta total
mort_bruta_total <- df_analise %>%
  count(ano_obito, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  mutate(taxa_bruta = (obitos / pop_total) * 100000)

# Mortalidade ajustada por idade — padronização direta (pop OMS)
mort_ajust <- df_analise %>%
  filter(!is.na(faixa_det)) %>%
  count(ano_obito, grupo_curto, faixa_det, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  left_join(prop_faixa_cg, by = "faixa_det") %>%
  left_join(pop_padrao_oms, by = "faixa_det") %>%
  mutate(
    pop_faixa = pop_total * prop_pop,
    taxa_esp = (obitos / pop_faixa) * 100000,
    contrib = taxa_esp * peso_oms
  ) %>%
  group_by(ano_obito, grupo_curto) %>%
  summarise(taxa_ajustada = sum(contrib, na.rm = TRUE), .groups = "drop")

# Mortalidade ajustada total
mort_ajust_total <- df_analise %>%
  filter(!is.na(faixa_det)) %>%
  count(ano_obito, faixa_det, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  left_join(prop_faixa_cg, by = "faixa_det") %>%
  left_join(pop_padrao_oms, by = "faixa_det") %>%
  mutate(
    pop_faixa = pop_total * prop_pop,
    taxa_esp = (obitos / pop_faixa) * 100000,
    contrib = taxa_esp * peso_oms
  ) %>%
  group_by(ano_obito) %>%
  summarise(taxa_ajustada = sum(contrib, na.rm = TRUE), .groups = "drop")

# Mortalidade por sexo
mort_sexo <- df_analise %>%
  count(ano_obito, SEXO, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  mutate(
    pop = ifelse(SEXO == "Masculino", pop_masc, pop_fem),
    taxa = (obitos / pop) * 100000
  )

# Tabela II completa
tab2 <- mort_bruta %>%
  select(ano_obito, grupo_curto, obitos, taxa_bruta) %>%
  left_join(mort_ajust, by = c("ano_obito", "grupo_curto")) %>%
  mutate(across(c(taxa_bruta, taxa_ajustada), ~round(., 1)))

write.csv(tab2, "tabelas/Table_II_mortality_rates.csv", row.names = FALSE)
cat("Table II salva.\n")

# ==========================================================================
# 3. MODELAGEM ESTATÍSTICA
# ==========================================================================

cat("\n========== MODELAGEM ==========\n\n")

# --- 3.1 Regressão de Poisson para tendência temporal ---

dados_modelo <- df_analise %>%
  count(ano_obito, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  mutate(ano_c = ano_obito - 2013,
         taxa = (obitos / pop_total) * 100000)

mod_poisson <- glm(obitos ~ ano_c + offset(log(pop_total)),
                   family = poisson(link = "log"), data = dados_modelo)

cat("Modelo Poisson Geral:\n")
summary(mod_poisson)
cat("APC (%):", round((exp(coef(mod_poisson)["ano_c"]) - 1) * 100, 2), "\n")

# Teste de superdispersão
disp <- mod_poisson$deviance / mod_poisson$df.residual
cat("Dispersão:", round(disp, 2), "\n")

if (disp > 1.5) {
  mod_nb <- glm.nb(obitos ~ ano_c + offset(log(pop_total)), data = dados_modelo)
  cat("Binomial Negativa - APC (%):",
      round((exp(coef(mod_nb)["ano_c"]) - 1) * 100, 2), "\n")
}

# Modelos por grupo
cat("\n--- APC por grupo (Poisson) ---\n")
dados_grupo <- df_analise %>%
  count(ano_obito, grupo_curto, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  mutate(ano_c = ano_obito - 2013)

res_apc <- dados_grupo %>%
  group_by(grupo_curto) %>%
  group_modify(~ {
    mod <- glm(obitos ~ ano_c + offset(log(pop_total)),
               family = poisson(link = "log"), data = .x)
    ci <- confint(mod)
    tibble(
      APC = (exp(coef(mod)["ano_c"]) - 1) * 100,
      APC_lower = (exp(ci["ano_c", 1]) - 1) * 100,
      APC_upper = (exp(ci["ano_c", 2]) - 1) * 100,
      p_value = summary(mod)$coefficients["ano_c", 4]
    )
  }) %>% ungroup()

print(res_apc %>% mutate(across(c(APC, APC_lower, APC_upper), ~round(., 2))))

# --- 3.2 Regressão de Prais-Winsten ---
cat("\n--- Prais-Winsten (correção de autocorrelação) ---\n")

# Total
# Prais-Winsten via cochrane-orcutt manual (alternativa ao pacote prais)
prais_winsten_manual <- function(y, x) {
  mod <- lm(y ~ x)
  res <- residuals(mod)
  n <- length(res)
  rho <- sum(res[-1] * res[-n]) / sum(res^2)

  y_star <- y[-1] - rho * y[-n]
  x_star <- x[-1] - rho * x[-n]
  mod_gls <- lm(y_star ~ x_star)

  s <- summary(mod_gls)
  list(
    coef = s$coefficients["x_star", "Estimate"],
    se = s$coefficients["x_star", "Std. Error"],
    p_value = s$coefficients["x_star", "Pr(>|t|)"],
    rho = rho
  )
}

pw_total <- prais_winsten_manual(dados_modelo$taxa, dados_modelo$ano_c)
cat("Prais-Winsten Geral:\n")
cat(sprintf("  β = %.3f (SE = %.3f), p = %.4f, ρ = %.3f\n",
            pw_total$coef, pw_total$se, pw_total$p_value, pw_total$rho))

# Por grupo
cat("\n--- Prais-Winsten por grupo ---\n")
res_pw <- dados_grupo %>%
  mutate(taxa = (obitos / pop_total) * 100000) %>%
  group_by(grupo_curto) %>%
  group_modify(~ {
    pw <- tryCatch(
      prais_winsten_manual(.x$taxa, .x$ano_c),
      error = function(e) list(coef = NA, se = NA, p_value = NA, rho = NA)
    )
    tibble(
      coef = pw$coef,
      se = pw$se,
      p_value = pw$p_value,
      trend = ifelse(is.na(pw$p_value), "Error",
                     ifelse(pw$p_value < 0.05,
                            ifelse(pw$coef > 0, "Increasing", "Decreasing"),
                            "Stationary"))
    )
  }) %>% ungroup()

print(res_pw)

# --- 3.3 Análise segmentada (joinpoint com segmented) ---
cat("\n--- Joinpoint (segmented) ---\n")

mod_lm <- lm(taxa ~ ano_obito, data = dados_modelo)
tryCatch({
  seg_mod <- segmented(mod_lm, seg.Z = ~ano_obito, npsi = 1)
  cat("Joinpoint detectado em:", round(seg_mod$psi[1, 2], 1), "\n")
  summary(seg_mod)

  # Salvar breakpoint
  bp <- round(seg_mod$psi[1, 2], 1)
  cat("Breakpoint:", bp, "\n")
  cat("Slopes:\n")
  print(slope(seg_mod))
}, error = function(e) {
  cat("Joinpoint não significativo (sem ponto de inflexão detectado)\n")
  cat("Mensagem:", e$message, "\n")
})

# --- 3.4 Modelo por sexo ---
cat("\n--- IRR por sexo ---\n")
dados_sexo <- df_analise %>%
  count(ano_obito, SEXO, name = "obitos") %>%
  left_join(pop_cg, by = "ano_obito") %>%
  mutate(
    pop = ifelse(SEXO == "Masculino", pop_masc, pop_fem),
    ano_c = ano_obito - 2013,
    SEXO = factor(SEXO, levels = c("Feminino", "Masculino"))
  )

mod_sexo <- glm(obitos ~ ano_c * SEXO + offset(log(pop)),
                family = poisson(link = "log"), data = dados_sexo)

irr <- exp(coef(mod_sexo)["SEXOMasculino"])
irr_ci <- exp(confint(mod_sexo)["SEXOMasculino", ])
cat(sprintf("IRR Male vs Female = %.2f (95%% CI: %.2f-%.2f)\n",
            irr, irr_ci[1], irr_ci[2]))

# --- 3.5 COVID-19 impact ---
cat("\n--- COVID-19 impact ---\n")
pre <- dados_modelo %>% filter(ano_obito %in% 2018:2019)
dur <- dados_modelo %>% filter(ano_obito %in% 2020:2021)
cat("Pre-COVID (2018-2019) mean rate:",
    round(mean(pre$taxa), 1), "/100,000\n")
cat("COVID (2020-2021) mean rate:",
    round(mean(dur$taxa), 1), "/100,000\n")
cat("Change:", round((mean(dur$taxa) / mean(pre$taxa) - 1) * 100, 1), "%\n")

# Tabela III: Resultados dos modelos
tab3 <- bind_rows(
  res_apc %>%
    mutate(Model = "Poisson",
           Result = sprintf("APC: %.2f%% (%.2f to %.2f)", APC, APC_lower, APC_upper)) %>%
    select(Group = grupo_curto, Model, Result, p_value),
  res_pw %>%
    mutate(Model = "Prais-Winsten",
           Result = sprintf("β: %.3f (SE: %.3f), %s", coef, se, trend)) %>%
    select(Group = grupo_curto, Model, Result, p_value)
)

write.csv(tab3, "tabelas/Table_III_models.csv", row.names = FALSE)
cat("Table III salva.\n")

# ==========================================================================
# 4. FIGURAS PARA PUBLICAÇÃO (limpas, sem grades)
# ==========================================================================

cat("\n========== GERANDO FIGURAS ==========\n\n")

# Paleta
cores_grupo <- c("IHD" = "#D62728", "HF" = "#1F77B4", "ARR" = "#2CA02C",
                 "CMP" = "#9467BD", "INF" = "#FF7F0E")
cores_sexo <- c("Masculino" = "#1F77B4", "Feminino" = "#D62728")
formas_grupo <- c("IHD" = 16, "HF" = 17, "ARR" = 15, "CMP" = 18, "INF" = 8)

# --- Fig. 1: Série temporal da taxa de mortalidade ---

fig1a <- mort_bruta_total %>%
  ggplot(aes(x = ano_obito, y = taxa_bruta)) +
  geom_line(linewidth = 0.7, colour = "black") +
  geom_point(size = 2.5, colour = "black", shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              colour = "grey50", linewidth = 0.5) +
  scale_x_continuous(breaks = 2013:2022) +
  scale_y_continuous(limits = c(90, 140)) +
  labs(tag = "A", x = "Year",
       y = "Mortality rate\n(per 100,000 pop.)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig1b <- mort_bruta %>%
  ggplot(aes(x = ano_obito, y = taxa_bruta,
             colour = grupo_curto, shape = grupo_curto)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = cores_grupo,
                      labels = c("IHD", "HF", "ARR", "CMP", "INF")) +
  scale_shape_manual(values = formas_grupo,
                     labels = c("IHD", "HF", "ARR", "CMP", "INF")) +
  scale_x_continuous(breaks = 2013:2022) +
  labs(tag = "B", x = "Year",
       y = "Mortality rate\n(per 100,000 pop.)",
       colour = "Group", shape = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig1 <- fig1a / fig1b + plot_layout(heights = c(1, 1.2))

ggsave("figuras/Fig1_temporal_trend.png", fig1,
       width = 8, height = 8, dpi = 300, bg = "white")
ggsave("figuras/Fig1_temporal_trend.tiff", fig1,
       width = 8, height = 8, dpi = 300, bg = "white", compression = "lzw")
cat("Fig. 1 salva.\n")

# --- Fig. 2: Mortalidade por sexo ---

fig2a <- df_analise %>%
  count(grupo_label, SEXO) %>%
  group_by(grupo_label) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = grupo_label, y = pct, fill = SEXO)) +
  geom_col(position = position_dodge(0.8), width = 0.7, colour = "black",
           linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.1f", pct)),
            position = position_dodge(0.8), vjust = -0.5, size = 2.8) +
  scale_fill_manual(values = cores_sexo,
                    labels = c("Male", "Female")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(tag = "A", x = NULL, y = "Proportion (%)", fill = "Sex") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 8))

fig2b <- mort_sexo %>%
  ggplot(aes(x = ano_obito, y = taxa, colour = SEXO, shape = SEXO)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.2) +
  scale_colour_manual(values = cores_sexo,
                      labels = c("Male", "Female")) +
  scale_shape_manual(values = c("Masculino" = 16, "Feminino" = 17),
                     labels = c("Male", "Female")) +
  scale_x_continuous(breaks = 2013:2022) +
  labs(tag = "B", x = "Year",
       y = "Mortality rate\n(per 100,000 pop.)",
       colour = "Sex", shape = "Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig2 <- fig2a + fig2b + plot_layout(widths = c(1, 1.1))
ggsave("figuras/Fig2_sex_distribution.png", fig2,
       width = 12, height = 5, dpi = 300, bg = "white")
ggsave("figuras/Fig2_sex_distribution.tiff", fig2,
       width = 12, height = 5, dpi = 300, bg = "white", compression = "lzw")
cat("Fig. 2 salva.\n")

# --- Fig. 3: Pirâmide etária ---

piramide_data <- df_analise %>%
  filter(!is.na(faixa_det), SEXO %in% c("Masculino", "Feminino")) %>%
  count(faixa_det, SEXO) %>%
  mutate(
    n = ifelse(SEXO == "Masculino", -n, n),
    faixa_det = factor(faixa_det,
                       levels = c("0-4", "5-14", "15-24", "25-34", "35-44",
                                  "45-54", "55-64", "65-74", "75+"))
  )

fig3 <- ggplot(piramide_data, aes(x = faixa_det, y = n, fill = SEXO)) +
  geom_col(width = 0.75, colour = "black", linewidth = 0.2) +
  coord_flip() +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ","),
                     breaks = seq(-2500, 2500, 500)) +
  scale_fill_manual(values = cores_sexo,
                    labels = c("Male", "Female")) +
  labs(x = "Age group (years)", y = "Number of deaths",
       fill = "Sex") +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.3)

ggsave("figuras/Fig3_age_pyramid.png", fig3,
       width = 7, height = 5, dpi = 300, bg = "white")
ggsave("figuras/Fig3_age_pyramid.tiff", fig3,
       width = 7, height = 5, dpi = 300, bg = "white", compression = "lzw")
cat("Fig. 3 salva.\n")

# --- Fig. 4: Heatmap taxas por ano e grupo ---

fig4 <- mort_bruta %>%
  ggplot(aes(x = factor(ano_obito), y = grupo_curto, fill = taxa_bruta)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f", taxa_bruta)),
            size = 3, colour = "black") +
  scale_fill_gradient(low = "#FFF5EB", high = "#C62828",
                      name = "Rate\n(/100,000)") +
  scale_y_discrete(labels = c("IHD" = "Ischaemic HD",
                               "HF" = "Heart failure",
                               "ARR" = "Arrhythmias",
                               "CMP" = "Cardiomyopathies",
                               "INF" = "Inflammatory")) +
  labs(x = "Year", y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

ggsave("figuras/Fig4_heatmap.png", fig4,
       width = 9, height = 4, dpi = 300, bg = "white")
ggsave("figuras/Fig4_heatmap.tiff", fig4,
       width = 9, height = 4, dpi = 300, bg = "white", compression = "lzw")
cat("Fig. 4 salva.\n")

# --- Figuras suplementares ---

# Fig. S1: Local do óbito por grupo
figS1 <- df_analise %>%
  count(grupo_label, LOCOCOR) %>%
  group_by(grupo_label) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = grupo_label, y = pct, fill = LOCOCOR)) +
  geom_col(width = 0.7, colour = "black", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2", name = "Place of death") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Proportion (%)") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 8))

ggsave("figuras/FigS1_place_of_death.png", figS1,
       width = 8, height = 5, dpi = 300, bg = "white")
cat("Fig. S1 salva.\n")

# Fig. S2: Distribuição etária por grupo (density)
figS2 <- df_analise %>%
  filter(!is.na(idade_anos)) %>%
  ggplot(aes(x = idade_anos, colour = grupo_curto)) +
  geom_density(linewidth = 0.6) +
  scale_colour_manual(values = cores_grupo,
                      labels = c("IHD", "HF", "ARR", "CMP", "INF")) +
  labs(x = "Age (years)", y = "Density", colour = "Group") +
  scale_x_continuous(breaks = seq(0, 100, 20))

ggsave("figuras/FigS2_age_density.png", figS2,
       width = 7, height = 4, dpi = 300, bg = "white")
cat("Fig. S2 salva.\n")

# Fig. S3: Taxa ajustada vs bruta
figS3 <- mort_bruta_total %>%
  left_join(mort_ajust_total, by = "ano_obito") %>%
  pivot_longer(cols = c(taxa_bruta, taxa_ajustada),
               names_to = "type", values_to = "rate") %>%
  mutate(type = ifelse(type == "taxa_bruta", "Crude", "Age-standardised")) %>%
  ggplot(aes(x = ano_obito, y = rate, colour = type, linetype = type)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("Crude" = "black",
                                  "Age-standardised" = "#D62728")) +
  scale_linetype_manual(values = c("Crude" = "solid",
                                    "Age-standardised" = "dashed")) +
  scale_x_continuous(breaks = 2013:2022) +
  labs(x = "Year", y = "Mortality rate (per 100,000 pop.)",
       colour = "Rate type", linetype = "Rate type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figuras/FigS3_crude_vs_adjusted.png", figS3,
       width = 7, height = 4, dpi = 300, bg = "white")
cat("Fig. S3 salva.\n")

# ==========================================================================
# 5. MATERIAL SUPLEMENTAR
# ==========================================================================

# Tabela S1: CID detalhado
tab_s1 <- df_analise %>%
  count(GRUPO_CARDIO, CID3, sort = TRUE) %>%
  group_by(GRUPO_CARDIO) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

write.csv(tab_s1, "tabelas/Table_S1_ICD_detail.csv", row.names = FALSE)

# Tabela S2: Mortalidade por faixa detalhada
tab_s2 <- df_analise %>%
  filter(!is.na(faixa_det)) %>%
  count(ano_obito, faixa_det, grupo_curto) %>%
  left_join(pop_cg, by = "ano_obito") %>%
  left_join(prop_faixa_cg, by = "faixa_det") %>%
  mutate(taxa = round((n / (pop_total * prop_pop)) * 100000, 2))

write.csv(tab_s2, "tabelas/Table_S2_age_specific_rates.csv", row.names = FALSE)

# Tabela S4: Completude
tab_s4 <- df_analise %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_pct") %>%
  filter(Missing_pct > 0) %>%
  arrange(desc(Missing_pct)) %>%
  mutate(Missing_pct = round(Missing_pct, 2))

write.csv(tab_s4, "tabelas/Table_S3_completeness.csv", row.names = FALSE)

cat("\n========== ANÁLISE CONCLUÍDA ==========\n")
cat("Figuras: figuras/\n")
cat("Tabelas: tabelas/\n")
