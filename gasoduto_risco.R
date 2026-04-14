# ==============================================================================
# MODELO DE RISCO - CUSTO DE PROJETO DE GASODUTO
# ==============================================================================
# Descrição : Simulação Monte Carlo para estimativa do custo total de
#             construção de um gasoduto, considerando:
#               - Incerteza na rota (preferida vs. alternativa)
#               - Incerteza nos itens de custo (distribuições triangulares)
# Estrutura : Custos organizados em três categorias:
#               (1) Material  (2) Mão de obra  (3) Serviços
# ==============================================================================


# ==============================================================================
# 0. PACOTES NECESSÁRIOS
# ==============================================================================
library(ggplot2)   # visualizações
library(scales)    # formatação de eixos (percent, dollar)


# ==============================================================================
# 1. CONSTANTES DO PROBLEMA  —  origem: especificação do projeto
#    (NÃO misturar com variáveis de simulação; toda alteração do escopo
#     deve ocorrer APENAS nesta seção)
# ==============================================================================

# --- Geometria das rotas ---
ROTA_PREFERIDA_KM    <- 260    # Extensão da rota preferida     (km)
ROTA_ALTERNATIVA_KM  <- 290    # Extensão da rota alternativa   (km)

# --- Incerteza sobre a probabilidade de usar a rota alternativa ---
PROB_ROTA_ALT_MIN    <- 0.35   # Limite inferior da probabilidade
PROB_ROTA_ALT_MAX    <- 0.45   # Limite superior da probabilidade

# --- Geometria da tubulação ---
COMPRIMENTO_SECAO_M  <- 8      # Comprimento de cada seção de tubo (m)

# ------------------------------------------------------------------------------
# Parâmetros das distribuições triangulares: vetor c(min, moda, max)
# ------------------------------------------------------------------------------

# (1) MATERIAL
TUB    <- c(min =  725,    moda =  740,    max =  790)   # Custo da tubulação      ($/seção 8m)

# (2) MÃO DE OBRA
VALA   <- c(min =   12,    moda =   16,    max =   25)   # Tempo para cavar vala   (hh/seção 8m)
MO     <- c(min =   17,    moda =   18.5,  max =   23)   # Custo de mão de obra    ($/hora)
SOLD   <- c(min =    4,    moda =    4.5,  max =    5)   # Tempo de soldagem       (horas/junção)

# (3) SERVIÇOS
TRANS  <- c(min =    6.1,  moda =    6.6,  max =    7.4) # Transporte da tubulação ($/seção 8m)
FILT   <- c(min = 165000,  moda = 173000,  max = 188000) # Sistema de filtragem    ($, custo fixo)
ACAB   <- c(min =  14000,  moda =  15000,  max =  17000) # Acabamento              ($/km)


# ==============================================================================
# 2. PARÂMETROS DE SIMULAÇÃO
# ==============================================================================
N_SIM   <- 100000   # Número de iterações Monte Carlo
SEMENTE <- 42       # Semente aleatória para reprodutibilidade


# ==============================================================================
# 3. FUNÇÕES AUXILIARES
# ==============================================================================

#' Amostrador da distribuição triangular (método da inversão da CDF)
#'
#' @param n     Número de amostras desejadas
#' @param param Vetor nomeado c(min, moda, max)
#' @return      Vetor numérico de comprimento n
rtriangular <- function(n, param) {
  a  <- param["min"]
  b  <- param["moda"]
  cc <- param["max"]
  u  <- runif(n)
  Fc <- (b - a) / (cc - a)          # probabilidade acumulada na moda
  ifelse(
    u < Fc,
    a  + sqrt(u       * (cc - a) * (b  - a)),   # ramo crescente
    cc - sqrt((1 - u) * (cc - a) * (cc - b))    # ramo decrescente
  )
}


# ==============================================================================
# 4. SIMULAÇÃO MONTE CARLO
# ==============================================================================
set.seed(SEMENTE)

# --- 4.1  Rota utilizada (variável discreta com probabilidade incerta) --------
#
#  A probabilidade de usar a rota alternativa é ela mesma incerta:
#  modelada como Uniforme(PROB_ROTA_ALT_MIN, PROB_ROTA_ALT_MAX).
#  Para cada iteração, primeiro sorteamos a probabilidade e depois
#  decidimos qual rota é usada — capturando tanto a incerteza epistêmica
#  (não sabemos a probabilidade exata) quanto a aleatória (evento em si).

p_rota_alt <- runif(N_SIM, PROB_ROTA_ALT_MIN, PROB_ROTA_ALT_MAX)
usa_alt    <- runif(N_SIM) < p_rota_alt
rota_km    <- ifelse(usa_alt, ROTA_ALTERNATIVA_KM, ROTA_PREFERIDA_KM)

# --- 4.2  Geometria derivada da rota -----------------------------------------
n_secoes  <- rota_km * 1000 / COMPRIMENTO_SECAO_M   # número de seções de 8 m
n_juncoes <- n_secoes - 1                            # junções soldadas

# --- 4.3  Amostragem dos itens de custo --------------------------------------

# (1) Material
c_tubulacao  <- rtriangular(N_SIM, TUB)    # $/seção

# (2) Mão de obra  —  custo/hora é compartilhado entre vala e soldagem
c_mo         <- rtriangular(N_SIM, MO)     # $/hora
t_vala       <- rtriangular(N_SIM, VALA)   # hh/seção
t_soldagem   <- rtriangular(N_SIM, SOLD)   # horas/junção

# (3) Serviços
c_transporte <- rtriangular(N_SIM, TRANS)  # $/seção
c_filtragem  <- rtriangular(N_SIM, FILT)   # $ (fixo, independente da rota)
c_acabamento <- rtriangular(N_SIM, ACAB)   # $/km


# --- 4.4  Custo por categoria -------------------------------------------------

# (1) MATERIAL
custo_material  <- n_secoes * c_tubulacao

# (2) MÃO DE OBRA
custo_vala      <- n_secoes  * t_vala     * c_mo
custo_solda     <- n_juncoes * t_soldagem * c_mo
custo_mao_obra  <- custo_vala + custo_solda

# (3) SERVIÇOS
custo_transp_total <- n_secoes * c_transporte
custo_servicos     <- custo_transp_total + c_filtragem + rota_km * c_acabamento

# CUSTO TOTAL
custo_total <- custo_material + custo_mao_obra + custo_servicos


# ==============================================================================
# 5. RESULTADOS
# ==============================================================================

# Função auxiliar de formatação
fmt_usd <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")

# --- 5.1  Estatísticas do custo total ----------------------------------------
cat("\n")
cat("================================================================\n")
cat("   ESTATÍSTICAS DO CUSTO TOTAL  (USD)\n")
cat("================================================================\n")
stats_total <- c(
  "Mínimo"        = min(custo_total),
  "P10"           = quantile(custo_total, 0.10),
  "P25"           = quantile(custo_total, 0.25),
  "Média"         = mean(custo_total),
  "Mediana (P50)" = median(custo_total),
  "P75"           = quantile(custo_total, 0.75),
  "P80"           = quantile(custo_total, 0.80),
  "P90"           = quantile(custo_total, 0.90),
  "Máximo"        = max(custo_total),
  "Desvio Padrão" = sd(custo_total)
)
for (nm in names(stats_total)) {
  cat(sprintf("   %-15s  $ %s\n", nm, fmt_usd(stats_total[nm])))
}
cat("================================================================\n\n")

# --- 5.2  Contribuição por categoria -----------------------------------------
cat("================================================================\n")
cat("   CUSTO MÉDIO POR CATEGORIA  (USD)\n")
cat("================================================================\n")
medias <- c(
  "(1) Material"   = mean(custo_material),
  "(2) Mão de obra"= mean(custo_mao_obra),
  "(3) Serviços"   = mean(custo_servicos)
)
total_medio <- sum(medias)
for (nm in names(medias)) {
  pct <- 100 * medias[nm] / total_medio
  cat(sprintf("   %-18s  $ %s  (%5.1f%%)\n", nm, fmt_usd(medias[nm]), pct))
}
cat(sprintf("   %-18s  $ %s\n", "TOTAL", fmt_usd(total_medio)))
cat("================================================================\n\n")

# --- 5.3  Questão 2: perspectiva do PROPONENTE -------------------------------
#
#  Prática usual em risk management:
#    - Preço proposto = P80  (80% de chance de não ter prejuízo)
#    - Contingência   = P80 − Valor Esperado

preco_proposto <- quantile(custo_total, 0.80)
valor_esperado <- mean(custo_total)
contingencia   <- preco_proposto - valor_esperado

cat("================================================================\n")
cat("   PERSPECTIVA DO PROPONENTE\n")
cat("================================================================\n")
cat(sprintf("   Valor esperado (média):  $ %s\n",   fmt_usd(valor_esperado)))
cat(sprintf("   Preço proposto  (P80):   $ %s\n",   fmt_usd(preco_proposto)))
cat(sprintf("   Contingência (P80 − E):  $ %s\n",   fmt_usd(contingencia)))
cat(sprintf("   Contingência / Média:       %.1f%%\n",
            100 * contingencia / valor_esperado))
cat("================================================================\n\n")

# --- 5.4  Questão 3: perspectiva do CONTRATANTE ------------------------------
#
#  Como contratante, aceitar uma proposta a preço fixo de $45 M significa
#  assumir o risco de que o custo real supere esse valor.

proposta <- 45e6
prob_excede  <- mean(custo_total > proposta)
prob_cobre   <- 1 - prob_excede

cat("================================================================\n")
cat("   PERSPECTIVA DO CONTRATANTE  —  proposta: USD $45,000,000\n")
cat("================================================================\n")
cat(sprintf("   P(custo ≤ $45 M):  %5.1f%%\n", 100 * prob_cobre))
cat(sprintf("   P(custo >  $45 M): %5.1f%%\n", 100 * prob_excede))
cat(sprintf("   $45 M corresponde ao percentil %.0f do custo\n",
            100 * ecdf(custo_total)(proposta)))

if (prob_excede > 0.20) {
  cat("   Recomendação: REJEITAR — probabilidade de prejuízo elevada (>20%)\n")
} else if (prob_excede > 0.10) {
  cat("   Recomendação: NEGOCIAR — risco moderado; buscar ajuste de preço\n")
} else {
  cat("   Recomendação: ACEITAR — risco de prejuízo aceitável (<10%)\n")
}
cat("================================================================\n\n")


# ==============================================================================
# 6. VISUALIZAÇÕES
# ==============================================================================

custo_M <- custo_total / 1e6   # custo em USD milhões (facilita leitura dos eixos)

PASTA <- "C:/Users/DavidCubric/Documents/lista02Risco"

# --- 6.1  PDF: histograma + curva de densidade --------------------------------
p1 <- ggplot(data.frame(c = custo_M), aes(x = c)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80,
                 fill = "steelblue", color = "white", alpha = 0.75) +
  geom_density(color = "navy", linewidth = 1) +
  geom_vline(xintercept = mean(custo_M),
             color = "firebrick", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = quantile(custo_M, 0.80),
             color = "darkorange", linetype = "dashed", linewidth = 1) +
  annotate("text",
           x = mean(custo_M) + 0.4, y = Inf,
           label = sprintf("Média\n$%.1f M", mean(custo_M)),
           color = "firebrick", vjust = 2, hjust = 0, size = 3.5) +
  annotate("text",
           x = quantile(custo_M, 0.80) + 0.4, y = Inf,
           label = sprintf("P80\n$%.1f M", quantile(custo_M, 0.80)),
           color = "darkorange", vjust = 2, hjust = 0, size = 3.5) +
  labs(
    title    = "Função de Probabilidade do Custo Total",
    subtitle = sprintf("Gasoduto — Monte Carlo (%s iterações)", format(N_SIM, big.mark = ".")),
    x = "Custo Total (USD Milhões)",
    y = "Densidade de Probabilidade"
  ) +
  theme_minimal(base_size = 13)
ggsave(file.path(PASTA, "grafico1_PDF.png"), plot = p1, width = 10, height = 6, dpi = 150)

# --- 6.2  CDF acumulada -------------------------------------------------------
p2 <- ggplot(data.frame(c = custo_M), aes(x = c)) +
  stat_ecdf(color = "steelblue", linewidth = 1.2) +
  geom_vline(xintercept = 45,
             color = "firebrick", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = quantile(custo_M, 0.80),
             color = "darkorange", linetype = "dotted", linewidth = 1) +
  geom_hline(yintercept = 0.80,
             color = "darkorange", linetype = "dotted", linewidth = 0.8) +
  annotate("text",
           x = 45.3, y = 0.25,
           label = "Proposta\n$45 M", color = "firebrick",
           hjust = 0, size = 3.5) +
  annotate("text",
           x = min(custo_M) + 0.2, y = 0.82,
           label = "P80", color = "darkorange", hjust = 0, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Função de Distribuição Acumulada (CDF) do Custo Total",
    subtitle = sprintf("Gasoduto — Monte Carlo (%s iterações)", format(N_SIM, big.mark = ".")),
    x = "Custo Total (USD Milhões)",
    y = "Probabilidade Acumulada"
  ) +
  theme_minimal(base_size = 13)
ggsave(file.path(PASTA, "grafico2_CDF.png"), plot = p2, width = 10, height = 6, dpi = 150)

# --- 6.3  Boxplot comparativo por categoria ----------------------------------
df_cat <- data.frame(
  Categoria = factor(
    rep(c("(1) Material", "(2) Mão de Obra", "(3) Serviços"), each = N_SIM),
    levels = c("(1) Material", "(2) Mão de Obra", "(3) Serviços")
  ),
  Custo_M = c(custo_material, custo_mao_obra, custo_servicos) / 1e6
)

p3 <- ggplot(df_cat, aes(x = Categoria, y = Custo_M, fill = Categoria)) +
  geom_boxplot(alpha = 0.75, outlier.size = 0.4, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Distribuição de Custos por Categoria",
    subtitle = sprintf("Gasoduto — Monte Carlo (%s iterações)", format(N_SIM, big.mark = ".")),
    x = NULL,
    y = "Custo (USD Milhões)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
ggsave(file.path(PASTA, "grafico3_boxplot.png"), plot = p3, width = 10, height = 6, dpi = 150)

cat("\nGráficos salvos em:\n")
cat(sprintf("  %s/grafico1_PDF.png\n", PASTA))
cat(sprintf("  %s/grafico2_CDF.png\n", PASTA))
cat(sprintf("  %s/grafico3_boxplot.png\n", PASTA))
