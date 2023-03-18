library(htmlwidgets)
library(ggplot2)
library(plotly)
library(tidyverse)
library(summarytools)
library(zoo)

data <- read.csv2("base/BaseDPEvolucaoMensalCisp_fixed.csv", sep = ";")

data <- data %>%
    select(
        `mes`,
        `ano`,
        `Regiao`,
        `hom_doloso`,
        `lesao_corp_morte`,
        `latrocinio`,
        `letalidade_violenta`,
        `tentat_hom`,
        `hom_culposo`
    )

data$Regiao <- trimws(data$Regiao)

data$mes_ano <- as.Date(paste(data$ano, data$mes, "01", sep = "-"))

data <- data %>% select(-mes, -ano)

data_gruped_by <- data %>%
    group_by(Regiao, mes_ano) %>%
    summarise(across(c(
        hom_culposo,
        hom_doloso,
        latrocinio,
        lesao_corp_morte,
        letalidade_violenta,
        tentat_hom
    ), sum), .groups = "drop")

data_mean <- data_gruped_by %>%
    select(
        Regiao, -mes_ano,
        hom_culposo,
        hom_doloso,
        latrocinio,
        lesao_corp_morte,
        letalidade_violenta,
        tentat_hom
    ) %>%
    group_by(Regiao) %>%
    summarise(
        mean_hom_culposo = mean(hom_culposo),
        mean_hom_doloso = mean(hom_doloso),
        mean_latrocinio = mean(latrocinio),
        mean_lesao_corp_morte = mean(lesao_corp_morte),
        mean_letalidade_violenta = mean(letalidade_violenta),
        mean_tentat_hom = mean(tentat_hom)
    )

data_sd <- data_gruped_by %>%
    select(-mes_ano) %>%
    group_by(Regiao) %>%
    summarise(
        sd_hom_culposo = sd(hom_culposo),
        sd_hom_doloso = sd(hom_doloso),
        sd_latrocinio = sd(latrocinio),
        sd_lesao_corp_morte = sd(lesao_corp_morte),
        sd_letalidade_violenta = sd(letalidade_violenta),
        sd_tentat_hom = sd(tentat_hom)
    )

data_quantile <- data_gruped_by %>%
    select(-mes_ano) %>%
    group_by(Regiao) %>%
    summarise(
        quantile_25_hom_culposo = quantile(hom_culposo, 0.25),
        quantile_75_hom_culposo = quantile(hom_culposo, 0.75),
        quantile_25_hom_doloso = quantile(hom_doloso, 0.25),
        quantile_75_hom_doloso = quantile(hom_doloso, 0.75),
        quantile_25_latrocinio = quantile(latrocinio, 0.25),
        quantile_75_latrocinio = quantile(latrocinio, 0.75),
        quantile_25_lesao_corp_morte = quantile(lesao_corp_morte, 0.25),
        quantile_75_lesao_corp_morte = quantile(lesao_corp_morte, 0.75),
        quantile_25_letalidade_violenta = quantile(letalidade_violenta, 0.25),
        quantile_75_letalidade_violenta = quantile(letalidade_violenta, 0.75),
        quantile_25_tentat_hom = quantile(tentat_hom, 0.25),
        quantile_75_tentat_hom = quantile(tentat_hom, 0.75)
    )

graph_lines <- ggplot(data_gruped_by, aes(
    x = mes_ano,
    y = hom_culposo,
    group = Regiao,
    color = Regiao
)) +
    geom_line() +
    labs(
        x = "Mês/Ano",
        y = "Número de ocorrências",
        color = "Região",
        title = "Casos de Homicidio Culposo por Região",
    )

interactive_graph <- ggplotly(graph_lines)

htmlwidgets::saveWidget(interactive_graph, "interactive_graph.html")

descr(data_gruped_by[
    c(
        "hom_culposo",
        "hom_doloso",
        "latrocinio",
        "lesao_corp_morte",
        "letalidade_violenta",
        "tentat_hom"
    )
])

cor(data_gruped_by[
    c(
        "hom_culposo",
        "hom_doloso",
        "latrocinio",
        "lesao_corp_morte",
        "letalidade_violenta",
        "tentat_hom"
    )
])

# hist(data_gruped_by$hom_culposo, freq = FALSE, main = "Histograma com curva de suavidade", breaks = "FD")
hist(
    data_gruped_by$hom_culposo,
    main = "Histograma com curva de suavidade",
    breaks = "FD",
    freq = FALSE
)

lines(density(data_gruped_by$hom_culposo))

bins <- ceiling(
    2 * IQR(data_gruped_by$hom_culposo) / (
        length(data_gruped_by$hom_culposo)^(1 / 3)
    )
)

media <- mean(data_gruped_by$hom_culposo)
mediana <- median(data_gruped_by$hom_culposo)

abline(v = media, col = "red", lty = 2)
abline(v = media, col = "blue", lty = 5)

qqnorm(data_gruped_by$hom_culposo, main = "Normal Q-Q Homicidio Culposo")
qqline(data_gruped_by$hom_culposo)

correlacao <- cor(data_gruped_by %>% select(-mes_ano, -Regiao))

dispersao <- ggplot(
    data_gruped_by, aes(x = hom_doloso, y = letalidade_violenta)
)

dispersao + geom_point(size = 3, color = "red") +
    labs(
        x = "Homicidio Doloso",
        y = "Letalidade Violenta",
        title = "Gráfico de dispersão"
    ) + geom_smooth(method = "lm")

plot_ly() %>%
    add_lines(x = data_gruped_by$mes_ano, y = data_gruped_by$hom_culposo, name = "Homicidio Culposo") %>%
    add_lines(x = data_gruped_by$mes_ano, y = data_gruped_by$letalidade_violenta, name = "Letalidade Violenta") %>%
    layout(xaxis = list(title = "Mês-Ano"), yaxis = list(title = "Número de Ocorrências"), title = "Número de casos de Letalidade Violenta e Homicidios Culposos por mês no estado do Rio de Janeiro")
