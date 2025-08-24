# ===================================================================
# PROJETO PÓS-GRADUAÇÃO - PARTE 2: SEGMENTAÇÃO DE CLIENTES
#
# OBJETIVO: Agrupar clientes de uma loja online em segmentos distintos
#           com base no seu comportamento de compra.
# TÉCNICA: Análise RFM (Recência, Frequência, Valor Monetário)
# ALGORITMOS: K-Médias (K-Means) e Clusterização Hierárquica
# ===================================================================


# --- 1. CONFIGURAÇÃO DO AMBIENTE ---

# Instalando pacotes (se necessário)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("cluster")
# install.packages("factoextra")

# Carregando as bibliotecas necessárias
library(readxl)     # Para ler arquivos Excel (.xlsx)
library(dplyr)      # Ferramenta principal para manipulação de dados
library(lubridate)  # Para facilitar o trabalho com datas
library(cluster)    # Contém os algoritmos de cluster
library(factoextra) # Funções para visualizar e avaliar clusters


# --- 2. CARREGAMENTO E PREPARAÇÃO DOS DADOS (ENGENHARIA DE FEATURES RFM) ---

# URL do dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"

# O código abaixo verifica se o arquivo já existe. Se não, faz o download.
if (!file.exists("Online_Retail.xlsx")) {
  download.file(url, destfile = "Online_Retail.xlsx", mode = "wb")
}

# 'dados_varejo' contém todas as transações brutas
dados_varejo <- read_excel("Online_Retail.xlsx")

# Limpeza e transformação inicial dos dados
dados_varejo <- dados_varejo %>% 
  filter(!startsWith(InvoiceNo, "C")) %>%      # Remove transações canceladas
  filter(!is.na(CustomerID)) %>%              # Remove linhas sem identificação do cliente
  filter(Quantity > 0, UnitPrice > 0)         # Remove dados inconsistentes

# Criando a variável 'PrecoTotal'
dados_varejo$PrecoTotal <- dados_varejo$Quantity * dados_varejo$UnitPrice

# 'data_referencia' é a data base para calcular a recência
data_referencia <- max(as.Date(dados_varejo$InvoiceDate)) + 1

# Criando a tabela de clientes com as métricas RFM
dados_clientes <- dados_varejo %>%
  group_by(CustomerID) %>% # Agrupa todas as transações por cliente
  summarise(
    Recencia = as.numeric(data_referencia - max(as.Date(InvoiceDate))),
    Frequencia = n_distinct(InvoiceNo),
    ValorMonetario = sum(PrecoTotal)
  ) %>%
  ungroup() %>%
  select(-CustomerID) # Removemos o ID para não influenciar o cluster

cat("\nBase de clientes pronta com", nrow(dados_clientes), "clientes.\n")


# --- 3. NORMALIZAÇÃO DOS DADOS ---

# Normalizando os dados com a função scale() para que tenham média 0 e desvio padrão 1.
# É um passo crucial para que variáveis de grande magnitude (ValorMonetario) não dominem o algoritmo.
dados_clientes_normalizados <- scale(dados_clientes)


# --- 4. ANÁLISE COM K-MÉDIAS ---

# 4.1. Determinação do Número Ótimo de Clusters (k)
set.seed(123)
fviz_nbclust(dados_clientes_normalizados, kmeans, method = "wss", main = "Método do Cotovelo (Elbow Method)")
fviz_nbclust(dados_clientes_normalizados, kmeans, method = "silhouette", main = "Método da Silhueta")
k_otimo <- 5 # Decisão baseada no pico do gráfico da Silhueta

# 4.2. Execução final do K-Médias
set.seed(123)
kmeans_final <- kmeans(dados_clientes_normalizados, centers = k_otimo, nstart = 25)

# 4.3. Visualização dos Clusters K-Médias
fviz_cluster(kmeans_final, data = dados_clientes_normalizados,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
) +
labs(title = paste("Clusters de Clientes (k =", k_otimo, ")"))

# 4.4. Interpretação dos Perfis de Cluster K-Médias
dados_clientes_com_cluster <- dados_clientes %>% mutate(Cluster = kmeans_final$cluster)
perfil_clusters_kmeans <- dados_clientes_com_cluster %>%
  group_by(Cluster) %>%
  summarise_all("mean")

cat("\nPerfil médio dos clusters (K-Médias):\n")
print(perfil_clusters_kmeans %>% arrange(desc(ValorMonetario)))


# --- 5. ANÁLISE COM CLUSTERIZAÇÃO HIERÁRQUICA ---

# 5.1. Cálculo da Matriz de Distâncias
# O algoritmo hierárquico precisa de uma matriz de distância como entrada.
matriz_distancias <- dist(dados_clientes_normalizados, method = "euclidean")

# 5.2. Execução do Algoritmo Hierárquico
# O método "ward.D2" busca minimizar a variância dentro dos clusters em cada etapa de fusão.
cluster_hierarquico <- hclust(matriz_distancias, method = "ward.D2")

# 5.3. Visualização do Dendrograma
plot(cluster_hierarquico,
     main = "Dendrograma da Clusterização Hierárquica",
     xlab = "Clientes",
     ylab = "Distância",
     labels = FALSE, # Remove os rótulos para não poluir o gráfico
     hang = -1
)
# Adicionamos uma linha de corte para sugerir um número de clusters (k=4)
abline(h = 35, col = "red", lty = 2)


# --- 6. COMPARAÇÃO FINAL E CONCLUSÕES ---

# Esta seção final serve como um resumo para o relatório do projeto.
#
# **Comparativo entre K-Médias e Clusterização Hierárquica**
#
# 1. ABORDAGEM:
#    - K-Médias (Particional): Divide os dados em um número 'k' de clusters pré-definido.
#      É mais direto e computacionalmente mais leve.
#    - Hierárquico (Aglomerativo): Constrói uma árvore de clusters aninhados, sem definir 'k'
#      previamente. Permite visualizar a estrutura de agrupamento em diferentes níveis.
#
# 2. RESULTADOS NO PROJETO:
#    - O K-Médias, apoiado pelo método da Silhueta, sugeriu k=5 como a melhor solução.
#      Isso permite uma segmentação mais granular, potencialmente útil para o negócio.
#    - O Hierárquico, através da análise do dendrograma, sugeriu k=4 como a estrutura
#      mais natural, baseada nas maiores distâncias entre os grupos.
#    - A concordância em um número de clusters próximo (4 ou 5) fortalece a conclusão
#      de que existem segmentos de clientes bem definidos nos dados.
#
# 3. PRÓS E CONTRAS:
#    - K-Médias:
#      - Pró: Rápido, escalável para grandes datasets.
#      - Contra: Requer o número 'k' como parâmetro de entrada.
#    - Hierárquico:
#      - Pró: Não requer 'k' inicial, o dendrograma é muito informativo.
#      - Contra: Computacionalmente caro e lento para grandes datasets.
#
# CONCLUSÃO GERAL:
# Ambos os métodos foram eficazes. O K-Médias forneceu uma segmentação prática e
# acionável, enquanto o Hierárquico validou a existência de uma forte estrutura
# de grupos e ofereceu uma visão alternativa da sua organização. Para este problema,
# as duas abordagens se mostraram complementares.
#