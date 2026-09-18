####################################################################################################

# ARQUIVO PARA CARREGAMENTO DAS DEPENDÊNCIAS

####################################################################################################

# LIMPEZA DO CONSOLE ANTES DE EXECUTAR

# Este comando limpa o console (semelhante a CTRL+L)
cat("\014")

#---------------------------------------------------------------------------------------------------
# CARREGAMENTO DAS IMAGENS

# Imagens na pasta "imagens" na raiz do projeto proj.Rproj
load("imagens/sf.Rdata")
load("imagens/fo.Rdata")
load("imagens/is.Rdata")

# RENOMEANDO AS IMAGENS PARA NOMES CURTOS

# Renomeando as imagens: San Francisco (sf), Foulum (fo), Imagem simulada (is) etc.
sf <- ImgMP_Sim
fo <- FoulumIIpart2
is <- X

# Limpeza dos arquivos desnecessários
rm(ImgMP_Sim)
rm(FoulumIIpart2)
rm(X)

#---------------------------------------------------------------------------------------------------
# CARREGAMENTO DE OUTROS ARQUIVOS NECESSÁRIOS

# Arquivo de funções
source("proxys/funcoes.R")

#---------------------------------------------------------------------------------------------------
# CARREGAMENTO DOS PACOTES

# Lista de pacotes
pacotes <- c("rsample","maxLik", "caret", "dplyr", "MASS", "grid", "rlang", "caTools", "foreach")

# Verificar instalação e instalar se não estiver instalado
lapply(pacotes, instalar)

# Removendo variáveis desnecessárias
rm(pacotes)
rm(instalar)

#---------------------------------------------------------------------------------------------------
# LIMPEZA DO CONSOLE NO FINAL

# Limpeza do console
cat("\014")

# Mensagem de limpeza para que ninguém fique perdido sem saber o que aconteceu
cat("\n-------------------------------\n")
cat("Console limpo e tudo carregado!")
cat("\n-------------------------------\n")
