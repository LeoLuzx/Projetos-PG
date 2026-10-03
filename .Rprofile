####################################################################################################

# ARQUIVO DE SETUP PARA O PROJETO

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

# Renomeando as imagens: San Francisco (sf), Foulum (fo), Imagem simulada (is) etc.
sf <- ImgMP_Sim
fo <- FoulumIIpart2
is <- X

# Limpeza dos arquivos desnecessários
rm(ImgMP_Sim)
rm(FoulumIIpart2)
rm(X)

#---------------------------------------------------------------------------------------------------
# CARREGAMENTO DOS SCRIPTS

# Define a função 'carregar_scripts' com o argumento 'pasta'
carregar_scripts <- function(pasta) {
  
  # Lista os arquivos presentes na pasta especificada.
  arquivos <- list.files(
    
    # Define a pasta que será consultada.
    path = pasta,
    
    # Mantém apenas os arquivos com extensão '.R'
    pattern = "\\.R$",
    
    # Retorna o caminho completo de cada arquivo.
    full.names = TRUE
  )
  
  # Ordena os arquivos em ordem alfabética.
  arquivos <- sort(arquivos)
  
  # Executa o conteúdo de cada arquivo listado.
  lapply(arquivos, source)
}

# Carregando os scripts das pastas
carregar_scripts("modelos")
carregar_scripts("utils")
carregar_scripts("tests")

# Apagando a função do "Global Environment'
rm(carregar_scripts)

#---------------------------------------------------------------------------------------------------
# CARREGAMENTO DOS PACOTES

# Lista de pacotes
pacotes <- c("rsample","maxLik", "caret", "dplyr", "MASS", "grid", "rlang", "caTools", "foreach")

# Verificar instalação e instalar se não estiver instalado
lapply(pacotes, instalar_pacotes)

# Removendo variáveis desnecessárias
rm(pacotes)
rm(instalar_pacotes)

#---------------------------------------------------------------------------------------------------
# LIMPEZA DO CONSOLE NO FINAL

# Limpeza do console
cat("\014")

# Mensagem de limpeza para que ninguém fique perdido sem saber o que aconteceu
cat("\n-------------------------------\n")
cat("Console limpo e tudo carregado!")
cat("\n-------------------------------\n")
