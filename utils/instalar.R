
# FUNÇÃO PARA INSTALAR PACOTES COM VERIFICAÇÕES

# Instala e carrega vários pacotes se não estiverem já instalados
instalar <- function(pkg) {
  
  # Verifica se os pacotes já estão instalados. Se não, instala-os.
  # A função'requireNamespace' verifica se o namespace está disponível
  # Uso de 'quietly = TRUE' para menos retornos no console
  if (!requireNamespace(pkg, quietly = TRUE)){
    
    # Instalação dos pacotes
    # Uso de 'dependecies = TRUE' para que não reste dependência solta
    install.packages(pkg, dependencies = TRUE)
  }
  
  # Após a instalação dos pacotes, carrega-os. Pois se serão instaldos, é para serem usados
  # Usando 'supressPackageStartupMessages' para não poluir o console
  suppressPackageStartupMessages({
    
    # Uso de 'character.only = TRUE' para que o R não interprete 'pkg' como nome de um pacote
    # Uso de 'quietly = TRUE' para menos retorno no console
    library(pkg, character.only = TRUE, quietly = TRUE)
  })
  
}

# OBS: Não foi implementada a opção por atualizar os pacotes automaticamente, pois as atualizações
# podem promover aletrações sensíveis que quebram o fluxo do projeto. Assim, qualquer pacote que se
# deseje atualizar deverá ser atualizado manualmente quando houver necessidade.
