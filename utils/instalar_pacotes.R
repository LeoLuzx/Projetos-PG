# FUNÇÃO PARA INSTALAR PACOTES COM VERIFICAÇÕES

# Instala e carrega vários pacotes se não estiverem já instalados.
instalar_pacotes <- function(pkg) {
  
  # ----- VALIDAÇÃO DE pkg -----
  
  # 'NULL' deve ser verificado antes da verificação do comprimento, pois NULL possui comprimento 
  # zero e, portanto, seria confundido facilmente com um vetor vazio caso fosse verificado apenas 
  # pela comparação 'length(pkg) == 0'.
  if (is.null(pkg)) {
    stop("pkg não pode ser NULL")
  }
  
  # O vetor de pacotes não pode ser vazio.
  if (length(pkg) == 0) {
    stop("pkg não pode ser um vetor vazio")
  }
  
  # Ser 'character' não garante que seja um vetor, pois matrizes e outras estruturas também podem
  # conter elementos do tipo 'character'. Por isso, verificamos aqui a dimensão do objeto para
  # garantir que ele seja um vetor (deve ter dimensão 'NULL').
  if (!is.null(dim(pkg))) {
    stop("pkg deve ser um vetor de caracteres")
  }
  
  # O vetor de pacotes deve ser um vetor de caracteres.
  if (!is.character(pkg)) {
    stop("pkg deve ser um vetor de caracteres")
  }
  
  # O vetor não deve ter valores inválidos. A função 'anyNA()' detecta NA's, NaN's.
  if (anyNA(pkg)) {
    stop("pkg possui valores inválidos")
  }
  
  # O vetor não deve conter strings vazias. A função 'trimws()' foi usada porque podemos ter
  # situações de presença de espaços entre as aspas como: "", " ", "     " etc.
  if (any(trimws(pkg) == "")) {
    stop("pkg não pode conter strings vazias")
  }
  
  # 'grepl()' verifica, para cada elemento de 'pkg', se o texto corresponde à dada expressão regular
  # '^[[:alnum:].]+$'. Nessa expressão, '^' indica o início da string, '$' indica o final da string,
  # '[[:alnum:].]' permite apenas caracteres alfanuméricos ou '.', e '+' exige pelo menos um desses
  # caracteres. Portanto, a expressão verifica se cada nome é composto exclusivamente por letras,
  # números ou pontos. O operador '!' inverte o resultado, identificando os elementos que não 
  # atendem ao padrão, enquanto 'any()' verifica se existe pelo menos um elemento inválido.
  if (any(!grepl("^[[:alnum:].]+$", pkg))) {
    stop("pkg contém nomes de pacotes inválidos")
  }
  
  # Verifica se os pacotes já estão instalados. Se não, instala-os. A função'requireNamespace' 
  # verifica se o namespace está disponível. Uso de 'quietly = TRUE' para menos retornos no console.
  if (!requireNamespace(pkg, quietly = TRUE)){
    
    # Instalação dos pacotes
    # Uso de 'dependecies = TRUE' para que não reste dependência solta.
    install.packages(pkg, dependencies = TRUE)
  }
  
  # Após a instalação dos pacotes, carrega-os. Pois se serão instaldos, é para serem usados
  # Usando 'supressPackageStartupMessages' para não poluir o console.
  suppressPackageStartupMessages({
    
    # Uso de 'character.only = TRUE' para que o R não interprete 'pkg' como nome de um pacote
    # Uso de 'quietly = TRUE' para menos retorno no console.
    library(pkg, character.only = TRUE, quietly = TRUE)
  })
  
}

# OBS: Não foi implementada a opção por atualizar os pacotes automaticamente, pois as atualizações
# podem promover aletrações sensíveis que quebram o fluxo do projeto. Assim, qualquer pacote que se
# deseje atualizar deverá ser atualizado manualmente quando houver necessidade.
