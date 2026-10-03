# VALIDAÇÃO DOS PARÂMETROS RECEBIDOS PELAS FUNÇÕES

# Verifica se um número passado para a função é um valor escalar numérico, sem dimensões e sem 
# valores inválidos. As restrições matemáticas devem ser avaliadas caso a caso fora desta função.
validar_valor <- function(param) {
  
  # Obtém automaticamente o nome da função que chamou 'validar_valor_numeric'.
  nome_funcao <- as.character(sys.call(sys.parent())[[1]])
  
  # Obtém automaticamente o nome utilizado para o parâmetro passado à função.
  nome_parametro <- deparse(substitute(param))
  
  # O valor do parâmetro não pode ser 'NULL'.
  if (is.null(param))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode ser NULL.")
  
  # Não pode possuir dimensões, pois deve ser um valor escalar e não uma matriz, array etc.
  if (!is.null(dim(param)))
    stop(nome_parametro, " na função '", nome_funcao, "' deve ser um valor escalar.")
  
  # Deve ser um valor único, não podendo aceitar um vetor (nem mesmo vazio).
  if (length(param) != 1)
    stop(nome_parametro, " na função '", nome_funcao, "' deve ser um valor único.")
  
  # Não pode ser um valor 'NaN'.
  if (is.nan(param))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode ser NaN.")
  
  # Não pode ser um valor 'NA'.
  if (is.na(param))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode ser NA.")
  
  # Deve ser um valor numérico (double ou integer).
  if (!is.numeric(param))
    stop(nome_parametro, " na função '", nome_funcao, "' deve ser um valor numérico.")
  
  # Não pode ser 'Inf' ou '-Inf'.
  if (is.infinite(param))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode ser Inf ou -Inf.")
}
