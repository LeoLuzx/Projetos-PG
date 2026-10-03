# VALIDAÇÃO ESTRUTURAL DE UM VETOR DE VALORES 'numeric' (double e integer)

# Verifica se um vetor passado para a função é um vetor (objeto sem dimensões) não vazio, numérico 
# e sem valores inválidos como 'NULL', 'NaN', 'NA', 'Inf', '-Inf' ou valores nulos e negativos.
validar_vetor <- function(vetor) {
  
  # Obtém automaticamente o nome da função que chamou 'validar_vetor_numeric'. Isso é importante 
  # para saber em que função ocorreu o erro quando estivermos rodando múltiplos modelos em k-fold.
  nome_funcao <- as.character(sys.call(sys.parent())[[1]])
  
  # Obtém automaticamente o nome utilizado para o parâmetro passado à função. Isso é importante 
  # para que o nome do vetor na mensagem de erro seja específico do vetor passado para a função e 
  # não um nome genérico, pois podemos rodar os modelos em amostras diferentes e é importante saber 
  # em qual delas o erro foi detectado.
  nome_parametro <- deparse(substitute(vetor))
  
  # ----- VERIFICAÇÕES DO TIPO DE ESTRUTURA -----
  
  # 'NULL' deve ser verificado antes da verificação do comprimento, pois o próprio objeto 'NULL' em 
  # R possui comprimento zero e, portanto, seria confundido facilmente com um vetor vazio caso 
  # fosse verificado apenas pela comparação 'length(vetor) == 0'.
  if (is.null(vetor))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode ser NULL.")
  
  # Mesmo não sendo 'NULL', vetores vazios 'c()' possuem comprimento zero (não permitido).
  if (length(vetor) == 0)
    stop(nome_parametro, " na função '", nome_funcao, "' deve ser um vetor não vazio.")
  
  # Ser 'numéric' (como verificado mais à frente) não garante que seja um vetor, pois matrizes e 
  # outras estruturas também podem ser do tipo numérico. Por isso, verificamos aqui a dimensão do 
  # objeto para garantir que ele seja um vetor (deve ter dimensão 'NULL').
  if (!is.null(dim(vetor)))
    stop(nome_parametro, " na função '", nome_funcao, "' deve ser um vetor.")
  
  # 'NaN' deve ser verificado antes de 'NA', pois 'is.na(NaN)' também retorna 'TRUE'. Portanto, se 
  # nós verificarmos 'NA' antes de 'NaN', um 'NaN' será identificado como um 'NA' e não poderemos
  # emitir uma mensagem específica para 'NaN'.
  if (any(is.nan(vetor)))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode conter NaN's.")
  
  # 'NA' deve ser tratado antes da verificação de tipo, pois 'NA' sozinho é do tipo 'logical' em R 
  # e é coagido para o tipo predominante quando está num vetor. Portanto, se 'NA' estiver sozinho 
  # no argumento ou num vetor só com valores 'NA', ele será considerado logical por 'is.numeric()' 
  # e será rejeitado como não numérico, em vez de ser identificado especificamente como um 'NA'.
  if (any(is.na(vetor)))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode conter NA's.")
  
  # Deve ser um vetor numérico. Em R, 'is.numeric()' retorna 'TRUE' para objetos dos tipos 'integer' 
  # e 'double', que são os tipos numéricos aceitos nesta função.
  if (!is.numeric(vetor))
    stop(nome_parametro, " na função '", nome_funcao, "' deve ter apenas dados numéricos.")
  
  # Uma vez que garantimos que os dados são numéricos, agora pode ser verificado se eles não são do
  # tipo infinito 'Inf' ou '-Inf', pois esses valores também são reconhecidos como 'numeric'.
  if (any(is.infinite(vetor)))
    stop(nome_parametro, " na função '", nome_funcao, "' não pode conter Inf's ou -Inf's.")
  
  # Se nos dados houver algum valor negativo (essa é um restrição para os dados de intensidade SAR).
  # Os dados reais (intensidades) podem estar armazenados na imagem na forma de números complexos 
  # com zeros na parte imaginária. Portanto, o parâmetro já deve ser passado para a função dos 
  # modelos convertido para 'numeric'.
  if (any(vetor < 0))
    stop(nome_parametro, " na função '", nome_funcao, "' não deve conter valores negativos.")
  
  # Se nos dados houver algum valor nulo (outra restrição de dados SAR).
  if (any(vetor == 0))
    stop(nome_parametro, " na função '", nome_funcao, "' não deve conter zeros.")
}
