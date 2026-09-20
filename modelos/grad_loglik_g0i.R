
# GRADIENTE DA LOG-VEROSSIMILHANÇA DA g0i

#Define o gradiente da log-verossimilhança
grad_loglik_g0i = function(Xobs, alfa, gama, L){
  
  # ----- VALIDAÇÃO DA AMOSTRA -----
  
  # NULL deve ser verificado antes da verificação do comprimento, pois NULL possui comprimento zero 
  # e, portanto, seria confundido facilmente com um vetor vazio caso fosse verificado apenas pela
  # comparação length(Xobs) == 0.
  if (is.null(Xobs))
    stop("Xobs no gradiente da log-verossimilhança da g0i não pode ser NULL.")
  
  # Ser numérico (verificção mais à frente) já garante que não é vazio, pois is.numeric(c()) retorna
  # FALSE. No entanto, essa parte é mantida por questões de clareza semântica e comunicação do tipo 
  # do problema caso ocorra.
  if (length(Xobs) == 0)
    stop("Xobs no gradiente da log-verossimilhança da g0i deve ser um vetor não vazio.")
  
  # Ser numérico não garante que seja um vetor, pois matrizes e outras estruturas de valores reais 
  # também podem ser do tipo numérico. Por isso, verificamos aqui a dimensão do objeto para garantir
  # que ele seja um vetor que deve ter dimensão 'NULL'.
  if (!is.null(dim(Xobs)))
    stop("Xobs no gradiente da log-verossimilhança da g0i deve ser um vetor.")
  
  # 'NaN' deve ser tratado antes de 'NA', pois is.na(NaN) também retorna TRUE. Portanto, se nós
  # verificarmos 'NA' antes de 'NaN', um 'NaN' será identificado como um 'NA' e não poderemos
  # emitir uma mensagem específica para 'NaN'.
  if (any(is.nan(Xobs)))
    stop("Xobs no gradiente da log-verossimilhança da g0i não pode conter NaN's.")
  
  # 'NA' deve ser tratado antes da verificação de tipo, pois 'NA' sozinho é do tipo 'logical' em R 
  # e é coagido para o tipo predominante quando está num vetor. Portanto, se 'NA' estiver sozinho no 
  # argumento ou num vetor só com um valor como em c(NA) sem ser num vetor com outros valores para 
  # coerção, ele será considerado logical por is.numeric() e será rejeitado como não numérico, em 
  # vez de ser identificado especificamente como um NA.
  if (any(is.na(Xobs)))
    stop("Xobs no gradiente da log-verossimilhança da g0i não pode conter NA's.")
  
  # Se os dados não forem numéricos, o R vai coagir o restante num tipo de dado diferente que está 
  # na parte mais acima na hierarquia dos dados e isso vai acusar que o vetor não é numérico já que
  # 'complex' e 'character' são tipos de dados de hierarquia mais alta. Os ipos de dados 'double' e 
  # 'integer' são numéricos e podem passar por esse nível pois serão coagidos a 'double'. 
  if (!is.numeric(Xobs))
    stop("Xobs no gradiente da log-verossimilhança da g0i deve ter apenas dados numéricos.")
  
  # Uma vez que garantimos que os dados são numéricos, agora pode ser verificado se eles não são do
  # tipo infinito Inf ou -Inf
  if (any(is.infinite(Xobs)))
    stop("Xobs no gradiente da log-verossimilhança da g0i não pode conter Inf's ou -Inf's.")
  
  # Se nos dados houver algum valor negativo
  if (any(Xobs < 0))
    stop("Xobs no gradiente da log-verossimilhança da g0i não deve conter valores negativos.")
  
  # Se nos dados houver algum valor nulo
  if (any(Xobs == 0))
    stop("Xobs no gradiente da log-verossimilhança da g0i não deve conter zeros.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO ALFA -----
  
  # Não pode ser NULL
  if (is.null(alfa))
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i não pode ser NULL.")
  
  # Não pode possuir dimensões, pois deve ser um valor escalar e não uma matriz, array etc.
  if (!is.null(dim(alfa)))
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i deve ser um valor escalar.")
  
  # Deve ser um valor único, não podendo aceitar um vetor (nem mesmo vazio)
  if (length(alfa) != 1)
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i deve ser um valor único.")
  
  # Não pode ser NaN
  if (is.nan(alfa))
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i não pode ser NaN.")
  
  # Não pode ser NA
  if (is.na(alfa))
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i não pode ser NA.")
  
  # Deve ser um valor numérico (double ou integer)
  if (!is.numeric(alfa))
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i deve ser um valor numérico.")
  
  # Não pode ser Inf ou -Inf
  if (is.infinite(alfa))
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i não pode ser Inf ou -Inf.")
  
  # Deve ser menor do que -1 (pela teoria)
  if (alfa >= -1)
    stop("O parâmetro alfa no gradiente da log-verossimilhança da g0i deve ser menor do que -1.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO GAMA -----
  
  # Não pode ser NULL
  if (is.null(gama))
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i não pode ser NULL.")
  
  # Não pode possuir dimensões, pois deve ser um valor escalar e não uma matriz, array etc.
  if (!is.null(dim(gama)))
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i deve ser um valor escalar.")
  
  # Deve ser um valor único, não podendo aceitar um vetor (nem mesmo vazio)
  if (length(gama) != 1)
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i deve ser um valor único.")
  
  # Não pode ser NaN
  if (is.nan(gama))
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i não pode ser NaN.")
  
  # Não pode ser NA
  if (is.na(gama))
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i não pode ser NA.")
  
  # Deve ser um valor numérico (double ou integer)
  if (!is.numeric(gama))
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i deve ser um valor numérico.")
  
  # Não pode ser Inf ou -Inf
  if (is.infinite(gama))
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i não pode ser Inf ou -Inf.")
  
  # Deve ser maior do que zero (pela teoria)
  if (gama <= 0)
    stop("O parâmetro gama no gradiente da log-verossimilhança da g0i deve ser maior do que zero.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO L -----
  
  # Não pode ser NULL
  if (is.null(L))
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i não pode ser NULL.")
  
  # Não pode possuir dimensões, pois deve ser um valor escalar e não uma matriz, array etc.
  if (!is.null(dim(L)))
    stop("O parâmetro L na log-verossimilhança da g0i deve ser um valor escalar.")
  
  # Deve ser um valor único, não podendo aceitar um vetor (nem mesmo vazio)
  if (length(L) != 1)
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i deve ser um valor único.")
  
  # Não pode ser NaN
  if (is.nan(L))
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i não pode ser NaN.")
  
  # Não pode ser NA
  if (is.na(L))
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i não pode ser NA.")
  
  # Deve ser um valor numérico (double ou integer)
  if (!is.numeric(L))
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i deve ser um valor numérico.")
  
  # Não pode ser Inf ou -Inf
  if (is.infinite(L))
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i não pode ser Inf ou -Inf.")
  
  # Verificando se L é inteiro (quer passado como 'double', quer passado como 'integer')
  if (L != floor(L))
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i deve ser um número inteiro.")
  
  # Deve ser maior do que 0 (pela teoria)
  if (L <= 0)
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i deve ser maior do que 0.")
  
  # ----- CONSTRUÇÃO DO GRADIENTE DA LOG-VEROSSIMILHANÇA DA g0i -----
  
  # Tamanho da amostra
  n = length(Xobs)
  
  # Derivada da log-verossimilhança em relação a alfa
  dalfa <- - n * log(gama)  - n * digamma(L - alfa) + n * digamma(-alfa) + sum(log(gama + L * Xobs))
    
  #Derivada da log-verossimilhança em relação a gama
  dgama <- - n * alfa / gama + (alfa - L) * sum(1 / (gama + L * Xobs) )
    
  # ----- RETORNO COMO UM VETOR -----
  
  return(c(dalfa, dgama))
  
}
