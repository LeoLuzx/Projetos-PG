
# GRADIENTE DA LOG-VEROSSIMILHANÇA DA g0i

#Define o gradiente da log-verossimilhança
grad_loglik_g0i = function(Xobs, alfa, gama, L){
  
  # OBS: A única condição de entrada não sanitizável nesta estrutura ocorre quando há algum valor do
  # tipo lógico dentro do vetor Xobs, pois nesse caso, esse valor lógico será coagido para 'double' 
  # fazendo TRUE virar 1 e FALSE virar 0. No caso de false, ao ser convertido para o valor 0, a 
  # restrição de valores não nulos evitará a sua aceitação, mas, no caso de TRUE sendo convertido 
  # para 1, não há verificação por causa da coerção. Portanto, nesse único caso, o usuário é quem 
  # deve zelar para que não haja valores lógicos intrusos no vetor de amostras. Dado o uso desta 
  # função, o evento de existirem valores do tipo lógico dentro do vetor de amostras é improvável, 
  # no entanto, devemos tomar nota.
  
  # ----- VALIDAÇÃO DA AMOSTRA -----
  
  # NULL deve ser verificado antes da verificação do comprimento, pois NULL possui comprimento zero 
  # e, portanto, seria confundido facilmente com um vetor vazio caso fosse verificado apenas pela
  # comparação length(Xobs) == 0.
  if (is.null(Xobs))
    stop("Xobs no gradiente da log-verossimilhança da g0i não pode ser NULL.")
  
  # Ser numérico (mais à frente) já garante que não é vazio, pois is.numeric(c()) retorna FALSE. No 
  # entanto, essa parte é mantida por questões de clareza semântica e comunicação do tipo do 
  # problema caso ocorra.
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
  
  # Deve ser um vetor numérico. Em R, is.numeric() retorna TRUE para objetos dos tipos integer e 
  # double, que são os tipos numéricos aceitos nesta função.
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
    stop("O parâmetro L no gradiente da log-verossimilhança da g0i deve ser um valor escalar.")
  
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
  
  # Esta quantidade comum às duas componentes do gradiente deve ser verificada porque pode ser
  # explosiva quando valores muito grandes surgem, por exemplo, quando seus elementos são 
  # estimados em ciclos bootstrap gerando overflow ou underflow. Os avisos aqui permitem comunicar 
  # se por acaso isso acontecer.
  aux <- gama + L * Xobs
  
  if (any(!is.finite(aux)))
    stop("O cálculo de (gama + L * Xobs) no gradiante da log-verossimilhança da g0i produziu valores não finitos.")
  
  # Recíproco utilizado na derivada em relação a gama
  inv_aux <- 1 / aux
  
  if (any(!is.finite(inv_aux)))
    stop("O cálculo de (1 / (gama + L * Xobs)) no gradiente da log-verossimilhança da g0i produziu valores não finitos.")
  
  dalfa <- -n * log(gama) - n * digamma(L - alfa) + n * digamma(-alfa) + sum(log(aux))
  
  dgama <- -n * alfa / gama + (alfa - L) * sum(inv_aux)
    
  # ----- RETORNO COMO UM VETOR -----
  
  return(c(dalfa, dgama))
  
}
