# GERADOR DE VALORES ALEATÓRIOS DA g0i

# Gera uma amostra de tamanho n da distribuição g0i
rG0I = function(n, alpha, gama, L){
  
  # ----- VALIDAÇÃO DO PARÂMETRO ALFA -----
  
  validar_valor(alfa)
  
  # Deve ser menor do que -1 (pela teoria)
  if (alfa >= -1)
    stop("O parâmetro alfa para geração de amostras que seguem a distribuição g0i deve ser menor do que -1.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO GAMA -----
  
  validar_valor(gama)
  
  # Deve ser maior do que zero (pela teoria)
  if (gama <= 0)
    stop("O parâmetro gama para geração de amostras que seguem a distribuição g0i deve ser maior do que zero.")
  
  # ----- VALIDAÇÃO DO PARÂMETRO L -----
  
  validar_valor(L)
  
  # Verificando se L é inteiro (quer passado como 'double', quer passado como 'integer')
  if (L != floor(L))
    stop("O parâmetro L para geração de amostras que seguem a distribuição g0i deve ser um número inteiro.")
  
  # Deve ser maior do que 0 (pela teoria)
  if (L <= 0)
    stop("O parâmetro L para geração de amostras que seguem a distribuição g0i deve ser maior do que zero.")
  
  # ----- VALIDAÇÃO DO TAMANHO AMOSTRAL -----
  
  validar_valor(n)
  
  # Verificando se n é inteiro
  if (n != floor(n)) {
    stop("O tamanho amostral n para a geração de amostras que seguem a distribuição g0i deve ser um número inteiro.")
  }
  
  # Verificando se n é positivo
  if (n <= 0) {
    stop("O tamanho amostral n para a geração de amostras que seguem a distribuição g0i deve ser um valor positivo.")
  }
  
  # ----- GERAÇÃO DOS VALORES ALEATÓRIOS -----
  
  # Gerando uma amostra da distribuição g0i pela definição como a razão de duas gamas
  res = rgamma(n, L, L) / rgamma(n, alpha, gama);
  
  return(res)
}
