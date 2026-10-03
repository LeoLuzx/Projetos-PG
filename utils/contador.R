# CONTADOR GERAL DE PROGRESSO

contador <- function(...) {
  
  # Obtém os tamanhos dos níveis de iteração informados
  niveis <- list(...)
  
  # Verifica se foi informado pelo menos um nível
  if (length(niveis) == 0) {
    stop("Informe pelo menos um nível de iteração.")
  }
  
  # Verifica se foram informados no máximo três níveis
  if (length(niveis) > 3) {
    stop("São permitidos no máximo três níveis de iteração.")
  }
  
  # Verifica se todos os níveis são inteiros positivos
  if (!all(vapply(niveis, function(x) {
    length(x) == 1 &&
      is.numeric(x) &&
      is.finite(x) &&
      x >= 1 &&
      x == as.integer(x)
  }, logical(1)))) {
    stop("Os tamanhos dos níveis devem ser inteiros positivos.")
  }
  
  # Converte os tamanhos para inteiros
  niveis <- as.integer(unlist(niveis))
  
  # Calcula o número total de iterações
  total <- prod(niveis)
  
  # Cria a barra de progresso
  barra <- utils::txtProgressBar(
    min = 0,
    max = total,
    style = 3
  )
  
  # Retorna uma função para atualizar a barra
  function(...) {
    
    # Obtém os índices atuais
    indices <- list(...)
    
    # Verifica se a quantidade de índices está correta
    if (length(indices) != length(niveis)) {
      stop(
        "Quantidade de índices incompatível com a quantidade de níveis."
      )
    }
    
    # Calcula a posição global da iteração
    posicao <- 1
    multiplicador <- 1
    
    for (n in seq_along(indices)) {
      
      posicao <- posicao +
        (indices[[n]] - 1) * multiplicador
      
      multiplicador <- multiplicador * niveis[[n]]
    }
    
    # Atualiza a barra de progresso
    utils::setTxtProgressBar(
      barra,
      posicao
    )
    
    # Fecha a barra na última iteração
    if (posicao == total) {
      close(barra)
    }
  }
}
