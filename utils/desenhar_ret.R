
desenhar_ret <- function(areas, imag, cor_borda = "red", largura = 2){
  
  # Capturando as áreas, ela itera sobre cada elemento em 'areas' (lista) e 'a' será cada elemento da lista
  for (a in areas){
    
    # rect() desenha um retângulo no sistema de coordenadas atual do plot
    # Aqui fornecemos as 4 coordenadas: xleft, xright, ybottom, ytop
    rect(
      # xleft: coordenada esquerda (em índices de coluna da matriz).
      xleft = a$y1,
      
      # xright: coordenada direita (em índices de coluna da matriz).
      xright = a$y2,
      
      # ybottom: coordenada inferior do retângulo.
      
      # - Matrizes em R têm índice 1 na linha superior, enquanto o gráfico tem origem (y=1) na parte inferior. 
      
      # - Para ajustar, usamos nrow(Re(imag[,,1])) para obter o número de linhas da imagem, e
      # subtraímos a$x2 e somamos 1 para mapear o índice da matriz para a coordenada gráfica.
      
      # Re() pega a parte real
      ybottom = nrow(Re(imag[,,1])) - a$x2 + 1,
      
      # ytop: coordenada superior do retângulo (também convertida para o sistema de coordenadas do gráfico)
      ytop = nrow(Re(imag[,,1])) - a$x1 + 1,
      
      # border: cor da borda do retângulo.
      border = cor_borda,
      
      # lwd: largura da linha da borda.
      lwd = largura
    )
  }
}
