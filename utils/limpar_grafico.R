# GERAR NOVA ÁREA GRÁFICA

# Gerar nova área gráfica para que não haja erro na plotagem
limpar_grafico <- function() {
  
  # Fecha o device gráfico atual, exceto o 'null device'
  if (dev.cur() != 1) {
    
    dev.off()
  }
  
  # Abre um novo device gráfico com dimensões maiores
  dev.new(width = 7, height = 5)
  
}