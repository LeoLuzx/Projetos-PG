####################################################################################################

# ARQUIVO PARA CHAMAR AS FUNÇÕES

####################################################################################################

# Devido à grande quantidade de funções, fica difícil manter o código. Desse modo, foram criadas 
# duas pastas para funções separadamente

# A pasta 'modelos' guarda as funções dos modelos
# A pasta 'utils' guarda as funções úteis de suporte

# Este arquivo 'funções' é um proxy para elas para evitar chamar muitos arquivos no 'main.R'
# Sempre que uma nova função for criada numa daquelas pastas, elas devem ser listadas abaixo

# Funções da pasta 'utils'
source("utils/instalar.R")

# Funções da pasta 'modelos'
