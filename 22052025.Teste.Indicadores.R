# Baixando pacotes 
library(readr)
library(tidyr)
library(dplyr)

# Baixando a base de dados 
guess_encoding(file = "microdados_ed_basica_2024.csv")
df = read_csv2(file = "microdados_ed_basica_2024.csv",
               locale = locale(encoding = "ISO-8859-1") )

#Visualizando o df
df

# Separando os municipios do RJ
baseRJ = df |> 
  filter(SG_UF == "RJ") |> 
  select(NO_MUNICIPIO,CO_MUNICIPIO, IN_DESKTOP_ALUNO,IN_ALIMENTACAO,
         IN_EDUC_AMBIENTAL, IN_REDES_SOCIAIS, IN_EXAME_SELECAO,
         TP_DEPENDENCIA,QT_MAT_MED, IN_AGUA_POTAVEL, IN_EQUIP_LOUSA_DIGITAL,
         IN_EQUIP_MULTIMIDIA, IN_INTERNET_ALUNOS,IN_INTERNET_APRENDIZAGEM, 
          IN_TRATAMENTO_LIXO_RECICLAGEM, IN_BANHEIRO_PNE,
         IN_ACESSIBILIDADE_ELEVADOR) # Quantidade de funcionarios observar pq nao foi 

# Arrumando a base de dados 
baseRJ$CO_MUNICIPIO = factor(baseRJ$CO_MUNICIPIO)

glimpse(baseRJ)

# Primeiro indicador 
## Porcentagem (%) de escolas que disponibilizam computadores desktop para os alunos usarem

P = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |>  # Agrupa os dados por município (nome e código)
  summarise(
    n = n(),  # Conta o número de observações por grupo
    soma = sum(IN_DESKTOP_ALUNO, na.rm = TRUE),  # Soma dos valores 1 (Sim) da variável 
    Pj = (soma / n) * 100  # Calcula a porcentagem de alunos com desktop no grupo
  ) |> 
  select(-n, -soma)  # Remove as colunas intermediárias 'n' e 'soma' do resultado final


base_final = P # Adicionando o indicador na base final 

#visualizando a base
base_final

# Segundo indicador 
## Porcentagem (%) de escolas que oferecem alimentação para os alunos
Z = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_ALIMENTACAO, na.rm = TRUE),
            Zj = (soma/n)*100) |> 
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = Z, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final 

#Visualizando a base
base_final

# Terceiro indicador 
## Porcentagem (%) de escolas que desenvolvem ações de educação ambiental
Y = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_EDUC_AMBIENTAL, na.rm = TRUE),
            Yj = (soma/n)*100) |> 
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = Y, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

# Quarto indicador 
## Porcentagem (%) de escolas que possuem sites ou blogs ou redes sociais para publicações institucionais
baseRJ$IN_REDES_SOCIAIS = if_else(baseRJ$IN_REDES_SOCIAIS == 1 ,1,0)
V = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_REDES_SOCIAIS, na.rm = TRUE),
            Vj = (soma/n)*100)  |> 
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = V, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

# Quinto indicador 
## Porcentagem (%) de escolas que fazem exames de seleção para ingresso dos alunos

baseRJ$IN_EXAME_SELECAO = if_else(baseRJ$IN_EXAME_SELECAO == 1 ,1,0)
T = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_EXAME_SELECAO, na.rm = TRUE),
            Tj = (soma/n)*100)  |> 
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = T, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

# Sexto indicador 
## Porcentagem (%) de escolas privadas no município

baseRJ$ESC_priv = if_else(baseRJ$TP_DEPENDENCIA == 4 ,1,0)
Q = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(ESC_priv, na.rm = TRUE),
            Qj = (soma/n)*100)  |> 
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = Q, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

# Setimo indicador 
## Número total (#) de matrículas no Ensino Médio no município

M = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |>
  summarise(Mj = sum(QT_MAT_MED, na.rm = TRUE))

base_final = full_join(x = base_final, 
                       y = M, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

# Oitavo indicador 
## Porcentagem (%) de escolas que fornece água potável para o consumo humano
H = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_AGUA_POTAVEL, na.rm = TRUE),
            Hj = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = H, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

# Nono indicador 
## Porcentagem (%) de escolas que possuem lousa digital.

G = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_EQUIP_LOUSA_DIGITAL, na.rm = TRUE),
            Gj = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = G, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

# Decimo indicador 
## Porcentagem (%) de escolas que possuem Projetor multimídia (datashow).

C = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_EQUIP_MULTIMIDIA, na.rm = TRUE),
            Cj = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = C, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO")) # Adicionando o indicador na base final

#Visualizando a base
base_final

#Decimo primeiro indicador
## Porcentagem (%) de escolas que possuem acesso à internet para uso dos alunos

W = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_INTERNET_ALUNOS, na.rm = TRUE),
            Wj = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = W, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO"))

#Visualizando a base
base_final

#Decimo segundo indicador
## Porcentagem (%) de escolas que possuem acesso à internet para uso nos processos de ensino e aprendizagem

D = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_INTERNET_APRENDIZAGEM, na.rm = TRUE),
            Dj = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = D, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO"))

#Visualizando a base
base_final

#Decimo terceiro indicador
## Número total de funcionários em escolas no município.

B = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(soma = sum(IN_INTERNET_APRENDIZAGEM, na.rm = TRUE),
            Bj = soma)  |>
  select(-soma)

base_final = full_join(x = base_final, 
                       y = B, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO"))

#Visualizando a base
base_final

#Decimo quarto indicador
## Porcentagem (%) de escolas que reciclam o seu lixo no município

R = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_TRATAMENTO_LIXO_RECICLAGEM, na.rm = TRUE),
            Rj = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = R, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO"))


#Visualizando a base
base_final

#Decimo quinto indicador
## Porcentagem (%) de escolas com banheiro acessível, por município.

N = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_BANHEIRO_PNE, na.rm = TRUE),
            Nj = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = N, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO"))

#Visualizando a base
base_final

#Decimo sexto indicador
## Porcentagem (%) de escolas com elevador, por município.

E = baseRJ |> 
  group_by(NO_MUNICIPIO, CO_MUNICIPIO) |> 
  summarise(n = n(),
            soma = sum(IN_ACESSIBILIDADE_ELEVADOR, na.rm = TRUE),
            Ej = (soma/n)*100)  |>
  select(-n, -soma)

base_final = full_join(x = base_final, 
                       y = E, 
                       by = c("NO_MUNICIPIO", "CO_MUNICIPIO"))


#Base final
glimpse(base_final)

#Mudando os nomes das colunas
base_final = base_final |> 
  rename(Municipio= "NO_MUNICIPIO",
         Codigo_municipio ="CO_MUNICIPIO",
         Porcentagem_DeskTops_Alunos="Pj",
         Porcentagem_Alimentação="Zj",
         Porcentagem_Eucacao_Ambiental="Yj",
         Porcentagem_Midias_Sociais="Vj",
         Porcentagem_Selecao="Tj",
         Porcentagem_Escolas_Privadas="Qj",
         Numero_Matriculados_EM="Mj",
         Porcentagem_Agua_Potavel="Hj",
         Porcentagem_Lousa_Digital="Gj",
         Porcentagem_DataShow="Cj",
         Porcentagem_Internet_Alunos="Wj",
         Porcetagem_Internet_Ensino="Dj",
         Numero_Funcionarios="Bj",
         Porcentagem_Reciclagem="Rj",
         Porcentagem_Banheiros="Nj",
         Porcentagem_Elevador="Ej")

#Visualizando a base de dados final
glimpse(base_final)


#Exportando a base de dados
#Ativando pacote
library(writexl)

#Exportando o arquivo em .xlsx
write_xlsx(x = base_final,
           path =  "Indicadores.xlsx")
