module UnBCare where

import ModeloDados

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

 
 
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}


{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

sort :: Ord a => [a] -> [a] --funcao de ordenacao auxiliar 
sort [] = []
sort (s:xs) = sort [x | x <- xs, x < s]
              ++ [s] ++
              sort [x | x <- xs, x >= s]


existeMedicamento :: Medicamento -> EstoqueMedicamentos -> Bool --funcao auxiliar 
existeMedicamento med [] = False
existeMedicamento med ((m, q):re) | med == m = True
                                  | otherwise = existeMedicamento med re 

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd [] = [(med,qtd)]
comprarMedicamento med qtd ((m,q):re) | (existeMedicamento med ((m,q):re) == False) = [(med,qtd)] ++ ((m,q):re)
                                      | med == m = (m,q+qtd):re
                                      | otherwise = [(m,q)] ++ comprarMedicamento med qtd re

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

temEstoque :: Medicamento -> EstoqueMedicamentos -> Bool --funcao auxiliar 
temEstoque med [] = False
temEstoque med ((m,q):re) | (med == m) && q > 0 = True
                          | (med == m) && q == 0 = False
                          | otherwise = temEstoque med re

retiraMedicamento :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos --funcao auxiliar 
retiraMedicamento med ((m, q):re) | med == m = ((m, q-1):re)
                                  | otherwise = [(m, q)] ++ retiraMedicamento med re

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
--tomarMedicamento = undefined
tomarMedicamento med ((m,q):re) | temEstoque med ((m, q):re) = Just $ retiraMedicamento med ((m, q):re)
                                | otherwise = Nothing


{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med ((m,q):re) | existeMedicamento med ((m,q):re) == False = 0
                                    | m == med = q
                                    | otherwise = consultarMedicamento med re

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

pegaDemanda :: Receituario -> EstoqueMedicamentos --funcao auxiliar 
pegaDemanda [] = []
pegaDemanda ((m, q):re) = [(m, length q)] ++ pegaDemanda re


demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos r = sort (pegaDemanda r)



{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

--sortHorarios :: Receituario -> Receituario
--sortHorarios (m, h):re = sort h ++ sort re

horRepetido :: [Horario] -> Bool --funcao auxiliar
horRepetido [a] = True
horRepetido (p:s:re) = (p /= s) && horRepetido (s:re) 

medRepetido :: [Medicamento] -> Bool --funcao auxiliar
medRepetido [a] = True
medRepetido (p:s:re) = (p /= s) && medRepetido (s:re) 


receituarioValido :: Receituario -> Bool
receituarioValido [] = False
receituarioValido [a] = True
receituarioValido (p:s:re) = (sort (p:s:re) == (p:s:re)) && (snd p == sort(snd p)) && (snd s ==  sort(snd s)) && (fst p /= fst s) && horRepetido(snd p) && horRepetido(snd s) && receituarioValido (s:re)


planoValido :: PlanoMedicamento -> Bool
planoValido [] = False
planoValido [a] = True 
planoValido (p:s:re) = (sort (p:s:re) == (p:s:re)) && (snd p == sort(snd p)) && (snd s ==  sort(snd s)) && (fst p /= fst s) && medRepetido(snd p) && medRepetido(snd s) && planoValido (s:re)



{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}

comparaCuidado :: Cuidado -> Cuidado -> Bool --funcao auxiliar
comparaCuidado (Comprar a q) (Comprar b c) = True
comparaCuidado (Comprar a q) (Medicar b) = a /= b 
comparaCuidado (Medicar a) (Comprar b q) = a /= b
comparaCuidado (Medicar a) (Medicar b) = [a,b] == (sort [a,b])

verificaCuidado :: [Cuidado] -> Bool --funcao auxiliar
verificaCuidado [] = False
verificaCuidado [a] = True
verificaCuidado (p:s:re) = comparaCuidado p s && verificaCuidado (s:re) 


plantaoValido :: Plantao -> Bool
plantaoValido [] = False
plantaoValido [a] = True
plantaoValido (p:s:re) = ([fst p, fst s] == (sort [fst p, fst s])) && (fst p /= fst s) && (verificaCuidado (snd p)) && (plantaoValido (s:re))


{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}

-- verificar se os horarios do receituario p estao no planomedicamento
-- se estiver, mandar pra funcao que adiciona o medicamento na posicao da hora
-- se nao, mandar pra funcao que adiciona o horario e o remedio no planomedicamento

alocaMedicamento :: Medicamento -> [Horario] -> PlanoMedicamento --funcao auxiliar
alocaMedicamento m [] = [] 
alocaMedicamento m (p:re) = [(p,[m])] ++ alocaMedicamento m re

novosHorarios :: [Horario] -> PlanoMedicamento -> [Horario] --funcao auxiliar
novosHorarios [] pm = []
novosHorarios (p:re) pm | (filter (\h -> fst h == p) pm) /= [] = novosHorarios re pm 
                        | otherwise = novosHorarios re pm ++ [p]


geraPlanoMedicamento :: Receituario -> PlanoMedicamento --funcao auxiliar
geraPlanoMedicamento [] = []
geraPlanoMedicamento (p:re) |(filter (\h -> elem (fst h) (snd p)) (geraPlanoMedicamento re) /= []) = 
                              map (\h -> if elem (fst h) (snd p) 
                                 then (fst h, fst p:(snd h)) 
                                 else (fst h, snd h)) (geraPlanoMedicamento re) ++ alocaMedicamento (fst p) ( novosHorarios (snd p) (geraPlanoMedicamento re)) 
                            |otherwise = alocaMedicamento (fst p) (snd p) ++ (geraPlanoMedicamento re)

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario (p:re) | receituarioValido(p:re) == False = []
                            | otherwise = sort (geraPlanoMedicamento (p:re))

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}

alocaHorario :: Horario -> [String] -> Receituario --funcao auxiliar
alocaHorario h [] = [] 
alocaHorario h (p:re) = [(p,[h])] ++ alocaHorario h re

novosMedicamentos :: [Medicamento] -> Receituario -> [Medicamento] --funcao auxiliar
novosMedicamentos [] r = []
novosMedicamentos (p:re) r | (filter (\m -> fst m == p) r) /= [] = novosMedicamentos re r 
                           | otherwise = novosMedicamentos re r ++ [p]

geraReceituario :: PlanoMedicamento -> Receituario --funcao auxiliar
geraReceituario [] = []
geraReceituario (p:re)| (filter (\h -> elem (fst h) (snd p)) (geraReceituario re) /= []) = 
                              map (\h -> if elem (fst h) (snd p) 
                                 then (fst h, fst p:(snd h)) 
                                 else (fst h, snd h)) (geraReceituario re) ++ alocaHorario (fst p) ( novosMedicamentos (snd p) (geraReceituario re)) 
                      |otherwise = alocaHorario (fst p) (snd p) ++ (geraReceituario re)

geraReceituarioPlano :: PlanoMedicamento -> Receituario
--geraReceituarioPlano = undefined
geraReceituarioPlano (p:re) | planoValido((p:re)) == False = []
                            | otherwise = sort (geraReceituario (p:re))



{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado 
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

fromJust :: Maybe a -> a --funcao auxiliar que retira o maybe do retorno de uma funcao maybe
fromJust (Just t) = t

executaCuidado :: Cuidado -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos --funcao auxiliar
executaCuidado (Comprar m q) est = Just $ comprarMedicamento m q est
executaCuidado (Medicar m) est | temEstoque m est = tomarMedicamento m est
                               | otherwise = Nothing 

plantaoExecucao :: [Cuidado] -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos --funcao auxiliar
plantaoExecucao [] est = Just $ est
plantaoExecucao (p:re) est | (executaCuidado p est) == Nothing = Nothing
                           | otherwise = plantaoExecucao re novoestoque
                           where novoestoque = fromJust (executaCuidado p est)

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] est = Just $ est
executaPlantao (p:re) est | (plantaoExecucao (snd p) est) == Nothing = Nothing
                          | otherwise = executaPlantao re (novoestoque)
                          where novoestoque = fromJust (plantaoExecucao (snd p) est)


{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano 
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão 
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

medicamento :: [Cuidado] -> [Medicamento] --funcao auxiliar
medicamento [] = []
medicamento (c:re) = case c of 
                     Medicar m -> [m] ++ medicamento re
                     _ -> medicamento re

montaPlano :: Plantao -> PlanoMedicamento --funcao auxiliar
montaPlano [] = []
montaPlano (p:re) = [(fst p, medicamento (snd p))] ++ montaPlano re

comparaPlano :: Plantao -> PlanoMedicamento -> Bool --funcao auxiliar
comparaPlano p pm | (montaPlano p == pm) = True
                  | otherwise = False

verificaPlantao :: Plantao -> Bool --funcao auxiliar
verificaPlantao (p:re) | medicamento(snd p) == [] && verificaPlantao re = True
                       | otherwise = False

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos  -> Bool
satisfaz [] pm _ | pm == [] = True
                 | otherwise = False

satisfaz p pm []  | verificaPlantao(p) && pm == [] = True
                  | otherwise = False

satisfaz p pm est | (plantaoValido(p) && planoValido(pm)) == False = False
                  | otherwise = (comparaPlano p pm) && (executaPlantao p est) /= Nothing


{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}

plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto = undefined

