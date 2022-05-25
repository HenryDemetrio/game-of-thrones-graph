#lendo a lista de arestas disponibiizada no github 

lista_de_arestas <- read.csv(file= "got-edges.csv",
                             header = TRUE, sep = ',', 
                             dec= ',')

is.data.frame(lista_de_arestas)

head(lista_de_arestas)




lista_de_vertices <- read.csv(file= "got-nodes.csv",
                              header = TRUE, sep = ',', 
                              dec= ',')

is.data.frame(lista_de_vertices)

head(lista_de_vertices)



install.packages('igraph')

library(igraph)


grafo<-graph_from_data_frame(lista_de_arestas,
                             directed = FALSE,
                             vertices = lista_de_vertices)

tkplot(grafo,
       layout=layout.lgl,
       vertex.color="purple")


vertex_attr(grafo)

edge_attr(grafo)                             

grau <- degree(grafo)

maiorgrau <- max(grau)
maiorgrau


############### maior grau ja foi agora maior intermediacao de vertices
#################

intermediacaoVertices <- betweenness(grafo)
intermediacaoVertices

maiorintermediacaoVertices <- max(intermediacaoVertices)
maiorintermediacaoVertices

######## agora o tamanho do grafo

gsize(grafo)

############# E por fim o diamêtro 

get_diameter(grafo)

diametro <- get_diameter(grafo)

diametro

diameter(grafo)


####### trabalho concluido agora colocar os graus de forma decrescente

sort(degree(grafo), decreasing = TRUE) [1:10]

sort(degree(grafo), decreasing = FALSE) [1:30]

################### agora o mesmo conceito aplicado no betweenness 

sort(closeness(grafo), decreasing = TRUE) [1:10]


sort(betweenness(grafo), decreasing = TRUE) [1:10]


################# Podemos procurar o grau de um personagem especifico 

degree(grafo) ["Robert"]

################### podemos pesquisar os personagens que possuem um grau maior 
###### q um valor especifico

degree(grafo)[degree(grafo) > 30]


##################  e podemos pesquisar por um valor especifico 

degree(grafo)[degree(grafo) == 12]


##### agora o valor medio dos graus do grafo 

mean(degree(grafo))


################# atributos do grafo (vertices)

vertex_attr(grafo)

#################### PLOT CONDICIONAL (colocar mais coisa aq depois)


plot(grafo,
     vertex.label = ifelse(V(grafo)$name %in% c("Tyrion","Jon","Sansa"),
                           V(grafo)$name,
                           NA),
     vertex.size = ifelse(V(grafo)$name %in% c("Tyrion","Jon","Sansa"),
                          30,
                          7), 
     vertex.color = ifelse(V(grafo)$name %in% c("Tyrion","Jon","Sansa"),
                           "purple", 
                           NA))

#############################################

par(mfrow = c(1,2)
    ,bty   = "n") 


hist(degree(grafo)
     ,col  ="lightblue"
     ,main = "Aula 25-04-2022")

stripchart(degree(grafo)
           ,method = "stack"
           ,pch    = 16
           ,cex    = 1.2
           ,at     = 0
           ,col    = "lightblue")


########################## agora centralidade do autovetor


eigen_centrality(grafo)

###########################
############################ Agora indentificar os vizinhos, com a função
#################### Neighbors 

neighbors(grafo, "Jon")


####################### Jon tem vizinho pa porra, vamo ver quem q tem 1 vizinho so

degree(grafo)[degree(grafo) == 1]

################# agora a vizinhança 

neighborhood(grafo, order = 1, "Amory")


############### todos q estaoconectados com 1 ou 2 arestas 

neighborhood(grafo, order = 2, "Amory")

############### vizinhos de primeira ordem da Amory 

neighborhood(grafo, order = 1, "Oberyn")

#### Subgrafo

grafo_sub <- subgraph.edges(grafo,
                            E(grafo)[inc(c("Amory", "Oberyn" ))])


grafo_sub <- subgraph.edges(grafo,
                            E(grafo)[inc(c("Tyrion", "Jon" ))])

plot(grafo_sub, vertex.color="lightgreen", vertex.size=40)


########################
tkplot(grafo_sub,
       vertex.color="orange",
       vertex.size=40)


###################### Agora vamos formar uma comunidade bla bla bla
#############################


### agora modularidade 
comunidade <- cluster_edge_betweenness(grafo)

modularity(comunidade)

plot(comunidade
     ,grafo
     ,vertex.label= NA
     ,vertex.size = 10)


membros_da_comunidade <-membership(comunidade)


table(membros_da_comunidade)

plot(comunidade
     ,grafo
     ,vertex.size = 20
     ,vertex.label = as.character(membros_da_comunidade))


############ modularidade dos membros  do  grupo

membros_da_comunidade[membros_da_comunidade == 8]

membros_da_comunidade[membros_da_comunidade == 6]


cluster_fast_greedy(grafo)
modularity(comunidade)

membros_da_comunidade <-membership(comunidade)
membros_da_comunidade
table(membros_da_comunidade)


intermediacaoVertices
cluster_edge_betweenness(grafo)

median(intermediacaoVertices(grafo))

mean(intermediacaoVertices)

