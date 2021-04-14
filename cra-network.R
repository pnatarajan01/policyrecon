library('visNetwork')
library('tidyr')
library('dplyr')
library('tibble')
library('igraph')

setwd("/Users/pnatarajan/Documents/Air Force/eitaas")
edge_list <- read.csv("./NER_edges (1).csv", header=TRUE, as.is=TRUE)
edge_list<-edge_list %>% filter(weight >= 1)
edge_list <- edge_list %>% select(source,target,weight)
colnames(edge_list) <- c("from","to","weight")
nodes <- read.csv("./NER_nodes (1).csv", header=TRUE, as.is=TRUE)
nodes <- nodes %>% select(X,publication,group)
colnames(nodes) <- c("id","label","group")
edge_list <- edge_list[which(edge_list$from %in% nodes$label) ,]
edge_list <- edge_list[which(edge_list$to %in% nodes$label) ,]
unique.nodes <- unique(c(edge_list$to,edge_list$from))
nodes <- nodes[which(nodes$label %in% unique.nodes) ,]
edges <- edge_list %>% left_join(nodes, by = c("from" = "label")) %>% rename(from_id = id)
edges <- edges %>% left_join(nodes, by = c("to" = "label")) %>% rename(to_id = id)
edges <- select(data.frame(edges), from_id, to_id, weight)
colnames(edges) <- c('from', 'to', 'value')
nodes<-add_column(nodes,font.color="white")
head(nodes)



graph <- graph.data.frame(edges, directed = T)
degree_value <- degree(graph, mode = "in")
nodes$value <- degree_value[match(nodes$id, names(degree_value))]

head(edges)

net <- visNetwork(nodes, edges, color="white",
                  main = list(text = "EITAAS Documents Reference Network",
                       style = "font-family:arial;color:#ffffff;font-size:32px;text-align:center;"),
                  submain = list(text = "The overall classification of this visualization is UNCLASSIFIED.  A legend to the right of the diagram helps identify the organizational level of the policy by the color of the dot. Drop down filters for policy number and issuing organization are at the top right of the diagram.",
                              style = "font-family:arial;color:#ffffff;font-size:14px;text-align:center;"),
                  width = "100%",
                  height = "80vh",
                  style = "color:#ffffff;font-size:14px;text-align:center;") %>%
  visEdges(arrows = "to",color = list(color = "#CDCDCD", highlight = "red"), font= list(color="#ffffff",size="14",face="arial")) %>%
  visPhysics(stabilization = FALSE) %>%
  visOptions(selectedBy="label", 
             manipulation = TRUE, 
             highlightNearest = TRUE, collapse = TRUE)%>% 
  visInteraction(navigationButtons = TRUE) %>%
  visGroups(groupname = "Cyberspace", color = "#FFFF00") %>%
  visGroups(groupname = "Manpower and Organization", color = "#FF00FF") %>%
  visGroups(groupname = "Financial Management", color = "#00FF00") %>%
  visGroups(groupname = "Other", color = "grey") %>%
  visLegend(position = 'right', main = list(text = "Functional Level",
                                            style = "font-family:arial;color:#ffffff;font-size:18px;text-align:center;")) %>%
  visLayout(randomSeed=1)

visSave(net, "eitaas_network_2.html", selfcontained = TRUE, background = "black")