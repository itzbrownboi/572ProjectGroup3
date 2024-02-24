rm(list=ls())

library(data.table)
movies=fread("imdb_movies_subset.csv", header=T)
movies$crew

#array of actor names
actorName = c()

# for each entry in the dataset
for (crew in movies$crew) {
  # split into separate names by comma
  actor = strsplit(crew, ", ")[[1]]
  # remove every 2nd value as this is the fictional character played by the actor
  actor = actor[c(T,F)]
  #add to the array
  actorName=c(actorName, actor)
}

#remove empty strings if any
actorName = actorName[actorName != "" 
                      & actorName != " "
                      & actorName != ","
                      & actorName != ", "]

#remove repeats
actorName=unique(actorName)

# left with 16,631 actors
length(actorName)

# creating the nodes.csv
id = c(0:(length(actorName)-1))
nodes = data.frame(id,actorName)
write.csv(nodes, file = "nodes.csv", row.names = FALSE)







# creating the edges.csv

# Initialize a dataframe
edges <- data.frame(
  source = character(),
  target = character(),
  weight = numeric(),
  stringsAsFactors = FALSE
)

test=actorName[1:100]
test

# Create an empty list to store edges
edges_list <- list()

# # compare permutation pairs of all actors
# for (i in 1:(length(actorName) - 1)) {
#   for (j in (i + 1):length(actorName)) {
#     source <- trimws(actorName[i])
#     target <- trimws(actorName[j])
#     cat("Pair:", source, "-", target, "\n")
#     
#     # Check if the actors are in the same crew
#     matching_rows <- sapply(movies$crew, function(crews) source %in% strsplit(crews, ", ")[[1]] & target %in% strsplit(crews, ", ")[[1]])
#   
#     # Extract matching crew members
#     matching_crew <- unlist(strsplit(movies$crew[matching_rows], ", "))[c(T, F)]
#     
#     if (length(matching_crew) > 0) {
#       # Create a new edge and store it in the list
#       newEdge = data.frame(source=source, target=target, weight=1)
#       edges_list[[length(edges_list) + 1]] <- newEdge
#       cat("Pair found:", source, "-", target, "\n")
#     }
#   }
# }
#
# # Combine the list of edges into a dataframe
# edges <- do.call(rbind, edges_list)





# OG CODE this still should work


# compare permutation pairs of all actors
for (i in 1:(length(actorName)-1)) {
  for (j in (i+1):length(actorName)) {
    source = actorName[i]
    target = actorName[j]
    cat("Pair:", source, "-", target, "\n")
    # for each crew list
    for (crew in movies$crew) {
      crewList = strsplit(crew, ", ")[[1]] # split crew into a list of arrays by comma
      crewList = crewList[c(T,F)] # remove every 2nd value
      # if the 2 actors are in the same crew
      if (source %in% crewList & target %in% crewList) {
        # make a new edge
        newEdge = data.frame(source=source, target=target, weight=1)
        edges=rbind(edges, newEdge)
        cat("Pair found:", source, "-", target, "\n")
      }
    }
  }
}




# determining weights

# Find duplicate rows based on all columns
library(dplyr)
edgesWeighted <- edges %>%
  group_by(source, target) %>%
  summarise(weight = sum(weight)) %>%
  ungroup()

edgesWeighted





# CODE GRAVEYARD

# #for each actor
# for (actorSource in test) {
#   # for each list of crews
#   for (crew in movies$crew) {
#     #crewList is the list of actors in each movie. ie ["x","y","z"]
#     crewList = strsplit(crew, ", ")[[1]]
#     crewList = crewList[c(T,F)]
# 
#     # if actor is in the crew list
#     if (actorSource %in% crewList) {
#       # for each member in the crew list
#       for (crewMember in crewList) {
#         # make new link with source = actor and target = crew member
#         newEdge = data.frame(source=crewMember, target=crewList, weight=1)
#         
#         # check if exists
#         if (any(duplicated(rbind(edges, newEdge)))) {
#           print("SDFDS")
#         }
#           
#         edges=rbind(edges, newEdge)
#       }
#     }
#   }
# }


library(dplyr)
library(tidyr)


# Initialize a dataframe
edges <- data.frame(
  source = character(),
  target = character(),
  weight = numeric(),
  stringsAsFactors = FALSE
)

test=actorName[1:20]
test

combinations <- combn(test, 2, simplify = TRUE)
combinations

check_crew <- function(actor1, actor2) {
  any(sapply(movies$crew, function(crew) actor1 %in% strsplit(crew, ", ")[[1]] & actor2 %in% strsplit(crew, ", ")[[1]]))
  cat("Pair:", actor1, "-", actor2, "\n")
}

edges <- data.frame(t(combinations)) %>%
  mutate(weight = mapply(check_crew, X1, X2))




# Omodify og CODE


# compare permutation pairs of all actors
for (i in 1:(length(test)-1)) {
  for (j in (i+1):length(test)) {
    source = test[i]
    target = test[j]
    cat("Pair:", source, "-", target, "\n")
    # for each crew list
    for (crew in movies$crew) {
      #crewList = strsplit(crew, ", ")[[1]] # split crew into a list of arrays by comma
      #crewList = crewList[c(T,F)] # remove every 2nd value
      # if the 2 actors are in the same crew
      if (grepl(source, crew) & grepl(target, crew)) {
        # make a new edge
        newEdge = data.frame(source=source, target=target, weight=1)
        edges=rbind(edges, newEdge)
        cat("Pair found:", source, "-", target, "\n")
      }
    }
  }
}
