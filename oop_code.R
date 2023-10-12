library(tidyverse)
library(purrr)

#--Import data
data <- read.csv('MIE.csv')

#--Define generic functions
subject <- function(loadlongdata, id) UseMethod("subject")

visit <- function(subject, visitid) UseMethod("visit")

room <- function(visit, roomid) UseMethod("room")

#--Convert a data frame into a “LongitudinalData” object
make_LD <- function(data) {
  nested_data <- data  %>% nest(data = -id)
  structure(nested_data, class = "LongitudinalData")
}

#x <- make_LD(data)
#print(class(x))

#--Define methods for LongitudionalData 
#---Print LongitudinalData
print.LongitudinalData <- function(ld_data){
  paste0("Longitudinal dataset with ", length(ld_data[["id"]]), " subjects")
}

#print(x)

#--Create Subject class and define methods for Subject
subject.LongitudinalData <- function(data, id){
  index <- which(data[["id"]] == id)
  
  if (!id %in% data[["id"]])
    return(NULL)
  
  structure(list(id = id,  data = data[["data"]][[index]]), class = "Subject")
}

#---Print Subject
print.Subject <- function(data){
  paste0("Subject ID: ", data[["id"]])
}
#out <- subject(x, 14)
#print(out)

#--Summarise Subject
summary.Subject <- function(data){

  output <- data[["data"]] %>%
    group_by(visit, room) %>%
    summarise(value = mean(value)) %>%
    pivot_wider(id_cols = visit, names_from = room, values_from = value) %>%
    select(visit, sort(colnames(.))) %>%
    as.data.frame()
  
  structure(list(id = data[["id"]], output = output), class = "Summary")
}

#out <- subject(x, 54) %>% summary
#print(out)

#--Define Visit class 
visit.Subject <- function(subject, visitid){
  data <- subject[["data"]] %>%
    filter(visit == visitid) %>%
    select(-visit)
  
  structure(list(id = subject[["id"]],
                 visitid = visitid,
                 data = data), class = "Visit")
}

#--Define Room class 
room.Visit <- function(visit, roomid){
  data <- visit[["data"]] %>%
    filter(room == roomid) %>%
    select(-room)
  
  structure(list(id = visit[["id"]],
                 visitid = visit[["visitid"]],
                 room = roomid,
                 data = data), class = "Room")
}

#---Print Room
print.Room <- function(object){
  cat("ID:", object[["id"]], "\n")
  cat("Visit:", object[["visitid"]], "\n") 
  cat("Room:", object[["room"]], "\n")
}

#out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
#print(out)

#---Summarise Room
summary.Room <- function(object){
  output <- summary(object[["data"]][["value"]])
  structure(list(id = object[["id"]], 
                 output = output), class = "Summary")
}


#---Print the summary of Room
print.Summary <- function(object) {
  cat("ID:", object[["id"]], "\n")
  print(object[["output"]])
}

#out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
#print(out)
