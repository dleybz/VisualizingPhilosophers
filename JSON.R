json_file <- fromJSON("~/R/minicomedians.json")
json_file <- json_file[2]
results <- json_file$results
json_file <- results$bindings
json_file=data.frame(json_file$Comedian$value, json_file$influenced$value, json_file$birthDate$value)
json_file$json_file.Comedian.value<-gsub("^.*e/","",json_file$json_file.Comedian.value)
json_file$json_file.Comedian.value<-gsub("_", " ", json_file$json_file.Comedian.value)

json_file$json_file.influenced.value<-gsub("^.*e/","",json_file$json_file.influenced.value)
json_file$json_file.influenced.value<-gsub("_", " ", json_file$json_file.influenced.value)

json_file$json_file.birthDate.value<-gsub("-.*$", "", json_file$json_file.birthDate.value)
json_file$json_file.birthDate.value<-strtoi(json_file$json_file.birthDate.value)

json_file<-json_file[order(json_file$json_file.birthDate.value),]

colnames(json_file) <- c("Name", "Influenced", "Year")

min<-min(json_file$Year)
max<-max(json_file$Year)

year_rank<-data.frame()
for(i in min:max) {
  if (i %in% json_file$Year) {
    year <- as.vector(subset(json_file, Year == i, select=Name))
    if (ncol(year_rank)>0) {
        year_rank<-data.frame(c(year_rank[,1:ncol(year_rank)], year))
    } else {year_rank<-data.frame(year)}
  }
}

names(year_rank) <- unique(json_file$Year)

year_char_string <- paste("")
for(j in 1:ncol(year_rank)){
year_char_string <- paste(year_char_string, "{ rank = same;", names(year_rank[j]))
for(i in 1:nrow(year_rank)) {
    if (!is.na(year_rank[i,j])){
        year_char_string <- paste(year_char_string, "; ", "\"", year_rank[i,j], "\"", sep="")
    }
}
year_char_string <- paste(year_char_string, "; }\n", sep="")
}

relationships<-paste("")
ancestors_vector<-vector()
for(i in 1:nrow(json_file)){
  relationships<-paste(relationships, "\"", json_file$Name[i], "\"", " -> ", "\"", json_file$Influenced[i], "\"", ";\n", sep="")

  if(!(json_file$Name[i] %in% json_file$Influenced)){
    if(length(ancestors_vector)>0)
    {
      ancestors_vector<-c(ancestors_vector[1:length(ancestors_vector)], json_file$Name[i])
    } else {ancestors_vector<-c(json_file$Name[i])}
    
  }
}

ancestors <- paste("")
for(i in 1:length(unique(ancestors_vector))){
  ancestors <- paste(ancestors, "\"", unique(ancestors_vector)[i], "\"", "; ", sep="")
}

timeline<-paste(unique(json_file$Year[1]))
for(i in 2:length(unique(json_file$Year))){
  timeline<-paste(timeline, " -> ", unique(json_file$Year)[i], sep="")
}
timeline<-paste(timeline, ";\n")

final<-(paste("digraph asde91 { \n ranksep=.75; size = \"7.5,7.5\";\n\n { \nnode [shape=plaintext, fontsize=16];\n", timeline, ancestors, "\n}\n\n node [shape=box]; \n", year_char_string, "\n", relationships, "\n}"))
cat(final)