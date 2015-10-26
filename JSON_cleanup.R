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