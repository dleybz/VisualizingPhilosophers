###This program converts a slightly-modified JSON file obtained from sparql (a Wikipedia database query tool) to a .dot file, with formatting obtained from page 17 of http://www.graphviz.org/pdf/dotguide.pdf

in_file = 'philosophers.json'
out_file = 'reweighted_philo.gv'

##Packages:
#These lines will check if you have the necessary package installed, installs it if it is not already installed, and opens it
list.of.packages <- c("jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(jsonlite)

## Cleanup:
#These lines create a  extract the three columns which have the info we require 
json_file <- fromJSON(in_file)
json_file <- json_file[2]$results$bindings
json_file <- data.frame(json_file$Person$value, json_file$influenced$value, json_file$birthDate$value)
#This line renames the columns so that they are easier to work with
colnames(json_file) <- c("Name", "Influenced", "Year")

#These lines clean up the values in the name column
json_file$Name<-gsub("^.*e/","",json_file$Name)
json_file$Name<-gsub("_", " ", json_file$Name)
json_file$Name<-gsub("\\(", " ", json_file$Name)
json_file$Name<-gsub("\\)", " ", json_file$Name)

#These lines clean up the values in the Influenced column
json_file$Influenced<-gsub("^.*e/","",json_file$Influenced)
json_file$Influenced<-gsub("_", " ", json_file$Influenced)
json_file$Influenced<-gsub("\\(", " ", json_file$Influenced)
json_file$Influenced<-gsub("\\)", " ", json_file$Influenced)

#These lines clean up the values in the Year column
json_file$Year<-as.character(json_file$Year)
json_file$Year<-substr(json_file$Year, 1, 4)
json_file$Year<-as.numeric(json_file$Year)

#This line re-orders the values so that they are listed according to year of birth
json_file<-json_file[order(json_file$Year),]

##Ranking:
#This line creates a data frame "year_rank" which is filled with NAs, has the number of columns equal to the number of unique birthyears and rows equal to the maximum number of people in our dataset born in any given year
year_rank<-as.data.frame(matrix(ncol=length(unique(json_file$Year)), nrow=max(table(json_file$Year))))
#This line creates a counter variable b, which will iterate as the for-loop goes through
b<-1
#This for loop will iterate from the first birthyear of the first person in our dataset born to the birthyear of the last person born, and for each specified column adds the people who were born in that year
for(i in unique(json_file$Year)) {
  year <- as.vector(unique(subset(json_file, Year == i, select=Name)))
  year_rank[1:dim(year)[1],b] <- year
  b<-b+1
}

#This line renames the columns in the dataframe containing the rankings so that it's easier to visualize
names(year_rank) <- unique(json_file$Year)

#This line creates a character string which we will with lines containing a year and the list of all people born in that year
year_char_string <- paste("")
#These for loops fill the character string created above
for(j in 1:ncol(year_rank)){
  year_char_string <- paste(year_char_string, "{ rank = same;", names(year_rank[j]))
  for(i in 1:nrow(year_rank)) {
    if (!is.na(year_rank[i,j])){
      year_char_string <- paste(year_char_string, "; ", "\"", year_rank[i,j], "\"", sep="")
    }
  }
  year_char_string <- paste(year_char_string, "; }\n", sep="")
}

#This line removes the rows in which the influenced person is not an influencer
json_file<-subset(json_file, Influenced %in% Name)

##Weight(Simple):
for(i in 1:nrow(json_file)){
  json_file$WeightS[i] <- length(grep(json_file$Name[i], json_file$Name))
}

json_file$WeightS <- json_file$WeightS / max(json_file$WeightS)

weight_size<-as.data.frame(matrix(ncol=length(unique(json_file$WeightS)), nrow=max(table(json_file$WeightS))))
b<-1
for(i in unique(json_file$WeightS)) {
  weight <- as.vector(unique(subset(json_file, WeightS == i, select=Name)))
  weight_size[1:dim(weight)[1],b] <- weight
  b<-b+1
}

names(weight_size) <- unique(json_file$WeightS)

weight_char_string <- paste("")
for(i in 1:ncol(weight_size)){
  weight_char_string <- paste(weight_char_string, "node [shape=plaintext, fontsize=", (32*as.numeric(names(weight_size)[i])), "];{\n", sep="")
  for(j in 1:nrow(weight_size)){
    if(!(is.na(weight_size[j,i]))){
      weight_char_string <- paste(weight_char_string, "\"", weight_size[j,i] , "\"; ", sep="")
    }
  }
  weight_char_string <- paste(weight_char_string, "}\n", sep="")
}

##Relationships:
#This line creates a character string which we will fill with the relationships between people, with the format "Influencer -> Influenced"
relationships<-paste("")
#This line fills the character string created above
for(i in 1:nrow(json_file)){
  relationships<-paste(relationships, "\"", json_file$Name[i], "\"", " -> ", "\"", json_file$Influenced[i], "\"", ";\n", sep="")
}

##Timeline:
#This line creates a character string which contains the first year in which people were born
timeline<-paste(unique(json_file$Year[1]))
#This for loop adds to the character string created above, adding an arrow as well as the next year in which people were born
for(i in 2:length(unique(json_file$Year))){
  timeline<-paste(timeline, " -> ", unique(json_file$Year)[i], sep="")
}
#This line adds a newline character to the end of this character string
timeline<-paste(timeline, ";\n")


##Final:
#This line will combine all of the strings we have created, along with general formatting information
final<-(paste("digraph timeline { \n ranksep=1; splines=polyline; nodesep=.01; fixedsize=false; size = \"1000,200\";\n\n { \nnode [shape=plaintext, fontsize=16];\n", timeline, "\n}\n\n", weight_char_string, "\n", "node [shape=plaintext, fontsize=", as.numeric(min(names(weight_size)))*32, "]; \n", year_char_string, "\n", relationships, "\n}"))
#This line prints out the combination of all the strings we have created with general formatting information
sink(out_file)
cat(final)
sink()
