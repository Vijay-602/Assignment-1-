#Q a
library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)
html<-read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
table_node<-html_nodes(html,"table")
table_content<-html_table(table_node)[[1]]
table_content<-table_content[,2:13]
table_content
#Q b
library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)
html<-read_html("https://www.moneyworks4me.com/indianstocks/large-cap/fmcg/household-personal-products/hindustan-unilever/company-info")
table_node<-html_nodes(html,"table")
table_content<-html_table(table_node)[[1]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1a<-table_content[-c(1,2,3,4,5),]
table_content<-html_table(table_node)[[3]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1b<-table_content[-c(1),]
table_content1<-rbind(table_content1a,table_content1b)
html<-read_html("https://www.moneyworks4me.com/indianstocks/large-cap/healthcare/pharmaceuticals-drugs/cipla/company-info")
table_node<-html_nodes(html,"table")
table_content<-html_table(table_node)[[1]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1a<-table_content[-c(1,2,3,4,5),]
table_content<-html_table(table_node)[[3]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1b<-table_content[-c(1),]
table_content2<-rbind(table_content1a,table_content1b)
html<-read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobiles-passenger-cars/maruti-suzuki/company-info")
table_node<-html_nodes(html,"table")
table_content<-html_table(table_node)[[1]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1a<-table_content[-c(1,2,3,4,5),]
table_content<-html_table(table_node)[[3]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1b<-table_content[-c(1),]
table_content3<-rbind(table_content1a,table_content1b)
html<-read_html("https://www.moneyworks4me.com/indianstocks/large-cap/healthcare/pharmaceuticals-drugs/dr-reddys-lab/company-info")
table_node<-html_nodes(html,"table")
table_content<-html_table(table_node)[[1]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1a<-table_content[-c(1,2,3,4,5),]
table_content<-html_table(table_node)[[3]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1b<-table_content[-c(1),]
table_content4<-rbind(table_content1a,table_content1b)
html<-read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobile-two-three-wheelers/hero-motocorp/company-info")
table_node<-html_nodes(html,"table")
table_content<-html_table(table_node)[[1]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1a<-table_content[-c(1,2,3,4,5),]
table_content<-html_table(table_node)[[3]]
table_content<-table_content[,1:11]
colnames(table_content)<-table_content[1,]
table_content1b<-table_content[-c(1),]
table_content5<-rbind(table_content1a,table_content1b)
#Q c
tennis<-function(p){
  q<-0
  r<-0
  count<-0
  while(q<3&&r<3){
    winner<- sample(1:2,1,prob=c(p,1-p))
    if(winner==1){q=q+1}
    else{r=r+1}
    count=count+1}
  return(count)
}
values<-numeric(length = 1000)
for(i in 1:1000){
  values[i]=tennis(0.7)
}
mean(values)
#Q d
MonteyHall<-function(){
  choose<- sample(1:3,1)
  car<- sample(1:3,1)
  if(choose==car){
    return(0)
  }
  else{
    return(1)
  }
}
result <-numeric(length = 1000)
for(i in 1:1000){
  result [i]<-MonteyHall()
}
mean(result)
#Question e
library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)
html<-read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
ranking<-html%>%html_elements(".countdown-index")%>%html_text()
ranking<-str_remove_all(ranking,"[#]")%>%as.numeric()
ranking
movie_names<-html%>%html_elements(".article_movie_title a")%>%html_text()
movie_names
rating <- html%>%html_elements(".article_movie_title span.tMeterScore")%>%html_text()
rating <-str_remove_all(rating,"[%]")%>%as.numeric()
rating
year <- html%>%html_elements(".article_movie_title span")%>%html_text()
year
for(i in 1:100){
  year[i] <-  year[3*i -2]
}
year <- year[1:100]
year<-str_remove_all(year,"[(,),%]")
year<-year%>%substr(1,100)%>%as.numeric()
movie_data <- data.frame("Ranking" = ranking,"Name of Movie" = movie_names,"Tomato % score" = rating, "Year of movie
" = year )