#20161220
#Function to get cluster, and
#Widgets that complement Fukan Visualization

library(data.table)
library(plyr)
library(dplyr)
library(plotly)
library(tools)
library(DT)
library(igraph)

#####################################################################
#####################################################################
#Network Functions
#Make comunities in descending order
clusterize <- function(a_network, algorithm = "louvain"){
  net <- simplify(a_network)
  if(algorithm == "louvain") {net <- cluster_louvain(net)}
  if(algorithm == "newman") {net <- cluster_fast_greedy(net)}
  if(algorithm == "infomap") {net <- cluster_infomap(net)}
  com <- membership(net)
  ordered <- table(com) %>% sort(decreasing = TRUE) %>% names %>% as.numeric 
  repl <- sapply(com, function(x) {which(ordered == x)})
  names(repl) <- names(com)
  return(repl)
}


#Compute number of vertex and edges within each community
#Create a summary with information of each cluster
v_and_e = function(a_network, a_com){
  n_clusters <- sort(unique(a_com))
  vert <- sapply(n_clusters, function(x){
    return(sum(a_com == x))
  })
  edg <- sapply(n_clusters, function(x){
    subg1 <- induced_subgraph(a_network, which(a_com == x))
    return(ecount(subg1))
  })
  
  df <- data.frame(cbind(vert, edg))
  return(df)
}


#Parse and get the country name
getCountry <- function(d) {
  addresses <- strsplit(d$RP, split = ", ") #Separate string by the comma
  addresses <- lapply(addresses, function(x) {x[length(x)]}) #Get the last value which is the country
  addresses <- sapply(addresses, function(x) gsub("\\.", "", x)) #Remove the dot
  addresses[grep("USA", addresses)] <- "USA" #Standardize all US addresses
  
  #Add the country
  Country <- as.character(addresses)
  Country[which(Country=="character(0)")] <- "-" #Correct the papers with no country info
  
  return(Country)
}

########################################################
########################################################
#Widgets

#Get top papers
TopPapers <- function(d, top = 10){
  top <- min(top, nrow(d)) #To fix top for clusters with less papers than top!
  index <- order(d$degrees, decreasing = TRUE)[1:top]
  Title <- d$TI[index] %>% tolower %>% toTitleCase
  Degree <- d$degrees[index] #Degree in the overall network (in + out)
  Cites <- d$TC[index]
  dois <- d$DI[index]
  dois[is.na(dois)] <- ""
  dois <- sapply(dois, function(x) {
    if(nchar(x) >= 1) {
      paste("https://doi.org/",x, sep = "")} 
    else {""}})
  DOI <- paste('<a href = ', shQuote(dois), "target=", shQuote("_blank"), '>', dois, '</a>') 
  return(data.frame(Title,Degree,Cites,DOI))
}


#Get Top Authors
TopAuthors <- function(d, top = 10){
  authors <- 
    strsplit(d$AU, split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(authors)
}

#Top WOS categories (Assigned to journal)
TopCategories <- function(d, top = 10){
  categories <- 
    strsplit(d$WC, split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(categories)
}

#Top Subject Areas (Assigned to paper)
TopSubjects <- function(d, top = 10){
  Subjects <- 
    strsplit(d$SC, split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(Subjects)
}

#Top Journal/Conference (Assigned to paper)
TopPublishers <- function(d, top = 10){
  Publishers <- 
    strsplit(d$SO, split ="; ") %>% 
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(Publishers)
}

#Get language summary
Languages <- function(d){
  lang <- 
    d$LA %>%
    table %>%
    sort(decreasing = TRUE)
  return(lang)
}

#Type of publication summary
Type_p <- function(d){
  type <- 
    d$DT %>%
    table %>%
    sort(decreasing = TRUE)
  return(type)
}


#Top Keywords by frequency (Author Keywords + Smart keywords)
TopKeywords <- function(d, top = 15){
  keys <- 
    paste(d$DE, d$ID, sep = "; ") %>%
    tolower %>%
    strsplit(split ="; ") %>% 
    lapply(function(x) x[x!=""]) %>%
    unlist %>% 
    table %>% 
    sort(decreasing = TRUE) %>% 
    .[1:top]
  return(keys)
}

#Get top years
TopYears <-  function(d, top = 5){
  years <- 
    table(as.numeric(d$PY)) %>% 
    sort(decreasing = TRUE) %>%
    .[1:top]
  return(years)
}

#Get top countries
TopCountries <-  function(d, top = 5){
  Countries <- 
    table(d$Country) %>% 
    sort(decreasing = TRUE) %>%
    .[1:top]
  return(Countries)
}


#Year summary
Years <- function(d){
  return(table(d$PY))
}

#Average year of each cluster
#paper_avYear <- sapply(clusters_data, function(x){
#  aveYear <- round(mean(x$PY, na.rm = TRUE),1)
#  return(aveYear)
#})



##################################################################
##################################################################
#Plotting
generate_plot <- function(a_table){
  yy <- a_table
  xx <- names(yy)
  xx <- paste("", xx, sep = "")
  zz <- as.numeric(yy)
  p <- plot_ly(y=xx, x=zz, type="bar", orientation = "h", hoverinfo = "none")
  p <- layout(p,
              yaxis = list(categoryarray = rev(xx), 
                           categoryorder = "array",
                           tick = "outside"), 
              margin = list(l=300),
              annotations = list(x = zz, y = xx, text = zz,
                                 xanchor = 'left', yanchor = 'middle',
                                 showarrow = FALSE))
  return(p)
}

##################################################################
#End of the code. Go to (2/3) Knowledge_Dashboard_Preparation.R
