#20170501 #20161221
#Knowledge Dashboard Preparation 
#(2/3)

#Codes to make analysis similar to FS, but using different algorithms
#There are 3 files:
#1- 01-widgets.R to run the functions
#2- This code 02-preparation. To read and prepare data
#3- 03-launch

#Here we take the inputs
#astro-ALP-2003-2010.csv
#direct_citations.txt

##############################################################
##############################################################
#Select the "mission.pairs.tsv" file.
g1 = read.graph(file.choose(), format = "ncol")

#get max component
cl <- decompose(g1) #Decompose into connected and unconnected components
g1 <- cl[[which.max(sapply(cl, vcount))]] #extract largest component

#Verify some properties of the graph
is.directed(g1) #Confirms that the network is directed
is.weighted(g1) #Confirms that the network is weighted
ecount(g1) #Return the number of edges
vcount(g1) #Return the number of vertices


#Read the table containing the vertex attributes "mission.facet.all.tsv"
data = read.csv(file.choose(), header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE)

#Put country names
data$Country <- getCountry(data)
###############################################################
###############################################################
#Get communities
com <- clusterize(g1)
n_clusters <- sort(unique(com))
#Get summary of number of nodes and edges for each community
v_e <- v_and_e(g1, com)

#Get degrees of each vertex
degrees <- degree(g1, V(g1))

#Order vector of communities and vector of degrees as they appear in the dataset
vertex <- names(V(g1))

#backup the original data, and remove disconnected components
all_data <- data
data <- data[data$UT %in% vertex, ]

#Reorder communities and degrees vector
ids <- data$UT
communities <-com[order(match(vertex, ids))]
degrees <- degrees[order(match(vertex, ids))]

#Add the to the dataset
data$community <- communities
data$degrees <- degrees

###############################################################
###############################################################
#End of the code. Go to (3/3) KD_Shiny_Dash
