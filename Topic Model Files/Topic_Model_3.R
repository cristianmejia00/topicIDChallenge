# 20160315
# Topic Model 3/3
# The topic model.

#####################################################
#Select the number of topics
K <- 36

#####################################################
# Fit the model:
set.seed(100)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  #For 11515 vocab, +1500 docs, 49topics, G=5000 iter, takes 2.5 hours

# The visualizer automatically reorder topics from the largest.
# To stay with the output of LDA we can add reorder.topics = FALSE in the parameters
######################################################
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))) #DxK, document topic matrix
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))) # KxW, topic word matrix 

# Write the topic model table outputs
if (fullReports) {
  write.csv(phi, file="KXW.csv", row.names = TRUE)
  write.csv(theta, file="DXK.csv", row.names = TRUE)
}

#####################################################
# Visualization
# create the JSON object to feed the visualization:
model_parameters <- list(phi = phi,
                         theta = theta,
                         doc.length = doc.length,
                         vocab = vocab,
                         term.frequency = term.frequency)

json <- createJSON(phi = model_parameters$phi, 
                   theta = model_parameters$theta, 
                   doc.length = model_parameters$doc.length, 
                   vocab = model_parameters$vocab, 
                   term.frequency = model_parameters$term.frequency)

serVis(json, out.dir = pName, open.browser = TRUE)

#####################################################
# END
