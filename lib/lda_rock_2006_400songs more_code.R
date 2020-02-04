## run LDA for all rock songs in 2006 and artists who had more than 400 songs in total
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# number of topics
k <- 10

# run LDA using Gibbs sampling
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, 
                                                 seed = seed, best=best,
                                                  burnin = burnin, iter = iter, 
                                                  thin=thin))

# write out results
#save(ldaOut,file = "C:/Users/Saier/Desktop/courses/5243/ldaOut.RData")