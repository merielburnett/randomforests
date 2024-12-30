library(tidyverse)
library(psych)
library(isotree)


#
# isolation forests----

## normal distribution test----

# proof of concept using a normal distribution
znum <- data.frame("zs" = rnorm(10000,0,1))

iso_z <- isotree::isolation.forest(data= znum, 
                                   ntrees=500, 
                                   ndim = 1,     # classic isotree (n > 1 extended)
                                   scoring_metric = "depth",
                                   output_score = T)
describe(iso_z$scores)
hist(iso_z$scores)
plot(znum$zs, iso_z$scores)


## appying to magic data----

# set up a smaller dataset
ms2 <- read_csv("realmagic_s2.csv")

isodat <- ms2 %>%
  select(2:16,18,21,22,23) %>% 
  drop_na()
describe(isodat)

iso1 <- isotree::isolation.forest(data=isodat,
                                  ntrees=1000, 
                                  max_depth = 
                                  ndim = 1,
                                  scoring_metric = "depth")


iso2 <- isotree::isolation.forest(data=isodat, 
                                  ntrees=1000, 
                                  ndim = 2,        # extended isolation forest
                                  scoring_metric = "depth")


iso_out <- data.frame("iso_depth" = predict.isolation_forest(iso1, isodat),
                      "iso_extended" = predict.isolation_forest(iso2, isodat),
                      isodat)

describe(iso_out)
lowerCor(iso_out)

hist(iso_out$iso_depth)
hist(iso_out$iso_extended)


#
# iso forests for participant similarity matrix----

## toy data: 2 equal groups, different corrs
library(faux)

rr1<-faux::rnorm_multi(n=600, vars = 2, varnames = c("x1","x2"),
                       mu = -1, sd = 1, r = .6)
rr2<-faux::rnorm_multi(n=300, vars = 2, varnames = c("x1","x2"),
                       mu = 3, sd = 2, r = -.7)

rr_sim<-bind_rows(rr1, rr2)
describe(rr_sim)

# check iso forest
iso_rr <- isolation.forest(data=rr_sim,
                            seed = 1213,
                            sample_size = NULL,
                            ntrees=100, 
                            ndim = 1,
                            output_score = T,
                            output_dist = T,
                            square_dist = T,
                            scoring_metric = "depth")

# distance score
iso_rr$scores %>% hist()

# similarity matrix
View(iso_rr$dist)

rr_prox <- iso_rr$dist


#
## clustering methods----

# follows "hands-on machine learning" book

## k-means clustering----
# https://bradleyboehmke.github.io/HOML/kmeans.html

c1<-stats::kmeans(rr_prox, centers = 2, nstart = 30)

plot(rr_sim$x1, rr_sim$x2, col=c1$cluster)

rr_sim1 <- data.frame(rr_sim, 
                      "cluster" = factor(c1$cluster))

ggplot(rr_sim1, aes(x=x1, y=x2, group=cluster, color=cluster))+
  geom_point(size=3, alpha=.5)

factoextra::fviz_cluster(c1, data=rr_sim)

# how many k clusters?
library(factoextra)
library(cluster)  # for pam

fviz <- fviz_nbclust(rr_sim, 
                     kmeans, # kmeans or pam
                     k.max=6,
                     method = "silhouette") # try "wss" and "silhouette"
fviz


## model-based clustering----
# https://bradleyboehmke.github.io/HOML/model-clustering.html

library(mclust)

rr_m <- Mclust(rr_sim, 1:5) # search all types, 1 to 5 clusters
summary(rr_m)

plot(rr_m, what="BIC")
plot(rr_m, what="uncertainty")


# model-based LOL classes?
loldat <- ms2 %>% select(2:16,18,21,23) %>% drop_na() %>% scale()
describe(loldat)

lol_m <- Mclust(loldat, 1:8)
summary(lol_m)

# classification
lol_m$classification
table(lol_m$classification)

# probability of cluster membership
lol_m$uncertainty  # i think this is 1-prob of most likely class
describe(lol_m$uncertainty)


loldat1 <- data.frame("cluster" = factor(lol_m$classification),
                      loldat)

loldat_long <- loldat1 %>% pivot_longer(cols=2:19, 
                            names_to = "Trait", values_to = "Score")

loldat_long %>%
  group_by(Trait) %>%
  filter(cluster==1) %>%
  summarize(avg = mean(Score)) %>%
  ggplot(aes(x=avg, y=Trait))+
  geom_point(color="blue", size=3, alpha=.5)+
  labs(title="Cluster 1")

loldat_long %>%
  group_by(Trait) %>%
  filter(cluster==2) %>%
  summarize(avg = mean(Score)) %>%
  ggplot(aes(x=avg, y=Trait))+
  geom_point(color="black", size=3, alpha=.5)+
  labs(title="Cluster 2")

