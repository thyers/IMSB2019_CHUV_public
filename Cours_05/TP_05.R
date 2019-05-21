library(tidyverse)
library(brms)
library(bayesplot)
library(gridExtra)
library(hrbrthemes)
library(gridExtra)

set.seed(8)

# génération de données normalement distribuées
y <- rnorm(100, mean = 0, sd = 1)

# modèle avec un prioir uniforme pour les beta
b5.1 <- brm(
  data = list(y           = y,
              intercept_1 = 1,
              intercept_2 = 1), 
  family = gaussian,
  y ~ 0 + intercept_1 + intercept_2,
  prior = c(prior(uniform(-1e10, 1e10), class = b),
            prior(cauchy(0, 1), class = sigma)),
  inits = list(list(intercept_1 = 0, intercept_2 = 0, sigma = 1),
               list(intercept_1 = 0, intercept_2 = 0, sigma = 1)),
  iter = 4000, warmup = 1000, chains = 2,
  seed = 8)
print(b5.1)

b5.2 <-
  brm(data = list(y           = y,
                  intercept_1 = 1,
                  intercept_2 = 1),
      family = gaussian,
      y ~ 0 + intercept_1 + intercept_2,
      prior = c(prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      inits = list(list(intercept_1 = 0, intercept_2 = 0, sigma = 1),
                   list(intercept_1 = 0, intercept_2 = 0, sigma = 1)),
      iter = 4000, warmup = 1000, chains = 2,
      seed = 8)

# sauvegarde du graph dans la colonne de gauche
post <- posterior_samples(b5.1, add_chain = T)
left_column <-
  mcmc_trace(post[, c(1:3, 5)],
             size = .25,
             facet_args = c(ncol = 1)) +
  scale_color_ipsum() +
  theme_ipsum() +
  theme(legend.position  = c(.85, 1.5),
        legend.direction = "horizontal")

# sauvegarde du graph dans la colonne de droite
post <- posterior_samples(b5.2, add_chain = T)
right_column <-
  mcmc_trace(post[, c(1:3, 5)],
             size = .25,
             facet_args = c(ncol = 1)) +
  scale_color_ipsum() +
  theme_ipsum() +
  theme(legend.position  = c(.85, 1.5),
        legend.direction = "horizontal")

# affichage
grid.arrange(left_column, right_column, ncol = 2)
