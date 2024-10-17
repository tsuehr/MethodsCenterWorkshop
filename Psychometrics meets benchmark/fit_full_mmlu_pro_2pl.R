# Clear the environment
rm(list = ls())
# Close all open graphics devices
graphics.off()

# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


############
# see: https://chatgpt.com/share/670fed05-7be4-8007-9b0e-8efce34610a2



#########################################################################################################

# see: https://www.r-bloggers.com/2012/12/simple-data-simulator-for-the-2pl-model/

twopl.sim         <- function( nitem = 20, npers = 100 ) {

  i.loc         <- rnorm( n = nitem, mean = 0, sd = 1 )
  p.loc         <- rnorm( n = npers, mean = 0, sd = 1 )
  i.slp         <- rlnorm( nitem, sdlog = .4 )

  temp          <- matrix( rep( p.loc, length( i.loc ) ), ncol = length( i.loc ) )

  logits        <- t( apply( temp  , 1, '-', i.loc) )
  logits        <- t( apply( logits, 1, '*', i.slp) )

  probabilities <- 1 / ( 1 + exp( -logits ) )

  resp.prob     <- matrix( probabilities, ncol = nitem)

  obs.resp      <- matrix( sapply( c(resp.prob), rbinom, n = 1, size = 1), ncol = length(i.loc) )

  output        <- list()
  output$i.loc  <- i.loc
  output$i.slp  <- i.slp
  output$p.loc  <- p.loc
  output$resp   <- obs.resp

  output
}





#install.packages('irtoys')

# Note: 'irtoys' might not load right away in OSX. You can try the following sequence in that case:
# install.packages('mvtnorm')
# install.packages('msm')
# install.packages('ltm',type='source')
# install.packages('irtoys',type='source')

library(mirt)

###### Running Simulation ######

data2pl <- twopl.sim(nitem=300,npers=2200)


# Fit 2PL model
fit2PL <- mirt(data = as.data.frame(data2pl$resp),
               model = 1,  #one-dimensional
               itemtype = "2PL")


item_parameters_2PL <- coef(fit2PL, IRTpars = TRUE, simplify = TRUE)$items

# item difficulty param
summary(item_parameters_2PL[,"b"])


# item discrimination param
summary(item_parameters_2PL[,"a"])



layout(matrix(c(1,2),nrow=1))

plot(item_parameters_2PL[,"a"], data2pl$i.slp)
cor(item_parameters_2PL[,"a"], data2pl$i.slp)

plot(item_parameters_2PL[,"b"], data2pl$i.loc)
cor(item_parameters_2PL[,"b"], data2pl$i.loc)


sim.loc.param <- data2pl$i.loc
sim.slp.param <- data2pl$i.slp

###### Estimation Using irtoys ######

analysis.2pl  <- est(data2pl$resp, model = '2PL', engine = 'ltm')

est.slp.param <- analysis.2pl[,1]
est.loc.param <- analysis.2pl[,2]






data_matrix <- as.data.frame(data2pl$resp)[,1:20]  # Replace with your actual data
polychoric_matrix <- polycor::hetcor(data_matrix)$correlations

psych::corPlot(r = polychoric_matrix)

psych::fa.parallel(polychoric_matrix, fa = "fa", n.obs = nrow(data_matrix), cor = "poly")

efa_results <- psych::fa(polychoric_matrix, nfactors = 8, fm = "ml")  # You can replace "ml" with other methods like "pa" for principal axis factoring
efa_results

print(efa_results$loadings)


#########################################################################################################










#####################
# load packages
#####################
# > This function is a wrapper for library and require. It checks to see if a package is installed, if not it attempts to install the package

require(pacman)
p_load('mirt', 'ggplot2', 'dplyr', 'tidyr', 'parallel')
# instead of 'ggplot2', 'dplyr', 'tidyr' you could load tidyverse

#####################
# load, prepare data
#####################
setwd("data")
data <- read.csv("mmlu_data.csv")
data <- select(data, -"model")


data <- spread(data, doc_id, acc)
data <- select(data, -"model_id")

#############
library(psych)
library(polycor)


data_matrix <- data[,1:40]  # Replace with your actual data
polychoric_matrix <- polycor::hetcor(data_matrix)$correlations

psych::corPlot(r = polychoric_matrix)
psych::corPlot(r = cor(data_matrix)) # simply use Phi coefficient

psych::fa.parallel(polychoric_matrix, fa = "fa", n.obs = nrow(data_matrix), cor = "poly")

efa_results <- psych::fa(polychoric_matrix, nfactors = 8, fm = "ml")  # You can replace "ml" with other methods like "pa" for principal axis factoring
efa_results

print(efa_results$loadings)
###############





# Mean and variance of each item
items_var <- data %>%
  group_by(doc_id) %>%
  summarize(variance = var(acc))

hist(items_var$variance)
abline(v = 0.20, col = "red")
sum(items_var$variance > 0.20) / nrow(items_var)

items_to_remove <- data %>%
  group_by(doc_id) %>%
  summarize(variance = var(acc)) %>%
  filter(variance <= 0.10) %>%
  pull(doc_id)

data <- data %>%
  filter(!doc_id %in% items_to_remove)

data <- spread(data, doc_id, acc)
data <- select(data, -"model_id")



items_mean_var <- apply(data, MARGIN = 2, FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = var(x, na.rm = TRUE)))

plot(items_mean_var[1,], items_mean_var[2,])


# proportion of correct responses for item
hist(as.numeric(colSums(x = data) / nrow(data)))

# proportion of correct responses for model
hist(as.numeric(rowSums(x = data) / ncol(data)))


data <- data[, colSums(x = data) >= 10]


####################
# Get observed response patterns
data$pattern <- apply(data, 1, paste, collapse = "_")
pattern_frequency <- table(data$pattern)
pattern_frequency_data <- as.data.frame(pattern_frequency)
colnames(pattern_frequency_data) <- c("Pattern", "Frequency")
pattern_frequency_data <- pattern_frequency_data[order(-pattern_frequency_data$Frequency), ]

table(pattern_frequency_data$Frequency)

stringdist::stringdist(a = data$pattern[1:3], b = data$pattern[22])


########
# sub-sample items
########
setSize <- 30
random_items <- sample(x = 1:ncol(data), size = setSize, replace = FALSE)
sub_data <- data[, random_items]
# sub_data <- data[, 1:30]


# Fit 1PL model (Rasch model)
fit1PLa <- mirt(data = sub_data,
               model = 1,  # one-dimensional model
               itemtype = "Rasch")

# Fit 2PL model
fit2PLa <- mirt(data = sub_data,
               model = 1,  #one-dimensional
               itemtype = "2PL",
               technical = list(NCYCLES = 2000, SEtol = 1e-4))
summary(fit2PLa)


# Calculate item fit statistics for the 2PL model
item_fit <- itemfit(fit2PLa)
item_fit[order(item_fit$S_X2),]

####



#############
response_patterns <- personfit(fit2PLa, stats.only = TRUE)

# Display the response patterns
head(response_patterns$pattern)
#####


# Extract residuals for items
residuals_2PL <- residuals(fit2PLa, type = "Q3")
print(residuals_2PL)

# compute a likelihood ratio test
anova(fit1PLa, fit2PLa)

item_parameters_1PL <- coef(fit1PLa, IRTpars = TRUE, simplify = TRUE)$items
item_parameters_2PL <- coef(fit2PLa, IRTpars = TRUE, simplify = TRUE)$items

# item difficulty param
#> 1PL
summary(item_parameters_1PL[,"b"])
#> 2PL
summary(item_parameters_2PL[,"b"])


checkCols <- rownames(item_parameters_2PL)[abs(item_parameters_2PL[,"b"]) > 70]



# item discrimination param
#> 1PL: X
#> 2PL
summary(item_parameters_2PL[,"a"])
var(item_parameters_2PL[,"a"])

plot(item_parameters_2PL[,"b"], item_parameters_2PL[,"a"])


theta <- seq(-3, 3, length.out = 100) # its treated like a z-score kind of and normally plotted from -3, 3


plot_data <- data.frame()

for (i in 1:30) { # nrow(item_parameters)
  a <- item_parameters_2PL[i, "a"]
  if (a < 0){ #if slope of ICC is <0, filter item out
    next
  }
  b <- item_parameters_2PL[i, "b"]
  p <- 1 / (1 + exp(-a * (theta - b)))
  plot_data <- rbind(plot_data, data.frame(theta = theta, probability = p, item = paste("Item", i)))
}
ggplot(plot_data, aes(x = theta, y = probability, color = item)) +
  geom_line() +
  labs(title = "Item Characteristic Curves of MMLU Pro Items (after filtering) - N=5525 items", x = "Ability (theta)", y = "Probability of Correct Response") +
  theme_minimal() +
  theme(legend.position = "none")

plot(fit2PLa, type = 'info')







#####################
# run analysis
#####################
# n_cores <-detectCores() - 1
# mirtCluster(n_cores)


# Fit 1PL model (Rasch model)
fit1PL <- mirt(data = data,
               model = 1,  # one-dimensional model
               itemtype = "Rasch")

# Fit 2PL model
fit2PL <- mirt(data = data,
               model = 1,  #one-dimensional
               itemtype = "2PL")

# Fit 2PL model - change estimator
fit2PL <- mirt(data = data,
               model = 1,  #one-dimensional
               itemtype = "2PL",
               optimizer = "nlminb")


# Fit 2PL model - change optimizer, settings
fit2PL <- mirt(data = data,
               model = 1,  #one-dimensional
               itemtype = "2PL",
               optimizer = "BFGS",
               control = list(maxit = 1000))


item_parameters <- coef(fit2PL, IRTpars = TRUE, simplify = TRUE)$items
# !! no variance
summary(item_parameters[,"a"])
var(item_parameters[,"a"])






