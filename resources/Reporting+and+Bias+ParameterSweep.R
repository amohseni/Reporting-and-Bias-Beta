# REPORTING AND BIAS
# by Aydin Mohseni

# Load packages
library(ggplot2)
library(ggthemes)
library(reshape2)
library(Bolstad)

# Set the working directory
getwd()
setwd("/Users/patience/Desktop/Images/Absolute2")

# Determine parameter values to test
TrueStateSD <- 1
TrueStateMeanVEC <- seq(from = -1, to = 1, by = 1)
BiasVEC <- seq(from = -1, to = 1, by = 1)
BiasStrengthVEC <- c(10 ^ -1, 1)
HyperboleVEC <- seq(from = 1, to = 2, by = 1)
ExtermityBiasVEC <- seq(from = 0, to = 1, by = 1)
FairAndBalancedVEC <- c(0, 1)
quantityOfEvidence <- 3

# Calculate the number of distinct parameter settings required
TotalDataPoints <-
  length(TrueStateMeanVEC) * length(BiasVEC) * length(BiasStrengthVEC) * length(ExtermityBiasVEC) * length(HyperboleVEC) * length(FairAndBalancedVEC)
SampleSize <- 10000

# Create the data frame in which to save the data
Df <- data.frame(matrix(
  data = NA,
  ncol = 8,
  nrow = TotalDataPoints,
  byrow = TRUE
))
colnames(Df) <-
  c(
    "TrueMean",
    "Bias",
    "BiasStrength",
    "Hyperbole",
    "ExtermityBias",
    "FaB",
    "BeliefMean",
    "BeliefVar"
  )
Df[, 1] <- rep(TrueStateMeanVEC,
               times = 1,
               each = TotalDataPoints / length(TrueStateMeanVEC))
Df[, 2] <- rep(BiasVEC,
               times = length(TrueStateMeanVEC),
               each = TotalDataPoints / (length(TrueStateMeanVEC) * length(BiasVEC)))
Df[, 3] <- rep(
  BiasStrengthVEC,
  times = length(TrueStateMeanVEC) * length(BiasVEC),
  each = length(HyperboleVEC) * length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
)
Df[, 4] <- rep(
  HyperboleVEC,
  times = TotalDataPoints / (
    length(HyperboleVEC) * length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
  ),
  each = length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
)
Df[, 5] <- rep(
  ExtermityBiasVEC,
  times = TotalDataPoints / (length(ExtermityBiasVEC) * length(FairAndBalancedVEC)),
  each = length(FairAndBalancedVEC)
)
Df[, 6] <- rep(FairAndBalancedVEC,
               times = TotalDataPoints / length(FairAndBalancedVEC),
               each = 1)

# Set up the for loops for the parameter sweep
for (i in 1:TotalDataPoints) {
  # Set model parameters
  TrueStateMean <- Df[i, 1]
  Bias <- Df[i, 2]
  BiasStrength <- Df[i, 3]
  Hyperbole <- Df[i, 4]
  ExtermityBias <- Df[i, 5]
  FairAndBalanced <- Df[i, 6]
  
  # Create the OBJECTIVE distribution of events for the world
  x <- seq(from = -10,
           to = 10,
           by = 0.1)
  WorldDistribution <-
    sapply(x, dnorm, mean = TrueStateMean, sd = TrueStateSD)
  NormalizingFactor <- sum(WorldDistribution)
  
  ### Determine the REPORTED distribution of events by news media
  NewsMean <- Hyperbole * TrueStateMean
  NewsSD <- Hyperbole ^ 2 * TrueStateSD
  NewsDistribution <-
    sapply(x, dnorm, mean = NewsMean, sd = NewsSD)
  NewsDistribution[which(-ExtermityBias < x &
                           x < ExtermityBias)] <- 0
  if (as.numeric(FairAndBalanced) == 1) {
    NewsDistribution[which(x < 0)] <-
      NewsDistribution[which(x < 0)] * (sum(NewsDistribution[which(x > 0)]) / (sum(NewsDistribution[which(x < 0)])))
  }
  NewsDistribution <-
    NewsDistribution * (NormalizingFactor / sum(NewsDistribution)) # renormalize
  Reported <-
    sample(x,
           size = SampleSize,
           prob = NewsDistribution,
           replace = TRUE)
  ReportedMean <- mean(Reported)
  ReportedSD <- sd(Reported)
  
  # Now, we determine the SUBJECTIVE distribution of events perceived by an individual
  # Sample (10^q) reports from the News Distribution to be observed by the agent
  SampleOfNewsReports <-
    sample(x,
           10 ^ (quantityOfEvidence),
           prob = NewsDistribution,
           replace = T)
  # The Bayesian update function which describes how agents learn from news reports
  # It take reports and prior beliefs about the mean and SD as inputs,
  # and has posterior beliefs aobut the population mean and SD as outputs.
  update <- function(priorMean, priorSD, report) {
    posteriorMean <-
      normnp(
        report,
        m.x = priorMean,
        s.x = priorSD,
        sigma.x = 1,
        mu = NULL,
        n.mu = 100,
        plot = FALSE
      )$mean
    posteriorSD <-
      normnp(
        report,
        m.x = priorMean,
        s.x = priorSD,
        sigma.x = 1,
        mu = NULL,
        n.mu = 100,
        plot = FALSE
      )$sd
  }
  
  # Given our updating function, individiual learning now proceeds as follows:
  # For each news report, the agent (1) decides whether to accept or reject it
  # which is determined by how close it is to her current belief.
  # (2) If she rejects the report, then her view remains unchanged.
  # If she accepts the report, then she updates her beliefs via Bayes rule.
  # (3) The process begins again with a new report and her new beliefs.
  for (obs in 1:length(SampleOfNewsReports)) {
    # Consider the news report
    report <- SampleOfNewsReports[obs]
    # And (1) Decide whether confirmation bias will allow you to update
    # If confirmation bias makes it so that the report is rejected,
    # then simply leave the belief state as is
    if (abs(priorMean - report) > abs(priorMean - rnorm(1, mean = priorMean, StrengthOfBias ^ -1))) {
      # Record that the report was rejected
      acceptRejectVector[obs] <- 0
      # Leave belief state as is
      updatedParams <- c(priorMean, priorSD)
    } else {
      # If conformation bias does not make it so that the report is rejected,
      # record that the report was rejected,
      acceptRejectVector[obs] <- 1
      # then update the belief state via Bayes' rule.
      updatedParams <- update(priorMean, priorSD, report)
    }
    # Update the belief means and SD for the next round of learning
    priorMean <- updatedParams[1]
    priorSD <- updatedParams[2]
    # Store thes in their respective vectors
    priorMeanVector[obs] <- priorMean
    priorSDVector[obs] <- priorSD
  }
  
  # Save the beliefs to the data frame
  Df[i, 7] <- PerceivedMean
}


# Correct Df for error
Df$BeliefMean <- (Df$BeliefMean - TrueStateMean)
# Df$BeliefVar <- (Df$BeliefSD - TrueStateSD)

# Plot Combinations of Parameters
for (i in 1:length(TrueStateMeanVEC)) {
  Mu <- TrueStateMeanVEC[i]
  for (j in 1:length(BiasVEC)) {
    b <- BiasVEC[j]
    for (k in 1:length(BiasStrengthVEC)) {
      s <- BiasStrengthVEC[k]
      for (l in 1:length(FairAndBalancedVEC)) {
        FaB <- FairAndBalancedVEC[l]
        # Having set a given combinatio of parameters,
        # open a PDF in which to save the plot
        pdf(paste("Mu=", Mu, "+b=", b, "+s=", s, "+FaB=", FaB, ".pdf", sep = ""))
        G <-
          ggplot(data = Df[Df$TrueMean == Mu &
                             # Create the plot for a given combinatio of parameters,
                             Df$Bias == b &
                             Df$BiasStrength == s &
                             Df$FaB == F1, ]) +
          geom_tile(aes(
            x = Hyperbole,
            y = ExtermityBias,
            fill = BeliefMean
          ),
          colour = "white") +
          theme_minimal() +
          scale_fill_gradient2(
            limits = c(-3, 3),
            name = "MSE",
            low = "black",
            mid = "white",
            high = "black",
            midpoint = 0
          ) +
          ggtitle(bquote(
            paste(
              mu,
              " = ",
              .(Mu),
              ", b = ",
              .(b),
              ", s = ",
              .(s),
              ", FaB = ",
              .(FaB),
              sep = " "
            )
          )) +
          labs(x = "h", y = "e") +
          scale_x_continuous(breaks = seq(1, 3, 1)) +
          scale_y_continuous(breaks = seq(0, 3, 1)) +
          theme(
            plot.title = element_text(
              hjust = 0.5,
              margin = margin(b = 10, unit = "pt"),
              lineheight = 1.15
            ),
            axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
            axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
            text = element_text(size = 16)
          )
        print(G) # Print the plot to the file
        dev.off() # Save and close the pdf file
      }
    }
  }
}

### EOD
