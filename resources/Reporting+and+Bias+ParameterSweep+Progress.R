# REPORTING AND BIAS
# by Aydin Mohseni

# Load packages
library(ggplot2)
library(ggthemes)
library(reshape2)
library(Bolstad)

# Set the working directory
getwd()
setwd("~/GitHub/Reporting-and-Bias-Beta/resources")

# Determine parameter values to test
Runs <- 10 # The number of runs of the simulation
TrueStateSD <- 1
TrueStateMeanVEC <- seq(from = -1, to = 1, by = 0.4)
BiasVEC <- seq(from = -1, to = 1, by = 0.4)
BiasStrengthVEC <- seq(from = 0, to = 2, by = 0.4)
HyperboleVEC <- seq(from = 1, to = 2, by = 0.2)
ExtermityBiasVEC <- seq(from = 0, to = 1, by = 0.2)
FairAndBalancedVEC <- c(0, 1)
QuantityOfEvidence <- 3

# Calculate the number of distinct parameter settings required
TotalNumberOfCases <-
  length(TrueStateMeanVEC) * length(BiasVEC) * length(BiasStrengthVEC) * length(ExtermityBiasVEC) * length(HyperboleVEC) * length(FairAndBalancedVEC)
SampleSize <- 10 ^ (QuantityOfEvidence + 2)

# Create the data frame in which to save the data
Df <- data.frame(matrix(
  data = NA,
  ncol = 12,
  nrow = TotalNumberOfCases,
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
    "BeliefVar",
    "BeliefMeanError",
    "BeliefVarError",
    "BeliefMeanMSE",
    "BeliefVarMSE"
  )
Df[, 1] <- rep(TrueStateMeanVEC,
               times = 1,
               each = TotalNumberOfCases / length(TrueStateMeanVEC))
Df[, 2] <- rep(BiasVEC,
               times = length(TrueStateMeanVEC),
               each = TotalNumberOfCases / (length(TrueStateMeanVEC) * length(BiasVEC)))
Df[, 3] <- rep(
  BiasStrengthVEC,
  times = length(TrueStateMeanVEC) * length(BiasVEC),
  each = length(HyperboleVEC) * length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
)
Df[, 4] <- rep(
  HyperboleVEC,
  times = TotalNumberOfCases / (
    length(HyperboleVEC) * length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
  ),
  each = length(ExtermityBiasVEC) * length(FairAndBalancedVEC)
)
Df[, 5] <- rep(
  ExtermityBiasVEC,
  times = TotalNumberOfCases / (length(ExtermityBiasVEC) * length(FairAndBalancedVEC)),
  each = length(FairAndBalancedVEC)
)
Df[, 6] <- rep(FairAndBalancedVEC,
               times = TotalNumberOfCases / length(FairAndBalancedVEC),
               each = 1)

# Set up the for loops for the PARAMETER SWEEP over the total number of cases
for (i in 1:TotalNumberOfCases) {
  # Set model parameters
  TrueStateMean <- Df[i, 1]
  Bias <- Df[i, 2]
  BiasStrength <- Df[i, 3]
  Hyperbole <- Df[i, 4]
  ExtermityBias <- Df[i, 5]
  FairAndBalanced <- Df[i, 6]
  
  ### Create the OBJECTIVE distribution of events for the world
  x <- seq(from = -10,
           to = 10,
           by = 0.1)
  WorldDistribution <-
    sapply(x, dnorm, mean = TrueStateMean, sd = TrueStateSD)
  NormalizingFactor <- sum(WorldDistribution)
  
  ### Determine the REPORTED distribution of events by news media
  NewsMean <- TrueStateMean
  NewsSD <- TrueStateSD
  # Apply hyperbole distortion
  NewsDistribution <-
    sapply(x,
           dnorm,
           mean = Hyperbole * NewsMean,
           sd = Hyperbole ^ 2 * NewsSD)
  # Apply extremity-bias distortion
  NewsDistribution[which(-ExtermityBias < x &
                           x < ExtermityBias)] <- 0
  # Apply fair-and-balanced distortion
  if (FairAndBalanced == 1) {
    NewsDistribution[which(x < 0)] <-
      NewsDistribution[which(x < 0)] * (sum(NewsDistribution[which(x > 0)]) / (sum(NewsDistribution[which(x < 0)])))
  }
  # Re-normalize distribution
  NewsDistribution <-
    NewsDistribution * (NormalizingFactor / sum(NewsDistribution)) # renormalize
  data <-
    sample(x,
           size = 10000,
           prob = NewsDistribution,
           replace = TRUE)
  # Output summary statistics
  ReportedMean <- mean(data)
  ReportedSD <- sd(data)
  
  # Now, we determine the SUBJECTIVE distribution of events perceived by an individual
  # First, set the initial values for the agent's beliefs
  priorMean <- Bias
  priorSD <- TrueStateSD
  
  # The Bayesian update function which describes how agents learn from news reports
  # It take reports and prior beliefs about the mean and SD as inputs,
  # and has posterior beliefs aobut the population mean and SD as outputs.
  update <- function(priorMean, priorSD, report) {
    x <- normnp(
      report,
      m.x = priorMean,
      s.x = priorSD,
      sigma.x = 1,
      mu = NULL,
      n.mu = 100,
      plot = FALSE
    )
    posteriorSD <- x$sd
    posteriorMean <- x$mean
    return(c(posteriorMean, posteriorSD))
  }
  
  # Create a vector in which to save the outcomes of each run of the learning process
  meanPerceptionVec <- rep(NA, Runs)
  sdPerceptionVec <- rep(NA, Runs)
  
  # Run the learning process 'Runs'-many times
  for (r in 1:Runs) {
    # Sample data points from the News Distribution to be observed by the agent
    SampleOfNewsReports <-
      sample(x,
             SampleSize,
             prob = NewsDistribution,
             replace = T)
    
    # And create a vector in which to store the means and SDs of the belief state
    priorMeanVector <- rep(NA, SampleSize)
    priorSDVector <- rep(NA, SampleSize)
    acceptRejectVector <- rep(NA, SampleSize)
    
    # Given our updating function, individiual learning now proceeds as follows:
    # For each news report, 
    # (1) the agent decides whether to accept or reject it
    # which is determined by how close the report is to her current belief;
    # (2) if she rejects the report, then her view remains unchanged,
    # and if she accepts the report, then she updates her beliefs via Bayes rule;
    # (3) the process begins again with a new report and her new beliefs.
    
    # But first, set the initial state of the counters
    # for the number of pieces of evidence considered
    # and the number of pieces of evdience update upon.
    reportsConsidered <- 0
    reportsUpdatedOn <- 0
    
    # Now, keep exposing the agent to evidence
    # until she has updated the required number of times
    while (reportsUpdatedOn < (10 ^ QuantityOfEvidence) &
           reportsConsidered < SampleSize) {
      # Increment the count for pieces of evidence considered
      reportsConsidered <- reportsConsidered + 1
      # Determine the index of the current data point to consider
      obs <- reportsConsidered
      
      # Consider the news report
      report <- SampleOfNewsReports[obs]
      
      # First, check if strength of bias = 0.
      # If so, the agent simply accepts the data.
      if (BiasStrength == 0) {
        # Record that the report was accepted,
        acceptRejectVector[obs] <- 1
        
        # Increment the count for pieces of evidence updated upon
        reportsUpdatedOn <- reportsUpdatedOn + 1
        
        # And update the belief state via Bayes' rule.
        updatedParams <- update(priorMean, priorSD, report)
        
      } else {
        # If there is confirmation bias, then
        # (1) Decide whether confirmation bias allows the agent to update.
        
        # If confirmation bias makes it so that the report is rejected,
        # Then simply leave the agent belief state as is.
        if (abs(priorMean - report) > abs(priorMean - rnorm(1, mean = priorMean, BiasStrength ^ -1))) {
          # Record that the report was rejected
          acceptRejectVector[obs] <- 0
          
          # And leave belief state as is
          updatedParams <- c(priorMean, priorSD)
          
        } else {
          # If conformation bias does not make it so that the report is rejected,
          # record that the report was accpeted.
          acceptRejectVector[obs] <- 1
          
          # Increment the count for pieces of evidence updated upon.
          reportsUpdatedOn <- reportsUpdatedOn + 1
          
          # Then update the belief state via Bayes' rule.
          updatedParams <- update(priorMean, priorSD, report)
        }
      }
      # Update the belief means and SD for the next round of learning.
      priorMean <- updatedParams[1]
      priorSD <- updatedParams[2]
      
      # Store these in their respective vectors.
      priorMeanVector[obs] <- priorMean
      priorSDVector[obs] <- priorSD
    }
    
    # Determine which reports were accepted,
    # and use these to calculate the Bayesian estimator of the SD of the accpeted reports
    whichReportsAccepted <- which(acceptRejectVector %in% c(1))
    
    # Record the final resulting posterior mean and SD for the run
    # and save these to their respective data vectors
    meanPerceptionVec[r] <-
      priorMeanVector[which.max(whichReportsAccepted)]
    sdPerceptionVec[r] <- sd(SampleOfNewsReports[whichReportsAccepted])
  }
  # Save the average across all runs for the case into the data frame
  Df[i, 7] <- mean(meanPerceptionVec)
  Df[i, 8] <- mean(sdPerceptionVec)
  # Print the progress of the algorithm
  paste("Run number ", i, " of ", TotalNumberOfCases, " completed.", sep = "")
}

# Calculate the errors in belief given the true states and agent beliefs
Df$BeliefMeanError <- (Df$BeliefMean - Df$TrueMean)
Df$BeliefVarError <- (Df$BeliefVar - TrueStateSD)
Df$BeliefMeanMSE <- Df$BeliefMeanError ^ 2
Df$BeliefVarMSE <- Df$BeliefVarError ^ 2
Df <- round(Df, digits = 3)

# Print all the results as a CSV file
write.csv(Df, file = "AllData.csv", row.names = FALSE)

round(cor(Df), digits = 2)
Df[Df$Hyperbole == 1 , ]
ggplot(data = Df) +
  geom_line(aes(x = Hyperbole,
                y = BeliefMeanError,
                color = BeliefMeanError),
            colour = "white") +
  theme_minimal() +
  ggtitle("Hyperbole and Error") +
  labs(x = "Hyperbole", y = "Belief Mean Error") +
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

# # Plot MSE for means for each combinations of parameters
# for (i in 1:length(TrueStateMeanVEC)) {
#   Mu <- TrueStateMeanVEC[i]
#   for (j in 1:length(BiasVEC)) {
#     b <- BiasVEC[j]
#     for (k in 1:length(BiasStrengthVEC)) {
#       s <- BiasStrengthVEC[k]
#       for (l in 1:length(FairAndBalancedVEC)) {
#         FaB <- FairAndBalancedVEC[l]
#         # Having set a given combinatio of parameters,
#         # open a PDF in which to save the plot
#         pdf(paste(
#           "MeanMSE+",
#           "Mu=",
#           Mu,
#           "+b=",
#           b,
#           "+s=",
#           s,
#           "+FaB=",
#           FaB,
#           ".pdf",
#           sep = ""
#         ))
#         G <-
#           ggplot(data = Df[Df$TrueMean == Mu &
#                              # Create the plot for a given combinatio of parameters,
#                              Df$Bias == b &
#                              Df$BiasStrength == s &
#                              Df$FaB == FaB, ]) +
#           geom_tile(aes(
#             x = Hyperbole,
#             y = ExtermityBias,
#             fill = BeliefMean
#           ),
#           colour = "white") +
#           theme_minimal() +
#           scale_fill_gradient2(
#             limits = c(-3, 3),
#             name = "MSE",
#             low = "black",
#             mid = "white",
#             high = "black",
#             midpoint = 0
#           ) +
#           ggtitle(bquote(
#             paste(
#               mu,
#               " = ",
#               .(Mu),
#               ", b = ",
#               .(b),
#               ", s = ",
#               .(s),
#               ", FaB = ",
#               .(FaB),
#               sep = " "
#             )
#           )) +
#           labs(x = "h", y = "e") +
#           scale_x_continuous(breaks = seq(1, 3, 1)) +
#           scale_y_continuous(breaks = seq(0, 3, 1)) +
#           theme(
#             plot.title = element_text(
#               hjust = 0.5,
#               margin = margin(b = 10, unit = "pt"),
#               lineheight = 1.15
#             ),
#             axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
#             axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
#             text = element_text(size = 16)
#           )
#         print(G) # Print the plot to the file
#         dev.off() # Save and close the pdf file
#       }
#     }
#   }
# }
#
# # Plot MSE for variances for each combinations of parameters
# for (i in 1:length(TrueStateMeanVEC)) {
#   Mu <- TrueStateMeanVEC[i]
#   for (j in 1:length(BiasVEC)) {
#     b <- BiasVEC[j]
#     for (k in 1:length(BiasStrengthVEC)) {
#       s <- BiasStrengthVEC[k]
#       for (l in 1:length(FairAndBalancedVEC)) {
#         FaB <- FairAndBalancedVEC[l]
#         # Having set a given combinatio of parameters,
#         # open a PDF in which to save the plot
#         pdf(paste(
#           "VarMSE+",
#           "Mu=",
#           Mu,
#           "+b=",
#           b,
#           "+s=",
#           s,
#           "+FaB=",
#           FaB,
#           ".pdf",
#           sep = ""
#         ))
#         G <-
#           ggplot(data = Df[Df$TrueMean == Mu &
#                              # Create the plot for a given combinatio of parameters,
#                              Df$Bias == b &
#                              Df$BiasStrength == s &
#                              Df$FaB == FaB, ]) +
#           geom_tile(aes(
#             x = Hyperbole,
#             y = ExtermityBias,
#             fill = BeliefVar
#           ),
#           colour = "white") +
#           theme_minimal() +
#           scale_fill_gradient2(
#             limits = c(-3, 3),
#             name = "MSE",
#             low = "black",
#             mid = "white",
#             high = "black",
#             midpoint = 0
#           ) +
#           ggtitle(bquote(
#             paste(
#               mu,
#               " = ",
#               .(Mu),
#               ", b = ",
#               .(b),
#               ", s = ",
#               .(s),
#               ", FaB = ",
#               .(FaB),
#               sep = " "
#             )
#           )) +
#           labs(x = "h", y = "e") +
#           scale_x_continuous(breaks = seq(1, 3, 1)) +
#           scale_y_continuous(breaks = seq(0, 3, 1)) +
#           theme(
#             plot.title = element_text(
#               hjust = 0.5,
#               margin = margin(b = 10, unit = "pt"),
#               lineheight = 1.15
#             ),
#             axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
#             axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
#             text = element_text(size = 16)
#           )
#         print(G) # Print the plot to the file
#         dev.off() # Save and close the pdf file
#       }
#     }
#   }
# }

### EOD

# Quick test of while loop
# x <- 0
# y <- 0
# while(x <= 10 & y <= 20) {
#   z <- runif(1, min = 0, max = 1)
#   if (z > 0.5) {
#     x <- x + 1
#     y <- y + 1
#   } else {
#     y <- y + 1
#   }
# }
# print(paste("x = ", x, "; y = ", y, ".", sep = ""))
