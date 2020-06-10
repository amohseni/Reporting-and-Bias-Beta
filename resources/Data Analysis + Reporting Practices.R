# Data Analysis | Reporting Practices

# Import data:
myData <- read.csv("AllData.csv", header = TRUE)
head(myData)

### Correlations:
correlationMatrix <- round(cor(myData), digits = 2)

# Bias.Strength & Belief-Mean.MSE = 0.43
correlationMatrix[4, 12]
# Hyperbole & Belief-Mean.MSE = 0.33
correlationMatrix[5, 12]
# Extermity Bias & Belief-Mean.MSE = 0.37
correlationMatrix[6, 12]
# Fair+Balanced & Belief-Mean.MSE = 0.09
correlationMatrix[7, 12]

# Bias.Strength & Belief.Var.Error = 0.43
correlationMatrix[4, 10]
# Hyperbole & Belief.Var.Error = 0.33
correlationMatrix[5, 10]
# Extermity Bias & Belief.Var.Error = 0.37
correlationMatrix[6, 10]
# Fair+Balanced & Belief.Var.Error = 0.09
correlationMatrix[7, 10]

colnames(myData)


### Conditional Correlation (Bias.Strength = 1):
conditionalcorrelationMatrix1 <- round(cor(myData[which(myData$Bias.Strength == 1),]), digits = 2)

# Bias.Strength & Belief-Mean.MSE = 0.43
conditionalcorrelationMatrix1[4, 12]
# Hyperbole & Belief-Mean.MSE = 0.33
conditionalcorrelationMatrix1[5, 12]
# Extermity Bias & Belief-Mean.MSE = 0.37
conditionalcorrelationMatrix1[6, 12]
# Fair+Balanced & Belief-Mean.MSE = 0.09
conditionalcorrelationMatrix1[7, 12]

# Bias.Strength & Belief.Var.Error = 0.43
conditionalcorrelationMatrix1[4, 10]
# Hyperbole & Belief.Var.Error = 0.33
conditionalcorrelationMatrix1[5, 10]
# Extermity Bias & Belief.Var.Error = 0.37
conditionalcorrelationMatrix1[6, 10]
# Fair+Balanced & Belief.Var.Error = 0.09
conditionalcorrelationMatrix1[7, 10]

### Conditional Correlation (Bias.Strength = 0.1):
conditionalcorrelationMatrix0 <- round(cor(myData[which(myData$Bias.Strength == 0.1),]), digits = 2)

# Bias.Strength & Belief-Mean.MSE = 0.43
conditionalcorrelationMatrix0[4, 12]
# Hyperbole & Belief-Mean.MSE = 0.33
conditionalcorrelationMatrix0[5, 12]
# Extermity Bias & Belief-Mean.MSE = 0.37
conditionalcorrelationMatrix0[6, 12]
# Fair+Balanced & Belief-Mean.MSE = 0.09
conditionalcorrelationMatrix0[7, 12]

# Bias.Strength & Belief.Var.Error = 0.43
conditionalcorrelationMatrix0[4, 10]
# Hyperbole & Belief.Var.Error = 0.33
conditionalcorrelationMatrix0[5, 10]
# Extermity Bias & Belief.Var.Error = 0.37
conditionalcorrelationMatrix0[6, 10]
# Fair+Balanced & Belief.Var.Error = 0.09
conditionalcorrelationMatrix0[7, 10]
