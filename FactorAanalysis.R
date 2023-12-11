library(tidyr)
library(lavaan)
library(bruceR)
library(readxl)
library(dplyr)
library(tidyr)

# Generate sample data
set.seed(123)
num_rows <- 100
num_cols <- 15

column_names <- c(
  paste0("Fluency", 1:3),
  paste0("Originality", 1:3),
  paste0("Elaboration", 1:3),
  paste0("Abstractness", 1:3),
  paste0("Resistance", 1:3)
)

sample_data <- as.data.frame(matrix(rnorm(num_rows * num_cols), nrow = num_rows, dimnames = list(NULL, column_names)))

head(sample_data)

# Reshape the dataframe
reshaped_data <- sample_data %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "set"),
               names_pattern = "([A-Za-z]+)(\\d+)",
               values_drop_na = TRUE)

head(reshaped_data)

columns_to_normalize <- c("Fluency", "Originality", "Elaboration", "Abstractness", "Resistance")

reshaped_data <- reshaped_data %>%
  mutate(across(all_of(columns_to_normalize), scale, center = TRUE, scale = TRUE))

str(reshaped_data)

# Calculate Creativity
sum_data <- reshaped_data %>%
  mutate(Creativity = rowSums(select(., Fluency, Originality, Elaboration, Abstractness, Resistance), na.rm = TRUE))

head(sum_data)


# Validate factor structure
install.packages('EFA.dimensions')
install.packages('psych')
library(EFA.dimensions)
library(psych)


# Test the facorability
corr.test(sum_data)

FACTORABILITY(sum_data)

# Determine the no. of factors
RAWPAR(sum_data)

MAP(sum_data)

SCREE_PLOT(sum_data)

SMT(sum_data)

EMPKC(sum_data)

fa(sum_data, nfactors = 2)
```

```{r}
# EFA
efamodel <- fa(sum_data, nfactors = 2, fm = "minres")

efamodel
head(efamodel$scores,13)
fa.diagram(efamodel, main = "sum_data")
pairs.panels(efamodel$scores)
fa.parallel(x = transformed_data,fa = "fa")
```

```{r}
#CFA
Creativity.model<-'Innovative =~ Fluency + Originality
                  Adaptive =~ Elaboration + Resistance + Abstractness'


fit<-cfa(Creativity.model,data = sum_data)
lavInspect(fit, "cov.lv") 

summary(Creativity.model)


library(semPlot)
semPaths(fit, "std", whatLabels = "std", edge.label.cex = 1.5)

summary(fit)

fitmeasures(fit)

finalmodel <-' 
#set the first order factor structure
 Innovative =~ Fluency + Originality
              Adaptive =~ Elaboration + Resistance + Abstractness
#set the higher order factor structure
  Creativity =~ NA*Innovative + Adaptive
  Creativity ~~1*Creativity'
finalmodel_fit <- cfa(finalmodel, data = sum_data)
semPaths(finalmodel_fit, "std", whatLabels = "std", edge.label.cex = 1.5)

summary(finalmodel_fit)
fitmeasures(finalmodel_fit)
```
