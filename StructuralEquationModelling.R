
```{r}
library(lavaan)
library(bruceR)
library(readxl)
library(dplyr)
library(tidyr)
```

```{r}
set.wd()

rawdata<-read_excel(".xlsx",sheet = "Test")
rawdata$Creativity <- rowSums(rawdata[,1:n],na.rm = TRUE)

# Reshape the dataframe using pivot_longer
transformed_data <- rawdata %>%
  pivot_longer(cols = starts_with(c("Fluency", "Originality", "Elaboration", "Abstractness", "Resistance")), 
               names_to = c(".value", "set"),
               names_pattern = "([A-Za-z]+)(\\d+)",
               values_drop_na = TRUE)

columns_to_normalize <- c("Fluency", "Originality", "Elaboration", "Abstractness", "Resistance", "Creativity")

transformed_data <- transformed_data %>%
  mutate(across(all_of(columns_to_normalize), scale, center = TRUE, scale = TRUE))

```

```{r}
# EFA and CFA
install.packages('EFA.dimensions')
install.packages('psych')
library(EFA.dimensions)
library(psych)
```

```{r}
# test the facorability
corr.test(transformed_data)
```
```{r}
FACTORABILITY(transformed_data)
```

```{r}
# determine the no. of factors
RAWPAR(transformed_data)

MAP(transformed_data)

SCREE_PLOT(transformed_data)

SMT(transformed_data)

EMPKC(transformed_data)

fa(transformed_data, nfactors = 2)
```

```{r}
# EFA
efamodel <- fa(transformed_data, nfactors = 2, fm = "minres")

efamodel
head(efamodel$scores,13)
fa.diagram(efamodel, main = "dtransformed_data")
pairs.panels(efamodel$scores)
fa.parallel(x = transformed_data,fa = "fa")
```

```{r}
#CFA
Creativity.model<-'Innovative =~ Fluency + Originality
                  Adaptive =~ Elaboration + Resistance + Abstractness'


fit<-cfa(Creativity.model,data = transformed_data)
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
finalmodel_fit <- cfa(finalmodel, data = transformed_data)
semPaths(finalmodel_fit, "std", whatLabels = "std", edge.label.cex = 1.5)

summary(finalmodel_fit)
fitmeasures(finalmodel_fit)
```
