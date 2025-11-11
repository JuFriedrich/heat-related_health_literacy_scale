# Setting options ---------------------------------------------------------
setwd("//directory") # set your own working directory where file HRHLS_data.xslx is located
options(scipen=999)
options(digits=2)

# Loading all packages -------------------------------------------------------
library(readxl)
library(sjPlot)
library(lavaan)
library(psych)
library(dplyr)
library(tidyr)
library(purrr)
library(flextable)
library(officer)
library(apaTables)
library(ggcorrplot)
library(semPlot)
library(car)

# Reading data set -----------------------------------------------------------
data <- read_excel("HRHLS_Data.xlsx")
View(data)

# Deleting outliers ----------------------------------------------------------------
# Mahalanobis distance
mahal <- mahalanobis(data[ , c(5:24)], colMeans(data [ , c(5:24)]), cov(data[ , c(5:24)])) 
cutoff_mahal = qchisq(1-.001, ncol(data[ , c(5:24)])) 
summary(mahal < cutoff_mahal)
data_clean <- subset(data, mahal < cutoff_mahal)

outliers <- which(mahal >= cutoff_mahal)
data[outliers, ]
print(outliers)

hist(mahal, breaks = 30, main = "Distribution of Mahalanobis distances", col = "lightblue")
abline(v = cutoff_mahal, col = "red", lwd = 2)

boxplot(mahal, main = "Mahalanobis distances", col = "lightblue")
abline(h = cutoff_mahal, col = "red", lwd = 2)

plot(mahal, pch = 19, col = ifelse(mahal >= cutoff_mahal, "red", "black"),
     main = "Mahalanobis distances")
abline(h = cutoff_mahal, col = "blue", lwd = 2)


# Descriptive statictics --------------------------------------------------
table(data_clean$GENDER)
describe(data_clean$AGE)

table(data_clean$Q13)
table(data_clean$Q14)

# Ensure HRHL items exist and select them
items <- data_clean %>%
  select(HRHL1:HRHL20)

# Convert all items to numeric (if not already)
items <- items %>%
  mutate(across(everything(), ~as.numeric(as.character(.x))))

# Check if values are within expected range (1-5)
summary(items)

# Function to compute frequency table for each item
freq_table <- map_df(names(items), function(item) {
  items %>%
    count(Response = .data[[item]]) %>%
    filter(!is.na(Response)) %>%  # exclude missing values
    mutate(
      Item = item,
      Percent = round(100 * n / sum(n), 1)
    )
})

# Pivot to wide format
freq_table_wide <- freq_table %>%
  mutate(Label = paste0(n, " (", Percent, "%)")) %>%
  select(Item, Response, Label) %>%
  pivot_wider(names_from = Response, values_from = Label,
              names_prefix = "Resp_") %>%
  arrange(Item)

# Optional: export to CSV for Excel formatting
write.csv(freq_table_wide, "HRHL_item_frequencies.csv", row.names = FALSE)

# View result
print(freq_table_wide, n = 20)
# View the resulting table
print(freq_table_wide)
# Splitting the data set -------------------------------------------------
seed <- 1111
set.seed(seed)

n <- nrow(data_clean)
half_size <- floor(n / 2)
indices <- sample(seq_len(n), size = half_size)

sample1 <- data_clean[indices, ]
sample2 <- data_clean[-indices, ]

cat("Sample 1 Size:", nrow(sample1), "\n")
cat("Sample 2 Size:", nrow(sample2), "\n")

# Data sets ---------------------------------------------------------------
HRHLd0 <- data_clean[,c("HRHL1", "HRHL2", "HRHL3", "HRHL4", "HRHL5", "HRHL6", "HRHL7", "HRHL8","HRHL9", 
                        "HRHL10", "HRHL11", "HRHL12","HRHL13", "HRHL14", "HRHL15", "HRHL16", "HRHL17", "HRHL18","HRHL19", "HRHL20")]

HRHLd1 <- sample1[,c("HRHL1", "HRHL2", "HRHL3", "HRHL4", "HRHL5", "HRHL6", "HRHL7", "HRHL8","HRHL9", 
                     "HRHL10", "HRHL11", "HRHL12","HRHL13", "HRHL14", "HRHL15", "HRHL16", "HRHL17", "HRHL18","HRHL19", "HRHL20")]

HRHLd2 <- sample2[,c("HRHL1", "HRHL2", "HRHL3", "HRHL4", "HRHL5", "HRHL6", "HRHL7", "HRHL8","HRHL9", 
                     "HRHL10", "HRHL11", "HRHL12","HRHL13", "HRHL14", "HRHL15", "HRHL16", "HRHL17", "HRHL18","HRHL19", "HRHL20")]

HRHLd1a <- sample1[,c("HRHL1", "HRHL2", "HRHL3", "HRHL4", "HRHL7", "HRHL8", "HRHL9",
                      "HRHL10", "HRHL11", "HRHL13", "HRHL14", "HRHL15", "HRHL16", "HRHL18", "HRHL19", "HRHL20")]

HRHLd2a <- sample2[,c("HRHL1", "HRHL2", "HRHL3", "HRHL4", "HRHL7", "HRHL8", "HRHL9",
                    "HRHL10", "HRHL11", "HRHL13", "HRHL14", "HRHL15", "HRHL16", "HRHL18", "HRHL19", "HRHL20")]

# Item difficulty --------------------------------------------
tab <- tab_itemscale(HRHLd0)
str(tab)
df <- do.call(rbind, tab$df.list)  # Bind all elements together
ft <- flextable(as.data.frame(df))

doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)

print(doc, target = "itemscale_table_final.docx")

# Cronbach's Alpha ------------------------------------------------
alphaHRHL <- psych::alpha(HRHLd0)
summary(alphaHRHL)

#Split half reliability
splitHalf(HRHLd0)

# Exploratory factor analysis (EFA) -------------------------------------
HRHLcor <- cor(HRHLd0,method ="spearman", use="p")

ggcorrplot(corr = HRHLcor)
describe(HRHLd0)
error.dots(x = HRHLd0,sort = F)
error.bars(HRHLd0)

#Factor analysis
#Bartlett-Test (tests whether items are correlated)
cortest.bartlett(HRHLd0) 

#KMO and MSA 
KMO(HRHLd0)

#principal component factor analysis
#2 factors
efa2pa <- fa(HRHLd0, nfactors=2, fm="pa", rotate="promax") # principal factor solution
sort(efa2pa$communality, decreasing = TRUE) 
summary(efa2pa)

#4 factors
efa4pa <- fa(HRHLd0, nfactors=4, fm="pa", rotate="promax") # principal factor solution
sort(efa4pa$communality, decreasing = TRUE) 
summary(efa4pa)

#Eigenvalues
efa4pa$e.values
# 2 eigenvalues > 1

# Kaiser-Dickman Criterium
num_components_kept <- sum(efa4pa$e.values > 1)
num_components_kept

total_variance <- sum(efa4pa$e.values)  # Gesamte Varianz
explained_variance_percentage <- efa4pa$e.values / total_variance * 100
explained_variance_percentage
cumulative_variance <- cumsum(explained_variance_percentage)
cumulative_variance

# Bootstrapping with Exploratory structural equation modeling (ESEM) -------------
n_bootstraps <- 1000

items <- paste0("HRHL", 1:20)
latent_vars <- c("F1", "F2", "F3", "F4")
crossload_matrix <- matrix(0, nrow = length(items), ncol = n_bootstraps)
colnames(crossload_matrix) <- paste0("Bootstrap_", 1:n_bootstraps)
rownames(crossload_matrix) <- items

loading_storage <- list()

for (i in 1:n_bootstraps) {
  cat("Bootstrap iteration:", i, "\n")
  
  # Resample the data
  sample_indices <- sample(1:nrow(HRHLd1), nrow(HRHLd1), replace = TRUE)
  boot_data <- HRHLd1[sample_indices, ]
  
  # Run Exploratory structural equation modeling
  ESEM_4f <- fa(boot_data, nfact = 4, rotate = "promax", fm = "ml")
  ESEM_4f.loadmat <- zapsmall(matrix(round(ESEM_4f$loadings, 4), nrow = 20, ncol = 4))
  rownames(ESEM_4f.loadmat) <- items
  
  # Store loadings for confidence intervals
  loading_storage[[i]] <- ESEM_4f.loadmat
  
  # Mark cross-loadings > 0.3
  for (j in 1:length(items)) {
    loadings <- abs(ESEM_4f.loadmat[j, ])
    
    # Count cross-loadings
    crossload_matrix[j, i] <- sum(loadings > 0.3) > 1
  }
}

# === Step 1: Stability Analysis ===
crossload_percent <- rowMeans(crossload_matrix) * 100
crossload_summary <- data.frame(Item = items, 
                                Crossload_Percent = crossload_percent)
crossload_summary <- crossload_summary %>%
  mutate(Deletion_Recommended = ifelse(Crossload_Percent > 50, "Yes", "No"))

print(crossload_summary)

# === Step 2: Confidence Intervals Calculation ===
ci_results <- data.frame(Item = items)
for (k in 1:4) {
  factor_col <- paste0("F", k)
  ci_values <- sapply(1:length(items), function(idx) {
    loadings <- sapply(loading_storage, function(x) x[idx, k])
    quantile(loadings, probs = c(0.025, 0.975))
  })
  ci_results[[paste0(factor_col, "_Lower")]] <- ci_values[1, ]
  ci_results[[paste0(factor_col, "_Upper")]] <- ci_values[2, ]
}

print(ci_results)

# === Step 3: ESEM Model and Fit Measures ===
terms <- vector()
for (i in 1:4) {
  terms[i] <- paste0("F", i, "=~ ", 
                     paste0(c(ESEM_4f.loadmat[, i]), "*", rownames(ESEM_4f.loadmat), collapse = "+"))
}
ESEM_f4 <- paste(terms, collapse = "\n")

ESEM4 <- cfa(ESEM_f4, data = HRHLd1, estimator = "MLR", missing='fiml')

fitmeasures(ESEM4, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
summary(ESEM4, fit.measures = T, standardized= T)

# Confirmatory factor analysis (CFA) ------------------------------------------
SEM <- "
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4
F2 =~ HRHL7 + HRHL8 + HRHL9 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL18 + HRHL19 + HRHL20
"
fit <- cfa(SEM,data=HRHLd2a,estimator = "MLR",missing='fiml')
summary(fit,standardized=TRUE,fit.measures=TRUE)
fitmeasures(fit)[c('cfi.robust', 'tli.robust','rmsea.robust','srmr')]

semPaths(object = fit,
         residuals = FALSE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree", 
         rotation = 1,
         label.color = "black", 
         edge.color = "black")

std_estimates <- standardizedSolution(fit)

items <- paste0("HRHL", 1:20)

latent_vars <- c("F1", "F2", "F3", "F4")

table_data <- data.frame(Item = items)

for (latent in latent_vars) {
  std_values <- rep(NA, length(items))
  
  for (i in seq_along(items)) {
    match_row <- std_estimates$rhs == items[i] & std_estimates$lhs == latent
    
    if (any(match_row)) {
      std_values[i] <- round(std_estimates$est.std[match_row], 1)
    }
  }
  
  table_data[[latent]] <- std_values
}


flextable_object <- flextable(table_data)

for (latent in latent_vars) {
  flextable_object <- flextable_object %>%
    bold(j = latent, i = ~ data[[latent]] > 0.5, bold = TRUE)
}

for (i in seq_len(nrow(table_data))) {
  count_above_0_4 <- sum(table_data[i, latent_vars] > 0.3, na.rm = TRUE)
  if (count_above_0_4 > 1) {
    for (latent in latent_vars) {
      if (!is.na(table_data[i, latent]) && table_data[i, latent] > 0.3) {
        flextable_object <- flextable_object %>%
          italic(j = latent, i = i, italic = TRUE)
      }
    }
  }
}

doc <- read_docx()
doc <- body_add_flextable(doc, flextable_object)
print(doc, target = "CFA_Results.docx")

# CFA with whole data set ------------------------------------------
SEM2 <- "
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4
F2 =~ HRHL7 + HRHL8 + HRHL9 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL18 + HRHL19 + HRHL20
"
fit2 <- cfa(SEM2,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit2,standardized=TRUE,fit.measures=TRUE)
fitmeasures(fit2)[c('cfi.robust', 'tli.robust','rmsea.robust','srmr')]

semPaths(object = fit2,
         residuals = FALSE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree", 
         rotation = 1,
         label.color = "black", 
         edge.color = "black")

# CFA with higher order factor without items 5, 6, 12, 17 ----------------------------
SEM3 <- "
HRHL =~ F1 + F2 + F3 + F4
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4
F2 =~ HRHL7 + HRHL8 + HRHL9 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL18 + HRHL19 + HRHL20
"
fit3 <- cfa(SEM3,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit3,standardized=TRUE,fit.measures=TRUE)
fitmeasures(fit3)[c('cfi.robust', 'tli.robust','rmsea.robust','srmr')]

semPaths(object = fit3,
         residuals = TRUE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree",
         rotation = 1,
         label.color = "black",
         edge.color = "black",
         intercepts = FALSE)

# Heat-related health literacy score ---------------------------------------------------------------
HRHLMean <- rowMeans(subset(data_clean, select=c(HRHL1, HRHL2, HRHL3, HRHL4, HRHL7, HRHL8, HRHL9, HRHL10, HRHL11,
                                                 HRHL13, HRHL14, HRHL15, HRHL16, HRHL18, HRHL19, HRHL20)))

HRHLscore <- (HRHLMean - 1) * (50 / 4)

HRHLIndex <- car::recode(HRHLscore, "lo:20=1;20.1:30=2;30.1:40=3;40.1:hi=4")

HRHLIndex2 <- factor(HRHLIndex,
                     levels = c(1,2,3,4),
                     labels = c("inadequate", "problematic", "sufficient", "excellent"))

table(HRHLIndex2)
freq_tableHRHLIndex2 <- prop.table(table(HRHLIndex2)) * 100
print(round(freq_tableHRHLIndex2, 2))

# Scores Knowledge -------------------------------------------------------
data_clean <- data_clean %>%
  mutate(across(K1:K15, ~ ifelse(. == 1, 1, 0)))

data_clean <- data_clean %>%
  mutate(
    K2R = ifelse(K2 == 1, 0, 1),
    K6R = ifelse(K6 == 1, 0, 1),
    K9R = ifelse(K9 == 1, 0, 1)
  ) %>%
  relocate(K2R, .after = K2) %>%
  relocate(K6R, .after = K6) %>%
  relocate(K9R, .after = K9)

data_clean <- data_clean %>%
  mutate(KSum = rowSums(select(., K1, K2R, K3, K4, K5, K6R, K7, K8, K9R, K10, K11, K12, K13, K14, K15))) %>%
  relocate(KSum, .before = K1)

split_value <- quantile(data_clean$KSum, 0.5, na.rm = TRUE)
data_clean <- data_clean %>%
  mutate(KSumGroup = ifelse(KSum <= split_value, "Lower", "Upper"))

# Validity ------------------------------------------------------
model <- "
AG =~ AGE
GE =~ GENDER
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4
F2 =~ HRHL6 + HRHL7 + HRHL8 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL18 + HRHL19 + HRHL20
K =~ KSum
A =~ A1 + A2 + A3 + A4 + A5 
P =~ B1 + B2 + B3 + B4 + B5
B =~ B6 + B7 + B8
"

fit4 <- sem(model,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit4,standardized=TRUE,fit.measures=TRUE,rsquare=TRUE)
fitmeasures(fit4)[c("cfi.robust","tli.robust","rmsea.robust","srmr")]

# Socioeconomic status ---------------------------------------------------
score_income <- function(income) {
  case_when(
    income < 708 ~ 1.0,
    income < 925 ~ 1.5,
    income < 1101 ~ 2.0,
    income < 1294 ~ 2.5,
    income < 1441 ~ 3.0,
    income < 1599 ~ 3.5,
    income < 1785 ~ 4.0,
    income < 1999 ~ 4.5,
    income < 2195 ~ 5.0,
    income < 2499 ~ 5.5,
    income < 2838 ~ 6.0,
    income < 3499 ~ 6.5,
    TRUE ~ 7.0
  )
}

score_education <- function(education, vocationaltraining) {
  case_when(
    education == 1 & vocationaltraining %in% c(1, 2) ~ 1.0,  
    education == 3 & vocationaltraining %in% c(1, 2) ~ 2.6,  
    education == 3 & vocationaltraining %in% c(3, 4) ~ 3.5,  
    education == 4 & vocationaltraining %in% c(1, 2) ~ 3.6,  
    education == 4 & vocationaltraining %in% c(3, 4) ~ 4.5,  
    education == 5 & vocationaltraining %in% c(1, 2) ~ 6.0,  
    education == 5 & vocationaltraining %in% c(5, 6) ~ 6.5,  
    education == 5 & vocationaltraining == 7 ~ 7.0,  
    TRUE ~ 7.0  
  )
}

score_position <- function(position, lead) {
  base_points <- case_when(
    position == 1 ~ 4.4,  
    position == 2 ~ 2.1,  
    position == 3 ~ 6.7,  
    position == 4 ~ 1.0,  
    position == 5 ~ 5.4,  
    position == 6 ~ 5.2,  
    position == 7 ~ 3.8, 
    position == 8 ~ 1.0,  
    position == 9 ~ 1.0,  
    position == 10 ~ 1.0, 
    position == 11 ~ 1.0, 
    TRUE ~ 3.8            
  )
  
  lead_bonus <- case_when(
    lead == 1 & position != 3 ~ 1.2,  
    lead == 2 & position != 3 ~ 0.3,  
    lead == 1 & position == 3 ~ 0.3, 
    lead == 2 & position == 3 ~ 0.1,
    TRUE ~ 0.0
  )
  
  points <- base_points + lead_bonus
}

data_clean <- data_clean %>%
  mutate(
    SES_income = score_income(Q18),
    SES_education = mapply(score_education, Q13, Q14), 
    SES_profession = mapply(score_position, Q16, Q17), 
    SES_all = SES_education + SES_profession # + SES_income
  )

describe(data_clean$SES_all)

p20 <- quantile(data_clean$SES_all, probs = 0.20, na.rm = TRUE)
p80 <- quantile(data_clean$SES_all, probs = 0.80, na.rm = TRUE)

data_clean$SES_category <- cut(
  data_clean$SES_all,
  breaks = c(-Inf, p20, p80, Inf),
  labels = c("niedrig", "mittel", "hoch"),
  include.lowest = TRUE
)

table(data_clean$SES_category)

p20b <- quantile(data_clean$SES_education, probs = 0.20, na.rm = TRUE)
p80b <- quantile(data_clean$SES_education, probs = 0.80, na.rm = TRUE)

SESprocentage <- data_clean %>%
  group_by(SES_category) %>%
  summarise(Frequency = n()) %>%
  mutate(procentage = (Frequency / sum(Frequency)) * 100)

# Ergebnis anzeigen
print(SESprocentage)

# Federal State -----------------------------------------------------------
state_freq <- table(data_clean$STATE)
state_freq_df <- as.data.frame(state_freq)

state_freq_df <- state_freq_df[order(state_freq_df$Var1), ]

colnames(state_freq_df) <- c("Federal State", "Frequency")

state_freq_df$Percentage <- round((state_freq_df$Frequency / sum(state_freq_df$Frequency)) * 100, 1)

state_freq_df$No <- seq(1, nrow(state_freq_df))

print(state_freq_df)