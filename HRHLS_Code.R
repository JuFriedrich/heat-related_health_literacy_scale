# Setting options ---------------------------------------------------------
setwd("//nas.ads.mwn.de/go69hew/Paper/Heat-related health literacy/GitHub")
options(scipen=999)
options(digits=2)

# Loading all packages -------------------------------------------------------
library(readxl)
library(lavaan)
library(psych)
library(dplyr)
library(officer)
library(flextable)
library(semPlot)
library(car)

# Read data set -----------------------------------------------------------
data <- read_excel("HRHLS_Data.xlsx")
View(data)

# Outliers ----------------------------------------------------------------
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

# Split new ---------------------------------------------------------------
# Set the seed for reproducibility
seed <- 1111
set.seed(seed)

n <- nrow(data_clean)

# Calculate half the size
half_size <- floor(n / 2)

# Randomly sample half of the indices
indices <- sample(seq_len(n), size = half_size)

# Create the two equally distributed samples
sample1 <- data_clean[indices, ]
sample2 <- data_clean[-indices, ]

# Output the sizes for verification
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

#Item difficulty
library(sjPlot)
tab <- tab_itemscale(HRHLd0)
str(tab)
# Convert df.list into a single data frame
df <- do.call(rbind, tab$df.list)  # Bind all elements together

# Descriptives and item difficulty --------------------------------------------
table(data_clean$GENDER)
library(psych)
describe(data_clean$AGE)

library(sjPlot)
tab <- tab_itemscale(HRHLd0)
str(tab)
# Convert df.list into a single data frame
df <- do.call(rbind, tab$df.list)  # Bind all elements together

# Convert to flextable
library(flextable)
ft <- flextable(as.data.frame(df))

# Create a Word document and add the table
library(officer)
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)

# Save the document
print(doc, target = "itemscale_table_final.docx")

#Correlations
library(apaTables)
correlationstable <- cbind(HRHL1=data$HRHL1,HRHL2=data$HRHL2,HRHL3=data$HRHL3,HRHL4=data$HRHL4,HRHL5=data$HRHL5,
                           HRHL6=data$HRHL6,HRHL7=data$HRHL7,HRHL8=data$HRHL8,HRHL9=data$HRHL9,HRHL10=data$HRHL10,
                           HRHL11=data$HRHL11,HRHL12=data$HRHL12,HRHL13=data$HRHL13,HRHL14=data$HRHL14,HRHL15=data$HRHL15,
                           HRHL16=data$HRHL16,HRHL17=data$HRHL17,HRHL18=data$HRHL18,HRHL19=data$HRHL19,HRHL20=data$HRHL20)
apa.cor.table(correlationstable, filename="Correlationstable.doc", table.number=1)

# Cronbach's Alpha ------------------------------------------------
alphaHRHL <- psych::alpha(HRHLd0)
summary(alphaHRHL)

#Split half reliability
splitHalf(HRHLd0)

# EFA ------------------------------------------------------------
HRHLcor <- cor(HRHLd0,method ="spearman", use="p")
library(ggcorrplot)
ggcorrplot(corr = HRHLcor)

#Descriptive analysis (data check): no outliers
describe(HRHLd0) #normal distribution
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

# Kaiser-Dickman-Kriterium anwenden
num_components_kept <- sum(efa4pa$e.values > 1)
num_components_kept

total_variance <- sum(efa4pa$e.values)  # Gesamte Varianz
explained_variance_percentage <- efa4pa$e.values / total_variance * 100
explained_variance_percentage
cumulative_variance <- cumsum(explained_variance_percentage)
cumulative_variance

# Bootstrapping -----------------------------------------------------------

# Parameters
n_bootstraps <- 1000

# Initialize storage for results
items <- paste0("HRHL", 1:20)
latent_vars <- c("F1", "F2", "F3", "F4")
crossload_matrix <- matrix(0, nrow = length(items), ncol = n_bootstraps)
colnames(crossload_matrix) <- paste0("Bootstrap_", 1:n_bootstraps)
rownames(crossload_matrix) <- items

loading_storage <- list()

# === Bootstrapping Loop ===
for (i in 1:n_bootstraps) {
  cat("Bootstrap iteration:", i, "\n")
  
  # Resample the data
  sample_indices <- sample(1:nrow(HRHLd1), nrow(HRHLd1), replace = TRUE)
  boot_data <- HRHLd1[sample_indices, ]
  
  # Run ESEM
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

# === Step 3: CFA Model and Fit Measures ===
# Build the model string
terms <- vector()
for (i in 1:4) {
  terms[i] <- paste0("F", i, "=~ ", 
                     paste0(c(ESEM_4f.loadmat[, i]), "*", rownames(ESEM_4f.loadmat), collapse = "+"))
}
ESEM_f4 <- paste(terms, collapse = "\n")

# Fit the CFA model
ESEM4_1 <- cfa(ESEM_f4, data = HRHLd1, estimator = "MLR", missing='fiml')

fitmeasures(ESEM4_1, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
summary(ESEM4_1, fit.measures = T, standardized= T)

# Confirmatory factor analysis (CFA) ------------------------------------------
SEM2_3 <- "
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4
F2 =~ HRHL7 + HRHL8 + HRHL9 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL18 + HRHL19 + HRHL20
"
fit2_3 <- cfa(SEM2_3,data=HRHLd2a,estimator = "MLR",missing='fiml')
summary(fit2_3,standardized=TRUE,fit.measures=TRUE)
fitmeasures(fit2_3)[c('cfi.robust', 'tli.robust','rmsea.robust','srmr')]

semPaths(object = fit2_3,
         residuals = FALSE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree", 
         rotation = 1,
         label.color = "black", 
         edge.color = "black")

# Extract the standardized solutions
std_estimates <- standardizedSolution(fit2_3)

# Define items (HRHL1 to HRHL20)
items <- paste0("HRHL", 1:20)

# Define latent variables (F1, F2, F3, F4)
latent_vars <- c("F1", "F2", "F3", "F4")

# Initialize the data frame with the first column for Items
table_data <- data.frame(Item = items)

# Loop through each latent variable
for (latent in latent_vars) {
  # Create an empty vector to store the values
  std_values <- rep(NA, length(items))
  
  # Loop through each item
  for (i in seq_along(items)) {
    # Find the corresponding row in the std_estimates
    match_row <- std_estimates$rhs == items[i] & std_estimates$lhs == latent
    
    # If a match is found, extract the value
    if (any(match_row)) {
      std_values[i] <- round(std_estimates$est.std[match_row], 1)
    }
  }
  
  # Add the vector to the data frame as a new column for the latent variable
  table_data[[latent]] <- std_values
}

# Create the flextable
flextable_object <- flextable(table_data)

# Apply bold for values > 0.5 and italics if the item loads > 0.4 on more than one factor
for (latent in latent_vars) {
  # Bold if greater than 0.5
  flextable_object <- flextable_object %>%
    bold(j = latent, i = ~ data[[latent]] > 0.5, bold = TRUE)
}

# Check for cross-loading (more than one factor > 0.3)
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

# Create the Word document and add the table
doc <- read_docx()
doc <- body_add_flextable(doc, flextable_object)

# Save the document
print(doc, target = "CFA_Results.docx")

# CFA whole data set ------------------------------------------
SEM2_4 <- "
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4
F2 =~ HRHL7 + HRHL8 + HRHL9 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL18 + HRHL19 + HRHL20
"
fit2_4 <- cfa(SEM2_4,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit2_4,standardized=TRUE,fit.measures=TRUE)
fitmeasures(fit2_4)[c('cfi.robust', 'tli.robust','rmsea.robust','srmr')]

semPaths(object = fit2_4,
         residuals = FALSE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree", 
         rotation = 1,
         label.color = "black", 
         edge.color = "black")

# CFA higher order factor without 5, 6, 12, 17 ----------------------------
SEM2_6 <- "
HRHL =~ F1 + F2 + F3 + F4
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4
F2 =~ HRHL7 + HRHL8 + HRHL9 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL18 + HRHL19 + HRHL20
"
fit2_6 <- cfa(SEM2_6,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit2_6,standardized=TRUE,fit.measures=TRUE)
fitmeasures(fit2_6)[c('cfi.robust', 'tli.robust','rmsea.robust','srmr')]

semPaths(object = fit2_6,
         residuals = TRUE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree",
         rotation = 1,
         label.color = "black",
         edge.color = "black",
         intercepts = FALSE)

# HRHL score ---------------------------------------------------------------
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

# Scores KSum -------------------------------------------------------
# Recode knowledge items und build sum score
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

table(data_clean$KSumGroup)

# Validity ------------------------------------------------------

# Check model
model <- "
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4 + HRHL5
F2 =~ HRHL7 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL17 + HRHL18 + HRHL19 + HRHL20
HRHL =~ F1 + F2 + F3 + F4
K =~ KSum
A =~ A1 + A2 + A3 + A4 + A5 
P =~ B1 + B2 + B3 + B4 + B5
B =~ B6 + B7 + B8
"

fit <- sem(model,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit,standardized=TRUE,fit.measures=TRUE,rsquare=TRUE)
fitmeasures(fit)[c("cfi.robust","tli.robust","rmsea.robust","srmr")]
semPaths(fit,whatLabels="std",layout="tree")

# Check for Regression
model1 <- "
AG =~ AGE
GE =~ GENDER
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4 + HRHL5
F2 =~ HRHL7 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL17 + HRHL18 + HRHL19 + HRHL20
HL =~ F1 + F2 + F3 + F4
K =~ KSum
A =~ A1 + A2 + A3 + A4 + A5 
P =~ B1 + B2 + B3 + B4 + B5
B =~ B6 + B7 + B8
"

fit1 <- sem(model1,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit1,standardized=TRUE,fit.measures=TRUE,rsquare=TRUE)
fitmeasures(fit1)[c("cfi.robust","tli.robust","rmsea.robust","srmr")]

semPaths(fit1,whatLabels="std",layout="tree")
semPaths(object = fit1,
         residuals = FALSE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree", rotation = 1,
         label.color = "black", edge.color = "black")

# Check subscales for Regression
model2 <- "
AG =~ AGE
GE =~ GENDER
F1 =~ HRHL1 + HRHL2 + HRHL3 + HRHL4 + HRHL5
F2 =~ HRHL7 + HRHL10
F3 =~ HRHL11 + HRHL13 + HRHL14 + HRHL15
F4 =~ HRHL16 + HRHL17 + HRHL18 + HRHL19 + HRHL20
K =~ KSum
A =~ A1 + A2 + A3 + A4 + A5 
P =~ B1 + B2 + B3 + B4 + B5
B =~ B6 + B7 + B8
"

fit2 <- sem(model2,data=data_clean,estimator = "MLR",missing='fiml')
summary(fit2,standardized=TRUE,fit.measures=TRUE,rsquare=TRUE)
fitmeasures(fit2)[c("cfi.robust","tli.robust","rmsea.robust","srmr")]

semPaths(fit2,whatLabels="std",layout="tree")
semPaths(object = fit1,
         residuals = FALSE,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree", rotation = 1,
         label.color = "black", edge.color = "black")

# SES neu -----------------------------------------------------------------
# Pakete laden
library(dplyr)

# Punktwert-Zuordnung für Einkommen
punkte_einkommen <- function(einkommen) {
  case_when(
    einkommen < 708 ~ 1.0,
    einkommen < 925 ~ 1.5,
    einkommen < 1101 ~ 2.0,
    einkommen < 1294 ~ 2.5,
    einkommen < 1441 ~ 3.0,
    einkommen < 1599 ~ 3.5,
    einkommen < 1785 ~ 4.0,
    einkommen < 1999 ~ 4.5,
    einkommen < 2195 ~ 5.0,
    einkommen < 2499 ~ 5.5,
    einkommen < 2838 ~ 6.0,
    einkommen < 3499 ~ 6.5,
    TRUE ~ 7.0
  )
}

# Punktwert-Kombination für Bildung (Q13 = schulische Bildung, Q14 = Berufsausbildung)
punkte_bildung <- function(bildung, berufsausbildung) {
  case_when(
    # Kein Schulabschluss & keine Berufsausbildung
    bildung == 1 & berufsausbildung %in% c(1, 2) ~ 1.0,  
    # Hauptschule ohne Berufsausbildung
    bildung == 3 & berufsausbildung %in% c(1, 2) ~ 2.6,  
    # Hauptschule + Lehre oder Berufsfachschule
    bildung == 3 & berufsausbildung %in% c(3, 4) ~ 3.5,  
    # Realschule ohne Berufsausbildung
    bildung == 4 & berufsausbildung %in% c(1, 2) ~ 3.6,  
    # Realschule + Lehre oder Berufsfachschule
    bildung == 4 & berufsausbildung %in% c(3, 4) ~ 4.5,  
    # Abitur ohne Berufsausbildung
    bildung == 5 & berufsausbildung %in% c(1, 2) ~ 6.0,  
    # Abitur + Fachhochschule/Fachschule
    bildung == 5 & berufsausbildung %in% c(5, 6) ~ 6.5,  
    # Abitur + Studium
    bildung == 5 & berufsausbildung == 7 ~ 7.0,  
    # Höchste Stufe (anderer Abschluss mit Studium)
    TRUE ~ 7.0  
  )
}

# Punktwert-Zuordnung für Berufliche Stellung (Q16) und Führungstätigkeit (Q17)
punkte_berufliche_stellung <- function(stellung, fuehrung) {
  base_points <- case_when(
    stellung == 1 ~ 4.4,  # Angestellter (ohne Führung)
    stellung == 2 ~ 2.1,  # Arbeiter
    stellung == 3 ~ 6.7,  # Beamter
    stellung == 4 ~ 1.0,  # Landwirt im Haupterwerb
    stellung == 5 ~ 5.4,  # Selbstständig mit Mitarbeitern
    stellung == 6 ~ 5.2,  # Selbstständig ohne Mitarbeiter
    stellung == 7 ~ 3.8,  # Mithelfender Familienangehöriger
    stellung == 8 ~ 1.0,  # Auszubildender
    stellung == 9 ~ 1.0,  # Freiwilliger Wehrdienst/BFD
    stellung == 10 ~ 1.0, # Freiwilliges Soziales Jahr
    stellung == 11 ~ 1.0, # Noch nie erwerbstätig
    TRUE ~ 3.8            # Sonstige
  )
  
  # Führungsbonus gemäß Müters: nur Beamte mit Führung kommen auf 7.0
  fuehrung_bonus <- case_when(
    fuehrung == 1 & stellung != 3 ~ 1.2,  # Führungskraft mit Entscheidungsbefugnis (außer Beamte)
    fuehrung == 2 & stellung != 3 ~ 0.3,  # Aufsichtskraft (außer Beamte)
    fuehrung == 1 & stellung == 3 ~ 0.3,  # Beamte mit Führung → genau 7.0
    fuehrung == 2 & stellung == 3 ~ 0.1,
    TRUE ~ 0.0
  )
  
  punktwert <- base_points + fuehrung_bonus
}

# Berechnung der Punktwerte inklusive kombinierter Bildung
data_clean <- data_clean %>%
  mutate(
    SES_Einkommen = punkte_einkommen(Q18),
    SES_Bildung = mapply(punkte_bildung, Q13, Q14), # Bildung kombiniert
    SES_Beruf = mapply(punkte_berufliche_stellung, Q16, Q17), # Berufliche Stellung + Führung
    SES_Gesamt = SES_Bildung + SES_Beruf # + SES_Einkommen
  )

describe(data_clean$SES_Gesamt)

# Berechnung der Perzentil-Schwellenwerte SES_Gesamt
p20 <- quantile(data_clean$SES_Gesamt, probs = 0.20, na.rm = TRUE)
p80 <- quantile(data_clean$SES_Gesamt, probs = 0.80, na.rm = TRUE)

# Klassifizierung in "niedrig", "mittel" und "hoch"
data_clean$SES_Kategorie <- cut(
  data_clean$SES_Gesamt,
  breaks = c(-Inf, p20, p80, Inf),
  labels = c("niedrig", "mittel", "hoch"),
  include.lowest = TRUE
)

# Ergebnis anzeigen
table(data_clean$SES_Kategorie)

# Berechnung der Perzentil-Schwellenwerte SES_Bildung
p20b <- quantile(data_clean$SES_Bildung, probs = 0.20, na.rm = TRUE)
p80b <- quantile(data_clean$SES_Bildung, probs = 0.80, na.rm = TRUE)

# Prozentuale Verteilung des SES
SESProzent <- data_clean %>%
  group_by(SES_Kategorie) %>%
  summarise(Anzahl = n()) %>%
  mutate(Prozent = (Anzahl / sum(Anzahl)) * 100)

# Ergebnis anzeigen
print(SESProzent)

# Federal State -----------------------------------------------------------

# Calculate the frequency table of the 'STATE' variable
state_freq <- table(data_clean$STATE)

# Convert the table into a data frame for easier manipulation
state_freq_df <- as.data.frame(state_freq)

# Sort the states alphabetically (1-16 order)
state_freq_df <- state_freq_df[order(state_freq_df$Var1), ]

# Rename the columns for clarity
colnames(state_freq_df) <- c("Federal State", "Frequency")

# Add a column for percentages
state_freq_df$Percentage <- round((state_freq_df$Frequency / sum(state_freq_df$Frequency)) * 100, 1)

# Add a sequential number column (1-16)
state_freq_df$No <- seq(1, nrow(state_freq_df))

# Print the result
print(state_freq_df)
