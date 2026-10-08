# ---- Load packages ----
library(psych)
library(GPArotation)

# ---- Load data ----
data <- read.csv('/path/to/PANSS_SANS_filtered.csv')

# ---- Select EFA items ----
efa_items <- data[, c("SANS_0PovertyOfSpeech",
                      "SANS_10PovertyOfContentOfSpeech",
                      "PANSS_N5DifficultyInAbstractThinking",
                      "PANSS_N6LackOfSpontaneityAndFlowOfConversation",
                      "PANSS_N7StereotypedThinking",
                      "PANSS_P2ConceptualDisorganization")]

# Check for missing values
sum(is.na(efa_items))

# Check dimensions
dim(efa_items)

# Check descriptive statistics
describe(efa_items)

# Create a complete cases index for EFA only
complete_idx <- complete.cases(efa_items)

# Check how many complete cases there are
sum(complete_idx)

# Use only complete cases for EFA, without deleting from original data
efa_items_complete <- efa_items[complete_idx, ]

# Check how many cases remain for EFA
dim(efa_items_complete)

# Check missing values per column
colSums(is.na(efa_items))

# Check correlation matrix
round(cor(efa_items_complete), 2)

# Create the correlation matrix
cor_matrix <- round(cor(efa_items_complete), 2)

# Export to CSV
write.csv(cor_matrix, '/volume/projects/NaMa_PPA/PPA/NaMa_PPA-copy/MUC_MIMICS_FA/correlation_matrix.csv')

# Correlation matrix with p-values
corr.test(efa_items_complete)

# Get correlations and p-values
cor_results <- corr.test(efa_items_complete)

# Extract correlations and p-values
cor_values <- round(cor_results$r, 2)
p_values <- round(cor_results$p, 3)

# Combine into one matrix (correlation + p-value)
combined_matrix <- matrix(paste0(cor_values, " (p=", p_values, ")"), 
                          nrow = nrow(cor_values),
                          dimnames = list(rownames(cor_values), 
                                          colnames(cor_values)))

# Export to CSV
file_path <- file.choose(new = TRUE)
write.csv(combined_matrix, file_path)

# Running EFA on selected Items

# Parallel analysis to determine number of factors
parallel <- fa.parallel(efa_items_complete, 
                        fm = "ml",      # maximum likelihood extraction
                        fa = "fa",      # factor analysis (not PCA)
                        n.iter = 100)   # number of iterations

png('output/parallel_analysis.png', width = 800, height = 600)
fa.parallel(efa_items_complete, 
            fm = "ml",
            fa = "fa",
            n.iter = 100)
dev.off()

# KMO test
KMO(efa_items_complete)

# Bartlett's test
cortest.bartlett(efa_items_complete)

# Run EFA with 2 factors
efa_result <- fa(efa_items_complete, 
                 nfactors = 2,        
                 rotate = "oblimin",  
                 fm = "ml")           

# Print the results
print(efa_result, digits = 2, cut = 0.3)

# Save full output to text file
sink('output/EFA_results.txt')
print(efa_result, digits = 2, cut = 0.3)
sink()

# Extract factor loadings
loadings_df <- as.data.frame(unclass(efa_result$loadings))

# Save to Excel
# Step 1: Extract factor loadings
loadings_df <- as.data.frame(unclass(efa_result$loadings))

# Step 2: Check it worked
loadings_df

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Factor Loadings")
writeData(wb, "Factor Loadings", loadings_df, rowNames = TRUE)

# Add model fit indices
fit_indices <- data.frame(
  Index = c("TLI", "RMSEA", "RMSR", "BIC"),
  Value = c(efa_result$TLI, efa_result$RMSEA[1], efa_result$rms, efa_result$BIC)
)
addWorksheet(wb, "Model Fit")
writeData(wb, "Model Fit", fit_indices)

# Save
saveWorkbook(wb, 'output/EFA_results.xlsx')

png('output/EFA_factor_loadings.png', width = 800, height = 600)
fa.diagram(efa_result)
dev.off()

