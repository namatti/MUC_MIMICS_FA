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
