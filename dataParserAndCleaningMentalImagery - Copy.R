rm(list=ls())
library(jsonlite)
library(tidyverse)



setwd("C:/Users/edenn/OneDrive/Desktop/jatos_results_data_20251130210138")
#files = list.files(pattern = "jatos_results_data_20241128121210.txt")
files = list.files(pattern ='txt', recursive=TRUE)
files



# Set up to tell it how many tasks to look for
numComponents = 9
#bonusComponents = c(2,3,4) # Unused as of yet


# Output file names
# outputNames = c("CoronaP40RepOA","SGTP40RepOA","UnicornP40RepOA","MixedGambleP40RepOA","IUSP40RepOA")
# 
#dataset <- () 
filenum <- 0  
for (file in files){
  filenum <- filenum+1
  if (filenum==1){
    dataset <- readLines(file, warn = FALSE)
  } else {
    temp_dataset <-readLines(file, warn = FALSE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merged dataset doesn't exist, create it
  #if (!exists("dataset")){
  #  dataset <- readLines(file, warn = FALSE)
  #}
  
  
}
dataset

# Create data list to hold all of the data regardless of numComponents
dataMatrix = list()


# Load in actual data file.
rawData = dataset#readLines(files, warn = FALSE)_rawd


#rawData[1] <- NULL
rawData[1]
rawData[7]
rawData[8]


#used to test what the different components are
fileLook <- fromJSON(rawData[1] ) #demographic data
fileLook
fileLook <- fromJSON(rawData[2] ) #OSPAN DATA 
fileLook 
fileLook <- fromJSON(rawData[3] ) #condition
fileLook
fileLook <- fromJSON(rawData[4] ) #word rating task
fileLook
fileLook <- fromJSON(rawData[5] ) #image rating task
fileLook
fileLook <- fromJSON(rawData[6] ) #SGT 
fileLook
fileLook <- fromJSON(rawData[7] ) #VVIQ
fileLook
fileLook <- fromJSON(rawData[8] ) #OSIQ
fileLook
fileLook <- fromJSON(rawData[9] ) #end
fileLook






# Get total number of subjects
numSubs = length(rawData)/(numComponents)
numSubs  #108 subjects in each condition on 4/15/22
#81 subjects between 4/18/22 and end of semester - 5/3/22
# --- replacement code --- #
# ------------------------------
# Subject parsing loop (drop-in replacement)
# ------------------------------
dataMatrix <- list()   # will hold single combined data.frame in [[1]]

# helper: compute intended row-count for a parsed JSON object
get_len <- function(x){
  lens <- unlist(rapply(x, length))
  if (length(lens) == 0) return(0)
  return(max(lens, na.rm = TRUE))
}

# safe rep: replicate/truncate to subj_nrows; attempt to parse JSON scalar
safe_rep <- function(x, subj_nrows) {
  if (is.null(x) || length(x) == 0) return(rep(NA, subj_nrows))
  if (is.atomic(x) && length(x) == 1) {
    parsed <- tryCatch(fromJSON(x), error = function(e) x)
    return(rep(parsed, length.out = subj_nrows))
  }
  rep(x, length.out = subj_nrows)
}

# place_then_fill: place values at beginning and fill remainder with zeros (numeric)
# or NA_character_ (non-numeric). Returns atomic vector (numeric or character).
place_then_fill <- function(x, subj_nrows) {
  # missing -> produce NA_character_ vector (will be coerced later where needed)
  if (is.null(x) || length(x) == 0) {
    return(rep(NA_character_, subj_nrows))
  }
  
  # try numeric coercion
  x_num <- suppressWarnings(as.numeric(x))
  if (!all(is.na(x_num))) {
    out <- rep(0, subj_nrows)
    n_put <- min(length(x_num), subj_nrows)
    out[1:n_put] <- x_num[1:n_put]
    return(out)
  }
  
  # otherwise treat as character
  x_chr <- as.character(x)
  out <- rep(NA_character_, subj_nrows)
  n_put <- min(length(x_chr), subj_nrows)
  out[1:n_put] <- x_chr[1:n_put]
  return(out)
}

# demographic parser that tries to parse JSON-encoded scalar but uses safe_rep output
safe_rep_demographic <- function(field, subj_nrows) {
  if (is.null(field) || length(field) == 0) return(rep(NA_character_, subj_nrows))
  parsed <- tryCatch({
    if (!is.atomic(field) && !is.null(field)) field else fromJSON(field[1])
  }, error = function(e) field[1])
  safe_rep(parsed, subj_nrows)
}

# Loop over subjects
for (i in seq_len(numSubs)) {
  
  subject_indices <- (((i-1)*numComponents)+1) : (((i-1)*numComponents)+numComponents)
  subject_files <- lapply(subject_indices, function(ind) fromJSON(rawData[ind]))
  
  subj_nrows <- max(sapply(subject_files, get_len), 1)
  tempMat <- data.frame("Subnum" = rep(i, subj_nrows), stringsAsFactors = FALSE)
  
  for (j in seq_len(numComponents)) {
    fileOne <- subject_files[[j]]
    this_len <- get_len(fileOne)
    if (this_len == 0L) {
      warning(sprintf("Subject %d component %d is empty (length 0).", i, j))
    }
    
    if (j == 1) {
      # demographics: keep repeating behavior (no place-and-fill)
      tempMat$Sex       <- safe_rep_demographic(fileOne$Gender, subj_nrows)
      tempMat$Race      <- safe_rep_demographic(fileOne$Race, subj_nrows)
      tempMat$Ethnicity <- safe_rep_demographic(fileOne$Ethnicity, subj_nrows)
      
    } else if (j == 2) {
      # OSPAN: place-then-fill for all OSPAN columns
      tempMat$OSPAN_MathCorrect    <- place_then_fill(fileOne$MathCorrect, subj_nrows)
      tempMat$OSPAN_SpanLength     <- place_then_fill(fileOne$SpanLength, subj_nrows)
      tempMat$OSPAN_TrialLetters   <- place_then_fill(fileOne$TrialLetters, subj_nrows)
      tempMat$OSPAN_ProbeTrial     <- place_then_fill(fileOne$ProbeTrial, subj_nrows)
      tempMat$OSPANResponses       <- place_then_fill(fileOne$OSPANResponses, subj_nrows)
      tempMat$OSPANCorrectLetters  <- place_then_fill(fileOne$OSPANCorrectLetters, subj_nrows)
      tempMat$MathProblem          <- place_then_fill(fileOne$MathProblem, subj_nrows)
      tempMat$MathCorrectAnswer    <- place_then_fill(fileOne$MathCorrectAnswer, subj_nrows)
      tempMat$OSPANRT              <- place_then_fill(fileOne$OSPANRT, subj_nrows)
      tempMat$QResp                <- place_then_fill(fileOne$QResp, subj_nrows)
      
    } else if (j == 3) {
      # Condition: keep repeating/truncating behavior
      tempMat$Condition <- safe_rep(fileOne$Condition, subj_nrows)
      
    } else if (j == 4) {
      tempMat$word_name      <- place_then_fill(fileOne$word_name, subj_nrows)
      tempMat$word_natural   <- place_then_fill(fileOne$naturalness, subj_nrows)
      tempMat$word_disorder  <- place_then_fill(fileOne$disorderliness, subj_nrows)
      tempMat$word_aesthetic <- place_then_fill(fileOne$aesthetic, subj_nrows)
      
    } else if (j == 5) {
      tempMat$image_name      <- place_then_fill(fileOne$image_name, subj_nrows)
      tempMat$image_natural   <- place_then_fill(fileOne$naturalness, subj_nrows)
      tempMat$image_disorder  <- place_then_fill(fileOne$disorderliness, subj_nrows)
      tempMat$image_aesthetic <- place_then_fill(fileOne$aesthetic, subj_nrows)
      
    } else if (j == 6) {
      tempMat$ReactTime   <- place_then_fill(fileOne$React, subj_nrows)
      tempMat$Reward      <- place_then_fill(fileOne$Reward, subj_nrows)
      tempMat$KeyResponse <- place_then_fill(fileOne$keyResponse, subj_nrows)
      tempMat$Trial       <- place_then_fill(fileOne$Trial, subj_nrows)
      tempMat$Bank        <- place_then_fill(fileOne$Bank, subj_nrows)
      tempMat$BonusAmount <- place_then_fill(fileOne$BonusAmount, subj_nrows)
      
    } else if (j == 7) {
      # VVIQResp: place-then-fill; VVIQScore: keep safe_rep (repeat/truncate)
      tempMat$VVIQResp_reverse <- place_then_fill(fileOne$VVIQResp, subj_nrows)
      tempMat$VVIQScore        <- safe_rep(fileOne$VVIQScore, subj_nrows)
      
    } else if (j == 8) {
      tempMat$OSIQResp_reverse <- place_then_fill(fileOne$OSIQResp, subj_nrows)
      tempMat$OSIQScore        <- safe_rep(fileOne$OSIQScore, subj_nrows)
    }
  }
  
  if (i == 1) {
    dataMatrix[[1]] <- tempMat
  } else {
    dataMatrix[[1]] <- rbind(dataMatrix[[1]], tempMat)
  }
}

AllDataMatrix <- dataMatrix[[1]]

# ------------------------------
# Final cleaning: convert any remaining list-columns to atomic vectors,
# and coerce character-looking numeric strings back to numeric where safe.
# ------------------------------

# identify list-columns and flatten them by collapsing multi-element cells into a ;-joined string
AllDataMatrix[] <- lapply(AllDataMatrix, function(col) {
  if (is.list(col)) {
    col_flat <- vapply(col, function(x) {
      if (is.null(x)) return(NA_character_)
      if (length(x) == 0) return(NA_character_)
      if (length(x) == 1) return(as.character(x))
      paste(as.character(x), collapse = ";")
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
    col <- col_flat
  }
  # now col is atomic (likely character); attempt numeric conversion if safe
  col_chr <- as.character(col)
  col_num <- suppressWarnings(as.numeric(col_chr))
  # if coercion produced at least one non-NA and not all NA due to coercion errors, return numeric
  # but avoid converting columns of mostly NA that came from character; use heuristic:
  coercible_fraction <- sum(!is.na(col_num)) / max(length(col_num), 1)
  if (coercible_fraction > 0.5) { # more than 50% numeric -> convert to numeric
    return(col_num)
  } else {
    return(col_chr)
  }
})

# Final sanity check (uncomment if you want to view structure)
# str(AllDataMatrix)

# Export to CSV
write.csv(AllDataMatrix, file = "MentalImageryData1.csv", row.names = FALSE)
# ------------------------------

library(dplyr)

# Load data
dat <- read.csv("MentalImageryData1.csv", stringsAsFactors = FALSE)

# Ensure numeric version of VVIQ response column
dat$VVIQ_item <- suppressWarnings(as.numeric(dat$VVIQResp_reverse))

# Reverse 1–5 scale: reversed = 6 - original
# Only reverse valid responses (1–5)
dat$VVIQ_item_rev <- ifelse(dat$VVIQ_item %in% 1:5, 6 - dat$VVIQ_item, NA)

# Summed score per participant (exclude placeholder 0 and NA)
VVIQ_scores <- dat %>%
  group_by(Subnum) %>%
  summarise(
    n_items_used = sum(!is.na(VVIQ_item_rev)),
    VVIQScore_new = sum(VVIQ_item_rev, na.rm = TRUE)
  )

# Merge new subject-level score back to all rows
dat <- left_join(dat, VVIQ_scores, by = "Subnum")

# Display quick check of first subjects
head(VVIQ_scores, 10)

# Ensure OSIQ column exists
if (!"OSIQResp_reverse" %in% names(dat)) {
  stop("Column 'OSIQResp_reverse' not found in dataset.")
}

# Convert to numeric
dat$OSIQ_item <- suppressWarnings(as.numeric(dat$OSIQResp_reverse))

# Reverse score: 1 ↔ 5, 2 ↔ 4, 3 stays 3
dat$OSIQ_item_rev <- ifelse(dat$OSIQ_item %in% 1:5, 6 - dat$OSIQ_item, NA)

# Summed score per participant (exclude 0 and NA)
OSIQ_scores <- dat %>%
  group_by(Subnum) %>%
  summarise(
    n_items_used_OSIQ = sum(!is.na(OSIQ_item_rev)),
    OSIQScore_new = sum(OSIQ_item_rev, na.rm = TRUE)
  )

# Merge new OSIQ score into full dataset
dat <- left_join(dat, OSIQ_scores, by = "Subnum")

# Save updated dataset with both new scores
write.csv(dat, "MentalImageryData1_with_VVIQ_OSIQ_Scores.csv", row.names = FALSE)

# Quick check output
head(OSIQ_scores, 10)
