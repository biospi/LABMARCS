# set seed if we want to keep the same data across runs
set.seed(39446)

# store_crossval.train.data_ls <- vector(mode = "list", length = repeats)
shuffle_index_ls <- vector(mode = "list", length = repeats)

#stratify CV as POS/NEG examples of severe outcomes are split ~30/70

# generate a list of indices for a CV split, we'll then shuffle this and save
POS = train.data[ train.data$outcome == TRUE, ]
NEG = train.data[ train.data$outcome == FALSE, ]

#sort data with TRUE outcomes first, so indices for stratifying map on correctly
train.data = rbind(POS, NEG)

data_split_idx_POS <- cut(seq(1,nrow(POS)),breaks = outsidefolds, labels = FALSE)
data_split_idx_NEG <- cut(seq(1,nrow(NEG)),breaks = outsidefolds, labels = FALSE)

# Loop for cross validation
for (j in 1:repeats) {
    
  # Randomly shuffle the training data
  shuffled_index_POS <- data_split_idx_POS[sample(nrow(POS)) ]
  shuffled_index_NEG <- data_split_idx_NEG[sample(nrow(NEG)) ]
  
  shuffle_index_ls[[j]] = c(shuffled_index_POS,shuffled_index_NEG)

}
