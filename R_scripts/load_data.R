
#Load OTU table
otu_wide <- read_delim("data/model_data/mothur.baxter.shared", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)  %>%
  rename_all(tolower) %>% 
  mutate(group = str_replace(group, ".sra", "")) %>% 
  rename(seq_id = group) %>% 
  select(-c("label", "numotus"))


otu_long <- otu_wide %>% pivot_longer(-seq_id, names_to="otu", values_to="count")


#Load taxonomy
taxonomy <- read_tsv("data/model_data/mothur.baxter.cons.taxonomy") %>%
  rename_all(tolower) %>%
  select(otu, taxonomy) %>%
  mutate(taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";$", ""),
         otu = str_replace_all(otu, "^O", "o"),
         taxonomy = str_replace_all(taxonomy, ";", "/")) %>% 
  mutate(TAXA = taxonomy) %>% 
  separate(TAXA,
           into=c("kingdom", "phylum", "class", "order", "family", "genus"),
           sep="/")


#Load metadata
metadata  <- read_csv("data/model_data/SraRunTable.txt",
                      col_types = cols(Organism = col_skip())) %>% 
  rename_all(tolower) %>%
  rename(seq_id = run) %>% 
  mutate(#CRC = diagnosis == "Adv Adenoma" | diagnosis == "Cancer",
    lesion = diagnosis == "Adv Adenoma" | diagnosis== "Cancer" | diagnosis == "Adenoma") %>% 
  filter(description == "human stool sample") %>% 
  mutate(dx = diagnosis,
         dx = str_replace_all(dx, "High Risk Normal", "normal"),
         dx = str_replace_all(dx, "adv Adenoma", "adenoma"),
         dx = tolower(dx)) %>% 
  select(c(seq_id, bases, age, bmi, gender, dx )) %>% 
  filter(dx == "cancer" |
           dx == "normal")


### Quality control

#prepare ampvis2 object
otu <-  t(otu_wide)
otu <-  cbind(rownames(otu), data.frame(otu, row.names=NULL))
otu[1,1] = "otu"
colnames(otu) <- otu[1,]
otutable <- otu[-1, ] 

otu_table <-  merge(taxonomy, otutable, by="otu") 
otu_table[, 1:8] <- sapply(otu_table[, 1:8], as.character)
otu_table[, 9:552] <- sapply(otu_table[, 9:552], as.numeric)  
otu_table <- otu_table %>% 
  select(-("taxonomy"))


d <- amp_load(otutable = otu_table,
              metadata = metadata)


#subset data for samples with reads above 10000
sd = amp_subset_samples(data = d, minreads = 10000, normalise = FALSE)

subsetmd <- sd[["metadata"]]

qc_fail <- anti_join(metadata, subsetmd, copy = TRUE)

md <- anti_join(metadata, qc_fail) #removes the samples from the metadata


#combining OTU with taxonomy into one df
df <- inner_join(otu_long, taxonomy, by="otu") %>%
  group_by(seq_id, genus) %>%
  summarize(count = sum(count), .groups="drop") %>%
  group_by(seq_id) %>%
  ungroup() %>% 
  mutate(row = row_number()) %>%
  group_by(seq_id) %>% 
  pivot_wider(names_from="genus", values_from="count") %>% 
  select(-c(row))
df[is.na(df)]<- 0
df <- df %>% summarise_if(is.numeric, sum) 

#Combining df with metadata
#df must be numeric and with the predicting outcome column as factor
df <- df %>% 
  inner_join(., md, by="seq_id") %>% 
  select("dx", everything()) %>% 
  filter(dx == "normal" | dx == "cancer") %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  select(-c(bases, gender, bmi, age)) %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>% #converts counts to relative abundance
  mutate_if(is.numeric, funs(./total))
df$dx <- as.factor(df$dx)
df <- df[,-461]




### Internal Validation - Data split
#Random samples
df_validation <- sample_n(df, 50)
#update dataframe - exculde validation dataset
df <- anti_join(df, df_validation, by = "seq_id")



### Preprocessing


##### The OTUs are used as predictors in each model, OTUs have zeros in all the samples when using a 97% classification. Thus, removing either zero variance or even near zero variance, may have an impact on the model performance. 

df_noid <- df %>% 
  remove_rownames() %>% 
  column_to_rownames("seq_id")

dim(df_noid)


#freqCut - the cutoff for the ratio of the most common value to the second most common value
#uniqueCut - the cutoff for the percentage of distinct values out of the number of total

set.seed(42)

var <- nearZeroVar(df_noid, saveMetrics= TRUE,
                   freqCut = 95/5,
                   uniqueCut = 10) %>%
  mutate(rown = row_number()) 


#removing near zero variance
nzv <- var %>% filter(nzv == TRUE)
nzv <- as.integer(nzv$rown)
nzv_df <- df_noid[, -nzv]
dim(nzv_df) #183 columns after removing near zero predictors 


#removing  zero variance
zv <- var %>% filter(zeroVar == TRUE)
zv <- as.integer(zv$rown)
zv_df <- df_noid[, -zv]
dim(zv_df) #396 columns after removing near zero predictors 


var <- nearZeroVar(df_noid, saveMetrics= TRUE,
                   freqCut = 2,
                   uniqueCut = 90) %>%
  mutate(rown = row_number()) 


#removing near zero variance
nzv_strict <- var %>% filter(nzv == TRUE)
nzv_strict <- as.integer(nzv_strict$rown)
nzv_strict <- df_noid[, -nzv_strict]
dim(nzv_strict) #30 columns after removing near zero predictors 




