tic()
exval_metadata <- read_excel("data/validation_data/metadata.xlsx", 
                           col_types = c("text", "skip", "skip", 
                                         "skip", "skip", "text", "text", "numeric", 
                                         "skip", "skip", "skip", "skip", "skip", 
                                         "skip")) %>% 
  rename_all(tolower) %>% 
  select(-c(samplename, 'concisus positiv pcr fæces')) %>% 
  mutate(dx = reactor1,
         dx = str_replace_all(dx, "HC", "normal"),
         dx = str_replace_all(dx, "LC", "cancer"),
         dx = str_replace_all(dx, "CC", "cancer"),
         seq_id = seqid) %>% 
  select(c(seq_id, dx), everything(),
         -c(reactor1, seqid))


exval_otutable <- read_excel("data/validation_data/illumina.shared.xlsx")  %>%
  rename_all(tolower) %>% 
  mutate(seq_id = seqid) %>% 
  select(-seqid)

exval_long_otu <-exval_otutable %>%
  select(-c("labels", "group")) %>% 
  pivot_longer(-seq_id, names_to="otu", values_to="count")


exval_taxonomy <- read_delim("data/validation_data/taxonomy.txt", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)%>%
  rename_all(tolower) %>%
  select(otu, taxonomy) %>%
  mutate(taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";$", ""),
         otu = str_replace_all(otu, "^O", "o")) %>%
  mutate(TAXA = taxonomy) %>% 
  separate(TAXA,
           into=c("kingdom", "phylum", "class", "order", "family", "genus"),
           sep=";")



##### Quality Control: Rarefaction Curve #######################################
#Rarefaction curve showing the number of reads in relation to the number of OTUs in the samples.

#prepare ampvis object
exval_otu <-  exval_otutable %>% 
  select(-c(labels, group, numotu0s))
exval_otu <-  t(exval_otu)
colnames(exval_otu) <- t(exval_metadata$seq_id)
exval_otu <- as.data.frame(exval_otu)
exval_otu <- rownames_to_column(exval_otu)
exval_otu <- exval_otu %>% 
  mutate(otu = rowname) %>% 
  select(-rowname) %>% 
  select(otu, everything())


exval_otu_table <-  merge(exval_taxonomy, exval_otu, by="otu") 
exval_otu_table[, 1:8] <- sapply(exval_otu_table[, 1:8], as.character)
exval_otu_table[, 9:75] <- sapply(exval_otu_table[, 9:75], as.numeric)  
exval_otu_table <- exval_otu_table %>% 
  select(-("taxonomy"))


exval_d <- amp_load(otutable = exval_otu_table,
              metadata = exval_metadata)


exval_rarecurve <- amp_rarecurve(
  exval_d,
  stepsize = 100,
  color_by = "dx",
  facet_by = NULL, #"dx",
  facet_scales = "fixed") +
  geom_vline(xintercept=1000, color = "darkred", lty = 2)+
  labs(x="Number of Reads", y="Number of Observed OTUs")+
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000), limits = c(0, 5000))+ 
  scale_color_discrete(breaks=c()) +
  theme_classic()+
  scale_color_brewer(palette = "Set1")+
  theme(legend.title = element_blank())

exval_sd = amp_subset_samples(data = exval_d, minreads = 1000, normalise = FALSE)

exval_subsetmd <- exval_sd[["metadata"]]

exval_qc_fail <- anti_join(exval_metadata, exval_subsetmd, copy = TRUE)

exval_md <- anti_join(exval_metadata, exval_qc_fail)


##### changing counts to relative abundance


exval_df <- inner_join(exval_long_otu, exval_taxonomy, by="otu") %>%
  group_by(seq_id, genus) %>%
  #summarize(count = sum(count), .groups="drop") %>%
  group_by(seq_id) %>%
  ungroup() %>% 
  mutate(row = row_number()) %>%
  group_by(seq_id) %>% 
  pivot_wider(names_from="genus", values_from="count") %>% 
  select(-c(row))
exval_df[is.na(exval_df)]<- 0
exval_df <- exval_df %>% summarise_if(is.numeric, sum) 

gc()

#Combining df with metadata
#df must be numeric and with the predicting outcome column as factor
exval_df <- exval_df %>% 
  inner_join(., exval_md, by="seq_id") %>% 
  select("dx", everything()) %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>% #converts counts to relative abundance
  mutate_if(is.numeric, funs(./total)) %>% 
  select(-total)
exval_df$dx <- as.factor(exval_df$dx)



### Extend dataframes to contain as many features as model data

exval_df <- full_join(df_validation, exval_df)
exval_df<-exval_df[-(1:50),-(461:464)]
exval_df[is.na(exval_df[,])] = 0
exval_df <- exval_df %>%  remove_rownames %>% 
  column_to_rownames(var="seq_id") 
  #select(-dx)





toc()

