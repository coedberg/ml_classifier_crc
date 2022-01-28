library(readr)
setwd("~/R/ML_pred_CRC/data/robustness_data/")


tic()

temp <- list.files(path = ".", pattern = "aligned.csv", recursive = TRUE)
myfiles <- lapply(temp, read_csv)
samples <- (t(1:24))



for (i in 1:24){
  df1<- as.data.frame(myfiles[[i]]$turn)
  colnames(df1) <- "tax"
  
  #df1$tax <- gsub("\\,.*","",df1$tax)  
  
  df1 <- df1 %>% 
    mutate(tax = str_replace(tax, "\\,.*",""),
           tax = str_replace(tax, " ","_"),
           tax = str_replace(tax, " ","_"),
           tax = str_replace(tax, " ","_"),
           tax = str_replace(tax, " ","_")) %>% 
    separate(tax,
             into=c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
             sep=";") %>% 
    filter(kingdom == c("Archaea", "Bacteria", "Unclassified"))
  df1[is.na(df1)]<- ""
  
  df1$abb <- 1  
  
  
  
  df1_abb <- aggregate(df1$abb,
                       by=list(df1$kingdom,
                               df1$phylum,
                               df1$class,
                               df1$order,
                               df1$family,
                               df1$genus,
                               df1$species),
                       FUN = sum) 
    #filter(Group.1 != "turn-check disabled")
  
  df2 <- paste(df1_abb$Group.1,
               df1_abb$Group.2,
               df1_abb$Group.3,
               df1_abb$Group.4,
               df1_abb$Group.5,
               df1_abb$Group.6,
               df1_abb$Group.7,
               df1_abb$x)
  
  samples[i] <- as.data.frame(df2)
  
}


d1 <-  as.data.frame(samples[2])
colnames(d1) <- "d1"
d1<- d1 %>% 
  separate("d1",
           into=c("kingdom", "phylum", "class", "order", "family", "genus", "species", "sample"),
           sep=" ") 

d1$tax <- paste(d1$kingdom, d1$phylum, d1$class, d1$order, d1$family, d1$genus, d1$species)

d1 <- d1 %>% 
  select(sample, tax)


df_total <- as.data.frame(d1$tax)
colnames(df_total) <- "tax"
for (i in 1:24){
  d1 <-  as.data.frame(samples[i])
  
  colnames(d1) <- "d1"
  d1<- d1 %>% 
    separate("d1",
             into=c("kingdom", "phylum", "class", "order", "family", "genus", "species", "sample"),
             sep=" ") 
  
  d1$tax <- paste(d1$kingdom, d1$phylum, d1$class, d1$order, d1$family, d1$genus, d1$species)
  
  d1 <- d1 %>% 
    select(sample, tax)
  
  
  df <- data.frame(d1)
  df_total <- merge(df_total,df, by = "tax", all.x = TRUE, all.y = TRUE)
  #colnames(df_total) <- c("tax", paste0("sample", i))
  
}


c1 <-  df_total[,1:13]

colnames(c1) <- c("tax", "sample12", "sample11", "sample10", "sample9",
                        "sample8", "sample7", "sample6", "sample5", "sample4", "sample3", "sample2", "sample1")
c1[is.na(c1)]<- 0
c1[,2:13] <- sapply(c1[,2:13], as.numeric)


c2 <-  df_total[,c(1,14:25)]

colnames(c2) <- c("tax", "sample12", "sample11", "sample10", "sample9",
                  "sample8", "sample7", "sample6", "sample5", "sample4", "sample3", "sample2", "sample1")


c2[is.na(c2)]<- 0
c2[,2:13] <- sapply(c2[,2:13], as.numeric)

df_final <- rbind(c1, c2)
df_final <- aggregate(df_final[,2:13],
                     by=list(df_final$tax),
                     FUN = sum) 
df_final<- df_final %>%
  mutate(taxonomy = Group.1) %>% 
  separate("Group.1",
           into=c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
           sep=" ") %>% 
  mutate(taxonomy = str_replace_all(taxonomy, " ", ";"))


otu_num <- data.frame(1:646)
otu_num[,2] <- "otu"
otu_num[,3] <- paste0(otu_num[,2],otu_num[,1])
otu_num <-  as.data.frame(otu_num[,3])
colnames(otu_num) <- "otu"

robust_df_tot <- cbind(otu_num, df_final)

robust_long_otu <- robust_df_tot %>% 
  select("otu", "sample12", "sample11", "sample10", "sample9",
         "sample8", "sample7", "sample6", "sample5", "sample4", "sample3", "sample2", "sample1")
robust_long_otu <- as.data.frame(t(robust_long_otu))
colnames(robust_long_otu) <- robust_long_otu[1,]
robust_long_otu <- robust_long_otu[2:13,]
robust_long_otu <- rownames_to_column(robust_long_otu, "seq_id")
robust_long_otu <- robust_long_otu %>% 
  pivot_longer(-seq_id, names_to = "otu", values_to = "count") %>% 
  mutate(count = as.double(count))
  

robust_taxonomy <- robust_df_tot %>% 
  select(c("otu", "taxonomy", "kingdom", "phylum", "class", "order", "family", "genus")) %>% 
  filter(genus != "",
         kingdom == "Bacteria")


robust_otutable <- robust_df_tot %>% 
  select(-taxonomy)


robust_metadata <- data.frame(seq_id = c("sample1", "sample2", "sample3", "sample4", "sample5","sample6", 
                               "sample7", "sample8", "sample9", "sample10", "sample11", "sample12"),
                    dx = c("normal", "normal", "normal", "normal", "normal","normal", 
                           "cancer", "cancer", "cancer", "cancer", "cancer", "cancer"))




##### Quality Control: Rarefaction Curve #######################################
#Rarefaction curve showing the number of reads in relation to the number of OTUs in the samples.

#prepare ampvis object
robust_d <- amp_load(otutable = robust_otutable,
                    metadata = robust_metadata)


robust_rarecurve <- amp_rarecurve(
  robust_d,
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

robust_sd = amp_subset_samples(data = robust_d, minreads = 1000, normalise = FALSE)

robust_subsetmd <- robust_sd[["metadata"]]

robust_qc_fail <- anti_join(robust_metadata, robust_subsetmd, copy = TRUE)

robust_md <- anti_join(robust_metadata, robust_qc_fail)


##### changing counts to relative abundance


robust_df <- inner_join(robust_long_otu, robust_taxonomy, by="otu") %>%
  group_by(seq_id, genus) %>%
  group_by(seq_id) %>%
  ungroup() %>% 
  mutate(row = row_number()) %>%
  group_by(seq_id) %>% 
  pivot_wider(names_from="genus", values_from=count) %>% 
  select(-c(row))
robust_df[is.na(robust_df)]<- 0
robust_df <- robust_df %>% summarise_if(is.numeric, sum) 

gc()

#Combining df with metadata
#df must be numeric and with the predicting outcome column as factor
robust_df <- robust_df %>% 
  inner_join(., robust_md, by="seq_id") %>% 
  select("dx", everything()) %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>% #converts counts to relative abundance
  mutate_if(is.numeric, funs(./total)) %>% 
  select(-total)
robust_df$dx <- as.factor(robust_df$dx)



### Extend dataframes to contain as many features as model data

robust_df <- full_join(df_validation, robust_df)
robust_df<-robust_df[-(1:50),-(461:800)]
robust_df[is.na(robust_df[,])] = 0
robust_df <- robust_df %>%  remove_rownames %>% 
  column_to_rownames(var="seq_id") 
#select(-dx)


robust_df_healthy <- subset(robust_df, dx == "normal")

robust_df_colitis <- subset(robust_df, dx == "cancer")


















