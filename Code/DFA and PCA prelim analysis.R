#DFA Prelim playground


MASS::lda()

#DFA for LSR's only


dfa_lsr_vars<- c("lsHl", "lsUl", "lsUol", "lsRl", "lsFl", "lsTl", "lsPpl")

dfa_lsr_dat<-dat |>
  select(catalog, genus_species, all_of(dfa_lsr_vars))|>
  drop_na()

dfa_lsr_dat |> count(genus_species) #65 specimens total after NA's dropped for LSR DFA


x<-dfa_lsr_dat |>select(all_of(dfa_lsr_vars))
grp<- dfa_lsr_dat$genus_species

fit<- MASS::lda(x,grouping=grp)

fit
##Coefficients of linear discriminants:
##LD1        LD2         LD3        LD4
##lsHl   0.2715398 -0.1459766 -0.75811049 -0.1718702
##lsUl  -0.5422881  0.2081314 -0.04560286  1.2604387
##lsUol  0.5486318  0.6317771 -0.37238692 -0.4377932
##lsRl   1.3195907 -1.0585011  0.23642214 -1.6177873
##lsFl  -0.4166175  0.4919300  0.56434068 -0.2018766
##lsTl  -1.6474060 -0.5219314 -0.43618726 -0.1263644
##lsPpl -0.4063497  0.1542006 -0.11531736  0.5771082

##Proportion of trace:
##  LD1    LD2    LD3    LD4 
##0.7521 0.2061 0.0223 0.0195 


# Share of between-group separation captured by each function
round(fit$svd^2 / sum(fit$svd^2), 3)

# Raw coefficients
fit$scaling

# Structure coefficients: correlation of each trait with each LD axis
pred <- predict(fit)
round(cor(x, pred$x), 2)
  


scores<- dfa_lsr_dat |>
  select(catalog, genus_species) |>
  bind_cols(as_tibble(pred$x))
scores  

ggplot(scores, aes(x = LD1, y = LD2, color = genus_species)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.95) +
  theme_bw()+labs(title="LSR DFA Plot")


#DFA for indices only

dfa_index_vars<-c("OLI","HPI","IM","HIND","MANUS","PES", "TAIL")

dfa_index_dat<- dat |>select(catalog, genus_species, all_of(dfa_index_vars))|>
  drop_na()

dfa_index_dat |> count(genus_species)  #63 total specimens in use for Index DFA

x.index<-dfa_index_dat |>select(all_of(dfa_index_vars))
grp.index<-dfa_index_dat$genus_species

fit.index<- MASS::lda(x.index,group=grp.index)

fit.index

round(fit.index$svd^2 / sum(fit.index$svd^2), 3)

fit.index$scaling

pred.index<-predict(fit.index)
round(cor(x, pred.index$x), 2)


scores.index<- dfa_index_dat |>
  select(catalog, genus_species) |>
  bind_cols(as_tibble(pred.index$x))
scores.index  


ggplot(scores.index, aes(x = LD1, y = LD2, color = genus_species)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.95) +
  theme_bw()+labs(title="Index Only DFA Plot")

#Combined DFA for LSR's and Indices

dfa_all_vars<- c("lsHl", "lsUl", "lsUol", "lsRl", "lsFl", "lsTl", "lsPpl","OLI","HPI","IM","HIND","MANUS","PES", "TAIL")

dfa_all_dat<- dat |> select(catalog, genus_species, all_of(dfa_all_vars)) |>
  drop_na()

dfa_all_dat |> count(genus_species)

x.all<-dfa_all_dat |> select(all_of(dfa_all_vars))
grp.all<-dfa_all_dat$genus_species

fit.all<-MASS::lda(x.all,grouping=grp.all)

fit.all

pred.all<-predict(fit.all)
round(cor(x.all,pred.all$x,2)

scores.all<-dfa_all_dat |> select(catalog, genus_species) |> bind_cols(as_tibble(pred.all$x))
scores.all

ggplot(scores.all, aes(x = LD1, y = LD2, color = genus_species)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.95) +
  theme_bw()+labs(title="LSR + Index DFA Plot")

ggplot(scores.all, aes(x = LD1, y = LD3, color = genus_species)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.95) +
  theme_bw()+labs(title="LSR + Index DFA Plot")




## PCA 

#PCA for log-shape ratio metrics

pca_lsr_vars <- c("lsHl", "lsUl", "lsUol", "lsFl","lsPpl") #Dropped lsRl from list
# because including all four variables that comprise the GM is apparently an issue (according to claude atleast).

dat |>
  summarise(across(all_of(pca_lsr_vars), ~ sum(!is.na(.x)))) |>
  pivot_longer(everything(), names_to = "var", values_to = "n_present")
#Tibia length has present in far fewer specimens (71 compared to ~90 for all other variables).
#Removed for now since NA's will be dropped.

pca_lsr_dat<- dat |> select(catalog, genus_species, all_of(pca_lsr_vars))|> drop_na()

pca_lsr_dat |> count(genus_species)

pca.lsr<-pca_lsr_dat|>select(all_of(pca_lsr_vars))|>prcomp(center=TRUE, scale.=TRUE)

summary(pca.lsr)
screeplot(pca.lsr, type="lines")
round(pca.lsr$rotation[,1:3],2)

scores.lsrpca <- pca_lsr_dat |>
  select(catalog, genus_species) |>
  bind_cols(as_tibble(pca.lsr$x))

var_exp.lsr <- round(100 * pca.lsr$sdev^2 / sum(pca.lsr$sdev^2), 1)

ggplot(scores.lsrpca, aes(x = PC1, y = PC2, color = genus_species)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.95) +
  labs(x = glue::glue("PC1 ({var_exp.lsr[1]}%)"),
       y = glue::glue("PC2 ({var_exp.lsr[2]}%)")) +
  theme_bw()+labs(title="LSR Only PCA")



#PCA for indices only

pca_index_vars <- c("OLI","HPI","IM","HIND","MANUS","PES", "TAIL")

dat |>
  summarise(across(all_of(pca_index_vars), ~ sum(!is.na(.x)))) |>
  pivot_longer(everything(), names_to = "var", values_to = "n_present")                       
#Keeping IM in for now despite its lack of availibility. Stems from the same issue of femurs being cut.

pca_index_dat<- dat |> select(catalog, genus_species, all_of(pca_index_vars)) |> drop_na()

pca_index_dat |> count(genus_species)

pca.index<- pca_index_dat |>select(all_of(pca_index_vars))|>prcomp(center=TRUE, scale.=TRUE)

summary(pca.index)
screeplot(pca.index, type="lines")
round(pca.index$rotation[,1:3],2)

scores.indexpca<- pca_index_dat |> select(catalog, genus_species) |>bind_cols(as.tibble(pca.index$x))

var_exp.index <- round(100 *pca.index$sdev^2 /sum(pca.index$sdev^2), 1)

ggplot(scores.indexpca, aes(x = PC1, y = PC2, color = genus_species)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.95) +
  labs(x = glue::glue("PC1 ({var_exp.index[1]}%)"),
       y = glue::glue("PC2 ({var_exp.index[2]}%)")) +
  theme_bw()+labs(title="Index Only PCA")



#PCA combined LSR and index

pca_all_vars<- c("OLI","HPI","IM","HIND","MANUS","PES", "TAIL","lsHl", "lsUl", "lsUol", "lsFl","lsPpl")
#Still lacking lsTl and lsRl for the reasons notated above

pca_all_dat<- dat |> select(catalog, genus_species, all_of(pca_all_vars))|>drop_na()

pca_all_dat |> count(genus_species)

pca.all<-pca_all_dat |> select(all_of(pca_all_vars)) |>prcomp(center=TRUE, scale.=TRUE)

summary(pca.index)
screeplot(pca.index, type="lines")
round(pca.index$rotation[,1:3],2)

scores.allpca<- pca_all_dat |> select(catalog, genus_species) |> bind_cols(as.tibble(pca.all$x))

var_exp.all<- round(100*pca.all$sdev^2 / sum(pca.all$sdev^2),1)

ggplot(scores.allpca, aes(x = PC1, y = PC2, color = genus_species)) +
  geom_point(size = 2) +
  stat_ellipse(level = 0.95) +
  labs(x = glue::glue("PC1 ({var_exp.all[1]}%)"),
       y = glue::glue("PC2 ({var_exp.all[2]}%)")) +
  theme_bw()+labs(title="Combined PCA")

