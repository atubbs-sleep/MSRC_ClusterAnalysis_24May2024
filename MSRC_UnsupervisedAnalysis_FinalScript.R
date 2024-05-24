#### Load Packages ####
library(lmtest)
library(sandwich)
library(VIM)
library(cluster)
library(NbClust)
library(gtsummary)
library(ggpubr)
library(broom)
library(tidyverse)
library(Rtsne)
library(missForest)
library(doParallel)
library(parallel)
library(factoextra)
library(ClusterR)
library(cobalt)
library(WeightIt)

#### Data Processing ####
# Load Data
msrc <- read_csv("../msrcdata.csv",
                 na=c("#NULL!","999.00","888.00","777.00",999,888,998),
                 guess_max = 6556) %>%
  # Format variables
  transmute(
    # Demographic Variables
    "Age" = Age,
    "Sex" = factor(Gender, levels = 1:2, labels = c("Male","Female")),
    "Race" = factor(Race, levels = 1:7, labels = c("White","Black","Other","Other","Other","Other","Other")),
    "Ethnicity" = factor(Ethnicity, levels = 1:2, labels = c("Hispanic","Non-Hispanic")),
    "Education" = factor(EducationJT, levels = 0:4, 
                         labels=c("HS or less","HS or less","Some college",
                                  "College degree or more","College degree or more")),
    "Relationship" = factor(Relationship_Status, levels = c(1:5),
                            labels = c("Married/Cohab","Single","Married/Cohab","Divorced/Separated/Widowed","Divorced/Separated/Widowed")),
    # DSISS
    "DSISS_Thoughts" = CDE1 %>% as.factor(),
    "DSISS_Plan" = CDE2 %>% as.factor(),
    "DSISS_Control" = CDE3 %>% as.factor(),
    "DSISS_Impulse" = CDE4 %>% as.factor(),
    # SBQ
    "SBQ_1" = case_when(CDE5==1 | CDE5_Schmidt==1 ~ 1,
                        CDE5==2 | CDE5_Schmidt==2 ~ 2,
                        CDE5==3 | CDE5_Schmidt==3 | CDE5==4 ~ 3,
                        CDE5==5 | CDE5_Schmidt==4 | CDE5==6 | CDE5==7 ~ 4) %>% as.factor(),
    "SBQ_2" = case_when(CDE6==0 | CDE6_Schmidt==1 ~ 1,
                        CDE6==1 | CDE6_Schmidt==2 ~ 2,
                        CDE6==2 | CDE6_Schmidt==3 ~ 3,
                        CDE6==3 | CDE6_Schmidt==4 ~ 4,
                        CDE6==4 | CDE6_Schmidt==5 ~ 4) %>% as.factor(),
    "SBQ_3" = case_when(CDE7==1 | CDE7_Schmidt==1 ~ 1,
                        CDE7==2 | CDE7==3 |CDE7_Schmidt==2 ~ 2,
                        CDE7==4 | CDE7==5 |CDE7_Schmidt==3 ~ 3) %>% as.factor(),
    "SBQ_4" = CDE8 %>% as.factor(),
    # SIS
    "SIS_1" = CDE9 %>% as.factor(),
    "SIS_2" = CDE10 %>% as.factor(),
    "SIS_3" = CDE11 %>% as.factor(),
    "SIS_4" = CDE12 %>% as.factor(),
    # Truncate self-reported suicide attempts by 90th percentile
    "nSA" = if_else(CDE13>4,NA_real_,CDE13),
    # Truncate self-reported non-suicidal self-injury by 90th percentile
    "nNSSI" = if_else(CDE17>50,NA_real_,CDE17),
    # INQ
    "INQ_7" = (8-if_else(is.na(CDE24)==F,CDE24,INQ7)) %>% as.factor(),
    "INQ_8" = (8-if_else(is.na(CDE25)==F,CDE25,INQ8)) %>% as.factor(),
    "INQ_10" = (8-if_else(is.na(CDE28)==F, CDE28, 
                         if_else(is.na(CDE28INQ)==F,CDE28INQ,INQ10))) %>% as.factor(),
    "INQ_13" = (8-if_else(is.na(CDE26)==F,CDE26,INQ13)) %>% as.factor(),
    "INQ_14" = (8-if_else(is.na(CDE27)==F,CDE27,INQ14)) %>% as.factor(),
    # BSS 1-5 + 20
    "BSS_1" = if_else(is.na(SSI1)==F,SSI1,if_else(is.na(CDE19)==F,CDE19,BSS1)) %>% as.factor(),
    "BSS_2" = if_else(is.na(SSI2)==F,SSI2,if_else(is.na(CDE20)==F,CDE20,BSS2)) %>% as.factor(),
    "BSS_3" = if_else(is.na(SSI3)==F,SSI3,BSS3) %>% as.factor(),
    "BSS_4" = if_else(is.na(SSI4)==F,SSI4,BSS4) %>% as.factor(),
    "BSS_5" = if_else(is.na(SSI5)==F,SSI5,BSS5) %>% as.factor(),
    "BSS_20" = BSS20 %>% as.factor(),
    # 3-item BHS
    "BHS_8" = if_else(is.na(CDE21)==F, CDE21, BHS8) %>% as.factor(),
    "BHS_12" = if_else(is.na(CDE22)==F, CDE22, BHS12) %>% as.factor(),
    "BHS_14" = if_else(is.na(CDE23)==F, CDE23, BHS14) %>% as.factor(),
    # AUDIT-C
    "AUDIT_1" = if_else(is.na(CDE46)==F,CDE46,AUDIT1) %>% as.factor(),
    "AUDIT_2" = if_else(is.na(CDE47)==F,CDE47,AUDIT2) %>% as.factor(),
    "AUDIT_3" = if_else(is.na(CDE48)==F,CDE48,AUDIT3) %>% as.factor(),
    # PCL
    "PCL_1" = if_else(is.na(CDE34)==F,CDE34,PCLM1) %>% as.factor(),
    "PCL_2" = if_else(is.na(CDE35)==F,CDE35,PCLM2) %>% as.factor(),
    "PCL_3" = if_else(is.na(CDE36)==F,CDE36,PCLM3) %>% as.factor(),
    "PCL_5" = if_else(is.na(CDE37)==F,CDE37,PCLM5) %>% as.factor(),
    "PCL_6" = if_else(is.na(CDE38)==F,CDE38,PCLM6) %>% as.factor(),
    "PCL_7" = if_else(is.na(CDE39)==F,CDE39,PCLM7) %>% as.factor(),
    "PCL_16" = if_else(is.na(CDE40)==F,CDE40,PCLM16) %>% as.factor(),
    "PCL_17" = if_else(is.na(CDE41)==F,CDE41,PCLM17) %>% as.factor(),
    # TBI x4
    "TBI_1" = CDE42 %>% as.factor(),
    "TBI_2" = CDE43 %>% as.factor(),
    "TBI_3" = CDE44 %>% as.factor(),
    "TBI_4" = CDE45 %>% as.factor(),
    # ISI
    "ISIFalling" = CDE52 %>% as.factor(),
    "ISIStaying" = CDE53 %>% as.factor(),
    "ISIEMA" = CDE54 %>% as.factor(),
    "ISISatisfied" = CDE55 %>% as.factor(),
    "ISIInterfere" = CDE56 %>% as.factor(),
    # ASI
    "ASI_1" = case_when(CDE29==1 | CDE29_7point==1 ~ 1,
                        CDE29==2 | CDE29_7point==2 | CDE29_7point==3 ~ 2,
                        CDE29==3 | CDE29_7point==4 ~ 3,
                        CDE29==4 | CDE29_7point==5 | CDE29_7point==6 ~ 4,
                        CDE29==5 | CDE29_7point==7 ~ 5) %>% as.factor(),
    "ASI_2" = case_when(CDE30==1 | CDE30_7point==1 ~ 1,
                        CDE30==2 | CDE30_7point==2 | CDE30_7point==3 ~ 2,
                        CDE30==3 | CDE30_7point==4 ~ 3,
                        CDE30==4 | CDE30_7point==5 | CDE30_7point==6 ~ 4,
                        CDE30==5 | CDE30_7point==7 ~ 5) %>% as.factor(),
    "ASI_3" = case_when(CDE31==1 | CDE31_7point==1 ~ 1,
                        CDE31==2 | CDE31_7point==2 | CDE31_7point==3 ~ 2,
                        CDE31==3 | CDE31_7point==4 ~ 3,
                        CDE31==4 | CDE31_7point==5 | CDE31_7point==6 ~ 4,
                        CDE31==5 | CDE31_7point==7 ~ 5) %>% as.factor(),
    "ASI_4" = case_when(CDE32==1 | CDE32_7point==1 ~ 1,
                        CDE32==2 | CDE32_7point==2 | CDE32_7point==3 ~ 2,
                        CDE32==3 | CDE32_7point==4 ~ 3,
                        CDE32==4 | CDE32_7point==5 | CDE32_7point==6 ~ 4,
                        CDE32==5 | CDE32_7point==7 ~ 5) %>% as.factor(),
    "ASI_5" = case_when(CDE33==1 | CDE33_7point==1 ~ 1,
                        CDE33==2 | CDE33_7point==2 | CDE33_7point==3 ~ 2,
                        CDE33==3 | CDE33_7point==4 ~ 3,
                        CDE33==4 | CDE33_7point==5 | CDE33_7point==6 ~ 4,
                        CDE33==5 | CDE33_7point==7 ~ 5) %>% as.factor(),
    "CurrentSI" = if_else(CDE1 + CDE2 + CDE3 + CDE4 > 3,1,0),
    "LifetimeSI" = if_else(SBQ_1==2|SBQ_1==3|SBQ_1==4|CurrentSI==1,1,0)) %>% 
  # Select only those with lifetime suicidal ideation
  filter(LifetimeSI==1) %>% 
  dplyr::select(-LifetimeSI,-CurrentSI)
  
#### Step 1: random forest single imputation to fill missing data ####
# Create core cluster for parallel processing
set.seed(165)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
# Impute data
msrc.imp <- msrc %>%
  # Convert data to data matrix
  as.data.frame() %>%
  # run random forest imputation
  missForest(
    xmis = .,
    variablewise = T,
    parallelize = "variables"
  )
# D/c the parallel core cluster
stopCluster(cl)

#### Step 2: Perform t-SNE process for data reduction / 2 dimensional mapping ####
set.seed(564)
msrc.clusdat <- bind_cols(
  msrc.imp$ximp %>%
    # Remove demographic variables except age and sex
    select(-c(3:9)) %>%
    # Round up if imputed numbers are decimals. 
    mutate(
      "nNSSI" = ceiling(nNSSI),
      "nSA" = ceiling(nSA),
    ),
  msrc.imp$ximp %>%
    # Convert to numeric
    mutate(across(c(10:21,24:62), ~as.numeric(.x))) %>%
    # Calculate summary scores; subtract for scales that start at 0 d/t factor adjustment
    transmute(
      "DSISS_Total" = DSISS_Thoughts+DSISS_Plan+DSISS_Control+DSISS_Impulse-4,
      "SBQ_Total" = SBQ_1+SBQ_2+SBQ_3+(SBQ_4-1),
      "SIS_Total" = SIS_1+SIS_2+SIS_3+SIS_4-4,
      "BSS_Total" = BSS_1+BSS_2+BSS_3+BSS_4+BSS_5-5,
      "INQ_Belong" = INQ_7 + INQ_8 + INQ_10 + INQ_13 + INQ_14,
      "PCL_Total" = PCL_1 + PCL_2 + PCL_3 + PCL_5 + PCL_6 + PCL_7 + PCL_16 + PCL_17,
      "ISI_Total" = ISIFalling+ISIStaying+ISIEMA+ISISatisfied+ISIInterfere-5,
      "ASI_Total" = ASI_1+ASI_2+ASI_3+ASI_4+ASI_5,
      "AUDIT_Total" = AUDIT_1+AUDIT_2+AUDIT_3-3,
      "BHS_subscore" = BHS_8+BHS_12+BHS_14-3,
      "TBI_Total" = TBI_1+TBI_2+TBI_3+TBI_4-4)
) 
msrc.sne <- msrc.clusdat %>%
  data.matrix() %>% 
  # Run TSNE algorithm with perplexity of 30 (default) 
  Rtsne(perplexity = 30,pca=F,normalize=F)
plot(msrc.sne$Y)+title("Perplexity = 30")

#### Step 3: Cluster the t-SNE mapping using GMM, hierarchical, and k-means ####
set.seed(111)

## Gaussian Mixture Modeling
# Identify optimal number of components
optGMMClus <- 
  Optimal_Clusters_GMM(
    msrc.sne$Y, 
    max_clusters = 10, 
    criterion = "BIC", 
    dist_mode = "eucl_dist", 
    seed_mode = "random_subset"
    )

GMMsils <- c(0)
# Calculate silhouette widths for 2 to 10 components
for(idx in 2:10){
  clusnum <- 
  GMMsils[idx] <- 
    silhouette_of_clusters(
      data = msrc.sne$Y,
      clusters = 
        predict(
          GMM(msrc.sne$Y,
              gaussian_comps = idx,
              dist_mode = "eucl_dist",
              seed_mode = "random_subset"),
          newdata = msrc.sne$Y)
      )$silhouette_global_average
}
# Model data using optimal components = 3
GMMclusters <- 
  predict(
    GMM(msrc.sne$Y,
        gaussian_comps = 3,
        dist_mode = "eucl_dist",
        seed_mode = "random_subset"),
    newdata = msrc.sne$Y
  )
# Visualize cluster assignment 
hullplot(msrc.sne$Y,GMMclusters, col = c("black","darkgreen","blue3","red3","cyan3"))

## Agglomerative Hierarchical Clustering (AGNES)
msrc.agnes <- agnes(
  msrc.sne$Y,
  diss=F,
  metric = "euclidean",
  stand=F,
  method = "average",
)
# Review Dendrogram 
plot(msrc.agnes,which.plot = 2)

# Review the silhouette metric to identify optimal cluster
agnes.p <- fviz_nbclust(
  msrc.sne$Y,
  FUNcluster = hcut,
  method = "silhouette",
  k.max = 10,
)
agnes.p1 <- agnes.p
agnes.p1$layers[[3]] <- NULL
agnes.p1$labels$title <- NULL
# Plot the cluster assignments over the t-SNE representation
hullplot(msrc.sne$Y,cutree(msrc.agnes,3),col = c("black","darkgreen","blue3","red3","cyan3","darkorange","blueviolet"))

# Check k-means silhouette cluster recommendation
pam.p <- fviz_nbclust(
  msrc.sne$Y,
  FUNcluster = pam,
  method = "silhouette",
  k.max = 10,
)
pam.p1 <- pam.p
pam.p1$layers[[3]] <- NULL
pam.p1$labels$title <- NULL

# Map out the recommended k-means clustering on t-SNE
msrc.pam <- pam(
  msrc.sne$Y,
  metric = "euclidean",
  k = 3,
  pamonce = 6
)
hullplot(msrc.sne$Y,msrc.pam$clustering, col = c("black","darkgreen","blue3","red3","cyan3"))

# Cluster justification figure
fig1 <- 
  ggarrange(labels = c("A","B"),nrow = 1, ncol = 2,widths = c(1,1.5),
    # t-SNE map
    ggscatter(as_tibble(msrc.sne$Y), x="V1",y="V2",shape = 1)+
      xlab("t-SNE Axis 1") + ylab("t-SNE Axis 2"),
    ggarrange(ncol = 1, nrow = 2, labels = c("","C"),
      # Cluster Selection
      ggarrange(ncol = 3, nrow = 1,
        # Silhouette Width
        ggline(
          tibble("Clusters" = factor(1:10),"Sils" = GMMsils), 
          x="Clusters", 
          y = "Sils",
          ylab = "Average silhouette width", 
          xlab = "Number of clusters k",
          color = "steelblue",
          title = "GMM") +
          geom_point(aes(x = 3, y = .4160443), shape = 1, size = 5, stroke = 2)+
          scale_y_continuous(limits = c(0,0.425)),
        # Silhouette Width
        agnes.p1+
          geom_point(aes(x = 3, y = .3830818), shape = 1, size = 5, stroke = 2)+
          ggtitle("HClust")+
          scale_y_continuous(limits = c(0,0.425)),
        # Silhouette Width
        pam.p1+
          geom_point(aes(x = 3, y = .4214362), shape = 1, size = 5, stroke = 2)+
          ggtitle("PAM")+
          scale_y_continuous(limits = c(0,0.425))
      ),
      # Cluster Hull Plots
      ggarrange(nrow = 1, ncol = 3,common.legend = T,
        # GMM
        ggscatter(
          bind_cols(as_tibble(msrc.sne$Y),"Cluster" = factor(GMMclusters)), 
          x="V1",y="V2",
          color="Cluster",shape="Cluster",palette = c("#7570B3","#1B9E77","#D95F02"),alpha=0.05,
          ellipse = T, mean.point=T, ellipse.alpha = 0.35,ellipse.type = "convex") +
          xlab("t-SNE Axis 1") + ylab("t-SNE Axis 2"),
        # AGNES
        ggscatter(
          bind_cols(as_tibble(msrc.sne$Y),"Cluster" = factor(cutree(msrc.agnes,3),levels = c(2,1,3))), 
          x="V1",y="V2",
          color="Cluster",shape="Cluster",palette = c("#7570B3","#1B9E77","#D95F02"),alpha=0.05,
          ellipse = T, mean.point=T, ellipse.alpha = 0.35,ellipse.type = "convex")+
          xlab("t-SNE Axis 1") + ylab("t-SNE Axis 2"),
        # PAM
        ggscatter(
          bind_cols(as_tibble(msrc.sne$Y),"Cluster" = factor(msrc.pam$clustering,levels = c(2,1,3))), 
          x="V1",y="V2",
          color="Cluster",shape="Cluster",palette = c("#7570B3","#1B9E77","#D95F02"),alpha=0.05,
          ellipse = T, mean.point=T, ellipse.alpha = 0.35,ellipse.type = "convex")+
          xlab("t-SNE Axis 1") + ylab("t-SNE Axis 2")
      )
    )
  )

# tiff("../fig1.tiff",
#      res = 300, width = 4000, height = 2000)
# fig1
# dev.off()

### Hierarchical/Connectivity clustering makes most visual sense. K-means has highest silhouette but not by much and clusters are a little "hard".
### Proceed with agnes cluster with 3 cluster cutpoint.

#### Step 4: Combine cluster assignments for comparison ####
msrc.tab <- 
  bind_cols(
    msrc,
    Cluster = factor(GMMclusters, levels = c(2,1,3), labels = 1:3)
  ) %>% 
  mutate(across(c(10:21,24:62), ~as.numeric(.x))) %>%
  mutate(
    Cluster1 = if_else(Cluster==1,1,0) %>% factor(),
    Cluster2 = if_else(Cluster==2,1,0) %>% factor(),
    Cluster3 = if_else(Cluster==3,1,0) %>% factor(),
    "nNSSI" = ceiling(nNSSI),
    "nSA" = ceiling(nSA),
    "DSISS_Total" = DSISS_Thoughts+DSISS_Plan+DSISS_Control+DSISS_Impulse-4,
    "SBQ_Total" = SBQ_1+SBQ_2+SBQ_3+(SBQ_4-1),
    "SBQc" = if_else(SBQ_Total >=8, 1, 0) %>% factor(., 0:1, c("Low Risk","High Risk")),
    "SIS_Total" = SIS_1+SIS_2+SIS_3+SIS_4-4,
    "BSS_Total" = BSS_1+BSS_2+BSS_3+BSS_4+BSS_5-5,
    "INQ_Belong" = INQ_7 + INQ_8 + INQ_10 + INQ_13 + INQ_14,
    "PCL_Total" = PCL_1 + PCL_2 + PCL_3 + PCL_5 + PCL_6 + PCL_7 + PCL_16 + PCL_17,
    "ISI_Total" = ISIFalling+ISIStaying+ISIEMA+ISISatisfied+ISIInterfere-5,
    "CurrentInsomnia" = if_else(ISI_Total >= 10, 1, 0),
    "ASI_Total" = ASI_1+ASI_2+ASI_3+ASI_4+ASI_5,
    "AUDIT_Total" = AUDIT_1+AUDIT_2+AUDIT_3-3,
    "AUDITc" = case_when(Sex=="Male" & AUDIT_Total>3 ~ "Misuse",
                         Sex=="Female" & AUDIT_Total>2 ~ "Misuse",
                         Sex=="Male" & AUDIT_Total<4 ~ "Typical",
                         Sex=="Female" & AUDIT_Total < 3 ~ "Typical") %>% 
      factor(levels = c("Typical","Misuse")),
    "BHS_subscore" = BHS_8+BHS_12+BHS_14-3,
    "CurrentSI" = if_else(DSISS_Total > 3,1,0),
    "TBI_Total" = TBI_1+TBI_2+TBI_3+TBI_4-4,
  ) %>% 
  as_tibble()

# Cluster Descriptions
# Compare cluster 2 to cluster 1
tbl2.1.1 <- 
  tbl_summary(
    msrc.tab %>% filter(Cluster!=3) %>% mutate(Cluster = factor(Cluster)),
    include = c(CurrentSI,
                Age, Sex, Race, Ethnicity, Education, Relationship,
                CurrentInsomnia,ISIFalling,ISIStaying,ISIEMA,ISIInterfere,ISISatisfied,
                nSA,nNSSI,SBQc,SBQ_Total,
                INQ_Belong,PCL_Total,ASI_Total,AUDITc,TBI_Total),
    by = "Cluster",
    type = list(Ethnicity ~ "dichotomous",
                starts_with("ISI") ~ "continuous",
                TBI_Total ~ "continuous",
                nSA ~ "continuous",
                SBQc ~ "dichotomous",
                AUDITc ~ "dichotomous"
    ),
    value = list(Sex = "Male",
                 Ethnicity = "Hispanic",
                 SBQc = "High Risk",
                 AUDITc = "Misuse"),
    label = list(Sex = "Male",
                 Ethnicity = "Hispanic"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ c(1,1),
                  all_categorical() ~ c(0,1)),
  ) %>% add_overall() %>%
  add_p(test = list(all_continuous() ~ "wilcox.test",all_categorical() ~ "chisq.test"))
# Compare cluster 3 to cluster 1
tbl2.1.2 <- 
  tbl_summary(
    msrc.tab %>% filter(Cluster!=2) %>% mutate(Cluster = factor(Cluster)),
    include = c(CurrentSI,
                Age, Sex, Race, Ethnicity, Education, Relationship,
                CurrentInsomnia,ISIFalling,ISIStaying,ISIEMA,ISIInterfere,ISISatisfied,
                nSA,nNSSI,SBQc,SBQ_Total,
                INQ_Belong,PCL_Total,ASI_Total,AUDITc,TBI_Total),
    by = "Cluster",
    type = list(Ethnicity ~ "dichotomous",
                starts_with("ISI") ~ "continuous",
                TBI_Total ~ "continuous",
                nSA ~ "continuous",
                SBQc ~ "dichotomous",
                AUDITc ~ "dichotomous"
    ),
    value = list(Sex = "Male",
                 Ethnicity = "Hispanic",
                 SBQc = "High Risk",
                 AUDITc = "Misuse"),
    label = list(Sex = "Male",
                 Ethnicity = "Hispanic"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ c(1,1),
                  all_categorical() ~ c(0,1)),
  ) %>% add_overall() %>%
  add_p(test = list(all_continuous() ~ "wilcox.test",all_categorical() ~ "chisq.test"))
# Compare Cluster 3 to cluster 2
tbl2.1.3 <- 
  tbl_summary(
    msrc.tab %>% filter(Cluster!=1) %>% mutate(Cluster = factor(Cluster)),
    include = c(CurrentSI,
                Age, Sex, Race, Ethnicity, Education, Relationship,
                CurrentInsomnia,ISIFalling,ISIStaying,ISIEMA,ISIInterfere,ISISatisfied,
                nSA,nNSSI,SBQc,SBQ_Total,
                INQ_Belong,PCL_Total,ASI_Total,AUDITc,TBI_Total),
    by = "Cluster",
    type = list(Ethnicity ~ "dichotomous",
                starts_with("ISI") ~ "continuous",
                TBI_Total ~ "continuous",
                nSA ~ "continuous",
                SBQc ~ "dichotomous",
                AUDITc ~ "dichotomous"
    ),
    value = list(Sex = "Male",
                 Ethnicity = "Hispanic",
                 SBQc = "High Risk",
                 AUDITc = "Misuse"),
    label = list(Sex = "Male",
                 Ethnicity = "Hispanic"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ c(1,1),
                  all_categorical() ~ c(0,1)),
  ) %>% add_overall() %>%
  add_p(test = list(all_continuous() ~ "wilcox.test",all_categorical() ~ "chisq.test"))
    
# Create Table 2
tbl2.2 <- 
  tbl_merge(tab_spanner = c("Cluster 1","Cluster 2", "Cluster 3"), list(
    tbl_summary(
      msrc.tab %>% filter(Cluster == 1),
      include = c(CurrentSI,
                  Age, Sex, Race, Ethnicity, Education, Relationship,
                  CurrentInsomnia,ISIFalling,ISIStaying,ISIEMA,ISIInterfere,ISISatisfied,
                  nSA,nNSSI,SBQc,SBQ_Total,
                  INQ_Belong,PCL_Total,ASI_Total,AUDITc,TBI_Total),
      by="CurrentSI",
      type = list(Ethnicity ~ "dichotomous",
                  starts_with("ISI") ~ "continuous",
                  TBI_Total ~ "continuous",
                  nSA ~ "continuous",
                  SBQc ~ "dichotomous",
                  AUDITc ~ "dichotomous"
      ),
      value = list(Sex = "Male",
                   Ethnicity = "Hispanic",
                   SBQc = "High Risk",
                   AUDITc = "Misuse"),
      label = list(Sex = "Male",
                   Ethnicity = "Hispanic"
      ),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} ({p}%)"),
      digits = list(all_continuous() ~ c(1,1),
                    all_categorical() ~ c(0,1)),
    ) %>% add_overall() %>%
      add_p(test = list(all_continuous() ~ "wilcox.test",all_categorical() ~ "chisq.test")),
    tbl_summary(
      msrc.tab %>% filter(Cluster == 2),
      include = c(CurrentSI,
                  Age, Sex, Race, Ethnicity, Education, Relationship,
                  CurrentInsomnia,ISIFalling,ISIStaying,ISIEMA,ISIInterfere,ISISatisfied,
                  nSA,nNSSI,SBQc,SBQ_Total,
                  INQ_Belong,PCL_Total,ASI_Total,AUDITc,TBI_Total),
      by="CurrentSI",
      type = list(Ethnicity ~ "dichotomous",
                  starts_with("ISI") ~ "continuous",
                  TBI_Total ~ "continuous",
                  nSA ~ "continuous",
                  SBQc ~ "dichotomous",
                  AUDITc ~ "dichotomous"
      ),
      value = list(Sex = "Male",
                   Ethnicity = "Hispanic",
                   SBQc = "High Risk",
                   AUDITc = "Misuse"),
      label = list(Sex = "Male",
                   Ethnicity = "Hispanic"
      ),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} ({p}%)"),
      digits = list(all_continuous() ~ c(1,1),
                    all_categorical() ~ c(0,1)),
    ) %>% add_overall() %>%
      add_p(test = list(all_continuous() ~ "wilcox.test",all_categorical() ~ "chisq.test")),
    tbl_summary(
      msrc.tab %>% filter(Cluster == 3),
      include = c(CurrentSI,
                  Age, Sex, Race, Ethnicity, Education, Relationship,
                  CurrentInsomnia,ISIFalling,ISIStaying,ISIEMA,ISIInterfere,ISISatisfied,
                  nSA,nNSSI,SBQc,SBQ_Total,
                  INQ_Belong,PCL_Total,ASI_Total,AUDITc,TBI_Total),
      by="CurrentSI",
      type = list(Ethnicity ~ "dichotomous",
                  starts_with("ISI") ~ "continuous",
                  TBI_Total ~ "continuous",
                  nSA ~ "continuous",
                  SBQc ~ "dichotomous",
                  AUDITc ~ "dichotomous"
      ),
      value = list(Sex = "Male",
                   Ethnicity = "Hispanic",
                   SBQc = "High Risk",
                   AUDITc = "Misuse"),
      label = list(Sex = "Male",
                   Ethnicity = "Hispanic"
      ),
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} ({p}%)"),
      digits = list(all_continuous() ~ c(1,1),
                    all_categorical() ~ c(0,1)),
    ) %>% add_overall() %>%
      add_p(test = list(all_continuous() ~ "wilcox.test",all_categorical() ~ "chisq.test"))
  ))


#### Step 5: Does insomnia relate to Current SI differently among clusters? ####
## Create dataset from RF imputation with cluster labels
msrc.moddat <- bind_cols(
  # Take imputed data
  msrc.imp$ximp,
  # Bind with cluster labels, relabeling to match earlier figures
  Cluster = factor(GMMclusters, levels = c(2,1,3), labels = 1:3)
) %>% 
  # Convert factor data to numerics)
  mutate(across(c(10:21,24:62), ~as.numeric(.x))) %>%
  mutate(
    Cluster1 = if_else(Cluster==1,1,0) %>% factor(),
    Cluster2 = if_else(Cluster==2,1,0) %>% factor(),
    Cluster3 = if_else(Cluster==3,1,0) %>% factor(),
    # Add in summary scores
    "nNSSI" = ceiling(nNSSI),
    "nSA" = ceiling(nSA),
    "DSISS_Total" = DSISS_Thoughts+DSISS_Plan+DSISS_Control+DSISS_Impulse-4,
    "SBQ_Total" = SBQ_1+SBQ_2+SBQ_3+(SBQ_4-1),
    "SBQc" = if_else(SBQ_Total >=8, 1, 0) %>% factor(., 0:1, c("Low Risk","High Risk")),
    "SIS_Total" = SIS_1+SIS_2+SIS_3+SIS_4-4,
    "BSS_Total" = BSS_1+BSS_2+BSS_3+BSS_4+BSS_5-5,
    "INQ_Belong" = INQ_7 + INQ_8 + INQ_10 + INQ_13 + INQ_14,
    "PCL_Total" = PCL_1 + PCL_2 + PCL_3 + PCL_5 + PCL_6 + PCL_7 + PCL_16 + PCL_17,
    "ISI_Total" = ISIFalling+ISIStaying+ISIEMA+ISISatisfied+ISIInterfere-5,
    "CurrentInsomnia" = if_else(ISI_Total >= 10, 1, 0),
    "ASI_Total" = ASI_1+ASI_2+ASI_3+ASI_4+ASI_5,
    "AUDIT_Total" = AUDIT_1+AUDIT_2+AUDIT_3-3,
    "AUDITc" = case_when(Sex=="Male" & AUDIT_Total>3 ~ "Misuse",
                         Sex=="Female" & AUDIT_Total>2 ~ "Misuse",
                         Sex=="Male" & AUDIT_Total<4 ~ "Typical",
                         Sex=="Female" & AUDIT_Total < 3 ~ "Typical") %>% 
      factor(levels = c("Typical","Misuse")),
    "BHS_subscore" = BHS_8+BHS_12+BHS_14-3,
    "CurrentSI" = if_else(DSISS_Total > 3,1,0),
    "TBI_Total" = TBI_1+TBI_2+TBI_3+TBI_4-4,
  ) %>% 
  as_tibble()
## Setup Modeling Loop Parameters
predictors <- c("CurrentInsomnia","ISIFalling","ISIStaying","ISIEMA","ISISatisfied","ISIInterfere")
covariates <- c("",
                "+Age+Sex+Race+Ethnicity+Education+Relationship",
                "+INQ_Belong+ASI_Total+BHS_subscore+AUDITc+PCL_Total")
modelname <- c("Model 1","Model 2","Model 3")
# Create inverse propensity score weighting to balance across lifetime suicide risk from SBQ 
wts <-
  weightit(
    formula = Cluster ~ SBQc,
    data = msrc.moddat,
    method = "cbps",
    stabilize = T,
    estimand = "ATE",
    stop.method = "es.mean",
    over=F
  )

clusmodtbl <- tibble()
# SI ~ Cluster
# For each model
for(idx in 1:length(modelname)){
  mod <- glm(
    formula(paste0("CurrentSI ~ Cluster",covariates[idx])),
    "poisson",
    msrc.moddat,
    # Insert propensity score weights
    weights = wts$weights
    )
  # Combine into a single formatted tibble object
  clusmodtbl <- bind_rows(
    clusmodtbl,
    crossing(
      "Model" = modelname[idx],
      bind_rows(
        tibble(term = "Cluster1",estimate = 0),
        coeftest(mod, sandwich(mod)) %>%
          tidy(conf.int=T) %>%
          slice(2:3)
        ) %>%
        transmute(
          "Cluster" = factor(
            term,
            levels = c("Cluster1","Cluster2","Cluster3"), 
            labels = c("Cluster 1","Cluster 2","Cluster 3")
            ),
          "PRR" = round(exp(estimate),2),
          "Lower" = round(exp(conf.low),2),
          "Upper" = round(exp(conf.high),2),
          "P" = round(p.value,4)
          )
      )
  )
}
# Create Figure 2A
fig2.1 <- 
  ggplot(
    clusmodtbl %>% mutate(Model = factor(Model)),
    aes(x = Cluster, y = PRR, ymin = Lower, ymax = Upper, shape = Model, group = Model))+
  geom_point(position = position_dodge(width = .5),size=3)+
  geom_errorbar(position = position_dodge(width = .5), width = .5)+
  theme_pubr()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=11),
        axis.text = element_text(size=11))+
  scale_color_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(5,6,1))+
  xlab("") + ylab("Prevalence Ratio") + 
  geom_hline(yintercept = 1,linetype=2)

modtbl <- tibble()
# For each predictor
for(jdx in 1:length(predictors)){
  dat <- msrc.moddat %>% filter(if_any(predictors[jdx],Negate(is.na)))
  # Calculate IP weights
  wts1 <-
    weightit(
      formula = as.formula(paste0(predictors[jdx],"~ SBQc")),
      data = dat,
      method = "cbps",
      stabilize = T,
      estimand = "ATE",
      stop.method = "es.mean",
      over=F
    ) %>% sbps(., moderator = ~Cluster, data = dat)
  # For each model (1, 2, or 3)
  for(idx in 1:length(modelname)){
    # Create model object
    mod <- glm(as.formula(paste0("CurrentSI ~ ", predictors[jdx], "* Cluster", covariates[idx])),
               family="poisson",
               dat, 
               weights = wts1$weights)
    # Convert to properly formatted tibble object
    modtbl <- bind_rows(
      modtbl,
      # Cluster 1 by Insomnia
      tibble(
        "Model" = modelname[idx],
        "Cluster" = "Cluster 1",
        "Predictor" = paste0("Cluster by ", predictors[jdx]),
        deltaMethod(object = mod,vcov. = sandwich(mod),g. = predictors[jdx]) %>% tibble() %>%
          transmute(
            "PRR" = round(exp(Estimate),2),
            "SE" = round(SE,4),
            "Lower" = round(exp(`2.5 %`),2),
            "Upper" = round(exp(`97.5 %`),2)
          )
      ),
      # Cluster 2 by Insomnia 
      tibble(
        "Model" = modelname[idx],
        "Cluster" = "Cluster 2",
        "Predictor" = paste0("Cluster by ", predictors[jdx]),
        deltaMethod(object = mod,vcov. = sandwich(mod),g. = paste0("`",predictors[jdx],"` + `", predictors[jdx], ":Cluster2`")) %>% 
          tibble() %>%
          transmute(
            "PRR" = round(exp(Estimate),2),
            "SE" = round(SE,4),
            "Lower" = round(exp(`2.5 %`),2),
            "Upper" = round(exp(`97.5 %`),2)
          )
      ),
      # Cluster 3 by Insomnia 
      tibble(
        "Model" = modelname[idx],
        "Cluster" = "Cluster 3",
        "Predictor" = paste0("Cluster by ", predictors[jdx]),
        deltaMethod(object = mod,vcov. = sandwich(mod),g. = paste0("`",predictors[jdx],"` + `", predictors[jdx], ":Cluster3`")) %>% 
          tibble() %>%
          transmute(
            "PRR" = round(exp(Estimate),2),
            "SE" = round(SE,4),
            "Lower" = round(exp(`2.5 %`),2),
            "Upper" = round(exp(`97.5 %`),2)
          )
      )
    )
    print(paste("Completed",modelname[idx],"for",predictors[jdx]))
  }
}
# Create Figure 2B
fig2.2 <- ggplot(
  modtbl %>% 
    filter(str_detect(Predictor, "by")) %>%
    mutate(Cluster = factor(Cluster),
           Predictor = str_remove(Predictor, "Cluster by ") %>% 
             factor(levels = c("CurrentInsomnia","ISIFalling","ISIStaying","ISIEMA","ISIInterfere","ISISatisfied"),
                    labels = c("Current Insomnia","DIS","DMS","EMA","Interference","Dissatisfaction"))),
  aes(x = Cluster, y = PRR, ymin = Lower, ymax = Upper, shape = Model, group = Model))+
  geom_point(position = position_dodge(width = .5),size=3)+
  geom_errorbar(position = position_dodge(width = .5), width = .5)+
  facet_wrap(~Predictor,strip.position = "top",scales = "free")+
  theme_pubr()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=11),
        axis.text = element_text(size=11),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size=11))+
  scale_color_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(5,6,1))+
  xlab("") + ylab("Prevalence Ratio") + 
  geom_hline(yintercept = 1,linetype=2)
# Create Figure 2
fig2 <- 
  ggarrange(
    fig2.1,fig2.2,
    ncol=2, nrow=1,common.legend = T,widths = c(1,1.5),labels = "AUTO"
  )
# Print fig
# tiff(".../fig2.tiff",
#      res = 300, width = 4000, height = 2000)
# fig2
# dev.off()
