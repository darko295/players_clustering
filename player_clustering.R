library(dplyr)
library(jsonlite)
library(stringr)

source("functions.R")

data <- read.csv("data/td.csv", header = T, stringsAsFactors = F, check.names = F)

d18 <- data %>% filter(Year_F == 2018 & DivFrom != "Other" & DivTo != "Other")
table(d18$DivFrom)
table(d18$DivTo)

#razmotriti samo transfere nastale tokom letnjeg prelaznog roka
d18$date <- as.integer(str_split_fixed(d18$TDate,"/",2)[,1])
d18$month <- as.integer(str_split_fixed(d18$TDate,"/",2)[,2])

table(d18$month)

d18 <- d18 %>% filter(month %in% 6:8)

#write.csv(d18,"data/ft_data.csv",row.names = F)


data <- read.csv("data/ft_data.csv",header = T, stringsAsFactors = F, check.names = F)
#rm(d18)


div <- c("eng","spa","fra","ger","ita")

for (j in 1:length(div)) {
base_path <- paste0("data/",div[j],"_")
  
df_pass <- fromJSON(paste0(base_path,"pass.csv"))
df_smmry <- fromJSON(paste0(base_path,"smry.csv"))
df_off <- fromJSON(paste0(base_path,"off.csv"))
df_def <- fromJSON(paste0(base_path,"def.csv"))



df_passing <- data.frame()
df_summary <- data.frame()
df_offense <- data.frame()
df_defense <- data.frame()

for (i in 1:nrow(df_smmry)) {
  temp <- data.frame(df_smmry$playerTableStats[i])  
  df_summary <- bind_rows(df_summary,temp)  
}
player_list <- df_summary %>% select(playerId,name,teamName)
df_summary <- df_summary %>% select(playerId,teamName,age,playedPositions,positionText,apps,subOn,minsPlayed,manOfTheMatch,yellowCard,redCard,rating)


for (i in 1:nrow(df_pass)) {
temp <- data.frame(df_pass$playerTableStats[i])  
df_passing <- bind_rows(df_passing,temp)  
}
df_passing <- df_passing %>% select(playerId,teamName,totalPassesPerGame,passSuccess)

for (i in 1:nrow(df_off)) {
temp <- data.frame(df_off$playerTableStats[i])  
df_offense <- bind_rows(df_offense,temp)  
}
df_offense <- df_offense %>% select(playerId,teamName,goal,assistTotal,shotsPerGame,keyPassPerGame,dribbleWonPerGame,
                                    foulGivenPerGame,offsideGivenPerGame,dispossessedPerGame)

for (i in 1:nrow(df_def)) {
temp <- data.frame(df_def$playerTableStats[i])  
df_defense <- bind_rows(df_defense,temp)  
}
df_defense <- df_defense %>% select(playerId,teamName,tacklePerGame,interceptionPerGame,foulsPerGame,clearancePerGame,outfielderBlockPerGame,goalOwn)

switch (div[j],
  "eng" = {
        eng_all <- merge(merge(merge(df_summary,df_offense,by=c("playerId","teamName")),df_passing,by=c("playerId","teamName")),df_defense,by=c("playerId","teamName"))
        eng_all <- left_join(player_list,eng_all,by=c("playerId","teamName"))
          },
  "spa" = {
        spa_all <- merge(merge(merge(df_summary,df_offense,by=c("playerId","teamName")),df_passing,by=c("playerId","teamName")),df_defense,by=c("playerId","teamName"))
        spa_all <- left_join(player_list,spa_all,by=c("playerId","teamName"))
          },
  "fra" = {
        fra_all <- merge(merge(merge(df_summary,df_offense,by=c("playerId","teamName")),df_passing,by=c("playerId","teamName")),df_defense,by=c("playerId","teamName"))
        fra_all <- left_join(player_list,fra_all,by=c("playerId","teamName"))
          },
  "ger" = {
        ger_all <- merge(merge(merge(df_summary,df_offense,by=c("playerId","teamName")),df_passing,by=c("playerId","teamName")),df_defense,by=c("playerId","teamName"))
        ger_all <- left_join(player_list,ger_all,by=c("playerId","teamName"))
          },
  "ita" = {
        ita_all <- merge(merge(merge(df_summary,df_offense,by=c("playerId","teamName")),df_passing,by=c("playerId","teamName")),df_defense,by=c("playerId","teamName"))
        ita_all <- left_join(player_list,ita_all,by=c("playerId","teamName"))
          }
)

}

eng_all <- eng_all %>% arrange(teamName,name)
spa_all <- spa_all %>% arrange(teamName,name)
fra_all <- fra_all %>% arrange(teamName,name)
ita_all <- ita_all %>% arrange(teamName,name)
ger_all <- ger_all %>% arrange(teamName,name)

top5_all <- rbind(eng_all,spa_all,fra_all,ita_all,ger_all)

data_merge <- read.csv("data/ft_data_merge.csv", header = T, stringsAsFactors = F, check.names = F)
data_merge <- data_merge %>% filter(!is.na(player_id))

data_merged <- left_join(data_merge,top5_all,by=c("player_id"="playerId"))

#those with count > 1 have played one or more game for the club they left
dual_club <- data_merged %>% group_by(player_id) %>% summarise(count=n()) 

#only records from the club where player has more appearances will be kept

dual_id <- dual_club %>% filter(count > 1) %>% select(player_id) %>% unlist() %>% as.vector() %>% unique()

dual_club_df <- data_merged %>% filter(player_id %in% dual_id)
data_merged <- data_merged %>% filter(!player_id %in% dual_id)

dual_club_df_clean <- data.frame()
for (i in 1:length(dual_id)) {
  temp <- dual_club_df %>% filter(player_id == dual_id[i])
  temp <- temp[which(temp$apps == max(temp$apps)),]
  dual_club_df_clean <- rbind(dual_club_df_clean,temp)
}

data_merged <- rbind(data_merged,dual_club_df_clean)

#some players went on loan to another club after playing few games for club that bought them
#that's the case with Vicenco Grifo. We are not interested in his loan performance 
data_merged <- data_merged %>% filter(player_id != 121192)

#write.csv(data_merged,"data/ft_data_merged.csv",row.names = F)

data_merged <- read.csv("data/ft_data_merged.csv", header = T, stringsAsFactors = F, check.names = F)

data_merged <- data_merged %>% filter(player_id != 121192)

data_merged_forwards <- data_merged %>% filter(positionText == "Forward") %>% select(player_id,name,positionText,Amount,
                                                                                  age,apps,subOn,minsPlayed,manOfTheMatch,yellowCard,redCard,
                                                                                  goal,assistTotal,shotsPerGame,keyPassPerGame,dribbleWonPerGame,
                                                                                  foulGivenPerGame,offsideGivenPerGame,dispossessedPerGame,
                                                                                  tacklePerGame,interceptionPerGame,foulsPerGame,clearancePerGame,outfielderBlockPerGame,
                                                                                  totalPassesPerGame,passSuccess,rating)
data_merged_midfielders <- data_merged %>% filter(positionText == "Midfielder") %>% select(player_id,name,positionText,Amount,
                                                                                        age,apps,subOn,minsPlayed,manOfTheMatch,yellowCard,redCard,
                                                                                        goal,assistTotal,shotsPerGame,keyPassPerGame,dribbleWonPerGame,
                                                                                        foulGivenPerGame,offsideGivenPerGame,dispossessedPerGame,
                                                                                        tacklePerGame,interceptionPerGame,foulsPerGame,clearancePerGame,outfielderBlockPerGame,goalOwn,
                                                                                        totalPassesPerGame,passSuccess,rating)
data_merged_defenders <- data_merged %>% filter(positionText == "Defender") %>% select(player_id,name,positionText,Amount,
                                                                                    age,apps,subOn,minsPlayed,manOfTheMatch,yellowCard,redCard,
                                                                                    goal,assistTotal,shotsPerGame,keyPassPerGame,dribbleWonPerGame,
                                                                                    foulGivenPerGame,offsideGivenPerGame,dispossessedPerGame,
                                                                                    tacklePerGame,interceptionPerGame,foulsPerGame,clearancePerGame,outfielderBlockPerGame,goalOwn,
                                                                                    totalPassesPerGame,passSuccess,rating)
data_merged_goalkeepers <- data_merged %>% filter(positionText == "Goalkeeper") %>% select(player_id,name,positionText,Amount,
                                                                                    age,apps,subOn,minsPlayed,manOfTheMatch,yellowCard,redCard,
                                                                                    goal,assistTotal,shotsPerGame,keyPassPerGame,dribbleWonPerGame,
                                                                                    foulGivenPerGame,offsideGivenPerGame,dispossessedPerGame,
                                                                                    tacklePerGame,interceptionPerGame,foulsPerGame,clearancePerGame,outfielderBlockPerGame,goalOwn,
                                                                                    totalPassesPerGame,passSuccess,rating)

#EXAMINING RATING VARIABLE
rating_values <- data_merged_forwards %>% select(positionText,rating)
rating_values <- bind_rows(rating_values,data_merged_midfielders %>% select(positionText,rating))
rating_values <- bind_rows(rating_values,data_merged_defenders %>% select(positionText,rating))

rating_values$positionText <- factor(rating_values$positionText,levels = c("Defender","Midfielder","Forward"))
names(rating_values)[1] <- "position"

ggplot(rating_values, aes(rating, x= position, color = position)) + geom_boxplot(trim = FALSE) +
  stat_summary(fun=mean, geom="point", size=2, color="red") + xlab("Position") + ylab("Rating")

ggplot(rating_values, aes(y=rating, x= position, color = position)) + geom_violin(trim = FALSE) + geom_point() +
  stat_summary(fun=median, geom="point", size=2, color="red") + xlab("Position") + ylab("Rating")


#DATA NORMALIZATION
data_merged_defenders_norm <- data_merged_defenders
data_merged_defenders_norm[4:ncol(data_merged_defenders)] <- as.data.frame(apply(data_merged_defenders[4:ncol(data_merged_defenders)], 2,FUN =  normalize.feature))

data_merged_midfielders_norm <- data_merged_midfielders
data_merged_midfielders_norm[4:ncol(data_merged_midfielders)] <- as.data.frame(apply(data_merged_midfielders[4:ncol(data_merged_midfielders)], 2,FUN =  normalize.feature))

#NORM
data_merged_forwards_norm <- data_merged_forwards
data_merged_forwards_norm[4:ncol(data_merged_forwards)] <- as.data.frame(apply(data_merged_forwards[4:ncol(data_merged_forwards)], 2,FUN =  normalize.feature))


#CHECKING BEST LINKAGE METHOD FOR EACH TEAM LINE

# methods to assess
m <- c("single", "average", "ward", "complete")
names(m) <- c("single", "average", "ward", "complete")

# function to compute coefficient
ac_forwards <- function(x) {
  agnes(data_merged_forwards_norm %>% select(-player_id,-name,-positionText,-rating), method = x)$ac
}

purrr::map_dbl(m, ac_forwards)

ac_defenders <- function(x) {
  agnes(data_merged_defenders_norm %>% select(-player_id,-name,-positionText,-rating), method = x)$ac
}

purrr::map_dbl(m, ac_defenders)
ac_midfielders <- function(x) {
  agnes(data_merged_midfielders_norm %>% select(-player_id,-name,-positionText,-rating), method = x)$ac
}

purrr::map_dbl(m, ac_midfielders)

#############
#### AHC ####
#############

library(cluster)
library(NbClust)
library(factoextra)
library(FactoMineR)
library(corrplot)

#CORPLOT
data_merged_cp <- data_merged %>% select(Amount,
                                         age,apps,subOn,minsPlayed,manOfTheMatch,yellowCard,redCard,
                                         goal,assistTotal,shotsPerGame,keyPassPerGame,dribbleWonPerGame,
                                         foulGivenPerGame,offsideGivenPerGame,dispossessedPerGame,
                                         tacklePerGame,interceptionPerGame,foulsPerGame,clearancePerGame,outfielderBlockPerGame,goalOwn,
                                         totalPassesPerGame,passSuccess,rating)

corrplot(cor(data_merged_cp), type = "lower", method = "circle", tl.cex = 0.7,diag = F,number.cex = 0.6,order="hclust")


#DEFENDERS
#assigning row names
row.names(data_merged_defenders_norm) <- data_merged_defenders_norm$name

corrplot(cor(data_merged_defenders_norm %>% select(-player_id,-name,-positionText,-rating)), type = "lower", method = "number", tl.cex = 0.7,diag = F,number.cex = 0.7,order="hclust")

#creating cluster model and printing dendogram
data_cl_def <- data_merged_defenders_norm %>% select(-player_id,-name,-positionText,-rating,-apps,-keyPassPerGame,-clearancePerGame) #,-clearancePerGame,-keyPassPerGame



cl_def <- cluster::agnes(data_cl_def,method = "ward")
cluster::pltree(cl_def,labels=data_merged_defenders_norm$name)


#pretty dendogram
fviz_dend(
  main = "",
  cl_def,
  horiz = T,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.6
)

#different methods to determine optimal number of clusters
p1 <- fviz_nbclust(data_cl_def, FUN = hcut, method = "wss", 
                   k.max = 8) +
  ggtitle("(A) Elbow method")
p2 <- fviz_nbclust(data_cl_def, FUN = hcut, method = "silhouette", 
                   k.max = 8) +
  ggtitle("(B) Silhouette method")

#NbClust has function that uses 30 different measures to determine optimal cluster number
def_nbclust <- NbClust(data_cl_def, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "ward.D", index ="all")
p3 <- factoextra::fviz_nbclust(def_nbclust) + theme_minimal() + ggtitle("")#ggtitle("NbClust's optimal number of clusters")

# Display plots side by side
gridExtra::grid.arrange(p1, p2, nrow = 1)
p3


#After we decided that the optimal number for k is 4, we can now print dendogram again
fviz_dend(
  main = "",
  cl_def,
  k = 4,
  horiz = T,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.6
)


#Assigning cluster number to each player
data_merged_defenders$cluster <- cutree(cl_def,4)

def_grouped <- data_merged_defenders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(avg=mean)) #med = median,
def_grouped_med <- data_merged_defenders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(med = median)) #,

def_grouped_t <- as.data.frame(t(def_grouped))
colnames(def_grouped_t) <- paste0("cluster",1:4)
def_grouped_t$variable <- row.names(def_grouped_t)
def_grouped_t <- def_grouped_t[,c(5,1:4)]
row.names(def_grouped_t) <- NULL

#write.csv(def_grouped_t,"def_cluster.csv",row.names = F)


#PCA
def_pca <- PCA(data_cl_def,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(def_pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(def_pca)
# Contributions of variables to PC1
fviz_contrib(def_pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(def_pca, choice = "var", axes = 2, top = 10)

fviz_pca_var(def_pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")


fviz_cluster(list(data = data_cl_def, cluster = cutree(cl_def,4)),main = "")


#MIDFIELDERS

#assigning row names
row.names(data_merged_midfielders_norm) <- data_merged_midfielders_norm$name

corrplot(cor(data_merged_midfielders_norm %>% select(-player_id,-name,-positionText,-rating)), type = "lower", method = "number", tl.cex = 0.7,diag = F,number.cex = 0.7,order="hclust")

#creating cluster model and printing dendogram
data_cl_mid <- data_merged_midfielders_norm %>% select(-player_id,-name,-positionText,-rating,-apps,-shotsPerGame,-dispossessedPerGame,-goalOwn)



cl_mid <- cluster::agnes(data_cl_mid,method = "ward")
cluster::pltree(cl_mid,labels=data_merged_midfielders_norm$name)


#pretty dendogram
fviz_dend(
  main = "",
  cl_mid,
  horiz = T,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.6
)

#different methods to determine optimal number of clusters
p1 <- fviz_nbclust(data_cl_mid, FUN = hcut, method = "wss", 
                   k.max = 8) +
  ggtitle("(A) Elbow method")
p2 <- fviz_nbclust(data_cl_mid, FUN = hcut, method = "silhouette", 
                   k.max = 8) +
  ggtitle("(B) Silhouette method")

#NbClust has function that uses 30 different measures to determine optimal cluster number
mid_nbclust <- NbClust(data_cl_mid, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "ward.D", index ="all")
p3 <- factoextra::fviz_nbclust(mid_nbclust) + theme_minimal() + ggtitle("")#ggtitle("NbClust's optimal number of clusters")

# Display plots side by side
gridExtra::grid.arrange(p1, p2, nrow = 1)
p3


#After we decided that the optimal number for k is 4, we can now print dendogram again
fviz_dend(
  main = "",
  cl_mid,
  k = 3,
  horiz = T,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.6
)


#Assigning cluster number to each player
data_merged_midfielders$cluster <- cutree(cl_mid,3)

mid_grouped <- data_merged_midfielders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(avg=mean)) #med = median,
mid_grouped_med <- data_merged_midfielders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(med = median)) #,

mid_grouped_t <- as.data.frame(t(mid_grouped))
colnames(mid_grouped_t) <- paste0("cluster",1:3)
mid_grouped_t$variable <- row.names(mid_grouped_t)
mid_grouped_t <- mid_grouped_t[,c(4,1:3)]
row.names(mid_grouped_t) <- NULL

#write.csv(mid_grouped_t,"mid_cluster.csv",row.names = F)


#PCA
mid_pca <- PCA(data_cl_mid,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(mid_pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(mid_pca)
# Contributions of variables to PC1
fviz_contrib(mid_pca, choice = "var", axes = 1, top = 7)
# Contributions of variables to PC2
fviz_contrib(mid_pca, choice = "var", axes = 2, top = 7)

fviz_pca_var(mid_pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")


fviz_cluster(list(data = data_cl_mid, cluster = cutree(cl_mid,3)),main = "")


#FORWARDS

#assigning row names
row.names(data_merged_forwards_norm) <- data_merged_forwards_norm$name

corrplot(cor(data_merged_forwards_norm %>% select(-player_id,-name,-positionText,-rating,-shotsPerGame,-manOfTheMatch,-keyPassPerGame,-minsPlayed)), type = "lower", method = "number", tl.cex = 0.7,diag = F,number.cex = 0.7,order="hclust")

#creating cluster model and printing dendogram
data_cl_for <- data_merged_forwards_norm %>% select(-player_id,-name,-positionText,-rating,-shotsPerGame,-manOfTheMatch,-keyPassPerGame,-minsPlayed)



cl_for <- cluster::agnes(data_cl_for,method = "ward")
cluster::pltree(cl_for,labels=data_merged_forwards_norm$name)


#pretty dendogram
fviz_dend(
  main = "",
  cl_for,
  horiz = T,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.6
)

#different methods to determine optimal number of clusters
p1 <- fviz_nbclust(data_cl_for, FUN = hcut, method = "wss", 
                   k.max = 8) +
  ggtitle("(A) Elbow method")
p2 <- fviz_nbclust(data_cl_for, FUN = hcut, method = "silhouette", 
                   k.max = 8) +
  ggtitle("(B) Silhouette method")

#NbClust has function that uses 30 different measures to determine optimal cluster number
for_nbclust <- NbClust(data_cl_for, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "ward.D", index ="all")

p3 <- factoextra::fviz_nbclust(for_nbclust) + theme_minimal() + ggtitle("")#ggtitle("NbClust's optimal number of clusters")

# Display plots side by side
gridExtra::grid.arrange(p1, p2, nrow = 1)
p3


#After we decided that the optimal number for k is 4, we can now print dendogram again
fviz_dend(
  main = "",
  cl_for,
  k = 4,
  horiz = T,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.6
)


#Assigning cluster number to each player
data_merged_forwards$cluster <- cutree(cl_for,4)

for_grouped <- data_merged_forwards %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(avg=mean)) #med = median,
for_grouped_med <- data_merged_forwards %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(med = median)) #,

for_grouped_t <- as.data.frame(t(for_grouped))
colnames(for_grouped_t) <- paste0("cluster",1:4)
for_grouped_t$variable <- row.names(for_grouped_t)
for_grouped_t <- for_grouped_t[,c(5,1:4)]
row.names(for_grouped_t) <- NULL

#write.csv(for_grouped_t,"for_cluster.csv",row.names = F)


#PCA
for_pca <- PCA(data_cl_for,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(for_pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(for_pca)
# Contributions of variables to PC1
fviz_contrib(for_pca, choice = "var", axes = 1, top = 7)
# Contributions of variables to PC2
fviz_contrib(for_pca, choice = "var", axes = 2, top = 7)

fviz_pca_var(for_pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")


fviz_cluster(list(data = data_cl_for, cluster = cutree(cl_for,4)),main = "")


#DBSCAN
library(dbscan)
#### DEFENDERS ####
#MIN PTS 4 TO REDUCE NOISE IN GROUPING (OUTLIERS FORMING CLUSTERS)

#REMOVE HIGHLY CORELATED VARS
data_cl_def <- data_merged_defenders %>% select(-player_id,-name,-positionText,-rating,-minsPlayed,-keyPassPerGame,-clearancePerGame) #,-clearancePerGame,-keyPassPerGame

#DETERMINING OPTIMAL eps 
kNNdistplot(as.matrix(data_cl_def),4,all=T)
abline(h = 10.5, lty = 3)
abline(h = 11.1, lty = 3)
abline(h = 12.4, lty = 3)
abline(h = 15.5, lty = 3)


cl_dbscan_def <- dbscan(as.matrix(data_cl_def),eps=11.1,minPts = 4)
cl_dbscan_def

row.names(data_cl_def) <- data_merged_defenders$name
fviz_cluster(cl_dbscan_def, data = data_cl_def, stand = T,
             ellipse = T, show.clust.cent = T, main = "",repel = F,outlier.shape = 5,outlier.color = "#000000",
             geom = c("point","text"),palette = "jco", ggtheme = theme_classic())

#Assigning cluster number to each player
data_merged_defenders$cluster <- cl_dbscan_def$cluster

def_grouped <- data_merged_defenders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(avg=mean)) #med = median,
def_grouped_med <- data_merged_defenders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(med = median)) #,

def_grouped_t <- as.data.frame(t(def_grouped))
colnames(def_grouped_t) <- paste0("cluster",0:4)
def_grouped_t$variable <- row.names(def_grouped_t)
def_grouped_t <- def_grouped_t[,c(6,1:5)]
row.names(def_grouped_t) <- NULL

write.csv(def_grouped_t,"def_cluster_dbscan.csv",row.names = F)



#### MIDFIELDERS ####
#MIN PTS 4 TO REDUCE NOISE IN GROUPING (OUTLIERS FORMING CLUSTERS)

#REMOVE HIGHLY CORELATED VARS
data_cl_mid <- data_merged_midfielders %>% select(-player_id,-name,-positionText,-rating,-minsPlayed,-shotsPerGame,-dispossessedPerGame,-goalOwn)

#DETERMINING OPTIMAL eps 
kNNdistplot(as.matrix(data_cl_mid),4,all=T)
abline(h = 13.2, lty = 3)
abline(h = 17.5, lty = 3)
abline(h = 12.4, lty = 3)
abline(h = 13.5, lty = 3)


cl_dbscan_mid <- dbscan(as.matrix(data_cl_mid),eps=12.4,minPts = 4) #eps=12.8,minPts = 5
cl_dbscan_mid

row.names(data_cl_mid) <- data_merged_midfielders$name
fviz_cluster(cl_dbscan_mid, data = data_cl_mid, stand = T,
             ellipse = T, show.clust.cent = T, main = "",repel = F,outlier.shape = 5,outlier.color = "#000000",
             geom = c("point"),palette = "jco", ggtheme = theme_classic())

#Assigning cluster number to each player
data_merged_midfielders$cluster <- cl_dbscan_mid$cluster

mid_grouped <- data_merged_midfielders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(avg=mean)) #med = median,
mid_grouped_med <- data_merged_midfielders %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(med = median)) #,

mid_grouped_t <- as.data.frame(t(mid_grouped))
colnames(mid_grouped_t) <- paste0("cluster",0:4)
mid_grouped_t$variable <- row.names(mid_grouped_t)
mid_grouped_t <- mid_grouped_t[,c(6,1:5)]
row.names(mid_grouped_t) <- NULL

write.csv(mid_grouped_t,"mid_cluster_dbscan.csv",row.names = F)

data_merged_midfielders$cluster_agnes <- cutree(cl_mid,3)

out_mid_gb <- data_merged_midfielders %>% filter(cluster == 0) %>% group_by(cluster_agnes) %>% summarise(count=n())


#### FORWARDS ####
#MIN PTS 4 TO REDUCE NOISE IN GROUPING (OUTLIERS FORMING CLUSTERS)

#REMOVE HIGHLY CORELATED VARS
data_cl_for <- data_merged_forwards %>% select(-player_id,-name,-positionText,-rating,-shotsPerGame,-manOfTheMatch,-keyPassPerGame,-minsPlayed)

#DETERMINING OPTIMAL eps 
kNNdistplot(as.matrix(data_cl_for),4,all=T)
abline(h = 19, lty = 3)


cl_dbscan_for <- dbscan(as.matrix(data_cl_for),eps=11.5,minPts = 4)
cl_dbscan_for

row.names(data_cl_for) <- data_merged_forwards$name
fviz_cluster(cl_dbscan_for, data = data_cl_for, stand = T,
             ellipse = T, show.clust.cent = T, main = "",repel = F,outlier.shape = 5,outlier.color = "#000000",
             geom = c("point"),palette = "jco", ggtheme = theme_classic())

#Assigning cluster number to each player
data_merged_forwards$cluster <- cl_dbscan_for$cluster

for_grouped <- data_merged_forwards %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(avg=mean)) #med = median,
for_grouped_med <- data_merged_forwards %>% select(-player_id,-name,-positionText) %>% group_by(cluster) %>% summarise_all(list(med = median)) #,

for_grouped_t <- as.data.frame(t(for_grouped))
colnames(for_grouped_t) <- paste0("cluster",0:3)
for_grouped_t$variable <- row.names(for_grouped_t)
for_grouped_t <- for_grouped_t[,c(5,1:4)]
row.names(for_grouped_t) <- NULL

write.csv(for_grouped_t,"for_cluster_dbscan.csv",row.names = F)

data_merged_forwards$cluster_agnes <- cutree(cl_for,4)

out_for_gb <- data_merged_forwards %>% filter(cluster == 0) %>% group_by(cluster_agnes) %>% summarise(count=n())
