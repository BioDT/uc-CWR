setwd("C:\\Users\\desalecg\\OneDrive - Universitetet i Oslo\\Documents\\MoDGPI")
uniLoc2 = read.csv("uniLoc2.csv")
uniLoc2=uniLoc2[ ,-1]
uniLoc2=uniLoc2[uniLoc2$species!="", ]
#uniLoc2 = read.csv("uniLoc22.csv")
##############
set.seed(42)
Abs_ls <- lapply(unique(uniLoc2$species), FUN = function(x){
  NotSpecies_df <- uniLoc2[uniLoc2$species != x, ]
  SampledAbs <- sample(1:nrow(NotSpecies_df), size = 1e5, replace = FALSE)
  Report_df <- NotSpecies_df[SampledAbs,c("decimalLongitude", "decimalLatitude")]
})
names(Abs_ls) <- unique(uniLoc2$species)
head(Abs_ls,3)
#######presenting names(Abs_ls) as list didn't help
####################
###################### absence  ##########
Abs_ls_presence=lapply(Abs_ls, cbind, "presence"=0)
dim(Abs_ls_presence[[1]])
head(Abs_ls_presence)
class(Abs_ls_presence)
names(Abs_ls_presence)
LsativusA = which(names(Abs_ls_presence)=="Lathyrus sativus")
dim(LsativusA)
head(LsativusA,3)
LsativusA=Abs_ls_presence[[27]]
head(LsativusA,3)
dim(LsativusA)
LsativusA=cbind(species="Lathyrus sativus",LsativusA)
names(LsativusA)=c("species","lon","lat","presence")
################### presence ##############
head(uniLoc2)
dim(uniLoc2)
class(uniLoc2)
uniLoc2_presence=cbind(uniLoc2,"presence"=1)
head(uniLoc2_presence)
ncol(uniLoc2_presence)
uniLoc2_presence=uniLoc2_presence[ ,-1]
names(uniLoc2_presence)=c("species","decimalLongitude","decimalLatitude","presence")
LsativusP=uniLoc2_presence[uniLoc2_presence$species=="Lathyrus sativus", ]
head(LsativusP,3)
##############
LsativusPA = rbind(LsativusP,LsativusA)
############ convering to list
uniLoc2_presencel = uniLoc2_presence %>%
  group_by(species)  %>%
  list()
head(uniLoc2_presence[[1]])
head(uniLoc2_presence)
class(uniLoc2_presence)
##########################
uniLoc2_presence
Abs_ls_presence

Abs_df <-do.call(rbind, Abs_ls_presence)
Pres_df <- uniLoc2_presence

UniLocations_df <- Pres_df[,c("decimalLongitude", "decimalLatitude")][
  !duplicated(Pres_df[,c("decimalLongitude", "decimalLatitude")])
  ,]
UniSpecies_vec <- unique(Pres_df$species)

UniLocations_df <- cbind(UniLocations_df, 
      matrix(data = NA, nrow= nrow(UniLocations_df), ncol = length(UniSpecies_vec))
      )                                                   
colnames(UniLocations_df)[-1:-2] <- UniSpecies_vec

print("Rearranging presences")
pb <- txtProgressBar(min = 0, max = nrow(Pres_df), style = 3)
for(i in 1:nrow(Pres_df)){
  # print(i)
  Spec_iter <- Pres_df[i,"species"] 
  Lon_iter <- Pres_df[i,"decimalLongitude"] 
  Lat_iter <- Pres_df[i,"decimalLatitude"] 
  
  TargetCol <- which(colnames(UniLocations_df) == Spec_iter)
  TargetRow <- which(UniLocations_df$decimalLongitude == Lon_iter & UniLocations_df$decimalLatitude == Lat_iter)
  
  UniLocations_df[TargetRow, TargetCol] <- 1 
  
  setTxtProgressBar(pb, i)
}

print("Rearranging absences")
pb <- txtProgressBar(min = 0, max = nrow(Abs_df), style = 3)
for(i in 1:nrow(Abs_df)){
  # print(i)
  Spec_iter <- Abs_df[i,"species"] 
  Lon_iter <- Abs_df[i,"decimalLongitude"] 
  Lat_iter <- Abs_df[i,"decimalLatitude"] 
  
  TargetCol <- which(colnames(UniLocations_df) == Spec_iter)
  TargetRow <- which(UniLocations_df$decimalLongitude == Lon_iter & UniLocations_df$decimalLatitude == Lat_iter)
  
  UniLocations_df[TargetRow, TargetCol] <- 0 
  
  setTxtProgressBar(pb, i)
}
