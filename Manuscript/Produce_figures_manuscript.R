
# in this file we generate the figures for the manuscript 
library(foreign)
require(ggplot2)
require(crayon)


#install.packages("gplots")
library(gplots)

#install.packages("viridis")
library(viridis)

 

# please set your working directory to the directory containing the Basis_functions.R and the needed data files
setwd()
source("Basis_functions.R") 


################################################################################################
################################################################################################

# FIGURE 1

################################################################################################
################################################################################################

# load data
t1c<- read.csv("table1c.csv", header = T)

# create matrix to store privacy values (PoAC)
PoAC<- matrix(NA, nrow(t1c), ncol(t1c))

# compute downward privacy
for(i in 1:ncol(t1c)){
  PoAC[,i]<- DownwardPrivacy(transf(t1c), A=c(1:ncol(t1c))[-i], S=i,q_threshold=0)$PoAC
}

# assign column names
colnames(PoAC)<-c(colnames(t1c)[1:2], "Age: generalized option 2")
rownames(PoAC)<-c("1","2","3","4","5")

# generate heatmap that visualizes the PoAC values. We do not generate a legend here and
# we do not allow the columns (individuals) to be ordered differently than in the original data set
# so that we can refer to specific people. And we do allow for column names (which are transposed to be the rows).
Visualize_privacy(PoAC,0, col_change=F, legend=F, labCol = T, margins= c(5, 30), xlab="ID")


################################################################################################
################################################################################################

# FIGURE 2

################################################################################################
################################################################################################



#########################################################################################################################
#    CERVICAL CANCER
#########################################################################################################################

# eerst kaggle data laden 
data_cer<- read.csv("kag_risk_factors_cervical_cancer.csv")


names_cerv<- c("Number of pregnancies" , "Smoking" , "Age" , "Biopsy result")

dim(data_cer) #858  36

# take out records with missing data 
data_cer<-as.data.frame(data_cer[which(data_cer$Smokes!="?"),])
data_cer<- data_cer[(data_cer$Num.of.pregnancies!="?"),]


# create the data set with the variables that we want to use
smallest<- cbind(data_cer$Num.of.pregnancies, data_cer$Smokes, data_cer$Age, data_cer$Biopsy)

smallest<- transf(smallest)
smallest[,3]<- data_cer$Age

dim(smallest) 

# transform the data so that it is suitable for our functions
t_cerv<-transf(smallest)


# matrix to store upward privacy results
priv_cerv<- matrix(NA, dim(t_cerv)[1], dim(t_cerv)[2])

system.time( 
  for( i in 1:dim(t_cerv)[2]){
    
    OneSens<-  UpwardAllAux(t_cerv, S=i, A=c(1:4)[-i])$PPP
    
    priv_cerv[,i]  <- apply(OneSens, 1, min)
    print(i)
    
  })

# smaller data set so no need to smooth the heatmap
colnames(priv_cerv)<- names_cerv
heat_priv_cerv<- Visualize_privacy(priv_cerv)
############################################################################### ##########################################
#    DIABETIC
#########################################################################################################################

library(foreign)


names_diab<- c("Gender" , "Age" , "Race" , "Number of lab procedures" ,
               "Number of medications" , "Change in medications" ,"Diabetes medications" , "Readmitted")



diabetic<-read.csv("diabetic_data.csv")

# make the selection
diab_small<- diabetic[,c(4,5,3,13,15,48,49,50)]


# delete missings
diab_small<- diab_small[rowSums(diab_small=="?")==0,]


# list of attributes http://downloads.hindawi.com/journals/bmri/2014/781670.pdf
# transform the data so that it is suitable for our functions
diab_t<- transf(diab_small)
diab_t[,3]<- as.numeric(diab_t[,3])

# matrix to store upward privacy results
priv_diab<- matrix(NA, dim(diab_t)[1], dim(diab_t)[2])


system.time(
  for( i in 1:dim(diab_t)[2]){
    
    
    OneSens<-  UpwardAllAux(diab_t, S=i, A=c(1:8)[-i])$PPP
    priv_diab[,i]  <- apply(OneSens, 1, min)
    print(i)
    
    
  })
colnames(priv_diab)<- names_diab


 
write.table(priv_diab,file="diab_u_all.txt") 

system.time(heat_priv_diab<- smooth_heatmap(priv_diab))


Visualize_privacy(t(heat_priv_diab$sample_heat_median))
############################################################################### ##########################################
#    ADULT
#########################################################################################################################


 

# load the adult data set
adult_total<- read.table("adult.data", sep=",")
adult_total<-as.data.frame(adult_total)

# save the variable names
names_adult<- c("Gender" , "Age" , "Race" , "Marital status" , "Education" ,
                "Native country" , "Work class" , "Salary" , "Occupation" )
dim(adult_total) # 32561    15

# delete missings
adult<- adult_total[rowSums(adult_total==" ?")==0,]

# take the set of variables we want and transform the data so that it is suitable for privacy compoutation
adult_kohl<- adult[,c(10,1,9,6,4,14,2,15,7)]
adult_t<- transf(adult_kohl)

dim(adult_t) # 30162 9 ( to indicate no. missings)


# matrix to store upward privacy of adult data set
priv_adult<- matrix(NA, dim(adult_t)[1], dim(adult_t)[2])


system.time(
  for( i in 1:dim(adult_t)[2]){
    
    OneSens<-  UpwardAllAux(adult_t, S=i, A=c(1:9)[-i])$PPP
    
    priv_adult[,i]  <- apply(OneSens, 1, min)
    print(i)
    
  })


colnames(priv_adult)<-names_adult


 
write.table(priv_adult,file="adult_u_all.txt") 

system.time(heat_priv_adult<- smooth_heatmap(priv_adult))

Visualize_privacy(t(heat_priv_adult$sample_heat_median))


















################################################################################################
################################################################################################

#FIGURE 3

################################################################################################
################################################################################################









#########################################################################################################################
#    CERVICAL CANCER
#########################################################################################################################



# matrix to store  Upward privacy for data set about cervical cancer
 Upward_Aux_cerv<-  matrix(NA, dim(t_cerv)[1], dim(t_cerv)[2]-1)

for( i in 1:(dim(t_cerv)[2]-1)){
  
  
  AllAux<-   UpwardPrivacy(t_cerv, S=4, A=c(1:3)[-i])$PPP
  OneSens_d<-  UpwardAllAux(t_cerv, S=4, A=c(1:3)[-i])$PPP
  
  Total_priv<-cbind(AllAux, OneSens_d)
   Upward_Aux_cerv[,i]  <- apply(Total_priv, 1, min)
  print(i)
  
}

colnames( Upward_Aux_cerv)<- names_cerv[1:3]

# small data set so no need to smooth
heat_Upward_Aux_cerv<-  Visualize_privacy( Upward_Aux_cerv)


############################################################################### ##########################################
#    DIABETIC
#########################################################################################################################

# matrix to store downward privacy for data in diabetics
Downward_diab<- matrix(NA, dim(diab_t)[1], dim(diab_t)[2]-1)

system.time(
  for( i in 1:(dim(diab_t)[2]-1)){
    
    AllAux<-  DownwardPrivacy(diab_t, S=8, A=c(1:7)[-i])$PoAC
    OneSens_d<- DownwardAllAux(diab_t, S=8, A=c(1:7)[-i])$PoAC
    
    Total_priv<-cbind(AllAux, OneSens_d)
    Downward_diab[,i]  <- apply(Total_priv, 1, min)
    print(i)
    
  })


# assign column names
colnames(Downward_diab)<-names_diab[1:7]

 
write.table(Downward_diab,file="diab_d.txt") 
system.time(heat_Downward_diab<- smooth_heatmap(Downward_diab))

 
write.table(heat_Downward_diab$sample_heat_median,file="diab_d_heat.txt") 


Visualize_privacy(t(heat_Downward_diab$sample_heat_median))


############################################################################### ##########################################
#    ADULT
#########################################################################################################################

# matrix to store downward privacy results for adult data
priv_Aux_adult<-  matrix(NA, dim(adult_t)[1], dim(adult_t)[2]-1)

system.time(
  for( i in 1:(dim(adult_t)[2]-1)){
    
    AllAux<-  UpwardPrivacy(adult_t, S=9, A=c(1:8)[-i])$PPP
    OneSens_d<- UpwardAllAux(adult_t, S=9, A=c(1:8)[-i])$PPP
    
    Total_priv<-cbind(AllAux, OneSens_d)
    priv_Aux_adult[,i]  <- apply(Total_priv, 1, min)
    
    
    print(i)
    
  })


colnames(priv_Aux_adult)<-names_adult[1:8]


 


write.table(priv_Aux_adult,file="adult_d.txt") 
system.time(heat_Downward_Aux_adult<-smooth_heatmap(priv_Aux_adult))

 
write.table(heat_Downward_Aux_adult$sample_heat_median,file="adult_d_heat.txt") 

Visualize_privacy(t(heat_Downward_Aux_adult$sample_heat_median))




##################################################################################################################
##################################################################################################################
# FIGURE 4
##################################################################################################################
##################################################################################################################

##########################################################################################################################



# FIGURE 4A: AGE

cut_two<- c(14:83)
x<- smallest[,3]

PPP_array<- array(0,dim=c(dim(smallest)[1],7,length(cut_two)))

for( i in 1:length(cut_two)){
  
  twos<- cut(x, breaks=c(12,cut_two[i],84))
  levels(twos)<-1:length(levels(twos))
  twos<-as.numeric(twos)
  
  sub_data<- cbind(smallest[,1], smallest[,2],twos, smallest[,4])
  
  ks_sub<- UpwardAllAux(sub_data, S=4, A=c(1:3))
  
  PPP_array[,,i]<- ks_sub$PPP>0
  
  
}

per_person2<- apply(PPP_array,c(1,3),sum)
per_cut2<- colSums(per_person2==7)/(dim(smallest)[1])


no.cats<- ceiling(72/c(1:72))
cat.sizes<- unique(no.cats)[-length(unique(no.cats))]
result_plot<- list()
result_vector<- c()
no_combinations<-c()
x_axis<- c()


# we loop twice: once for each generalization size and once to go over the options to generalize with that size

for(j in 1:length(cat.sizes)){
  
  cat_size<- cat.sizes[j]
  
  b<-ceiling(72/cat_size)
  
  add<- 0:(70-(cat_size-2)*b) 
  
  z<- 13+c(0:(cat_size-2))*b
  
  PPP_array<- array(0, dim=c(dim(smallest)[1],7,length(add)))
  
  
  for( i in 1:length(add)){
    
    # create generalized variable  
    twos<- cut(x, breaks=c(12,z+add[i],84))
    levels(twos)<-1:length(levels(twos))
    twos<-as.numeric(twos)
    
    # create generalized data set
    sub_data<- cbind(smallest[,1], smallest[,2],twos, smallest[,4])
    
    ks_sub<- UpwardAllAux(sub_data, S=4, A=c(1:3))
    
    PPP_array[,,i]<- ks_sub$PPP>0
    
  }
  
  
  
  per_person<- apply(PPP_array,c(1,3),sum)
  per_cut<- colSums(per_person==7)/dim(smallest)[1]
  result_plot[[j]]<-per_cut
  result_vector<- c(result_vector, per_cut)
  x_axis<- c(x_axis, rep(cat_size, length(per_cut)))
  no_combinations<-c(no_combinations,length(per_cut))
}




# add the one with one category:


sub_data<- cbind(smallest[,1], smallest[,2],rep(0, dim(smallest)[1]), smallest[,4])
ks_sub<- UpwardAllAux(sub_data, S=4, A=c(1:3))
PPP_array<- ks_sub$PPP>0
cat_1<- sum(rowSums(PPP_array)==7)/dim(smallest)[1]


x_axis<-c(x_axis,1)
result_vector<-c(result_vector, cat_1)
result_plot[[16]]<-cat_1

# sla de indices op van de generalizatie die de beste privacy opbrengst heeft per categorie size


# dit zijn het aantal opties per categorie
lengths_t<-c()
for(i in 1:16){lengths_t[i]<- length(result_plot[[i]])}
max_ind_or<-1
max_ind<- which(result_plot[[1]]==max(result_plot[[1]]))
for(i in 2:16){
  max_ind_or<-c(max_ind_or, which(result_plot[[i]]==max(result_plot[[i]])))
  max_ind<- c(max_ind, cumsum(lengths_t)[i-1]+which(result_plot[[i]]==max(result_plot[[i]])))}




length_max<- c()
max.ind<-c()
for(i in 1:length(unique(x_axis))){
  max.ind<- c(max.ind,which(result_vector[which(x_axis==unique(x_axis)[i])]==
                              max(result_vector[which(x_axis==unique(x_axis)[i])]))[1]+min(which(x_axis==unique(x_axis)[i]))-1)
  length_max[i]<-length(which(result_vector[which(x_axis==unique(x_axis)[i])]==
                                max(result_vector[which(x_axis==unique(x_axis)[i])]))[1]+min(which(x_axis==unique(x_axis)[i]))-1)
}


# geef de best indices een andere vorm in de plot en die we hebben gekozen
pch_vect<- rep(20, length(result_vector))
pch_vect[max_ind]<- 8


col_vect<- rep("gray38", length(result_vector))
col_vect[max_ind]<- "black"
col_vect[max_ind][14]<- "black"

plot(x_axis, result_vector, xlab= "Number of age categories", ylab="Proportion of individuals with privacy", 
     ylim=c(0,1), pch=pch_vect, col=col_vect, cex.axis=1.5 , cex.lab=1.5)


########################################################################################################################


# FIGURE 4B: NUMBER OF PREGNANCIES


cut_two<- c(2:10)

x<- smallest[,1]

PPP_array<- array(0,dim=c(dim(smallest)[1],7,length(cut_two)))

for( i in 1:length(cut_two)){
  # create generalized variable  
  twos<- cut(x, breaks=c(0,cut_two[i],12))
  levels(twos)<-1:length(levels(twos))
  twos<-as.numeric(twos)
  
  # add it to the new generalized data set
  sub_data<- cbind(twos, smallest[,2],smallest[,3], smallest[,4])
  
  # compute privacy
  ks_sub<- UpwardAllAux(sub_data, S=4, A=c(1:3))
  
  PPP_array[,,i]<- ks_sub$PPP>0
  
  
}



per_person2<- apply(PPP_array,c(1,3),sum)
# % of people with privacy per generalization 
per_cut2<- colSums(per_person2==7)/(dim(smallest)[1])


no.cats<- ceiling(11/c(1:11))

cat.sizes<- unique(no.cats)[-length(unique(no.cats))]
result_plot<- list()
result_vector<- c()
no_combinations<-c()
x_axis<- c()


for(j in 1:length(cat.sizes)){
  
  cat_size<- cat.sizes[j]
  
  b<-ceiling(11/cat_size)
  
  add<- 0:(9-(cat_size-2)*b) 
  
  z<- 1+c(0:(cat_size-2))*b
  
  PPP_array<- array(0, dim=c(dim(smallest)[1],7,length(add)))
  
  
  for( i in 1:length(add)){
    
    twos<- cut(x, breaks=c(0,z+add[i],11))
    
    levels(twos)<-1:length(levels(twos))
    twos<-as.numeric(twos)
    
    
    sub_data<- cbind(twos, smallest[,2],smallest[,3], smallest[,4])
    
    ks_sub<- UpwardAllAux(sub_data, S=4, A=c(1:3))
    
    PPP_array[,,i]<- ks_sub$PPP>0
    
  }
  
  
  
  per_person<- apply(PPP_array,c(1,3),sum)
  per_cut<- colSums(per_person==7)/dim(smallest)[1]
  result_plot[[j]]<-per_cut
  result_vector<- c(result_vector, per_cut)
  x_axis<- c(x_axis, rep(cat_size, length(per_cut)))
  no_combinations<-c(no_combinations,length(per_cut))
}




# add the one with one category:


sub_data<- cbind(rep(0, dim(smallest)[1]), smallest[,2],smallest[,3], smallest[,4])
ks_sub<- UpwardAllAux(sub_data, S=4, A=c(1:3))
PPP_array<- ks_sub$PPP>0
cat_1<- sum(rowSums(PPP_array)==7)/dim(smallest)[1]


x_axis<-c(x_axis,1)
result_vector<-c(result_vector, cat_1)
####################################################################
result_plot[[(length(cat.sizes)+1)]]<-cat_1

lengths_t<-c()
for(i in 1:(length(cat.sizes)+1)){lengths_t[i]<- length(result_plot[[i]])}
max_ind_or<-1
max_ind<- which(result_plot[[1]]==max(result_plot[[1]]))
for(i in 2:(length(cat.sizes)+1)){
  max_ind_or<-c(max_ind_or, which(result_plot[[i]]==max(result_plot[[i]])))
  max_ind<- c(max_ind, cumsum(lengths_t)[i-1]+which(result_plot[[i]]==max(result_plot[[i]])))}




length_max<- c()
max.ind<-c()
for(i in 1:length(unique(x_axis))){
  max.ind<- c(max.ind,which(result_vector[which(x_axis==unique(x_axis)[i])]==
                              max(result_vector[which(x_axis==unique(x_axis)[i])]))[1]+min(which(x_axis==unique(x_axis)[i]))-1)
  length_max[i]<-length(which(result_vector[which(x_axis==unique(x_axis)[i])]==
                                max(result_vector[which(x_axis==unique(x_axis)[i])]))[1]+min(which(x_axis==unique(x_axis)[i]))-1)
}


pch_vect<- rep(20, length(result_vector))
pch_vect[max_ind]<- 8


col_vect<- rep("gray38", length(result_vector))
col_vect[max_ind]<- "black"

plot(x_axis, result_vector, xlab= "Number of pregnancy categories", ylab="Proportion of individuals with privacy", 
     ylim=c(0,1), pch=pch_vect, col=col_vect, cex.axis=1.5 , cex.lab=1.5)








