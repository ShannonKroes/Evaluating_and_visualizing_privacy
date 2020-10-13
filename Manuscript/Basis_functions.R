
# This file contains all functions that we use to compute and visualize privacy
# for all functions we assume that the data entered is tabular with variables in the columns
# and individuals/patients in the rows

#install.packages("utils")
require(utils)
#install.packages("gplots")
require(gplots)
#install.packages("RColorBrewer")
require(RColorBrewer)


combinations<- function(A){
  # the function 'combinations' creates a list that contains the indicices for all variable combinations.
  # this function is used in functions that compute privacy by assessing the PPP or PoAC for all combinations 
  # of auxiliary information 
  
  
  # vector to store all combinations
  combs<- list()
  
  
  # Find all combinations
  for( i in 1:(length(A)-1)){
    
    combs[[i]]<-combn(A, i)
    
    
  }
  
  combs
  
  vect<-1  
  #  Loop over all possible combinations
  for(i in 1:(length(A)-1)){
    
    # Take the number of columns in each slice
    for(j in 1:dim(combs[[i]])[2]){
      
      between<- i+j
      vect<- cbind(vect, between)
    }}
  
  # Total number of aux, inculding max aux
  n.o.aux<- length(vect)
  
  
  result<-list(n.o.aux=n.o.aux, combs=combs)
  return(result)
  
  
  
}

transf<-function(data_set){
  
  
  # This function changes the data so that every possible value for each variable is assigned one specific integer
  # that means that if the domain size of a variable is 5, the variable will contain values 1 through 5 once it has
  # been transformed, e.g. a variable 'eye color' with values 'blue', 'green', and 'brown' will now contain values 1, 2 and 3.  
  # Note that if you have a variable that contains missings and these are denoted by -9999, this will be considered its own value
  n<- dim(data_set)[1]
  K<-dim(data_set)[2]
  
  factors<- mapply(function(x){length(levels(x))}, data_set)>0
  
  
  for(i in 1:K){
 
    if(length(levels(data_set[,i]))>0){levels(data_set[,i])<- c(1:length(levels(data_set[,i])))}
    
    # 
    if(factors[i]==F){transform_matrix<- cbind(sort(unique(data_set[,i])), 1:length(unique(data_set[,i])))
    for(j in 1:dim(data_set)[1]){data_set[j,i]<- transform_matrix[transform_matrix[,1]==data_set[j,i],2]
    }
    }
  }
  
  data_set2<- matrix(NA, dim(data_set)[1], dim(data_set)[2])
  for(i in 1:dim(data_set)[2]){
    
    data_set2[,i]<-c(as.numeric(data_set[,i]))
  }
  colnames(data_set2)<- colnames(data_set)
  return(data_set2)
  
}


UpwardPrivacy<- function(data_set, A, S, Transform=F ){
  # a function to compute upward privacy
  
  # For S and A enter the column indices of the variables that serve as sensitive of auxiliary, respectively.
  # This function assumes there is one sensitive variable S and that there is some auxiliary information
  # This information is represented by a vector with the columns corresponding to the auxiliary information: A
  
  # Transform indicates whether the data should be transformed with the transf function
  if(Transform==T){data_set<- transf(data_set)}
  
  n<- dim(data_set)[1]
  K<- dim(data_set)[2]
  
  
  CCD<- matrix(NA,n,1)
  
  
  # Arrays to indicate for each row in the original dataset, which rows in the anonymized dataset are equal to this row
  # We do this for all but one variable (SameRow) and for the entire row (SameCell). 
  
  SameRow<- matrix(NA, n,1)
  SameCell<-matrix(NA, n,1)
  
  
  
  ##########################################################################################
  # Paste the data together, so that the function is much quicker
  
  
  if(length(A)>1){
    data_setaux<-apply( data_set[ , A ] , 1 , paste, collapse = "-"  )
  }
  
  
  if(length(A)==1){
    data_setaux<-data_set[ ,A]
  }
  
  
  data_setpasted<- apply( cbind(data_set[,A],data_set[,S]) , 1 , paste, collapse = "-"  )
  
  ###################################################################################
  True<- as.numeric(data_set[,S])
  
  # Compare data_set to data_set
  for( i in 1:n){  
    
    SameRow[i]<-sum(data_setaux==data_setaux[i])
    SameCell[i]<- sum(data_setpasted==data_setpasted[i])
  }
  
  ###################################################################################
  
  
  # calculate ccd 
  CCD<- SameCell/SameRow
  PPP<- 1-CCD
  result<-list(PPP=PPP, CCD=CCD)
  
  return(result)
}






DownwardPrivacy<- function(data_set, A, S, Transform=F,q_threshold=0  ){
  
  # with this function we compute downward privacy
  
  # This function assumes there is one sensitive variable S and that there is some auxiliary information
  # This information is represented by a vector with the columns corresponding to the auxiliary information: A
  
  if(Transform==T){data_set<- tranf(data_set)}
  n<- dim(data_set)[1]
  K<- dim(data_set)[2]
  # P indicates the number of unique value the sensitive value takes on (i.e. its domain)
  P<-length(unique(data_set[,S]))
  sens<- data_set[,S]
  # CCD<- matrix(NA,n,1)
  
  
  # Arrays to indicate for each row in the original dataset, which rows in the anonymized dataset are equal to this row
  # We do this for all but one variable (SameRow) and for the entire row (SameCell). 
  # in other words, SameRow indicates the number of peers
  # and SameCell is the number of non-protective peers (i.e. the number of peers-number of protective peers).
  SameCell<- matrix(NA, n,P)
  SameRow<-matrix(NA, n,1)
  
  
  
  ##########################################################################################
  # Paste the data together, so that the function is much quicker
  
  
  if(length(A)>1){
    data_setaux<-apply( data_set[ , A ] , 1 , paste, collapse = "-"  )
  }
  
  
  if(length(A)==1){
    data_setaux<-data_set[ ,A]
  }
  
  
  data_setpasted<- apply( cbind(data_set[,A],data_set[,S]) , 1 , paste, collapse = "-"  )
  
  ###################################################################################
  # Compare data_set to data_set
  for( i in 1:n){ 
    SameRow[i]<-sum(data_setaux==data_setaux[i])
    
    for(p in 1:P){
      
      SameCell[i,p]<- sum(data_setpasted==paste(c(data_set[i,A],p), collapse = "-"  ))
    }
  }
  ###################################################################################

  PP<- apply(SameCell, 2, function(x){x/SameRow}) 
  
  sensmin1<- sens-1
  
  # In order to compute the PoAC we extract all the PP values that are not computed for the true sensitive value.
  PoAC<- (rowSums(PP>q_threshold)- as.numeric(t(SameCell)[seq(1,n*P,P)+sensmin1]>q_threshold))/(P-1)

  PPP<- 1-t(SameCell)[seq(1,n*P,P)+sensmin1]/SameRow
  
  result<-list(PPP=PPP,PoAC=PoAC,SameCell=SameCell, SameRow=SameRow, PP=PP)
  
  
  return(result)
  
  
  
}





UpwardAllAux<- function( data_set, S, A){
  # with this function you can compute upward privacy for all combinations of auxiliary information
  
  n<- dim(data_set)[1]
  K<-dim(data_set)[2]
  
  # Starting matrix
  PPP<- matrix(NA, n, 1)
  
  # Find all combinations of auxiliary information
  aux<-combinations(A)$combs
  
  
  #  Loop over all possible combinations
  for(i in 1:(length(A)-1)){
    
    # Take the number of columns in each slice
    for(j in 1:dim(aux[[i]])[2]){
      
      
      privacy<- UpwardPrivacy(data_set, A=c(aux[[i]][,j]), S=S)$PPP
      
      
      # Because the combinations are multidimensional and we need two loops, which is hard with the indicies
      # We just add the results for each iteration to all previous ones
      NewPPP <-privacy
      
      
      PPP<- cbind(PPP, NewPPP)
      
      
    }
  }
  
  
  PPP<-PPP[,-1]
  
  
  MaxAux<- UpwardPrivacy(data_set,  S=S, A=A)
  
  
  PPP<- cbind(PPP, MaxAux$PPP)
  
  
  result<- list( PPP=PPP, Combinations=aux)
  
  return(result)
  
  
  
}



DownwardAllAux<- function( data_set, S, A,q_threshold=0){
  # with this function you can compute downward privacy for all combinations of auxiliary information
  
  n<- dim(data_set)[1]
  K<-dim(data_set)[2]
  
  PoAC<- matrix(NA, n, 1)
  
  # Find all combinations of auxiliary information
  aux<-combinations(A)$combs
  
  
  #  Loop over all possible combinations
  for(i in 1:(length(A)-1)){
    
    # Take the number of columns in each slice
    for(j in 1:dim(aux[[i]])[2]){
      
      
      privacy<- DownwardPrivacy(data_set, A=c(aux[[i]][,j]), S=S,q_threshold=q_threshold)$PoAC
      
      
      # Because the combinations are multidimensional and we need two loops, which is hard with the indicies
      # We just add the results for each iteration to all previous ones
      NewPoAC <-privacy
      
      
      PoAC<- cbind(PoAC, NewPoAC)
      
      
    }
  }
  
  
  PoAC<-PoAC[,-1]
  
  
  MaxAux<- UpwardPrivacy(data_set,  S=S, A=A)
  
  
  PoAC<- cbind(PoAC, MaxAux$PoAC)
  
  
  result<- list( PoAC=PoAC, Combinations=aux)
  
  return(result)
  
  
  
}



Visualize_privacy<-function(x, threshold=0, col_change=T, legend=T, cextitles=3, delta=0.01, labCol = F){
  
  
  # With this function we visualize privacy with a heatmap. 
  # We use this function so it is easy to have specific settings for the heatmap.2 function
  
  # colchange is whether the order of the columns is allowed to change
  # cextitles is the font size of the rownames (and thus variable names)
  # delta and the threshold combined indicate which range of values will be dark red
  # the legend indicates whether the legend will appear
  # col_change indicates whether the order of the columns (in this case individuals/patients)
  # is allowed to be altered, as to make the visualization easier to interpret
  # as delta decreases, it becomes more accurate but it takes more time to generate the legend
  coul2<- colorRampPalette(brewer.pal(8,"RdYlGn"))(25)
  coul2<- coul2[2:length(coul2)]
  # we chose the color red4 to more strongly emphasize values below or equal to the threshold
  coul<- c("red4", coul2)
  coul<- coul[4*c(1:(length(coul)/4))-3]
  
  
  col_breaks<-c(seq(0, threshold+ delta,length=1), 
                seq(threshold+ delta,1,length=6))
  
  
  col_breaks[3:length(col_breaks)]<-   round(col_breaks[3:length(col_breaks)],2)
  
  x<- heatmap.2(t(x), density.info = "none", trace = "none", 
                dendrogram="none", labRow = rownames(x), labCol=labCol, col=coul, Colv=col_change, Rowv = F,key=legend,
                cexRow = cextitles, margins = c(1, 30), key.title = " ", key.xlab = " ", breaks=col_breaks,
                key.par=list(mgp=c(1.5, 0.5, 0),
                             mar=c(1, 2.5, 1, 0)),
                key.xtickfun=function() {
                  cex <- 1 # par("cex")*par("cex.axis")
                  side <- 1
                  line <- 0
                  col <- par("col.axis")
                  font <- par("font.axis")
                  mtext("0", side=side, at=0, adj=0,
                        line=line, cex=cex, col=col, font=font)
                  mtext("1", side=side, at=1, adj=1,
                        line=line, cex=cex, col=col, font=font)
                  return(list(labels=FALSE, tick=FALSE))}
  )
  return(x)
}


smooth_heatmap<- function(x, n=6000,z=40, seed=1893, threshold=0, delta=0.01){
  
  # In this function we 'smooth' over the heatmap. 
  # Specifically, we take subsamples of the data and compute the assigned color
  # and then take the median over these values
  # we do this for data sets that contain more data points than pixels, so no heatmap can be constructed.
  # z is the number of samples you take
  # n is the size of each sample (i.e. in the default case we take 40 samples of size 6000)
  # the choices for n and z should depend on your data
  # you can set a seed so your results are reproducible 
  
  set.seed(seed)
  samples<- sample(1:dim(x)[1], z*n, replace=T)
  
  # maak de date numeric
  for(i in 1:dim(x)[2]){
    
    x[,i]<- as.numeric(x[,i])
  }
  x<- as.matrix(x)
  
  # we will save the samples in this array 
  sample_heat<- array(NA, dim=c(dim(x)[2],n,z))
  # loop over alle samples
  for(z_i in 1:z){
    
    # take the z_i'd n sampled data points 
    sample_inter<- x[(samples[(n*z_i-(n-1)):(n*z_i)]),]
    # create a heatmap of this sample
    x_inter<- Visualize_privacy(sample_inter, legend=F, threshold=threshold, delta=delta)
    # save the order of the values
    sample_heat[,,z_i]<- t(sample_inter[x_inter$colInd,])
  }


  # these matrices are created so you can check whether the results are in line with expectations
  check_mean<- apply(sample_heat,2,mean) 
  original_mean<- colMeans(x)
  
  # use medians so that you can still have zeroes
  # sample_heat_median contains the final values that will be visualized
  sample_heat_median<- apply(sample_heat,1:2,median)
  
    rownames(sample_heat_median)<- colnames(x)
  
  heat_map<- Visualize_privacy(t(sample_heat_median))
  
  
  result<- list(heat_map=heat_map, sample_heat_median=sample_heat_median, check_mean=check_mean, original_mean=original_mean, x=x)
}




