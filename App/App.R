
library(utils)
library(gplots)
library(RColorBrewer)

# Functions needed
combinations<- function(A){
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
  n<- dim(data_set)[1]
  K<-dim(data_set)[2]
  factors<- mapply(function(x){length(levels(x))}, data_set)>0
  
  for(i in 1:K){
    if(length(levels(data_set[,i]))>0){levels(data_set[,i])<- c(1:length(levels(data_set[,i])))}
    if(factors[i]==F){transform_matrix<- cbind(sort(unique(data_set[,i])), 1:length(unique(data_set[,i])))
    for(j in 1:dim(data_set)[1]){data_set[j,i]<- transform_matrix[transform_matrix[,1]==data_set[j,i],2]
    }# end j for loop
    }# end if statement if factors=F
  }# loop factor
  
  data_set2<- matrix(NA, dim(data_set)[1], dim(data_set)[2])
  for(i in 1:dim(data_set)[2]){
    
    data_set2[,i]<-c(as.numeric(data_set[,i]))
  }
  colnames(data_set2)<- colnames(data_set)
  return(data_set2)
  
}

UpwardPrivacy<- function(data_set, max.aux=T, A, S, corr=F, Transform=F ){
  # This function assumes there is one sensitive variable S and that there is some auxiliary information
  # This information is represented by a vector with the columns corresponding to the auxiliary information: A
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

combinations<- function(A){
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







heatmap.2x_withlegend<-function(x, threshold=0, col_change=T, legend=T, cextitles=3, delta=0.01){
  # colchange is whether the order of the columns is allowed to change
  # cextitles is the font size of the rownames (and thus variable names)
  # delta and the threshold combined indicate which range of values will be dark red
  # the legend indicates whether the legend will appear
  coul2<- colorRampPalette(brewer.pal(8,"RdYlGn"))(25)
  coul2<- coul2[2:length(coul2)]
  coul<- c("red4", coul2)
  coul<- coul[4*c(1:(length(coul)/4))-3]
  col_breaks<-c(seq(0, threshold+ delta,length=1), 
                seq(threshold+ delta,1,length=6))
  
  col_breaks[3:length(col_breaks)]<-   round(col_breaks[3:length(col_breaks)],2)
  x<- heatmap.2(t(x), density.info = "none", trace = "none", 
                # The columns are allowed to be switched in order.    
                
                dendrogram="none", labRow = rownames(x), labCol=F, col=coul, Colv=col_change, Rowv = F,key=legend,
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

DownwardPrivacy<- function(data_set, A, S, corr=F, Transform=F,q_threshold=0,max.aux=T  ){
  # This function assumes there is one sensitive variable S and that there is some auxiliary information
  # This information is represented by a vector with the columns corresponding to the auxiliary information: A
  if(Transform==T){data_set<- tranf(data_set)}
  n<- dim(data_set)[1]
  K<- dim(data_set)[2]
  P<-length(unique(data_set[,S]))
  sens<- data_set[,S]
  # CCD<- matrix(NA,n,1)
  
  # Arrays to indicate for each row in the original dataset, which rows in the anonymized dataset are equal to this row
  # We do this for all but one variable (SameRow) and for the entire row (SameCell). 
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
  # Samecell is matrix met op welke sens value elk van die peers staat
  # SameRow is peer
  PP<- apply(SameCell, 2, function(x){x/SameRow}) # alleen is hier  true sens  ook bij
  sensmin1<- sens-1
  
  # very elaborate way to find PoAC
  PoAC<- (rowSums(PP>q_threshold)- as.numeric(t(SameCell)[seq(1,n*P,P)+sensmin1]>q_threshold))/(P-1)
  # calculate ccd 
  PPP<- 1-t(SameCell)[seq(1,n*P,P)+sensmin1]/SameRow
  result<-list(PPP=PPP,PoAC=PoAC,SameCell=SameCell, SameRow=SameRow, PP=PP)
  return(result)
}


smooth_heatmap<- function(x, n=6000,z=40, seed=1893){
  # z is the number of samples you take
  # n is the size of every sample
  # this needs to be proportionate to the original data size.
  # we create a heatmap for every sample and take the mean.
  set.seed(seed)
  samples<- sample(1:dim(x)[1], z*n, replace=T)
  
  # make the date numeric
  for(i in 1:dim(x)[2]){
    x[,i]<- as.numeric(x[,i])
  }
  x<- as.matrix(x)
  
  # save the samples
  sample_heat<- array(NA, dim=c(dim(x)[2],n,z))
  # loop over alle samples
  for(z_i in 1:z){
    
    # make heatmap of every sample, select the sample by using the right indices
    sample_inter<- x[(samples[(n*z_i-(n-1)):(n*z_i)]),]
    # save the heatmap and how it is sorted
    x_inter<- heatmap.2x_withlegend(sample_inter, legend=F)
    # save the values
    sample_heat[,,z_i]<- t(sample_inter[x_inter$colInd,])
  }
  
  check_mean<- apply(sample_heat,2,mean) #[1] 0.10211172 0.19259878 0.09886850 0.13485578 0.15476687 0.09642135 0.12542433 0.15124938
  original_mean<- colMeans(x)# 0.10201724 0.19252669 0.09874690 0.13474677 0.15474972 0.09647474 0.12545721 0.15101291 
  
  # use medians so that you can still have zeroes
  sample_heat_median<- apply(sample_heat,1:2,median)
  rownames(sample_heat_median)<- colnames(x)
  heat_map<- heatmap.2x_withlegend(t(sample_heat_median))
  result<- list(heat_map=heat_map, sample_heat_median=sample_heat_median, check_mean=check_mean, original_mean=original_mean, x=x)
}









DownwardAllAux<- function( data_set, S, A,q_threshold){
  
  n<- dim(data_set)[1]
  K<-dim(data_set)[2]
  
  # Starting matrix
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



UpwardAllAux<- function( data_set, S, A){
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

library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  #sidebarLayout(
  #    sidebarPanel(
  
  # App title ----
  titlePanel("Visualize privacy in your data"),
  sidebarLayout(
    sidebarPanel(
      # Sidebar layout with input and output definitions ----
      fluidRow(column(7,
                      
                      # Sidebar panel for inputs ----
                      #sidebarPanel(
                      
                      # Input: Select a file ----
                      fileInput("file1", "Please upload a csv file with variable names as the first row",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))
                      

      )
      ), 
      fluidRow(column(8,
                      selectInput(inputId = "UD",
                                  label = "Select either upward or downward privacy",
                                  choices =c("Upward privacy", "Downward privacy"),
                                  selected = "Upward privacy",
                                  width = "300px")
      )
      ),
      #hr(),
      fluidRow(column(8,
                      textInput(inputId = "threshold",
                                label = "Insert privacy threshold between 0 and 1:",
                                value = "0",
                                width = "300px")
      )),
      
      fluidRow(column(8,
                      selectInput(inputId = "aux",
                                  label = "Select whether you want to evaluate privacy for all combinations of auxiliary information (may be overly time-consuming) or only for maximum auxiliary information",
                                  choices =c("Only maximum auxiliary information", "All combinations of auxiliary information"),
                                  selected = "Only maximum auxiliary information",
                                  width = "300px")
      )
      ),
      h6("Columns will be ordered such that the picture is easiest to interpret. Please note that the data will be processed on the shiny.io server. For more information, and for the source code that can be used for data containing identifiable individuals, please see our repository https://github.com/ShannonKroes/Evaluating_and_visualizing_privacy")
    ),
    # Main panel for displaying outputs ----
    # Output: Data file ----
    #tableOutput("contents")
    mainPanel(
      #h1("Heatmap that visualizes privacy"),
      
      imageOutput("myImage")
      
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$myImage <- renderImage({
    req(input$file1)
    rawcsv <- read.csv(input$file1$datapath, header = T)
    threshold_input<-as.numeric(input$threshold)
    df<-transf(rawcsv)
    
    # count domain size each variable
    uniques<- apply(df,2,function(x){length(unique(x))})
    k<- dim(df)[2]
    outfile <- tempfile(fileext = '.png')
    # Generate the PNG
    png(outfile, width = 1500, height = 600)
      withProgress(message = 'Making plot', value = 0, {
        # Number of times we'll go through the loop
        
        Outcome<- matrix(NA, dim(df)[1],k)
        
        #############################################################################################      
        
        # Only max aux
        
        #############################################################################################
        
        for( i in 1:k){
          if(input$aux== "Only maximum auxiliary information"){
            if(input$UD=="Upward privacy"){Outcome[,i]<-  DownwardPrivacy(df, A=c(1:k)[-i], S=i )$PPP}
            if(input$UD=="Downward privacy"){Outcome[,i]<-  DownwardPrivacy(df, A=c(1:k)[-i], S=i,q_threshold=threshold_input)$PoAC}
          }
          
          #############################################################################################      
          
          # All combinations of auxiliary information
          
          #############################################################################################
          if(input$aux== "All combinations of auxiliary information"){
            if(input$UD=="Upward privacy"){
              OneSens<-  UpwardAllAux(df, S=i, A=c(1:k)[-i])$PPP
              Outcome[,i]  <- apply(OneSens, 1, min)

            }
            if(input$UD=="Downward privacy"){
              # Check it for all combinations of auxiliary information (Onesens) and for maximum auxiliary information
              # (AllAux)
              AllAux<-  DownwardPrivacy(df, S=i, A=c(1:k)[-i], q_threshold=threshold_input)$PoAC
              OneSens<- DownwardAllAux(df, S=i, A=c(1:k)[-i], q_threshold=threshold_input)$PoAC
              
              Total_priv<-cbind(AllAux, OneSens)
              Outcome[,i]  <- apply(Total_priv, 1, min)
            } #end downward privacy if statement
            
          } # end all comb aux if statement
          incProgress(1/k, detail = paste("Computing variable", i))
          
        }# end loop    

      })
      colnames(Outcome)<- colnames(rawcsv)
      heatmap.2x_withlegend(Outcome, cextitles=2, threshold =threshold_input)
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 1000,
           height = 600,
           alt = "This is alternate text")
    }, deleteFile = TRUE)  
  
  }
  # Run the app ----
  shinyApp(ui, server)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
