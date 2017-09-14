# Multiple_Plot_R_Code

    #multiplot is the function:
    
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
    

    #For an example for usage:
    plots <- list()
    for (i in 1:length(names)){
      data <- c()
      data1 <- c()
      data2 <- c()
      data3 <- c()
      data4 <- c()
      data5 <- c()
      n <- c()
      name <- c()
      data <- read.csv(paste(names[i],".All.Sig.FPKM.Cluster.txt",sep=""),fill=T,header=T,sep="\t",stringsAsFactors = F)
      rownames(data1) <- data1[,1]
      data1 <- data1[,-1]
      average <-  apply(data1,2,mean)
      n <- nrow(data1)
      data1 <- rbind(data1,Mean=average)
      name <- rownames(data1)
      colnames(data1) <- c("T2","T4","T6")
      rownames(data1) <- name
      T2 <- data1$T2
      T4 <- data1$T4
      T6 <- data1$T6
      data2 <- data.frame(T2=T2,T4=T4,T6=T6,group=as.factor(rownames(data1)),color=as.factor(c(rep("grey",n),"blue")))
      data3 <- melt(data2,id=c("group","color"))
      m <-subset(data3,group=="Mean")
      data4 <- data3[-which(data3[,1]=="Mean"),]
      pp <- ggplot(data4,aes(x=factor(variable),y=value,colour=color,group=as.factor(group)))
      pp <- pp +geom_line(size=1)+xlab("")+ylab("Log2 FPKM Value")+scale_color_manual(values=c("grey30","grey70"),guide=FALSE)+geom_line(data=m,size=2)+geom_point(data=m)
      pp <- pp+theme(axis.title.y=element_text(face="bold",size=25),axis.text.x=element_text(face="bold",size=25),axis.text.y=element_text(face="bold",size=25))+theme_bw(base_size=18,base_family="Arial")
      plots[[i]] <- pp
    } 
    
    
    tiff(filename="test.tiff",width = 28.267, height = 8.692, units = "in",res=100,compression = c("none"))
    multiplot(plotlist=plots,cols=4)
    dev.off()
    
    
