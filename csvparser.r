# PLOTTING DATA POINTS FOR JOHN JAY CSV

# Created and Developed by Jake Essman
# Further Creation by Chetram Dasrat

# FUNCTION: fplot
# Variables: 
#   fdata       : csv datafile
#   sensors     : sensor lists
#   vav_locations: Valve Numbers by Quadrant Systems
#   plot_limits : limits
#   ylabs       : Y - Label
#   Legend_cols : Legend using Columns
#   Output Prefix: Data Graph
#   Prezero     : False
#   Print       : Print to Console : False
#   Year        : 2013
#   Plot_Style  : "L" for Connected Lines, "P" for Points



fplot<- function (fdata,sensors,vav_locations,plot_limits,ylabs,legend_cols=2,output_prefix="data_graph",prezero=F,print=F,year,plot_style) 
{ 
    # Read fdata, remove invalid rows and reorder sensor list
    data <- read.csv(fdata,colClasses="character");
    data <- data[substring(data[,1],17)!="RET.DMPR",];
    data <- data.frame(sapply(data[,1], as.character), data.matrix(data[,2:99]), stringsAsFactors=FALSE);
    data <- data[complete.cases(data),];
    data <- data[order(data[,1]),];
  
    # Data Matrix data[,2:99]
    
    
    # Data frame correlates to VAV number to building quadrant
    quads <- data.frame(read.csv(vav_locations));
  
    # Month Vectors
    months <- c("January","February","March","April","May","June","July","August","September","October","November","December");
  
    # Plot counter
    n<-0;
  
    # X-Axis Labels
    xaxis_labels <- paste(floor((plot_limits-4)/4),60*(((plot_limits-4)/4)-floor((plot_limits-4)/4)),sep=":");
    xaxis_labels <- sapply(xaxis_labels,
                          function(label) 
                          {
                              llength <- nchar(label);
                              if(llength == 3) 
                              {
                                label<-paste(label,"0",sep="");
                              }
                              if(llength == 4 && substring(label,3,3) == ":") 
                              {
                                label<-paste(label,"0",sep="");
                              }
                              if(llength == 4 && prezero == T) 
                              {
                                label<-paste("0",label,sep="");
                              }
                              return(label);
                          });
    
    # Label Indices
    label_indices <- seq(1,length(xaxis_labels),4);
    
    # Split Sensors into Indices
    sensor_names <- do.call(rbind,strsplit(unlist(lapply(sensors,function(sensor) 
    {
        return(c(sensor$name,ifelse(substr(sensor$name,1,3)=="ACS",paste("ACS2",substring(sensor$name,4),sep=""),NA)));
    })),":"));
    
    # Split Indices to Sensor_Names
    sensor_names <- sensor_names[!is.na(sensor_names[,1]),];
    
    # Legend Titles
    legend_titles <- unlist(lapply(sensors,function(sensor) 
    {
        return(rep(sensor$ltitle,ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
    }));
    
    # Sensors Axes
    axes <- unlist(lapply(sensors,function(sensor) 
    {
        return(rep(sensor$yaxis,ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
    }));
    
    # Plot Colors
    plot_colors <- unlist(lapply(sensors,function(sensor) 
    {
        return(c(sensor$color,ifelse(substr(sensor$name,1,3)=="ACS",sensor$color2,NA)));
    }));
    plot_colors <- plot_colors[!is.na(plot_colors)];
  
    # Peak Limits
    peak_limits <- unlist(lapply(sensors,function(sensor) 
    {
        return(rep(list(sensor$peaklim),ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
    }),recursive=F);
  
    # Peak Ranges
    peak_ranges <- unlist(lapply(sensors,function(sensor) 
    {
        return(rep(list(sensor$peakrng),ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
    }),recursive=F);
  
    # Recursive SOrting of Parameters
    rorder <- order(sensor_names[,1],decreasing=T);
    sensor_names <- sensor_names[rorder,];
    legend_titles <- legend_titles[rorder];
    axes <- axes[rorder];
    plot_colors <- plot_colors[rorder];
    peak_limits <- peak_limits[rorder];
    peak_ranges <- peak_ranges[rorder];
    sindices <- split(1:nrow(sensor_names),sensor_names[,1]);
    
    # Split DataFile by Sensor Lists
    rc <- 1;
    NAm <- data.frame(matrix(rep(NA,nrow(data)*ncol(data)),nrow=nrow(data)),stringsAsFactors=F);
    vav_data <- rep(list(NAm),length(sindices$VAV));
    t_vav_data <- vector("list",length(sindices$VAV));
    ahu_data <- rep(list(NAm),length(sindices$AHU));
    t_ahu_data <- vector("list",length(sindices$AHU));
    acs1_data <- rep(list(NAm),length(sindices$ACS));
    t_acs1_data <- vector("list",length(sindices$ACS));
    acs2_data <- rep(list(NAm),length(sindices$ACS2));
    t_acs2_data <- vector("list",length(sindices$ACS2));
  
    # Data Manipulation 1
    first_vav_data <- data[grep(paste("^JJCEX[.][0-9]{2}[.]VAV[.][0-9]{5}",sensor_names[sindices$VAV[1],2],sep=":"),data[,1]),];
    first_vav_data <- first_vav_data[order(first_vav_data[,2],first_vav_data[,3]),];
    vavnums <- cbind(substr(first_vav_data[,1],7,8),substr(first_vav_data[,1],14,15));
    vavseries <- cbind(rep(as.matrix(aggregate(as.numeric(vavnums[,2]),list(as.numeric(vavnums[,1])),min))[,2],each=2)+c(0,1),c(1,2));
            apply(first_vav_data,1,function(vav) 
            {
              name <- strsplit(vav[1],"[.:]")[[1]];
              northsouth <- vavseries[vavseries[,1]==substr(name[4],1,2),2];
              dtg <- (data[,2]==vav[2]&data[,3]==vav[3]);
              full <- T;
              v <-2;
              
              while(v <= length(vav_data)) 
              {
                t_vav_data[[v]] <<- data[dtg&data[,1]==paste(paste(name[1:4],collapse="."),sensor_names[sindices$VAV[v],2],sep=":"),];
                full <- (full&nrow(t_vav_data[[v]])==1);
                v<-v+1;
              }
              
              a<-1;
              while(a <= length(ahu_data)) 
              {
                t_ahu_data[[a]] <<- data[dtg&data[,1]==paste(paste("JJCEX.AHU.",name[2],".0",northsouth,sep=""),sensor_names[sindices$AHU[a],2],sep="."),];
                full <- (full&nrow(t_ahu_data[[a]])==1);
                a<-a+1;
              }
    
              a<-1;
              while(a <= length(acs1_data)) 
              {
                t_acs1_data[[a]] <<- data[dtg&data[,1]==paste(paste("JJCEX.ACS.PH.0",northsouth*2+1,sep=""),sensor_names[sindices$ACS[a],2],sep="."),];
                full <- (full&nrow(t_acs1_data[[a]])==1);
                a<-a+1;
              }
    
              a<-1;
              while(a <= length(acs2_data)) 
              {
                t_acs2_data[[a]] <<- data[dtg&data[,1]==paste(paste("JJCEX.ACS.PH.0",northsouth*2+2,sep=""),sensor_names[sindices$ACS2[a],2],sep="."),];
                full <- (full&nrow(t_acs2_data[[a]])==1);
                a<-a+1;
              }
              
              if(full) 
              {
                vav_data[[1]][rc,] <<- vav;
                
                v<-2;
                while(v <= length(t_vav_data)) 
                {
                  vav_data[[v]][rc,] <<- t_vav_data[[v]];
                  v<-v+1;
                }
      
                a<-1;
                while(a <= length(t_ahu_data)) 
                {
                  ahu_data[[a]][rc,] <<- t_ahu_data[[a]];
                  a<-a+1;
                }
                
                
                a<-1;
                while(a <= length(t_acs1_data)) 
                {
                  acs1_data[[a]][rc,] <<- t_acs1_data[[a]];
                  a<-a+1;
                }
      
                a<-1;
                while(a <= length(t_acs2_data)) 
                {
                  acs2_data[[a]][rc,] <<- t_acs2_data[[a]];
                  a<-a+1;
                }
                rc<<-rc+1;
              }
          });
    
    sensor_data <- lapply(c(vav_data,ahu_data,acs2_data,acs1_data),function(sm) 
    {
      return(data.frame(sm[!is.na(sm[,1]),1],matrix(as.numeric(as.matrix(sm[!is.na(sm[,1]),-1])),nrow=nrow(sm[!is.na(sm[,1]),])),stringsAsFactors=F));
    });
    
    # Data Manipulation 2
    # Remove Non-Curvy Plots
    
    curvy_sensors <- c();
    i <- 1;
    while(i <= length(peak_limits)) 
    {
      if(!is.null(peak_limits[[i]])) 
      {
        curvy_sensors <- c(curvy_sensors,i);
      }
      i<-i+1;
    }
    
    curvy_plots <- rep(TRUE,nrow(sensor_data[[1]]));
  
    for(i in 1:nrow(sensor_data[[1]])) 
    {
      for(j in curvy_sensors) 
      {
        curvy_plots[i] <- ((curvy_plots[i]==T)&(diff(range(sensor_data[[j]][i,peak_limits[[j]]]))>=peak_ranges[[j]]));
      }
    }
  
    sensor_data <- lapply(sensor_data,function(sensor) 
    {
      return(sensor[curvy_plots,]);
    });
    
    #set Row Ranges for Each Sensor Sections
    data_sections <- ceiling(nrow(sensor_data[[1]])/50);
    filerows <- ceiling(nrow(sensor_data[[1]])/data_sections);
    rowranges <- matrix(c(1:nrow(sensor_data[[1]]),rep(NA,filerows*data_sections-nrow(sensor_data[[1]]))),nrow=filerows,ncol=data_sections);
    lrows <- ceiling(length(legend_titles)/legend_cols);
  
    
    # Output Data for Each Section
    
    for(g in 1:data_sections) 
    {
      # Plot Ranges
      r1data <- unlist(lapply(sensor_data[axes==1],function(sensor) 
      {
        return(sensor[,plot_limits]);
      }));
    
      r2data <- unlist(lapply(sensor_data[axes==2],function(sensor) 
      {
        return(sensor[,plot_limits]);
      }));
    
      numticks <- 10;
      range1 <- range(ifelse(rep(length(r1data)==0,ifelse(length(r1data)==0,1,length(r1data))),NA,r1data));
      range1 <- c(floor(range1[1]),ceiling(range1[2]));
      axis1step <- floor(diff(range1)/numticks);
      hrange1 <- range1[1]+numticks*axis1step;
    
      while(!is.na(hrange1)&hrange1 < range1[2]) 
      {
        hrange1 <- hrange1 + axis1step;
      }
    
      range1[2] <- hrange1;
      range2 <- range(ifelse(rep(length(r2data)==0,ifelse(length(r2data)==0,1,length(r2data))),NA,r2data));
      range2 <- c(floor(range2[1]),ceiling(range2[2]));
      axis2step <- floor(diff(range2)/numticks);
      hrange2 <- range2[1]+numticks*axis2step;
    
      while(!is.na(hrange2)&hrange2 < range2[2]) 
      {
        hrange2 <- hrange2 + axis2step;
      }
      
      range2[2] <- hrange2;
    
      # Collaborate PDF File from Plots
      if(print==T) 
      {
        pdf(paste(c(output_prefix,"_",g,".pdf"),collapse=""),pointsize=16,width=11,height=8.5);
        par(mar=c(0,0,0,0));
      }
      else 
      {
        pdf(paste(c(output_prefix,"_",g,".pdf"),collapse=""),pointsize=16,height=4*length(rowranges[!is.na(rowranges[,g]),g]));
        par(mfrow=c(length(rowranges[!is.na(rowranges[,g]),g]),1));
      }
    
      # Plot for Each Row
      for(i in rowranges[!is.na(rowranges[,g]),g]) 
      {
        # Set X values
        xvals <- plot_limits;
        # Set Y values
        all_yvals <- t(do.call(rbind,lapply(sensor_data,function(sensor) 
        {
          return(as.vector(sensor[i,plot_limits]));
        })));
        
        # Set Graphical Parameters (axis label style, margins)
        par(las=2,mar=c(5,4,lrows+2,4)+0.1);
      
        # Plot sensor data columns
        
        yd<-F;
        for(p in 1:ncol(all_yvals)) 
        {
          primary <- (axes[p] == 1);
          axispts <- seq(range1[1],range1[2],axis1step);
          plot(xvals,all_yvals[,p],pch=20,type=plot_style,col=plot_colors[p],xlab="",ylab="",ylim=ifelse(rep(primary,2),range1,range2),axes=F,cex=1);
          if(primary==T && yd==F) 
          {
            Axis(side=2,at=axispts);
          }
          par(new=T);
          if(yd==F&&primary==T) 
          {
            yd <- T;
          }
        }
        par(new=F);
      
        # Set Y-Label X-axis for time and y-axis
      
        Axis(side=1,labels=xaxis_labels[label_indices],at=xvals[label_indices]);
      
        # Set a secondary y-axis and secondary y-axis label
        if(!is.na(range2[1])) 
        {
          axispts <- seq(range2[1],range2[2],axis2step);
          Axis(side=4,at=axispts);
          mtext(ylabs[2],side=4,las=0,line=3,cex=ifelse(print==T,1.0,0.66));
        }
        
        # Legend Labels
        name <- strsplit(sensor_data[[sindices$VAV[1]]][i,1],"[.:]")[[1]];
        legends <- legend_titles;
        northsouth <- vavseries[vavseries[,1]==substr(name[4],1,2),2];
        legends[sindices$ACS] <- paste("ACS 0",northsouth*2+1," ",legend_titles[sindices$ACS],sep="");
        legends[sindices$ACS2] <- paste("ACS 0",northsouth*2+2," ",legend_titles[sindices$ACS2],sep="");
        fauxlegend <- legend("center",legend=legends,ncol=legend_cols,plot=F);
        legend(fauxlegend$rect$left,par("usr")[4]+fauxlegend$rect$h,legend=legends,fill=plot_colors,ncol=legend_cols,xpd=NA);
      
        # Set plot title, x-axis label and possibly a primary y-axis label
        title(main=paste("Floor ",name[2],", Location ",substr(name[4],1,2)," (",quads[quads[,1]==name[4],2],"), FLN ",substr(name[4],3,3),", VAV Box #",substr(name[4],4,5),", ",weekdays(as.Date(paste(year,sensor_data[[1]][i,2],sensor_data[[1]][i,3],sep="-")))," ",months[sensor_data[[1]][i,2]]," ",sensor_data[[1]][i,3],sep=""),cex=0.9,outer=F,line=par("mar")[3]-1);
        title(xlab="Time",outer=F);
        if(yd==T) 
        {
          mtext(ylabs[1],side=2,las=0,line=3,cex=ifelse(print==T,1.0,0.66));
        }
      
        # Draw a box on plot
        box();
        
        # Sensor Counter
        n<-n+1;
    }
      
    #close PDF file
    dev.off();
  }
    
  # Return Successful Plots
  return(n);
}


# PRINT FUNCTION fplot with parameters

print
(
  system.time
  ( 
    print
    (
      paste
      ( 
        fplot
        (
            # Parameter: Valve # Series
            vav_locations ="C:\\Users\\CUNYBPL2\\Desktop\\Graphs\\VAV_box_locations.csv",
            
            # Parameter: Datafile
            fdata = "C:\\Users\\CUNYBPL2\\Desktop\\Graphs\\20130925-151804-ted.csv",
            
            sensors = 
              list
              ( 
                ###################################### AHU #############################################
                
                # AHU Supply Temp - Color: RED
                list(name="AHU:SUP.TEMP",yaxis=2,color="#FF0000",ltitle="AHU Supply Temp"),
                
                # AHU Return Temp - Color: ORANGE 
                list(name="AHU:RET.TEMP",yaxis=2,color="#FFA500",ltitle="AHU Return Temp"),
                
                # AHU Supply Temperature SetPt - Color: BLUE
                list(name="AHU:SUP.TEMP.STPT",yaxis=2,color="#0000FF",ltitle="AHU Supply Temp SetPt"),
                
                # AHU Supply Temperature SetPt - Color: GREEN
                list(name="AHU:OAT",yaxis=2,color="#00FF00",ltitle="AHU Outside Air Temp"),
                
                
                ###################################### VAV #############################################
                
                # VAV Damper Position - Color: Light Purple   
                list(name="VAV:DMPR POS",yaxis=1,color="#FFFFFF",ltitle="VAV Damper Position"),
                
                # VAV Room Temperature - Color: Dark Purple
                list(name="VAV:ROOM TEMP",yaxis=2,color="#FFFFFF",ltitle="VAV Room Temp",peaklim=68:99,peakrng=2)
                
              ),
            #plot_limits=4:99,
            plot_limits = 4:60,
            ylabs=c("VAV Damper Open (%)",expression(paste("Temperature (",degree,"F)"))),
            output_prefix="unit_plots",
            year=2013,
            plot_style = "l"
        )
        ," Plots Successfully Completed.",sep=""
      )
    ) 
  )
);