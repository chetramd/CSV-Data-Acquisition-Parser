# Created and Developed by Jake Essman
# Further Creation by Chetram Dasrat


#plotdata function return value: n, the number of plots completed
#datafile: a string containing the name of the input data file
#sensors: a list of lists containing:
	#...name    = a vector containing the names of the sensor series to be plotted in the format "(ACS|AHU|VAV):SENSOR NAME"
	#...yaxis   = a vector containing the y-axis used for each plotted series (1|2) in the order (sensor1,sensor2,sensor3,...)
	#...color   = a vector of colors for the plotted series in the order (sensor1,sensor2,sensor3,...)
	#...color2  = a vector of colors for the secondary acs series in the order (acs_sensor1,acs_sensor2,acs_sensor3...)
	#...ltitle  = a vector of titles for the legend in the order (sensor1, sensor2, sensor3,...)
	#...peaklim = a vector containing the range of columns to be tested for a peak
	#...peakrng = a number containing the minimum value-range required to recognize a peak
#plot_limits: a vector containing the range of columns to be plotted
#ylabs: a vector of two y labels, the first for the primary y-axis and the second for the (possible) secondary y-axis
#legend_cols: an integer stating the number of columns for the legend
#output_prefix: a string stating the prefix appended before each outputted file
##h: the height of each plot, in inches
#prezero: a boolean value stating whether times before noon are prefaced with a zero in x-axis tick labels
plotdata <- function(datafile,sensors,plot_limits,ylabs,legend_cols=2,output_prefix="data_graph",prezero=F,print=F,year) {#h=4,prezero=F) {
	#read in the data, remove invalid rows, and order by sensor name
	data <- read.csv(datafile,colClasses="character");
	data <- data[substring(data[,1],17)!="RET.DMPR",];
	data <- data.frame(sapply(data[,1], as.character), data.matrix(data[,2:99]), stringsAsFactors=FALSE);
	data <- data[complete.cases(data),];
	data <- data[order(data[,1]),];
	#data frame correlating VAV number to building quadrant
	quads <- data.frame(read.csv("C:\\Users\\CUNYBPL8\\Desktop\\Graphs\\VAV_box_locations.csv"));
	#vector of the months
	months <- c("January","February","March","April","May","June","July","August","September","October","November","December");
	#plot counter
	n<-0;
	#make x-axis labels
	xaxis_labels <- paste(floor((plot_limits-4)/4),60*(((plot_limits-4)/4)-floor((plot_limits-4)/4)),sep=":");
	xaxis_labels <- sapply(xaxis_labels,function(label) {
		llength <- nchar(label);
		if(llength == 3) {
			label<-paste(label,"0",sep="");
		}
		if(llength == 4 && substring(label,3,3) == ":") {
			label<-paste(label,"0",sep="");
		}
		if(llength == 4 && prezero == T) {
			label<-paste("0",label,sep="");
		}
		return(label);
	});
	label_indices <- seq(1,length(xaxis_labels),4);
	#split sensors into the various variables
	sensor_names <- do.call(rbind,strsplit(unlist(lapply(sensors,function(sensor) {
		return(c(sensor$name,ifelse(substr(sensor$name,1,3)=="ACS",paste("ACS2",substring(sensor$name,4),sep=""),NA)));
	})),":"));
	sensor_names <- sensor_names[!is.na(sensor_names[,1]),];
	legend_titles <- unlist(lapply(sensors,function(sensor) {
		return(rep(sensor$ltitle,ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
	}));
	axes <- unlist(lapply(sensors,function(sensor) {
		return(rep(sensor$yaxis,ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
	}));
	plot_colors <- unlist(lapply(sensors,function(sensor) {
		return(c(sensor$color,ifelse(substr(sensor$name,1,3)=="ACS",sensor$color2,NA)));
	}));
	plot_colors <- plot_colors[!is.na(plot_colors)];
	peak_limits <- unlist(lapply(sensors,function(sensor) {
		return(rep(list(sensor$peaklim),ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
	}),recursive=F);
	peak_ranges <- unlist(lapply(sensors,function(sensor) {
		return(rep(list(sensor$peakrng),ifelse(substr(sensor$name,1,3)=="ACS",2,1)));
	}),recursive=F);
	#sort, for some reason...
	rorder <- order(sensor_names[,1],decreasing=T);
	sensor_names <- sensor_names[rorder,];
	legend_titles <- legend_titles[rorder];
	axes <- axes[rorder];
	plot_colors <- plot_colors[rorder];
	peak_limits <- peak_limits[rorder];
	peak_ranges <- peak_ranges[rorder];
	#
	sindices <- split(1:nrow(sensor_names),sensor_names[,1]);
	#split the data into sensor series
	#allocate!
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
	#magic...
	first_vav_data <- data[grep(paste("^JJCEX[.][0-9]{2}[.]VAV[.][0-9]{5}",sensor_names[sindices$VAV[1],2],sep=":"),data[,1]),];
	first_vav_data <- first_vav_data[order(first_vav_data[,2],first_vav_data[,3]),];
	vavnums <- cbind(substr(first_vav_data[,1],7,8),substr(first_vav_data[,1],14,15));
	vavseries <- cbind(rep(as.matrix(aggregate(as.numeric(vavnums[,2]),list(as.numeric(vavnums[,1])),min))[,2],each=2)+c(0,1),c(1,2));
	apply(first_vav_data,1,function(vav) {
		name <- strsplit(vav[1],"[.:]")[[1]];
		northsouth <- vavseries[vavseries[,1]==substr(name[4],1,2),2];
		dtg <- (data[,2]==vav[2]&data[,3]==vav[3]);
		full <- T;
		v<-2;
		while(v <= length(vav_data)) {
			t_vav_data[[v]] <<- data[dtg&data[,1]==paste(paste(name[1:4],collapse="."),sensor_names[sindices$VAV[v],2],sep=":"),];
			full <- (full&nrow(t_vav_data[[v]])==1);
			v<-v+1;
		}
		a<-1;
		while(a <= length(ahu_data)) {
			t_ahu_data[[a]] <<- data[dtg&data[,1]==paste(paste("JJCEX.AHU.",name[2],".0",northsouth,sep=""),sensor_names[sindices$AHU[a],2],sep="."),];
			full <- (full&nrow(t_ahu_data[[a]])==1);
			a<-a+1;
		}
		a<-1;
		while(a <= length(acs1_data)) {
			t_acs1_data[[a]] <<- data[dtg&data[,1]==paste(paste("JJCEX.ACS.PH.0",northsouth*2+1,sep=""),sensor_names[sindices$ACS[a],2],sep="."),];
			full <- (full&nrow(t_acs1_data[[a]])==1);
			a<-a+1;
		}
		a<-1;
		while(a <= length(acs2_data)) {
			t_acs2_data[[a]] <<- data[dtg&data[,1]==paste(paste("JJCEX.ACS.PH.0",northsouth*2+2,sep=""),sensor_names[sindices$ACS2[a],2],sep="."),];
			full <- (full&nrow(t_acs2_data[[a]])==1);
			a<-a+1;
		}
		if(full) {
			vav_data[[1]][rc,] <<- vav;
			v<-2;
			while(v <= length(t_vav_data)) {
				vav_data[[v]][rc,] <<- t_vav_data[[v]];
				v<-v+1;
			}
			a<-1;
			while(a <= length(t_ahu_data)) {
				ahu_data[[a]][rc,] <<- t_ahu_data[[a]];
				a<-a+1;
			}
			a<-1;
			while(a <= length(t_acs1_data)) {
				acs1_data[[a]][rc,] <<- t_acs1_data[[a]];
				a<-a+1;
			}
			a<-1;
			while(a <= length(t_acs2_data)) {
				acs2_data[[a]][rc,] <<- t_acs2_data[[a]];
				a<-a+1;
			}
			rc<<-rc+1;
		}
	});
	sensor_data <- lapply(c(vav_data,ahu_data,acs2_data,acs1_data),function(sm) {
		return(data.frame(sm[!is.na(sm[,1]),1],matrix(as.numeric(as.matrix(sm[!is.na(sm[,1]),-1])),nrow=nrow(sm[!is.na(sm[,1]),])),stringsAsFactors=F));
	});
	#...magic
	#remove non-curvy plots (plots with a range < peak_range)
	curvy_sensors <- c();
	i <- 1;
	while(i <= length(peak_limits)) {
		if(!is.null(peak_limits[[i]])) {
			curvy_sensors <- c(curvy_sensors,i);
		}
		i <- i+1;
	}
	curvy_plots <- rep(TRUE,nrow(sensor_data[[1]]));
	for(i in 1:nrow(sensor_data[[1]])) {
		for(j in curvy_sensors) {
			curvy_plots[i] <- ((curvy_plots[i]==T)&(diff(range(sensor_data[[j]][i,peak_limits[[j]]]))>=peak_ranges[[j]]));
		}
	}
	sensor_data <- lapply(sensor_data,function(sensor) {
		return(sensor[curvy_plots,]);
	});
	#set row ranges for each data section
	data_sections <- ceiling(nrow(sensor_data[[1]])/50);
	filerows <- ceiling(nrow(sensor_data[[1]])/data_sections);
	rowranges <- matrix(c(1:nrow(sensor_data[[1]]),rep(NA,filerows*data_sections-nrow(sensor_data[[1]]))),nrow=filerows,ncol=data_sections);
	lrows <- ceiling(length(legend_titles)/legend_cols);
	#output a file of plots for each data section
	for(g in 1:data_sections) {
		#find plot ranges
		r1data <- unlist(lapply(sensor_data[axes==1],function(sensor) {
			return(sensor[,plot_limits]);
		}));
		r2data <- unlist(lapply(sensor_data[axes==2],function(sensor) {
			return(sensor[,plot_limits]);
		}));
		numticks <- 10;
		range1 <- range(ifelse(rep(length(r1data)==0,ifelse(length(r1data)==0,1,length(r1data))),NA,r1data));
		range1 <- c(floor(range1[1]),ceiling(range1[2]));
		axis1step <- floor(diff(range1)/numticks);
		hrange1 <- range1[1]+numticks*axis1step;
		while(!is.na(hrange1)&hrange1 < range1[2]) {
			hrange1 <- hrange1 + axis1step;
		}
		range1[2] <- hrange1;
		range2 <- range(ifelse(rep(length(r2data)==0,ifelse(length(r2data)==0,1,length(r2data))),NA,r2data));
		range2 <- c(floor(range2[1]),ceiling(range2[2]));
		axis2step <- floor(diff(range2)/numticks);
		hrange2 <- range2[1]+numticks*axis2step;
		while(!is.na(hrange2)&hrange2 < range2[2]) {
			hrange2 <- hrange2 + axis2step;
		}
		range2[2] <- hrange2;
		#make pdf
		if(print==T) {
			pdf(paste(c(output_prefix,"_",g,".pdf"),collapse=""),pointsize=16,width=11,height=8.5);
			par(mar=c(0,0,0,0));
		}
		else {
			pdf(paste(c(output_prefix,"_",g,".pdf"),collapse=""),pointsize=16,height=4*length(rowranges[!is.na(rowranges[,g]),g]));
			par(mfrow=c(length(rowranges[!is.na(rowranges[,g]),g]),1));
		}
		#make a plot for each row
		for(i in rowranges[!is.na(rowranges[,g]),g]) {
			#set x values
			xvals <- plot_limits;
			#set y values
			all_yvals <- t(do.call(rbind,lapply(sensor_data,function(sensor) {
				return(as.vector(sensor[i,plot_limits]));
			})));
			#set graphical parameters (axis label style, margins)
			par(las=2,mar=c(5,4,lrows+2,4)+0.1);
			#plot sensor data columns
			yd<-F;
			for(p in 1:ncol(all_yvals)) {
				primary <- (axes[p] == 1);
				axispts <- seq(range1[1],range1[2],axis1step);
				plot(xvals,all_yvals[,p],pch=20,type="l",col=plot_colors[p],xlab="",ylab="",ylim=ifelse(rep(primary,2),range1,range2),axes=F,cex=1);
				if(primary==T && yd==F) {
					Axis(side=2,at=axispts);
				}
				par(new=T);
				if(yd==F&&primary==T) {
					yd <- T;
				}
			}
			par(new=F);
			#add an appropriately-labeled x-axis for time and y-axis
			Axis(side=1,labels=xaxis_labels[label_indices],at=xvals[label_indices]);
			#possibly add a secondary y-axis and secondary y-axis label
			if(!is.na(range2[1])) {
				axispts <- seq(range2[1],range2[2],axis2step);
				Axis(side=4,at=axispts);
				mtext(ylabs[2],side=4,las=0,line=3,cex=ifelse(print==T,1.0,0.66));
			}
			#add a legend
			name <- strsplit(sensor_data[[sindices$VAV[1]]][i,1],"[.:]")[[1]];
			legends <- legend_titles;
			northsouth <- vavseries[vavseries[,1]==substr(name[4],1,2),2];
			legends[sindices$ACS] <- paste("ACS 0",northsouth*2+1," ",legend_titles[sindices$ACS],sep="");
			legends[sindices$ACS2] <- paste("ACS 0",northsouth*2+2," ",legend_titles[sindices$ACS2],sep="");
			fauxlegend <- legend("center",legend=legends,ncol=legend_cols,plot=F);
			legend(fauxlegend$rect$left,par("usr")[4]+fauxlegend$rect$h,legend=legends,fill=plot_colors,ncol=legend_cols,xpd=NA);
			#add a plot title, x-axis label and possibly a primary y-axis label
			title(main=paste("Floor ",name[2],", Location ",substr(name[4],1,2)," (",quads[quads[,1]==name[4],2],"), FLN ",substr(name[4],3,3),", VAV Box #",substr(name[4],4,5),", ",weekdays(as.Date(paste(year,sensor_data[[1]][i,2],sensor_data[[1]][i,3],sep="-")))," ",months[sensor_data[[1]][i,2]]," ",sensor_data[[1]][i,3],sep=""),cex=0.9,outer=F,line=par("mar")[3]-1);
			title(xlab="Time",outer=F);
			if(yd==T) {
				mtext(ylabs[1],side=2,las=0,line=3,cex=ifelse(print==T,1.0,0.66));
			}
			#draw a box around the plot
			box();
			#increment plot counter
			n<-n+1;
		}
		#close the pdf file
		dev.off();
	}
	#return the number of successful plots
	return(n);
}
print(system.time( print(paste( 
	plotdata(
		datafile="C:\\Users\\CUNYBPL8\\Desktop\\Graphs\\20130908-224439-ted.csv",#"small_data_fullfn.csv",#"data_fullvav.csv",
		sensors=list(
			list(name="ACS:OAT",yaxis=2,color="#00CCCC",ltitle="Outside Air Temp",color2="#00449F"),
			list(name="VAV:DMPR POS",yaxis=1,color="#00FF00",ltitle="VAV Damper Position"),
			list(name="AHU:RET.TEMP",yaxis=2,color="#0000FF",ltitle="AHU Return Temp"),
			list(name="ACS:SUP.TEMP",yaxis=2,color="#FFFF00",color2="#AAAA00",ltitle="Supply Temp",peaklim=4:99,peakrng=1),
			list(name="VAV:ROOM TEMP",yaxis=2,color="#FF0000",ltitle="VAV Room Temp",peaklim=68:99,peakrng=5)
		),
		plot_limits=4:99,
		ylabs=c("VAV Damper % Open",expression(paste("Temperature (",degree,"F)"))),
		output_prefix="unit_plots",
		year=2013
	)
," plots completed.",sep="")) ));
#Note to self: Error in rep(NA, filerows * data_sections - nrow(sensor_data[[1]])) : means there is nothing to plot (check peaks?)