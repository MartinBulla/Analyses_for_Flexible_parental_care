{# INFO
 # data are in longitudinal time
 # data used in the analyses are from 2 hours after the equipment was placed on the nest, and 6 hours before the hatching start or 12 hours before the nest hatched
 # uniparental incubation, if one parent deserted, starts median bout length of the species after the bird has arrived to the nest
 
 # NESTS
	#BARRROW
		#nests=c('a301','a302','b301','b501','s602') # deserted 2011
		#nests=c('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s901','s902') # deserted 2012 (s712 right during hatching, it may even be no desertion); 's815' taken out as birds incubate one egg and its all weired
		#nests=c('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804') # deserted 2013 ('s707' not used as one parent keeps showing up
	# OTHER
		#2013	isla	blgo	ka1411_is2014
		#2014	neth	reds	re117_nl2014
		#2014	neth	reds	re133_nl2014
		#2014	neth	reds	re213_nl2014

# add the experimental ones
# add script for lines for unip start and inc end
 
 # ISSUES
	# two nest temperatures
		# a802 2012 
		# w508 2013
		# re133 should start 2014-06-03 20:31:38
		
	# no use for contancy:
		# dislocated temperature probe
		c('s704')
		's804', 'w504' # part has poor temperature reading, but part is ok
	# one egg incubation	
		's310','s815'
# considered, but have not met UNIPARENTAL definition
	c('l301','l401','l501','s304','s902','w504')		
}

{# TOOLS
	{# define time 
	  Sys.setenv(TZ="UTC")	
	}
	
	{# load packages
	   sapply(c('ggplot2', 'ggthemes','grid','plyr','lattice', 'latticeExtra','magrittr','maptools','raster', 'rgeos', 'rgdal', 'RSQLite','XLConnect','zoo'),
			function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))  
		
		sapply(c('AICcmodavg', 'arm','effects','multcomp'),
			function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))
			require(AICcmodavg)
	}
	
	{# define working and output directories
	    # metadata
		  wd="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Analyses/Data/"	
		# scripts
		  wds="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Analyses/Scripts/"	  
		# SQLite database
		  wd2="C:/Users/mbulla/Documents/ownCloud/BIPtoUNIP/"
		# nest files
		  wd3="C:/Users/mbulla/Documents/ownCloud/BIPtoUNIP/nest_files/"
		  #wd3="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Data/nest_files/"
		# define output folder for actograms
		  outdir="C:/Users/mbulla/Documents/ownCloud/BIPtoUNIP/actos/"
		# define output folder for figures
		  out_="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/figs/"
	}

	{# establish database connections
	   dbq=dbGetQuery
	  
	  # name of sqlite database
	    db=paste(wd2,"bip_to_unip.sqlite",sep="")
	    db2=paste(wd2,"bip_to_unip_metadata.sqlite",sep="")
	}
	
	{# load functions & constants
		source(paste(wds, 'Functions_&_constants.R',sep=""))
		
		col_p="gray53"  # color of point's outline
		col_pb="gray98"  # color of point's background
		col_l="gray73"  # line color of prediction '#FCB42C'
		col_lb="gray92"  # line color of prediction '#FCB42C'
	}
}

{# RESULTS
  {# Abundance of uniparental incubation
	{# run first
		    n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				n$end=as.POSIXct(n$end)
				n$start=as.POSIXct(n$start)
				n=n[!n$sp%in%c('pesa','rnph'),]
					p=readWorksheetFromFile(paste(wd,'populations.xls', sep=""), sheet='populations')
				n$bout=p$bout[match(paste(n$site,n$sp), paste(p$site_abbreviation, p$sp))]
				
				n$start=n$start+n$bout*60*60
				n$end=as.POSIXct(ifelse(n$state=='s', as.character(n$end-6*60*60), ifelse(n$state%in%c('l','h'), as.character(n$end-24*60*60), as.character(n$end))))
					s=read.csv(paste(wd,'inc_start.csv', sep=""), stringsAsFactors=FALSE)
					s$inc_start=as.POSIXct(s$inc_start,tz='UTC')
				n$inc_start=s$inc_start[match(paste(n$year,n$nest),paste(s$year,s$nest))]
				n$day_s = as.Date(trunc(n$start, "day"))
				n$day_j=as.numeric(format(n$day_s ,"%j")) - as.numeric(format(as.Date(trunc(n$inc_start, "day")),"%j"))+1
						ip=read.csv(paste(wd,'inc_period.csv', sep=""), stringsAsFactors=FALSE)
				n$inc_per_sp=ip$inc_period[match(n$sp,ip$sp)]
				n$prop_ip=n$day_j/n$inc_per_sp
				n$uni_last=as.numeric(difftime(n$end, n$start, units='hours'))
				n_=n[n$uni_last>=2*n$bout,]
				
				{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				}
		}	
	
	{# number of species with uniparental incubation from number of species studied
		# uniparental found in 
				length(unique(n_$sp))
						
		# studied species
				nn=readWorksheetFromFile(paste(wd,'populations.xls', sep=""), sheet='populations')
				nn=nn[!nn$sp%in%c('rnph','pesa'),]
				length(unique(nn$sp))
	}
	{# number of uniparental nests per species	
	  {# results within text	
		length(unique(n_$nest)) # overall number of nests
		summary(factor(n_$sex)) # uniparental females and males
		dd=ddply(n_,.(sp,nest), summarise,nn=1)
		ddply(dd,.(sp), summarise,nn=sum(nn)) # numer of nests per species
		
		dd=ddply(n_,.(sp,nest,sex), summarise,nn=1)
		ddply(dd,.(sp,sex), summarise,nn=sum(nn)) # numer of nests per species and sex
		}
	 } 
	{# day in incubation period when the uniparental incubation started and for how long it lasted
		{# started
			summary(n_$prop_ip) # distribution of start of uniparental incubation
			nrow(n_) # number of cases
			length(unique(n_$nest)) # number of nests
			
			densityplot(~n_$prop_ip*100)
			
			summary(n_$prop_ip[n_$prop_ip<1])
			densityplot(~n_$prop_ip[n_$prop_ip<1]*100)
			length(unique(n_$nest[n_$prop_ip<1]))
			length(n_$prop_ip[n_$prop_ip<1])
		}
		{# lasted
			summary(n_$uni_last/24)
			n_$uni_last=n_$uni_last/24
			n_[n_$uni_last>10,]
			densityplot(~n_$uni_last)
		}
		{# nests left because field work ended
			length(n_$nest[n_$state=='w'])+ length(n_$nest[n_$state=='u'])-3 # 3 nests where uncertainty was about hatching, desertion or depredation
		}
	}	 
	
	{# Figure 1a
		sp_=sp[!is.na(sp$order),]
		sp_$order=-sp_$order
		sp_=sp_[order(sp_$order),]
		
		n_$species=sp_$species[match(n_$sp,sp_$sp)]
		n_$order=sp_$order[match(n_$sp,sp_$sp)]
		n_$sex=factor(n_$sex, levels=c('m','f'))
		counts=table(n_$sex,n_$order)
		
		# par(mfrow=c(1,3),mar=c(0.0,0,0,0.4),oma = c(1.8, 1.8, 0.2, 0.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
		 #dev.new(width=3.5*0.75,height=1.85)
		 png(paste(out_,"Figure_1.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
		 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 5, 0.2, 0.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70",
		 cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) # 0.6 makes font 7pt, 0.7 8pt
				#par(ps=12,	cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE)
				
		 barplot(counts, beside=TRUE, horiz=TRUE,
						names.arg=sp_$species,
						xlab="Cases of uniparental incubation [count]", 
						col=c(male_col,female_col), 
						#legend = rownames(counts),args.legend = list(bty='n', legend=c('\u2642','\u2640')),
						xaxt='n'
						)
											
					axis(1, at=seq(0,30,by=5),labels=c(0,'',10,'',20,"",30),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext('Cases of uniparental incubation\n[count]',side=1,line=1, cex=0.6, las=1, col='grey30')
						
					text(y=22.5,x=28, labels='\u2640', col='#FCB42C', cex=0.6)
					text(y=23,x=30, labels='\u2642', col='#535F7C', cex=0.6)
					
						#axis(2, at=seq(0,1,by=0.25), labels=c('0.0','','0.5','','1.0'))
					mtext('a',side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)
		 dev.off()				
		}
	{# Figure 1bc 
		 {# run first	
			n_$uni_last=as.numeric(difftime(n_$end, n_$start, units='days'))
			# create dummy values for species missing nests for one sex
				n1=n_[n_$sp=='blgo',][1,]
				n1$sex='f'
				n1$prop_ip=-50
				n1$uni_last=-2
				n2=n_[n_$sp=='dunl',][1,]
				n2$sex='f'
				n2$prop_ip=-50
				n2$uni_last=-2
				n3=n_[n_$sp=='lbdo',][1,]
				n3$sex='f'
				n3$prop_ip=-50
				n3$uni_last=-2
				n4=n_[n_$sp=='reds',][1,]
				n4$sex='f'
				n4$prop_ip=-50
				n4$uni_last=-2
				n_=rbind(n_,n1,n2,n3,n4)
				
		sp_=sp[!is.na(sp$order),]
		sp_$order=-sp_$order
		sp_=sp_[order(sp_$order),]
		
		n_$species=sp_$species[match(n_$sp,sp_$sp)]
		n_$order=sp_$order[match(n_$sp,sp_$sp)]
		n_$sex=factor(n_$sex, levels=c('m','f'))
			n_$order=ifelse(n_$sex=='m', 2*n_$order-0.4, 2*n_$order+0.4)
			unique(n_$order)[order(unique(n_$order))]
			n_$prop_ip_=n_$prop_ip*100
		n_$col_=ifelse(n_$sex=='f',female_col,male_col)
		table(n_$sex,n_$sp)
					
		
			}
		 {# Figure 1b points	
		   #dev.new(width=3.5*0.75,height=1.85)
		    png(paste(out_,"Figure_1b.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			#ggplot(n_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(n_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = position_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			plot(jitter(n_$order)~n_$prop_ip_,xlim=c(0,160), ylim=c(-16.5,-1), 
						xaxt='n',yaxt='n',type='n',
						#xlab="Cases of uniparental incubation [count]", 
						pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6)
							)
									
					axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext("Start of uniparental incubation\n[% of species' incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
					
					#axis(2, at=seq(-16,-2,2), labels=FALSE)
					abline(h=seq(-16,-2,2), par(xpd=FALSE), col="grey90")
					points(jitter(n_$order)~n_$prop_ip_,pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6))
					#mtext('Nest attendance',side=2,line=1, cex=0.6, las=3, col='grey30')
					#text(y=23,x=28, labels='\u2640', col='#FCB42C', cex=0.6)
					#text(y=23.5,x=30, labels='\u2642', col='#535F7C', cex=0.6)
					mtext('b',side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)	
		 dev.off()				
		}
		 {# Figure 1b points and boxplot
				
		n_$species=sp_$species[match(n_$sp,sp_$sp)]
		n_$order=sp_$order[match(n_$sp,sp_$sp)]
		n_$sex=factor(n_$sex, levels=c('m','f'))
		n_$col_=ifelse(n_$sex=='f',female_col,male_col)
		table(n_$sex,n_$sp)
					
		 #dev.new(width=3.5*0.75,height=1.85)
		 png(paste(out_,"Figure_1b_boxplot.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
		 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			at_=c(seq(1,15,by=2)+0.15, seq(2,16,2)-0.15)
			at_=at_[order(at_)]
			#ggplot(n_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(n_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = position_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			boxplot(n_$prop_ip*100~n_$sex*n_$order, horizontal=TRUE,
						ylim=c(0,160),
						xaxt='n',yaxt='n',
						#xlab="Cases of uniparental incubation [count]", 
						at=at_,#seq(1,16,1),
						outcex=0.5, outpch=20,boxwex=0.5,whisklty=1,staplelty=0,#medlwd=1, 
						lwd = 0.25, 
						#outcol="darkgrey",boxcol='darkgrey',whiskcol='darkgrey',staplecol='darkgrey',medcol='darkgrey',
						#col=c(male_col,female_col)
						outcol="white",boxcol='white',whiskcol='white',staplecol='white',medcol='white'
								
						#legend = rownames(counts),args.legend = list(bty='n', legend=c('\u2642','\u2640')),
							)
			stripchart(n_$prop_ip*100~n_$sex*n_$order, vertical = FALSE, method = "jitter",jitter=0.05, add = TRUE, 
										at=at_,
										pch = 21,cex=0.5, 
										col="gray63",
										#bg=adjustcolor(c(male_col,female_col), alpha.f = 0.4)
										#bg=c(male_col,female_col)
										bg=adjustcolor("gray63", alpha.f = 0.4)
										)
										
			boxplot(n_$prop_ip*100~n_$sex*n_$order,horizontal=TRUE,
										#ylab = NULL,
										xaxt='n', yaxt='n',
										ylim=c(0,160),
										at=at_, 
										outcex=0.5, outpch=20,boxwex=0.5,whisklty=1,staplelty=0,#
										lwd = 0.5,
										border=c(male_col,female_col),
										col = adjustcolor("white", alpha.f = 0), # trick for PNGs, to show what is underneath the boxplot else can be taken out
										#outcol="darkgrey",boxcol='darkgrey',whiskcol='darkgrey',staplecol='darkgrey',medcol='darkgrey', 
										#par(bty='l'),
										add=TRUE
										)					
					
					
											
					axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext("Start of uniparental incubation\n[% of species' incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
					
					axis(2, at=seq(1.5,16,2), labels=FALSE)
					mtext('b',side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)
		 dev.off()				
		}
		
		 {# Figure 1c points	
		   #dev.new(width=3.5*0.75,height=1.85)
		    png(paste(out_,"Figure_1c.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			#ggplot(n_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(n_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = position_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			plot(jitter(n_$order)~n_$uni_last,xlim=c(0,20), ylim=c(-16.5,-1), 
						xaxt='n',yaxt='n',type='n',
						#xlab="Cases of uniparental incubation [count]", 
						pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6)
							)
									
						axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext("Duration of uniparental incubation\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
					
					#axis(2, at=seq(-16,-2,2), labels=FALSE)
					abline(h=seq(-16,-2,2), par(xpd=FALSE), col="grey90")
					points(jitter(n_$order)~n_$uni_last,pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6))
						#mtext('Nest attendance',side=2,line=1, cex=0.6, las=3, col='grey30')
					#text(y=23,x=28, labels='\u2640', col='#FCB42C', cex=0.6)
					#text(y=23.5,x=30, labels='\u2642', col='#535F7C', cex=0.6)
					mtext('c',side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)	
		 dev.off()				
		}
	}
  }
	
  {# Nest success	
		{# run first
		    n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				n$end=as.POSIXct(n$end)
				n$start=as.POSIXct(n$start)
				n=n[!n$sp%in%c('pesa','rnph'),]
					p=read.csv(paste(wd,'populations.csv', sep=""))
				n$bout=p$bout[match(paste(n$site,n$sp), paste(p$site_abbreviation, p$sp))]
				
				n$start=n$start+n$bout*60*60
				n$end=as.POSIXct(ifelse(n$state=='s', as.character(n$end-6*60*60), ifelse(n$state%in%c('l','h'), as.character(n$end-24*60*60), as.character(n$end))))
					s=read.csv(paste(wd,'inc_start.csv', sep=""), stringsAsFactors=FALSE)
					s$inc_start=as.POSIXct(s$inc_start,tz='UTC')
				n$inc_start=s$inc_start[match(paste(n$year,n$nest),paste(s$year,s$nest))]
				n$day_s = as.Date(trunc(n$start, "day"))
				n$day_j=as.numeric(format(n$day_s ,"%j")) - as.numeric(format(as.Date(trunc(n$inc_start, "day")),"%j"))+1
						ip=read.csv(paste(wd,'inc_period.csv', sep=""), stringsAsFactors=FALSE)
				n$inc_per_sp=ip$inc_period[match(n$sp,ip$sp)]
				n$prop_ip=n$day_j/n$inc_per_sp
				n$uni_last=as.numeric(difftime(n$end, n$start, units='days'))
				n_=n[n$uni_last>=2*n$bout/24,]
				g=n_[!n_$state%in%c('r','u','w'),]
				{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				}
		}	
		
		{# number of nests with given state and distribution of success/failure across cay and durantion of uniparental
			unique(n_$state)
			summary(factor(g$state))
			g$success=ifelse(g$state%in%c('s','l','h'),'yes','no')
			table(g$sp,g$success)
			nrow(g)
			
				densityplot(~g$prop_ip)
				densityplot(~g$prop_ip, groups=g$success, auto.key=TRUE)
				
				densityplot(~g$uni_last)
				densityplot(~g$uni_last, groups=g$success, auto.key=TRUE)
				densityplot(~log(g$uni_last))
				
			{# Figure 2a distribution across nests
			sp_=sp[!is.na(sp$order),]
			sp_$order=-sp_$order
			sp_=sp_[order(sp_$order),]
			
			g$species=sp_$species[match(g$sp,sp_$sp)]
			g$order=sp_$order[match(g$sp,sp_$sp)]
			g$success=factor(g$success, levels=c('yes','no'))
			counts=table(g$success,g$order)
			
			# par(mfrow=c(1,3),mar=c(0.0,0,0,0.4),oma = c(1.8, 1.8, 0.2, 0.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
			 #dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_2a.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 5, 0.2, 0.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70",
			 cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) # 0.6 makes font 7pt, 0.7 8pt
					#par(ps=12,	cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE)
					
			 barplot(counts, beside=TRUE, horiz=TRUE,
							names.arg=sp_$species,
							xlab="Number of nests", 
							xlim=c(0,30),
							col=c(male_col,female_col), 
							#legend = rownames(counts),args.legend = list(bty='n', legend=c('\u2642','\u2640')),
							xaxt='n'
							)
												
						axis(1, at=seq(0,30,by=5),labels=c(0,'',10,'',20,"",30),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext('Number of nests\n ',side=1,line=1, cex=0.6, las=1, col='grey30')
							
						text(y=23.5,x=32, labels='Sucessful', col='#FCB42C', cex=0.5, pos=2)
						text(y=22.0,x=32, labels='Failed', col='#535F7C', cex=0.5,pos=2)
						
							#axis(2, at=seq(0,1,by=0.25), labels=c('0.0','','0.5','','1.0'))
						mtext('a',side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)
			 dev.off()				
			}
			{# Figure 2b densityplot across day
				dens_suc <- density(g$prop_ip[g$success=='yes'])
				dens_fail <- density(g$prop_ip[g$success=='no'])
				range(dens_fail$y, dens_suc$y)
				g$col_=ifelse(g$state%in%c('s','l','h'),female_col,male_col)
				
				points_=data.frame( x=g$prop_ip, success=g$success, col_=g$col_, stringsAsFactors=FALSE)
				points_$y=ifelse(points_$success=='yes', -0.125,-0.25)
			#dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_2b.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2, 0.2, 3.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70",
			 cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) # 0.6 makes font 7pt, 0.7 8pt
				
				plot(NA,xlim=c(0,1.60), ylim=c(-0.25,3),ylab='Kernel density',
							xaxt='n'#yaxt='n',
							#xlab="Cases of uniparental incubation [count]", 
							#pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6)
								)
					
						axis(1, at=seq(0,1.60,by=0.20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Start of uniparental incubation\n[% of species' incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext("Kernel density",side=2,line=1, cex=0.6, las=3, col='grey30')
					lines(dens_suc,col=female_col, xpd=FALSE)
					lines(dens_fail,col=male_col,xpd=FALSE)
					points(y=jitter(points_$y),x=points_$x, pch = 21,cex=0.5, col="gray63",bg=adjustcolor(points_$col_, alpha.f = 0.6))
					#points(y=jitter(points_$y),x=points_$x, col=points_$col_, pch=19, cex=0.5)
					
					mtext('b',side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
		}
			{# Figure 2c densityplot across duration
				dens_suc <- density(g$uni_last[g$success=='yes'])
				dens_fail <- density(g$uni_last[g$success=='no'])
				range(dens_fail$y, dens_suc$y)
				summary(g$uni_last)
				g$col_=ifelse(g$state%in%c('s','l','h'),female_col,male_col)
				
				points_=data.frame( x=g$uni_last, success=g$success, col_=g$col_, stringsAsFactors=FALSE)
				points_$y=ifelse(points_$success=='yes', -0.26/2*0.25/3,-0.26*0.25/3)
			#dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_2c.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2, 0.2, 3.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70",
			 cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) # 0.6 makes font 7pt, 0.7 8pt
				
				plot(NA,xlim=c(0,20), ylim=c(-0.26*0.25/3,0.26),ylab='Kernel density',
							xaxt='n'#yaxt='n',
							#xlab="Cases of uniparental incubation [count]", 
							#pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6)
								)
					
						axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Duration of uniparental incubation\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext("Kernel density",side=2,line=1.2, cex=0.6, las=3, col='grey30')
					lines(dens_suc,col=female_col, xpd=FALSE)
					lines(dens_fail,col=male_col,xpd=FALSE)
					points(y=jitter(points_$y),x=points_$x, pch = 21,cex=0.5, col="gray63",bg=adjustcolor(points_$col_, alpha.f = 0.6))
					#points(y=jitter(points_$y),x=points_$x, col=points_$col_, pch=19, cex=0.5)
					
					mtext('c',side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
		}
		}
		{# effect of day and length of uniparental incubation
			g$success_bin=ifelse(g$state%in%c('s','l','h'),1,0)
			g$prop_ip=g$prop_ip*100
			
			{# correlation of predictors
				cor(g$uni_last,g$prop_ip,method = c("pearson"))
				cor(g$uni_last,g$prop_ip,method = c("spearman"))
				cor(log(g$uni_last),g$prop_ip,method = c("pearson"))
				cor(log(g$uni_last),g$prop_ip,method = c("spearman"))
				
				plot(g$uni_last,g$prop_ip)
				plot(log(g$uni_last),g$prop_ip)
								
				nrow(g)
			}	
			
			{# compare models
				m=glmer(success_bin~scale(prop_ip)+scale(uni_last)+(1|sp),data=g[g$prop_ip<100,],family='binomial')
				m=glmer(success_bin~scale(prop_ip)+scale(uni_last)+(1|sp),data=g,family='binomial')
				#mi=glmer(success_bin~scale(prop_ip)*scale(uni_last)+(1|sp),data=g,family='binomial')
				m2=glmer(success_bin~scale(prop_ip)+scale(log(uni_last))+(1|sp),data=g,family='binomial')
				
				ms=glm(success_bin~scale(prop_ip)+scale(uni_last),data=g[g$sp=='sesa',],family='binomial')
			
				AICc(m)
				#AICc(mi)
				AICc(m2)
				summary(m)
				plot(allEffects(m))
			}
			
			{# Figure 3
				{# predictions
				# model
					m=glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')
						  
				# simulation		
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
					apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				
				# coefficients
					v <-apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				{# for day of incubation
					# specify dataset
						newD=data.frame(prop_ip=seq(min(g$prop_ip),max(g$prop_ip),length.out=200),
										uni_last=mean(g$uni_last)
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last,data=newD)	
										
						# calculate predicted values and creditability intervals
								newD$pred = plogis(X%*%v) # in case on binomial scaleback
								
								predmatrix = matrix(nrow=nrow(newD), ncol=nsim)
									for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
									newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
									newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
									#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
									#newD=newD[order(newD$t_tundra),]
								pp=newD
								pp$predictor=pp$prop_ip
			}
				{# for duration of uniparental
					# specify dataset
						newD=data.frame(prop_ip=mean(g$prop_ip),
										uni_last=seq(min(g$uni_last),max(g$uni_last),length.out=200)
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last,data=newD)	
										
						# calculate predicted values and creditability intervals
								newD$pred = plogis(X%*%v) # in case on binomial scaleback
								
								predmatrix = matrix(nrow=nrow(newD), ncol=nsim)
									for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
									newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
									newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
									#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
									#newD=newD[order(newD$t_tundra),]
								pd=newD
								pd$predictor=pd$uni_last
			}
		    }			
				
				png(paste(out_,"Figure_3.png", sep=""), width=3.5,height=1.85,units="in",res=600)
				#dev.new(width=3.5,height=1.85)
				{# a
					par(mfrow=c(1,2),mar=c(2.2,2.1,0.5,0.1),  mgp=c(1.2,0.35,0),oma = c(0, 0, 0, 1),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
				
					plot(NA, xlim=c(0,160), ylim=c(0,1), ylab='Nest success [probability]',xlab=NA, xaxt='n', type='n')#yaxt='n',
							axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Start of uniparental\n[% of incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext('a',side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.48)
							
							polygon(c(pp$predictor, rev(pp$predictor)), c(pp$lwr, 
									rev(pp$upr)), border=NA, col=col_lb)#adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
							lines(pp$predictor, pp$pred, col=col_l,lwd=1)			
							
								# prepare points for plotting
									g$prop_cut=as.character(cut(g$prop_ip, 10))
									g$n=1
									gg=ddply(g,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(prop_ip), n=sum(n))
										# add fake point to match sizes in (a) and (b)
												gg_=gg[1,]
												gg_$mean_=-1
												gg_$se_=0
												gg_$n=24
												gg=rbind(gg,gg_)
							
							#arrows(x0=gg$prop_cut_m, y0=gg$mean_-gg$se_,x1=gg$prop_cut_m,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
								
					}
				{# b
					par(mar=c(2.2,0.2,0.5,2))
				
					plot(NA, xlim=c(0,20), ylim=c(0,1), ylab=NA,xlab=NA, xaxt='n', type='n',yaxt='n')
							#axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
							axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Duration of uniparental\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext('b',side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.86)
							
							polygon(c(pd$predictor, rev(pd$predictor)), c(pd$lwr, 
									rev(pd$upr)), border=NA, col=col_lb)#col=adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
							lines(pd$predictor, pd$pred, col=col_l,lwd=1)			
								
								# prepare points for plotting
									g$prop_cut=as.character(cut(g$uni_last, 10))
									g$n=1
									gg=ddply(g,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(uni_last), n=sum(n))
							
							#arrows(x0=gg$prop_cut_m, y0=gg$mean_-gg$se_,x1=gg$prop_cut_m,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
							
					}
				{# legend
							mtext(expression(italic('N')*' nests:'),side = 4,line=0.2, padj=-6.5,cex=0.5,las=1,col='grey30', xpd=TRUE) 
								symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(23,23,23)+2.7,c(0.77,0.66,0.5),labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
				}			
				dev.off()
			}
			{# Figure 3 only start <100% incubation period
				{# predictions
				# model
					m=glmer(success_bin~prop_ip+uni_last+(1|sp),data=g[g$prop_ip<100,],family='binomial')
						  
				# simulation		
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
					apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				
				# coefficients
					v <-apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				{# for day of incubation
					# specify dataset
						newD=data.frame(prop_ip=seq(min(g$prop_ip[g$prop_ip<100]),max(g$prop_ip[g$prop_ip<100]),length.out=200),
										uni_last=mean(g$uni_last[g$prop_ip<100])
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last,data=newD)	
										
						# calculate predicted values and creditability intervals
								newD$pred = plogis(X%*%v) # in case on binomial scaleback
								
								predmatrix = matrix(nrow=nrow(newD), ncol=nsim)
									for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
									newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
									newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
									#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
									#newD=newD[order(newD$t_tundra),]
								pp=newD
								pp$predictor=pp$prop_ip
			}
				{# for duration of uniparental
					# specify dataset
						newD=data.frame(prop_ip=mean(g$prop_ip[g$prop_ip<100]),
										uni_last=seq(min(g$uni_last[g$prop_ip<100]),max(g$uni_last[g$prop_ip<100]),length.out=200)
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last,data=newD)	
										
						# calculate predicted values and creditability intervals
								newD$pred = plogis(X%*%v) # in case on binomial scaleback
								
								predmatrix = matrix(nrow=nrow(newD), ncol=nsim)
									for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
									newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
									newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
									#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
									#newD=newD[order(newD$t_tundra),]
								pd=newD
								pd$predictor=pd$uni_last
			}
		    }			
				
				dev.new(width=3.5,height=1.85)
				#png(paste(out_,"Figure_3_less_100.png", sep=""), width=3.5,height=1.85,units="in",res=600)
				{# a
					par(mfrow=c(1,2),mar=c(2.2,2.1,0.5,0.1),  mgp=c(1.2,0.35,0),oma = c(0, 0, 0, 1),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
				
					plot(NA, xlim=c(0,100), ylim=c(0,1), ylab='Nest success [probability]',xlab=NA, xaxt='n', type='n')#yaxt='n',
							axis(1, at=seq(0,100,by=10),labels=c(0,"",20,"",40,"",60,"",80,"",100),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Start of uniparental\n[% of incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext('a',side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.48)
							
							polygon(c(pp$predictor, rev(pp$predictor)), c(pp$lwr, 
									rev(pp$upr)), border=NA, col=col_lb)#adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
							lines(pp$predictor, pp$pred, col=col_l,lwd=1)			
							
								# prepare points for plotting
									g_=g[g$prop_ip<100,]
									g_$prop_cut=as.character(cut(g_$prop_ip, 10))
									g_$n=1
									gg=ddply(g_,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(prop_ip), n=sum(n))
										# add fake point to match sizes in (a) and (b)
												gg_=gg[1,]
												gg_$mean_=-1
												gg_$se_=0
												gg_$n=20
												gg=rbind(gg,gg_)
							
							#arrows(x0=gg$prop_cut_m, y0=gg$mean_-gg$se_,x1=gg$prop_cut_m,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
								
					}
				{# b
					par(mar=c(2.2,0.2,0.5,2))
				
					plot(NA, xlim=c(0,20), ylim=c(0,1), ylab=NA,xlab=NA, xaxt='n', type='n',yaxt='n')
							#axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
							axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Duration of uniparental\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext('b',side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.86)
							
							polygon(c(pd$predictor, rev(pd$predictor)), c(pd$lwr, 
									rev(pd$upr)), border=NA, col=col_lb)#col=adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
							lines(pd$predictor, pd$pred, col=col_l,lwd=1)			
								
								# prepare points for plotting
									g_=g[g$prop_ip<100,]
									g_$prop_cut=as.character(cut(g_$uni_last, 10))
									g_$n=1
									gg=ddply(g_,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(uni_last), n=sum(n))
							
							#arrows(x0=gg$prop_cut_m, y0=gg$mean_-gg$se_,x1=gg$prop_cut_m,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
							
					}
				{# legend
							mtext(expression(italic('N')*' cases:'),side = 4,line=0.2, padj=-6.5,cex=0.5,las=1,col='grey30', xpd=TRUE) 
								symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(23,23,23)+2.7,c(0.77,0.66,0.5),labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
				}			
				dev.off()
			}
			{# Figure 3 only SESA
				{# predictions
				# model
					m=glm(success_bin~prop_ip+uni_last,data=g[g$sp=='sesa',],family='binomial')
						  
				# simulation		
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
					apply(bsim@coef, 2, quantile, prob=c(0.025,0.975))	
				
				# coefficients
					v <-apply(bsim@coef, 2, quantile, prob=c(0.5))
				
				{# for day of incubation
					# specify dataset
						newD=data.frame(prop_ip=seq(min(g$prop_ip[g$sp=='sesa']),max(g$prop_ip[g$sp=='sesa']),length.out=200),
										uni_last=mean(g$uni_last[g$sp=='sesa'])
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last,data=newD)	
										
						# calculate predicted values and creditability intervals
								newD$pred = plogis(X%*%v) # in case on binomial scaleback
								
								predmatrix = matrix(nrow=nrow(newD), ncol=nsim)
									for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@coef[i,])
									newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
									newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
									#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
									#newD=newD[order(newD$t_tundra),]
								pp=newD
								pp$predictor=pp$prop_ip
			}
				{# for duration of uniparental
					# specify dataset
						newD=data.frame(prop_ip=mean(g$prop_ip[g$sp=='sesa']),
										uni_last=seq(min(g$uni_last[g$sp=='sesa']),max(g$uni_last[g$sp=='sesa']),length.out=200)
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last,data=newD)	
										
						# calculate predicted values and creditability intervals
								newD$pred = plogis(X%*%v) # in case on binomial scaleback
								
								predmatrix = matrix(nrow=nrow(newD), ncol=nsim)
									for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@coef[i,])
									newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
									newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
									#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
									#newD=newD[order(newD$t_tundra),]
								pd=newD
								pd$predictor=pd$uni_last
			}
		    }			
				
				dev.new(width=3.5,height=1.85)
				#png(paste(out_,"Figure_3_sesa.png", sep=""), width=3.5,height=1.85,units="in",res=600)
				{# a
					par(mfrow=c(1,2),mar=c(2.2,2.1,0.5,0.1),  mgp=c(1.2,0.35,0),oma = c(0, 0, 0, 1),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
				
					plot(NA, xlim=c(0,100), ylim=c(0,1), ylab='Nest success [probability]',xlab=NA, xaxt='n', type='n')#yaxt='n',
							axis(1, at=seq(0,100,by=10),labels=c(0,"",20,"",40,"",60,"",80,"",100),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Start of uniparental\n[% of incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext('a',side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.48)
							
							polygon(c(pp$predictor, rev(pp$predictor)), c(pp$lwr, 
									rev(pp$upr)), border=NA, col=col_lb)#adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
							lines(pp$predictor, pp$pred, col=col_l,lwd=1)			
							
								# prepare points for plotting
									g_=g[g$sp=='sesa',]
									g_$prop_cut=as.character(cut(g_$prop_ip, 10))
									g_$n=1
									gg=ddply(g_,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(prop_ip), n=sum(n))
										# add fake point to match sizes in (a) and (b)
												gg_=gg[1,]
												gg_$mean_=-1
												gg_$se_=0
												gg_$n=20
												gg=rbind(gg,gg_)
							
							#arrows(x0=gg$prop_cut_m, y0=gg$mean_-gg$se_,x1=gg$prop_cut_m,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
								
					}
				{# b
					par(mar=c(2.2,0.2,0.5,2))
				
					plot(NA, xlim=c(0,20), ylim=c(0,1), ylab=NA,xlab=NA, xaxt='n', type='n',yaxt='n')
							#axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
							axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Duration of uniparental\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
							mtext('b',side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.86)
							
							polygon(c(pd$predictor, rev(pd$predictor)), c(pd$lwr, 
									rev(pd$upr)), border=NA, col=col_lb)#col=adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
							lines(pd$predictor, pd$pred, col=col_l,lwd=1)			
								
								# prepare points for plotting
									g_=g[g$sp=='sesa',]
									g_$prop_cut=as.character(cut(g_$uni_last, 10))
									g_$n=1
									gg=ddply(g_,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(uni_last), n=sum(n))
							
							#arrows(x0=gg$prop_cut_m, y0=gg$mean_-gg$se_,x1=gg$prop_cut_m,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
							
					}
				{# legend
							mtext(expression(italic('N')*' cases:'),side = 4,line=0.2, padj=-6.5,cex=0.5,las=1,col='grey30', xpd=TRUE) 
								symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(23,23,23)+2.7,c(0.77,0.66,0.5),labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
				}			
				dev.off()
			}
			{# Supplementary Data Table 1
				m=glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')
			 
				pred=c('Intercept','Incubation period', 'Duration')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='1',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,2)
						oi$lwr_r=round(oi$lwr,2)
						oi$upr_r=round(oi$upr,2)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects
					l=data.frame(summary(m)$varcor)
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(l$vcov,2), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				# create xlsx		
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o1, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
			
		}
}
  }	
	
  {# Change in nest attendance

  }
		
		{# LATER DELETE calculate same but using data from the database
			load(paste(wd,'for_analyses.RData',sep="")) 
			d_=d[!d$sp%in%c('rnph','pesa') & d$type=='uni',]
			dd=ddply(d_,.(sp,nest), summarise,uni_start=min(prop_ip))
			summary(dd)
			dd[dd$uni_start>1,] # s807 started uniparental incubation earlier (based on our visits), but we are missing the incubation record of it)
			nrow(dd)
		}
}	
	
{# SUPPLEMENTARY
	{# ACTOGRAMS
	  {# load metadata
		{# nests to extract the data for
				nests =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				nests$on=as.POSIXct(nests$on)
				nests$off=as.POSIXct(nests$off)
				nests$end=as.POSIXct(nests$end)
				nests$start=as.POSIXct(nests$start)
				nests_=nests[which(nests$circumstances!='temporal'),]
				#p=readWorksheetFromFile(paste(wd,'populations.xls', sep=""), sheet='populations')
				#nests_$bout=p$bout[match(paste(nests_$site,nests_$sp), paste(p$site_abbreviation, p$sp))]
		}
		{# incubation start
				s=read.csv(paste(wd,'inc_start.csv', sep=""), stringsAsFactors=FALSE)
				s$inc_start=as.POSIXct(s$inc_start,tz='UTC')			
		}
		{# incubation period
			 ip=read.csv(paste(wd,'inc_period.csv', sep=""), stringsAsFactors=FALSE)
		}
		{# end states
			 n=read.csv(paste(wd,'end_states.csv', sep=""), stringsAsFactors=FALSE)
			 n$datetime_=as.POSIXct(n$datetime_, tz="UTC")
			 n$nest_year=paste(n$nest,n$year)
			 n_=n[n$sp!='pesa',]
						n=n[n$sp=='pesa',]
						nt=n[n$state=='tag_loss',]
						n2=n[which(!n$nest_year%in%nt$nest_year),]
						n2_=n2[n2$state!='p',]
						n=rbind(nt,n2_)
			n=rbind(n_,n)			
		}
		{# birds
				birds=read.csv(paste(wd,'birds.csv', sep=""), stringsAsFactors=FALSE)
				birds=birds[which(!is.na(birds$tag_ID)),]	
		}			
		{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
		}
	}
	  {# create actograms
			for (i in (1:nrow(nests_))) {
				nest=nests_$nest[i]
				yr= nests_$year[i]
				act_ID=nests_$act_ID[i]
		
				inc_start=s$inc_start[s$nest==nest & s$year==yr]
				ip_=ip$inc_period[ip$sp==nests_$sp[i]]
				print(paste(nest, i,sep=" "))
					
				# load data from database
					conLite = dbConnect(dbDriver("SQLite"),dbname = db)
					dfr_=dbq(conLite,paste("SELECT*FROM", act_ID))
					dbDisconnect(conLite)
					
				dfr_=dfr_[which(!is.na(dfr_$datetime_)),]
				dfr=dfr_
								
				# define captions and nest location
					figCap_=data.frame(sp=dfr$sp[1],scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1], ID=dfr$act_ID[i],stringsAsFactors=FALSE)
					figCap=figCap_
					
					latlon_=data.frame(lat=nests_$lat[nests_$nest==nest],lon=nests_$lon[nests_$nest==nest],stringsAsFactors=FALSE) # for the map
					latlon=latlon_
				
				# generate actograms
					RFID.temperature_actogram(dfr=dfr_,type="PNG", UTC=TRUE,UTC_met=TRUE,day=FALSE, signal=ifelse(figCap_$sp=='pesa',TRUE,FALSE)) #type="SAVE")# 
				
				print(paste(act_ID, nest, i,sep=" "))
				}
	  }
    }	  
}
	
	table(d$sp, freq(d$nest)
	
	

	
{# not USED in the analyses
	{# day in incubation period when the uniparental incubation started and for how long it lasted
		{# run first
		     n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				n$end=as.POSIXct(n$end)
				n$start=as.POSIXct(n$start)
				n=n[!n$sp%in%c('pesa','rnph'),]
					p=readWorksheetFromFile(paste(wd,'populations.xls', sep=""), sheet='populations')
				n$bout=p$bout[match(paste(n$site,n$sp), paste(p$site_abbreviation, p$sp))]
				n$start=n$start+n$bout*60*60
					s=read.csv(paste(wd,'inc_start.csv', sep=""), stringsAsFactors=FALSE)
					s$inc_start=as.POSIXct(s$inc_start,tz='UTC')
				n$inc_start=s$inc_start[match(paste(n$year,n$nest),paste(s$year,s$nest))]
				n$day_s = as.Date(trunc(n$start, "day"))
				n$day_j=as.numeric(format(n$day_s ,"%j")) - as.numeric(format(as.Date(trunc(n$inc_start, "day")),"%j"))+1
						ip=read.csv(paste(wd,'inc_period.csv', sep=""), stringsAsFactors=FALSE)
				n$inc_per_sp=ip$inc_period[match(n$sp,ip$sp)]
				n$prop_ip=n$day_j/n$inc_per_sp
				n$uni_last=as.numeric(difftime(n$end, n$start, units='hours'))
				n_=n[n$uni_last>=2*n$bout,]
		}	
		  
		{# all nests
		 {# started
			summary(n_$prop_ip)
			length(unique(n_$nest))
			nrow(n_)
			densityplot(~n_$prop_ip*100)
			
			summary(n_$prop_ip[n_$prop_ip<1])
			densityplot(~n_$prop_ip[n_$prop_ip<1]*100)
			length(unique(n_$nest[n_$prop_ip<1]))
			length(n_$prop_ip[n_$prop_ip<1])
		}
		 {# lasted
			summary(n_$uni_last/24)
			n_$uni_last=n_$uni_last/24
			n_[n_$uni_last>10,]
			densityplot(~n_$uni_last)
		}
		 {# nests left because field work ended
			length(n_$nest[n_$state=='w'])+ length(n_$nest[n_$state=='u'])-3 # 3 nests where uncertainty was about hatching, desertion or depredation
			n_[n_$state=='u',]
		 }
		}
		{# for nests usted for nest attendance (6h prior to hatching and 24h prior to hatched or left nest excluded)
			n_$end=as.POSIXct(ifelse(n_$state=='s', as.character(n_$end-6*60*60), ifelse(n_$state%in%c('l','h'), as.character(n_$end-24*60*60), as.character(n_$end))))
			n_$uni_last=as.numeric(difftime(n_$end, n_$start, units='hours'))
				n2_=n_[n_$uni_last>=2*n_$bout,]
			
			# lasted
			 nrow(n2_)
			 length(unique(n2_$nest))
			 summary(n2_$uni_last/24)
			 n2_$uni_last=n2_$uni_last/24
			 n2_[n2_$uni_last>10,]
			 densityplot(~n2_$uni_last)
			 
			 
			 n_[!paste(n_$nest,n_$circumstances)%in%paste(n2_$nest,n2_$circumstances),] # nests left out
			 dd=ddply(n_,.(sp,nest), summarise,nn=1)
			 ddply(dd,.(sp), summarise,nn=sum(nn))
		}
	}	
	{# only plots
		{# biparental only plot
				for (i in (1:nrow(nests_))) {
					nest=nests_$nest[i]
					yr= nests_$year[i]
					act_ID=nests_$act_ID[i]
					inc_start=s$inc_start[s$nest==nest & s$year==yr]
					ip_=ip$inc_period[ip$sp==nests_$sp[i]]
					print(paste(nest, i,sep=" "))				
								# get raw incubation data and prepare them for plotting
									#aa=read.table(paste(wd3,nest,".txt",sep=""), sep=",", header=TRUE,stringsAsFactors=FALSE)
								# get raw incubation data and prepare them for plotting
									conLite = dbConnect(dbDriver("SQLite"),dbname = db)
									dfr_=dbq(conLite,paste("SELECT*FROM", act_ID))
									dbDisconnect(conLite)
							
								dfr_=aa[which(!is.na(aa$datetime_)),]
								dfr=dfr_
								
								# define captions and nest location
									figCap_=data.frame(scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1], ID=dfr$act_ID[i],stringsAsFactors=FALSE)
									figCap=figCap_
								
									latlon_=data.frame(lat=nests_$lat[nests_$nest==nest],lon=nests_$lon[nests_$nest==nest],stringsAsFactors=FALSE) # for the map
									latlon=latlon_
								
								# generate actograms
									RFID.temperature_actogram(dfr=dfr_,type="PNG", UTC=FALSE) #type="SAVE")# 
					print(paste(nest, i,sep=" "))	
			}
				}
		{# RNPH only plot
				for (i in (1:nrow(nests_))) {
					nest=nests_$nest[i]
					yr= nests_$year[i]
					act_ID=nests_$act_ID[i]
					inc_start=s$inc_start[s$nest==nest & s$year==yr]
					ip_=ip$inc_period[ip$sp==nests_$sp[i]]
					
					print(paste(nest, i,sep=" "))				
					# get raw incubation data and prepare them for plotting
						#aa=read.table(paste(wd3,nest,".txt",sep=""), sep=",", header=TRUE,stringsAsFactors=FALSE)
					# get raw incubation data and prepare them for plotting
									conLite = dbConnect(dbDriver("SQLite"),dbname = db)
									dfr_=dbq(conLite,paste("SELECT*FROM", act_ID))
									dfr_=aa[which(!is.na(aa$datetime_)),]
									#dfr_$datetime_=as.POSIXct(dfr_$datetime_)
									dfr=dfr_
									
								# define captions and nest location
									figCap_=data.frame(scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1], ID=dfr$act_ID[i],stringsAsFactors=FALSE)
									figCap=figCap_
								
									latlon_=data.frame(lat=nests_$lat[nests_$nest==nest],lon=nests_$lon[nests_$nest==nest],stringsAsFactors=FALSE) # for the map
									latlon=latlon_
								
								# generate actograms
									RFID.temperature_actogram(dfr=dfr_,type="PNG", UTC=TRUE, UTC_met=FALSE) #type="SAVE")# 
					print(paste(nest, i,sep=" "))	
						}
				}
		{# PESA only plot
				for (i in (1:nrow(nests_))) {
					nest=nests_$nest[i]
					yr= nests_$year[i]
					act_ID=nests_$act_ID[i]
					tag_ID= birds$tag_ID[birds$nest==nest & birds$year_==yr]
					bird_ID=birds$bird_ID[birds$nest==nest & birds$year_==yr]
					
					inc_start=s$inc_start[s$nest==nest & s$year==yr]
					ip_=ip$inc_period[ip$sp==nests_$sp[i]]
					# get raw incubation data and prepare them for plotting
						#aa=read.table(paste(wd3,yr,nest,".txt",sep=""), sep=",", header=TRUE,stringsAsFactors=FALSE)
					# get raw incubation data and prepare them for plotting
									conLite = dbConnect(dbDriver("SQLite"),dbname = db)
									dfr_=dbq(conLite,paste("SELECT*FROM", act_ID))
									dfr_=aa[which(!is.na(aa$datetime_)),]
									#dfr_$datetime_=as.POSIXct(dfr_$datetime_)
									dfr=dfr_
									
					# define captions and nest location
						figCap_=data.frame(scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1], ID=dfr$act_ID[i],stringsAsFactors=FALSE)
						figCap=figCap_
								
						latlon_=data.frame(lat=nests_$lat[i],lon=nests_$lon[i],stringsAsFactors=FALSE) # for the map
						latlon=latlon_
								
					# generate actograms
						RFID.temperature_actogram(dfr=dfr_,type="PNG", UTC=TRUE, UTC_met=FALSE,min_=-.05, max_=0.22, signal=TRUE,day=FALSE) #type="SAVE")# 	
					print(paste(nest, i,sep=" "))	
						}
		}
}

}	
	
nests_[,c('nest','bird_ID','sex','datetime_')]		
	
