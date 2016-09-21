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
	   sapply(c('ggplot2', 'ggthemes','grid','gridExtra','plyr','lattice', 'latticeExtra','magrittr','maptools','raster', 'rgeos', 'rgdal', 'RSQLite','XLConnect','zoo'),
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

{# METHODS - Extraction of incubation - Sample sizes
		{# run first
			load(paste(wd,'for_analyses.RData',sep="")) 
			# limit to periods with at least 75% of uniparental or biparental incubation
				d=d[which((d$sp=='pesa' & 0.75<(d$n/1440))| (d$sp!='pesa' & 0.75<(d$n/17280))),]
				d=ddply(d,.(act_ID,type), transform, start_j=day_j-min(day_j)+1)
				h=h[which((h$sp=='pesa' & 0.75<(h$n/60))| (h$sp!='pesa' & 0.75<(h$n/720))),]
				h=ddply(h,.(act_ID,type), transform, start_j=day_j-min(day_j)+1)
				h$hour=as.numeric(h$hour)
				
			{# species
				sp_ =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				sp_$order=-sp_$order
				sp_=sp_[order(sp_$order),]
		
				d$species=sp_$species[match(d$sp,sp_$sp)]
				d$order=as.factor(sp_$order[match(d$sp,sp_$sp)])
				
				h$species=sp_$species[match(h$sp,sp_$sp)]
				h$order=as.factor(sp_$order[match(h$sp,sp_$sp)])

				}	
		}
		
		{# number of cases of daily and hourly nest attendance
			nrow(d) # cases of daily nest attendance
			length(unique(d$act_ID)) # number of nests
			length(unique(d$act_ID[d$sys=='biparental'])) # number of biparental nests
			length(unique(d$act_ID[d$sys=='uniparental'])) # number of biparental nests
			length(unique(d$act_ID[d$sp=='pesa'])) # number of pectoral sandpiper nests
			length(unique(d$act_ID[d$sp=='rnph'])) # number of red-necked phalarope nests
			length(unique(d$sp)) # number of species
			
			nrow(h) # cases of hourly nest attendance
			length(unique(h$act_ID)) # number of nests
			length(unique(h$sp)) # number of species
		
		}
		

}

{# RESULTS & Supplementary
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
		nrow(n_)# number of cases
		length(unique(n_$act_ID)) # overall number of nests
		summary(factor(n_$sex)) # uniparental females and males
		dd=ddply(n_,.(sp,nest), summarise,nn=1)
		ddply(dd,.(sp), summarise,nn=sum(nn)) # numer of nests per species
		
		dd=ddply(n_,.(sp,nest,sex), summarise,nn=1)
		ddply(dd,.(sp,sex), summarise,nn=sum(nn)) # numer of nests per species and sex
		
	 } 
	{# day in incubation period when the uniparental incubation started and for how long it lasted
		{# started
			summary(n_$prop_ip) # distribution of start of uniparental incubation
			nrow(n_) # number of cases
			length(unique(n_$nest)) # number of nests
			
			densityplot(~n_$prop_ip*100)
			
			# limited to those before the eggs were supposed to hatch
			summary(n_$prop_ip[n_$prop_ip<1])
			densityplot(~n_$prop_ip[n_$prop_ip<1]*100)
			length(unique(n_$nest[n_$prop_ip<1])) # number of nests
			length(n_$prop_ip[n_$prop_ip<1]) # number of cases
		}
		{# lasted
			summary(n_$uni_last/24)
			n_$uni_last=n_$uni_last/24
			n_[n_$uni_last>10,]
			densityplot(~n_$uni_last)
		}
		{# nests left because field work ended = 10
			length(n_$nest[n_$state=='w'])+ length(n_$nest[n_$state=='u'])-3+1 # 3 nests with uncertainty was about hatching, desertion or depredation
																			   # 1 nest with thee system taken off for over 10 days and then placed back again
		}
	}	 
	
	{# Figure 1a
		sp_=sp[!sp$sp%in%c('pesa','rnph'),]
		sp_$order=-sp_$order
		sp_=sp_[order(sp_$order),]
		
		n_$species=sp_$species[match(n_$sp,sp_$sp)]
		n_$order=sp_$order[match(n_$sp,sp_$sp)]
		n_$sex=factor(n_$sex, levels=c('m','f'))
		counts=table(n_$sex,n_$order)
		
		# par(mfrow=c(1,3),mar=c(0.0,0,0,0.4),oma = c(1.8, 1.8, 0.2, 0.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
		 #dev.new(width=3.5*0.75,height=1.85)
		 png(paste(out_,"Figure_1a.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
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
					mtext(expression(bold("a")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)
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
					mtext(expression(bold("b")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)	
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
					mtext(expression(bold("c")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)	
		 dev.off()				
		}
			{# not used Figure 1b points and boxplot
				
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
		
	}
	
  }

  {# Change in nest attendance
		{# run first
			load(paste(wd,'for_analyses.RData',sep="")) 
			 n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
			 n=n[!n$circumstances=='temporal',]
			 d$lat=n$lat[match(d$act_ID,n$act_ID)]
			 h$lat=n$lat[match(h$act_ID,n$act_ID)]
			 d$lon=n$lon[match(d$act_ID,n$act_ID)]
			 h$lon=n$lon[match(h$act_ID,n$act_ID)]
			 
			# limit to periods with at least 75% of uniparental or biparental incubation
				d=d[which((d$sp=='pesa' & 0.75<(d$n/1440))| (d$sp!='pesa' & 0.75<(d$n/17280))),]
				d=ddply(d,.(act_ID,type), transform, start_j=day_j-min(day_j)+1)
				h=h[which((h$sp=='pesa' & 0.75<(h$n/60))| (h$sp!='pesa' & 0.75<(h$n/720))),]
				h=ddply(h,.(act_ID,type), transform, start_j=day_j-min(day_j)+1)
				h$hour=as.numeric(h$hour)
				
			{# species
				sp_ =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				sp_$order=-sp_$order
				sp_=sp_[order(sp_$order),]
		
				d$species=sp_$species[match(d$sp,sp_$sp)]
				d$order=as.factor(sp_$order[match(d$sp,sp_$sp)])
				
				h$species=sp_$species[match(h$sp,sp_$sp)]
				h$order=as.factor(sp_$order[match(h$sp,sp_$sp)])

				}	
		}
		
		{# Figure 2a
			{# (a) 
				{# run first - species and order variables
				sp_ =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				sp_$order=-sp_$order
				sp_=sp_[order(sp_$order),]
				sp_$at=+1+sp_$order*2.5
				
				d$species=sp_$species[match(d$sp,sp_$sp)]
				d$order=sp_$order[match(d$sp,sp_$sp)]
				d$at=sp_$at[match(d$sp,sp_$sp)]
				
				 k=0.2
				 k2=0.5
				 d$order_inc=ifelse(d$sys=='uniparental', d$order, ifelse(d$type=='bip', d$order+k, d$order-k))
				 d$at=ifelse(d$sys=='uniparental', d$at, ifelse(d$type=='bip', d$at+k2, d$at-k2))
				 d$cols=ifelse(d$sys=='uniparental', uni_col, ifelse(d$type=='bip',bip_bip_col, bip_uni_col))#
				 
				 col_=ddply(d,.(at,cols), summarise, n=1)
					
						}	
				{# boxplot
					#dev.new(width=0.51+3.5*0.5,height=1.85)
					png(paste(out_,"Figure_4a.png", sep=""), width=0.51+3.5*0.5,height=1.85,units="in",res=600)
					par(mar=c(0.0,0,0,0.4),oma = c(2.1, 5, 0.2, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
				
					boxplot(att ~ order_inc, data = d,
											ylab = NULL,xaxt='n', yaxt='n',
											ylim=c(0.2,1),								
											at=unique(d$at)[order(unique(d$at))],
											#1,2,3.5,4.5,7,8,9.5,10.5
											#at=unique(d$order_inc)[order(unique(d$order_inc))]*2,
											#type='n',
											outcex=0.3,outpch=20,boxwex=0.6,whisklty=1,staplelty=0,#medlwd=1, 
											lwd = 1,
											border=col_$cols,
											#col = adjustcolor("white", alpha.f = 0), # trick for PNGs, to show what is underneath the boxplot else can be taken out
											#outcol="darkgrey",boxcol='darkgrey',whiskcol='darkgrey',staplecol='darkgrey',medcol='darkgrey', 
											horizontal=TRUE
											#add=TRUE
											)					
						axis(1, at=seq(0.2,1,by=0.2), ,mgp=c(0,-0.20,0))
						mtext("Nest attendance\n[proportion]",side=1,line=1, cex=0.6, las=1, col='grey30')
						
						axis(2, at=sp_$at,cex.axis=0.5, labels=sp_$species)
						
						mtext(expression(bold("a")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
					dev.off()
				}
			}
			
			{# not used
			{# ggplot
				{# species
				sp_ =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				sp_$order=-sp_$order
				sp_=sp_[order(sp_$order),]
		
				d$species=sp_$species[match(d$sp,sp_$sp)]
				d$order=as.factor(sp_$order[match(d$sp,sp_$sp)])
				
				h$species=sp_$species[match(h$sp,sp_$sp)]
				h$order=as.factor(sp_$order[match(h$sp,sp_$sp)])

				}	
				dev.new(width=3.5,height=2.8)
				ggplot(d,aes(x=order,y=att, col=type))+	geom_boxplot()+
														coord_flip(ylim = c(0.2, 1))+
														scale_x_discrete(labels=sp_$species)+
														#scale_y_continuous(breaks=c(0.2,0.4,0.6,0.8,1.0))+
														
														scale_colour_discrete(name="Incubation", breaks=c("bip","uni"), labels=c("biparental", "uniparental"))+		
														ylab("Nest attendance [proportion]")+
														annotate("text", label = "Uniparental\nspecies", x = 1.5, y = 0.22, size = 1.8, colour = "grey60",angle = 90)+#y = 0.95,
														theme_light()+
														theme(	
															axis.line=element_line(colour="grey70", size=0.25),
															panel.border=element_rect(colour="grey70", size=0.25),
															panel.grid = element_blank(),
											
															axis.title=element_text(size=7, colour="grey30"),
															axis.title.y = element_blank(),
															axis.title.x = element_text(vjust=0.2),
															axis.text=element_text(size=6),# margin=units(0.5,"mm")),
															axis.ticks.length=unit(0.5,"mm"),
															#axis.ticks.margin,
															
															strip.background = element_blank(),
															strip.text.x = element_blank(),
															#strip.background = element_blank(), 
															#strip.text = element_blank(),
															panel.margin = unit(1, "mm"),
															#legend.position="none"
															#legend.background=element_rect(colour="grey80"),
															legend.key=element_blank(),
															#legend.justification=c(0,0), legend.position=c(-0.03,0.75),
															legend.key.size = unit(0.75, 'lines'),
															legend.text=element_text(size=6, colour="grey30"),
															legend.title=element_text(size=7, colour="grey30")
																)
															
				ggsave(paste(out_,"Figure 4a_.png", sep=""),width=3.5,height=2.8, units='in',dpi=600)	
			}
			
			ggplot(d,aes(x=order,y=att, col=type))+geom_boxplot()+
													scale_x_discrete(labels=sp_$species)+
													theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust=0))
													
			ggplot(d,aes(x=prop_ip,y=att,  col=type, shape=sys))+geom_point()+stat_smooth()
			ggplot(d,aes(x=prop_ip,y=att,  col=type, shape=sys))+geom_point(alpha=0.2)+stat_smooth(method='lm')+theme_bw()
			ggplot(d[d$type=='uni',],aes(x=prop_ip,y=att,  col=sp))+geom_point(alpha=0.2)+stat_smooth(method='lm', se=FALSE)+theme_bw()
			ggplot(d[d$type=='uni',],aes(x=prop_ip,y=att,  col=act_ID))+geom_point(alpha=0.2)+stat_smooth(method='lm', se=FALSE)+theme_bw()
			}
		}	
		{# Supplementary Figure 1
				{# species
				sp_ =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				#sp_$order=-sp_$order
				sp_=sp_[order(sp_$order),]
		
				d$species=sp_$species[match(d$sp,sp_$sp)]
				d$order=as.factor(sp_$order[match(d$sp,sp_$sp)])
				
				h$species=sp_$species[match(h$sp,sp_$sp)]
				h$order=as.factor(sp_$order[match(h$sp,sp_$sp)])

				}	
				{# add sex
					n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
					n=n[!duplicated(n$act_ID),]
					d$sex=NA
					d$sex[d$type=='uni']=n$sex[match(d$act_ID[d$type=='uni'],n$act_ID)]
					d$sex=factor(ifelse(is.na(d$sex),'biparental', ifelse(d$sex=='m', 'uniparental male', 'uniparental female')),levels=c('biparental','uniparental female', 'uniparental male'))#'uniparental \u2640', 'uniparental \u2642'))
					}
				{# create order for plotting
					d=d[order(d$order, d$act_ID),]
						xx=data.frame(act_ID=unique(d$act_ID))
						xx$order_=1:nrow(xx)
					d$order_all=xx$order_[match(d$act_ID,xx$act_ID)]	
					
					# create dummy value for one nest without >0.75 uni incubation in one day 
					dd=d[d$type=='uni',]
					d2=d[!d$act_ID%in%c(unique(dd$act_ID)),][1,]
					d2$type='uni'
					d2$sex='uniparental male'
					d2$att=-1
					dd=rbind(dd,d2)
				}
				
				dev.new(width=7, height=6.5)
				ggplot(dd,aes(x=prop_ip,y=att))+
										geom_point(aes(fill=sex),shape=21,size=0.9,col='grey50')+#	#geom_point(aes(shape=sys),alpha=0.3,size=1)+
										scale_fill_manual(name="Incubation",breaks=c('uniparental female', 'uniparental male'),values=alpha(c("#FCB42C","#535F7C"),0.6))+
										stat_smooth(aes(col=order),method='lm', se=FALSE, lwd=1)+
										scale_colour_discrete(name="Species", labels=sp_$species[order(sp_$order)])+
										scale_x_continuous(limits=c(0,1.6),breaks=seq(0,1.5,by=0.5), labels=c('0','50','100','150'))+	
										scale_y_continuous(limits=c(-0.05,1.05),breaks=seq(0,1,by=0.25), labels=c('0.0','','0.5','','1.0'))+										
										facet_wrap(~order_all, ncol=8)+#, scales="free_x")+
										guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2))+
										xlab("Species' incubation period [%]")+
										ylab("Nest attendance [proportion]")+
										theme_light()+
										theme(	
												axis.line=element_line(colour="grey70", size=0.25),
												panel.border=element_rect(colour="grey70", size=0.25),
												panel.grid = element_blank(),
								
												axis.title=element_text(size=7, colour="grey30"),
												axis.title.y = element_text(vjust=1.1),
												axis.title.x = element_text(vjust=0.2),
												axis.text=element_text(size=6),# margin=units(0.5,"mm")),
												axis.ticks.length=unit(0.5,"mm"),
												#axis.ticks.margin,
												
												strip.background = element_blank(),
												strip.text.x = element_blank(),
												#strip.background = element_blank(), 
												#strip.text = element_blank(),
												panel.margin = unit(1, "mm"),
												#legend.position="none"
												#legend.background=element_rect(colour="white"),
												legend.key=element_blank(),
												legend.key.size = unit(0.75, 'lines'),
												legend.text=element_text(size=6, colour="grey30"),
												legend.title=element_text(size=7, colour="grey30")
													)
		
				ggsave(paste(out_,"Supplementary_Figure_1.png", sep=""),width=7, height=6.5, units='in',dpi=600)						
				}
		
		{# Figure 2b and Supplementary Table 1
			{# run first)
			d$actID_type=interaction(d$act_ID,d$type)
			d$sp_type=interaction(d$sp,d$type)
			d$types=factor(ifelse(d$sys=='uniparental', 'unip_sp', ifelse(d$type=='bip', 'bip_sp_bip', 'bip_sp_uni')))#,levels=c('unip_sp','bip_sp_uni','bip_sp_bip'))
			d$cols=factor(ifelse(d$sys=='uniparental', uni_col, ifelse(d$type=='bip',bip_bip_col, bip_uni_col)))#,levels=c('unip_sp','bip_sp_uni','bip_sp_bip'))
			dd=d[d$sp!='pesa',]
			m=lmer(att~scale(prop_ip)*types+(prop_ip|actID_type)+(prop_ip|sp_type),d, REML=FALSE)	
			#ms=lmer(att~types+(prop_ip|actID_type)+(prop_ip|sp_type),d, REML=FALSE)	
			#mp=lmer(att~poly(prop_ip,2)*types+(prop_ip|actID_type)+(prop_ip|sp_type),d, REML=FALSE)	
			#m=lmer(att~scale(prop_ip)*types+(prop_ip|act_ID)+(prop_ip|sp),d)	
			
			#plot(allEffects(m))	
			#summary(glht(m))
			#summary(m)
			}
			{# run first - prepare predictions
							m=lmer(att~scale(prop_ip)*types+(prop_ip|actID_type)+(prop_ip|sp_type),d, REML=FALSE)	
							#summary(m)
							#plot(allEffects(m))
						
							# simulation		
								nsim <- 5000
								bsim <- sim(m, n.sim=nsim)  
								apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
							
							# coefficients
								v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))	
							# predicted values		
								newDa=data.frame(prop_ip=seq(min(d$prop_ip[d$types=='bip_sp_bip']),max(d$prop_ip[d$types=='bip_sp_bip']),length.out=200),
												types='bip_sp_bip')
								newDb=data.frame(prop_ip=seq(min(d$prop_ip[d$types=='bip_sp_uni']),max(d$prop_ip[d$types=='bip_sp_uni']),length.out=200),
												types='bip_sp_uni')	
								newDc=data.frame(prop_ip=seq(min(d$prop_ip[d$types=='unip_sp']),max(d$prop_ip[d$types=='unip_sp']),length.out=200),
												types='unip_sp')
								newD=rbind(newDa,newDb,newDc)			
																
							# exactly the model which was used has to be specified here 
								X <- model.matrix(~ prop_ip*types,data=newD)	
											
							# calculate predicted values and creditability intervals
								newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
										predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
										for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
										newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
										newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
										#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
										#newD=newD[order(newD$t_tundra),]
								p=newD
								pu=p[p$types=='unip_sp',]
								pbb=p[p$types=='bip_sp_bip',]
								pbu=p[p$types=='bip_sp_uni',]
				}			
			{# plot 
				 #dev.new(width=3.5*0.5,height=1.85)
					png(paste(out_,"Figure_4b_.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,1.6), ylim=c(0,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,1.6,by=0.4),labels=seq(0,1.6,by=0.4),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Species' incubation period\n[proportion]",side=1,line=1, cex=0.6, las=1, col='grey30')
							
							axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("b")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							#lines(c(0,0),c(0,16.5), lty=3, col="red")							
						# data
							points(d$att~d$prop_ip, col=adjustcolor(d$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
						
						
						# predictions
							# uniparental species
							polygon(c(pu$prop_ip, rev(pu$prop_ip)), c(pu$lwr, 
								rev(pu$upr)), border=NA, col=adjustcolor(uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pu$prop_ip, pu$pred, col=uni_col,lwd=1)
							
							# biparental species biparental incubation
							polygon(c(pbb$prop_ip, rev(pbb$prop_ip)), c(pbb$lwr, 
								rev(pbb$upr)), border=NA, col=adjustcolor(bip_bip_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pbb$prop_ip, pbb$pred, col=bip_bip_col,lwd=1)
							
							# biparental species uniparental incubation
							polygon(c(pbu$prop_ip, rev(pbu$prop_ip)), c(pbu$lwr, 
								rev(pbu$upr)), border=NA, col=adjustcolor(bip_uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pbu$prop_ip, pbu$pred, col=bip_uni_col,lwd=1)
							
																			
					
							text(x=0.2,y=0.13, labels='Biparental\nspecies\nbiparental\nincubation', col=bip_bip_col, cex=0.4)
							text(x=0.8,y=0.13, labels='Biparental\nspecies\nuniparental\nincubation', col=bip_uni_col, cex=0.4)
							text(x=1.4,y=0.13, labels='Uniparental\nspecies\nuniparental\nincubation', col=uni_col, cex=0.4)
							
													
				dev.off()
			}	
			
			{# Supplementary Data Table 1
				m=lmer(att~scale(prop_ip)*types+(prop_ip|actID_type)+(prop_ip|sp_type),d, REML=FALSE)	
			 
				pred=c('Intercept (bip_bip)','Incubation period', 'Type (bip_uni)','Type (uni)', 'Incubation period x bip_sp_uni', 'Incubation period x uni')
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
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(l$vcov*100,2), lwr_r=NA, upr_r=NA)
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
					{# model assumptions simple
						#dev.new(width=6,height=9)
						png(paste(out_,"model_ass/Supplementary_Table1.png", sep=""), width=6,height=9,units="in",res=600)
						m=lmer(att~scale(prop_ip)*types+(prop_ip|actID_type)+(prop_ip|sp_type),d, REML=FALSE)	
						par(mfrow=c(5,3))
						
						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									 
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
						
							
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
						qqline(resid(m))
			 
						qqnorm(unlist(ranef(m)$actID_type[1]), main = " actID_type",col='red')
						qqline(unlist(ranef(m)$actID_type[1]))
						
						qqnorm(unlist(ranef(m)$actID_type[2]), main = " prop_ip|actID_type",col='red')
						qqline(unlist(ranef(m)$actID_type[2]))
						
						qqnorm(unlist(ranef(m)$sp_type[1]), main = " sp_type",col='red')
						qqline(unlist(ranef(m)$sp_type[1]))
						
						qqnorm(unlist(ranef(m)$sp_type[2]), main = " prop_ip|sp_type",col='red')
						qqline(unlist(ranef(m)$sp_type[2]))
						
						scatter.smooth(resid(m)~d$prop_ip);abline(h=0, lty=2, col='red')
						plot(resid(m)~d$types);abline(h=0, lty=2, col='red')
						
						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=d$lon, y=d$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
								
								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					mtext("lmer(att~scale(prop_ip)*types+(prop_ip|actID_type)+(prop_ip|sp_type),d, REML=FALSE)", side = 3, line = -1.2, cex=0.8,outer = TRUE)	
				dev.off()
				}
			
		}
		{# Supplementary Figure 2a, Supplementary Table 2
			{# run first - prepare data	
				# limited to species with uniparental data for both sexes	
					dd=d[d$sys=='biparental' & d$type=='uni' & d$sp%in%c('amgp','basa','sesa','wesa'),]
					nrow(dd) # N
					length(unique(dd$act_ID)) # N nests
					length(unique(dd$sp)) # N species
					
									
				{# add sex
				nest =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				nest_=nest[nest$circumstances!='temporal',]

				dd$sex=as.factor(nest_$sex[match(dd$act_ID,nest_$act_ID)])
				dd$cols=ifelse(dd$sex=='f', uni_col, bip_uni_col)
				
				}	
				
			}
			{# run first - prepare predictions
							m=lmer(att~scale(prop_ip)*sex+(prop_ip|act_ID)+(prop_ip|sp),dd, REML=FALSE)		
							#summary(m)
							#plot(allEffects(m))
						
							# simulation		
								nsim <- 5000
								bsim <- sim(m, n.sim=nsim)  
								apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
							
							# coefficients
								v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))	
							# predicted values		
								newDa=data.frame(prop_ip=seq(min(dd$prop_ip[dd$sex=='f']),max(d$prop_ip[dd$sex=='f']),length.out=200),
												sex='f')
								newDb=data.frame(prop_ip=seq(min(d$prop_ip[dd$sex=='m']),max(d$prop_ip[dd$sex=='m']),length.out=200),
												sex='m')	
							
								newD=rbind(newDa,newDb)			
																
							# exactly the model which was used has to be specified here 
								X <- model.matrix(~ prop_ip*sex,data=newD)	
											
							# calculate predicted values and creditability intervals
								newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
										predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
										for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
										newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
										newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
										#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
										#newD=newD[order(newD$t_tundra),]
								p=newD
								p_f=p[p$sex=='f',]
								p_m=p[p$sex=='m',]
						
				}			
			
			{# Supplementary Figure 2a 
				 #dev.new(width=3.5*0.5,height=1.85)
					png(paste(out_,"Supplementary_Figure_2a_sex.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,1.6), ylim=c(-0.05,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,1.6,by=0.4),labels=seq(0,1.6,by=0.4),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Species' incubation period\n[proportion]",side=1,line=1, cex=0.6, las=1, col='grey30')
							
							axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("a")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							#lines(c(0,0),c(0,16.5), lty=3, col="red")							
						# data
							points(dd$att~dd$prop_ip, col=adjustcolor(dd$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
						
						
						# predictions
							# uniparental species
							polygon(c(p_f$prop_ip, rev(p_f$prop_ip)), c(p_f$lwr, 
								rev(p_f$upr)), border=NA, col=adjustcolor(uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(p_f$prop_ip, p_f$pred, col=uni_col,lwd=1)
														
							# biparental species uniparental incubation
							polygon(c(p_m$prop_ip, rev(p_m$prop_ip)), c(p_m$lwr, 
								rev(p_m$upr)), border=NA, col=adjustcolor(bip_uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(p_m$prop_ip, p_m$pred, col=bip_uni_col,lwd=1)
							
																			
					
							text(y=0.045,x=0.9333333*1.6, labels='\u2640', col='#FCB42C', cex=0.6)
							text(y=0.05,x=1.6, labels='\u2642', col='#535F7C', cex=0.6)
					
												
													
				dev.off()
			}	
			
			{# Supplementary Data Table 2
				pred=c('Intercept (female)','Incubation period', 'Sex (male)', 'Incubation period x male')
						
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
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(l$vcov*100,2), lwr_r=NA, upr_r=NA)
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
					{# model assumptions simple
						#dev.new(width=6,height=9)
						png(paste(out_,"model_ass/Supplementary_Table2.png", sep=""), width=6,height=9,units="in",res=600)
						m=lmer(att~scale(prop_ip)*sex+(prop_ip|act_ID)+(prop_ip|sp),dd, REML=FALSE)	
						
						par(mfrow=c(5,3))
												
						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
													
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
						qqline(resid(m))
			 
						qqnorm(unlist(ranef(m)$act_ID[1]), main = " act_ID")
						qqline(unlist(ranef(m)$act_ID[1]))
						
						qqnorm(unlist(ranef(m)$act_ID[2]), main = " prop_ip|act_ID")
						qqline(unlist(ranef(m)$act_ID[2]))
						
						qqnorm(unlist(ranef(m)$sp[1]), main = " sp")
						qqline(unlist(ranef(m)$sp[1]))
						
						qqnorm(unlist(ranef(m)$sp[2]), main = " prop_ip|sp")
						qqline(unlist(ranef(m)$sp[2]))
						
						scatter.smooth(resid(m)~scale(dd$prop_ip),col='red');abline(h=0, lty=2, col='blue')
						scatter.smooth(resid(m)~as.factor(dd$sex),col='red');abline(h=0, lty=2, col='blue')
						plot(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
						
						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=dd$lon, y=dd$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
								
								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
						mtext("lmer(att~scale(prop_ip)*sex+(prop_ip|act_ID)+(prop_ip|sp),dd, REML=FALSE)", side = 3, line = -1.2, cex=0.8,outer = TRUE)	
					dev.off()
				}
			
		}
		
		{# Figure 2c-d &  Supplementary Table 3
		  {# run first	
			h$actID_type=interaction(h$act_ID,h$type)
			h$sp_type=interaction(h$sp,h$type)
			h$types=factor(ifelse(h$sys=='uniparental', 'unip_sp', ifelse(h$type=='bip', 'bip_sp_bip', 'bip_sp_uni')))
			h$cols=factor(ifelse(h$sys=='uniparental', uni_col, ifelse(h$type=='bip', bip_bip_col, bip_uni_col)))
			h$hour=as.numeric(h$hour)
			h$rad=as.numeric(h$hour)*pi/12
			h$sin_=sin(h$rad)
			h$cos_=cos(h$rad)
			h=ddply(h,.(nest, type, sys, year), transform, att_previous=c(NA,att[-length(att)]))
				
			#m=lmer(att~sin(rad)*types+cos(rad)*types+(sin(rad)+cos(rad)|actID_type)+(sin(rad)+cos(rad)|sp_type),data=h[!is.na(h$att_previous),], REML=FALSE)	
			#m2=lmer(att~att_previous+sin(rad)*types+cos(rad)*types+(sin(rad)+cos(rad)|actID_type)+(sin(rad)+cos(rad)|sp_type),data=h[!is.na(h$att_previous),], REML=FALSE)	
			#plot(allEffects(m))	
			#summary(glht(m))
			#summary(m)
		  }	
		  {# run first - prepare predictions
							m=lmer(att~sin_*types+cos_*types+(sin(rad)+cos(rad)|actID_type)+(sin(rad)+cos(rad)|sp_type),data=h, REML=FALSE)	
							#summary(m)
							#plot(allEffects(m))
						
							# simulation		
								nsim <- 5000
								bsim <- sim(m, n.sim=nsim)  
								apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
							
							# coefficients
								v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))	
							# predicted values		
								ll=list()
								for(i in c('bip_sp_bip','bip_sp_uni','unip_sp')){
									newD=data.frame(hour=seq(0,24,0.25))
										newD$rad=2*pi*newD$hour / 24
										newD$sin_=sin(newD$rad)
										newD$cos_=cos(newD$rad)
										newD$types=i
										ll[[i]]=newD
										}
								newD=do.call(rbind,ll)		
								
							# exactly the model which was used has to be specified here 
								X <- model.matrix(~ sin_*types+cos_*types,data=newD)	
											
							# calculate predicted values and creditability intervals
								newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
										predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
										for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
										newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
										newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
										#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
										#newD=newD[order(newD$t_tundra),]
								p=newD
								pu=p[p$types=='unip_sp',]
								pbb=p[p$types=='bip_sp_bip',]
								pbu=p[p$types=='bip_sp_uni',]
				}			
		  {# Figure 2c-d
					#dev.new(width=3.5*0.5,height=1.85)
					{# (c) raw data
						png(paste(out_,"Figure_4c.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0.2,0,0.2),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,24), ylim=c(0,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("c")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							lines(c(0,24),c(0.19,0.19),, lty=3, col="grey80")							
						# data
							# aggregate per hour, species and type of incubation		
								h$nn=1
								gg=ddply(h,.(sp,cols, types, hour), summarise,mean_=mean(att),se_=sd(att)/sqrt(length(att)),n=sum(nn))
								gg=gg[order(gg$types),]							
							#arrows(x0=gg$hour, y0=gg$mean_-gg$se_,x1=gg$hour,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE, fg=col_p) #bg=alpha(col_p,0.1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,fg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE), fg=col_p) #bg=alpha(col_p,0.1)
								
														#points(h$att~h$hour, col=adjustcolor(h$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
							
								text(x=-2, y=0.105, pos=4, expression(italic('N')*' hours:'),cex=0.5,las=1,col='grey30') 
								symbols(c(10,15,20),c(0.105,0.105,0.105),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(10,15,20),c(0.007,0.007,0.007),labels=c(10,100,300), xpd=TRUE, cex=0.5,col='grey30') 
						dev.off()															
						
						}
					{# (d) 
						png(paste(out_,"Figure_4d.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,24), ylim=c(0,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							mtext(expression(bold("d")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
						# predictions
							# uniparental species
							polygon(c(pu$hour, rev(pu$hour)), c(pu$lwr, 
								rev(pu$upr)), border=NA, col=adjustcolor(uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pu$hour, pu$pred, col=uni_col,lwd=1)
							
							# biparental species biparental incubation
							polygon(c(pbb$hour, rev(pbb$hour)), c(pbb$lwr, 
								rev(pbb$upr)), border=NA, col=adjustcolor(bip_bip_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pbb$hour, pbb$pred, col=bip_bip_col,lwd=1)
							
							# biparental species uniparental incubation
							polygon(c(pbu$hour, rev(pbu$hour)), c(pbu$lwr, 
								rev(pbu$upr)), border=NA, col=adjustcolor(bip_uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pbu$hour, pbu$pred, col=bip_uni_col,lwd=1)
							dev.off()
						}
			}	
		  {# Supplementary Table 3
				m=lmer(att~sin(rad)+cos(rad) + types +sin(rad)*types+cos(rad)*types+(sin(rad)+cos(rad)|actID_type)+(sin(rad)+cos(rad)|sp_type),data=h, REML=FALSE)	
			 
				pred=c('Intercept (bip_bip)','Sin', 'Cos','Type (bip_uni)','Type (uni)', 'Sin x bip_sp_uni', 'Sin x uni', 'Cos x bip_sp_uni', 'Cos x uni')
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
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(l$vcov*100,2), lwr_r=NA, upr_r=NA)
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
					{# model assumptions simple
						#dev.new(width=6,height=9)
						m=lmer(att~sin(rad)+cos(rad) + types +sin(rad)*types+cos(rad)*types+(sin(rad)+cos(rad)|actID_type)+(sin(rad)+cos(rad)|sp_type),data=h, REML=FALSE)	
						
						png(paste(out_,"model_ass/Supplementary_Table3.png", sep=""), width=6,height=9,units="in",res=600)
						par(mfrow=c(4,4))
						
						scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col='red')
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
						
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
						qqline(resid(m))
						
						qqnorm(unlist(ranef(m)$actID_type[1]), main = " actID_type",col='grey')
						qqline(unlist(ranef(m)$actID_type[1]))
						
						qqnorm(unlist(ranef(m)$actID_type[2]), main = " sin|actID_type",col='grey')
						qqline(unlist(ranef(m)$actID_type[2]))
						
						qqnorm(unlist(ranef(m)$actID_type[3]), main = " cos|actID_type",col='grey')
						qqline(unlist(ranef(m)$actID_type[3]))
						
						qqnorm(unlist(ranef(m)$sp_type[1]), main = " sp_type",col='grey')
						qqline(unlist(ranef(m)$sp_type[1]))
						
						qqnorm(unlist(ranef(m)$sp_type[2]), main = " sin|sp_type",col='grey')
						qqline(unlist(ranef(m)$sp_type[2]))
						
						qqnorm(unlist(ranef(m)$sp_type[3]), main = " cos|sp_type",col='grey')
						qqline(unlist(ranef(m)$sp_type[3]))
						
						scatter.smooth(resid(m)~sin(h$rad),col='grey');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~cos(h$rad),col='grey');abline(h=0, lty=2, col='red')
						#scatter.smooth(resid(m)~as.factor(h$types));abline(h=0, lty=2, col='red')
						plot(resid(m)~h$types);abline(h=0, lty=2, col='red')
						
						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=h$lon, y=h$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
								
								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					mtext("lmer(att~sin(rad)+cos(rad) + types +sin(rad)*types+cos(rad)*types+(sin(rad)+cos(rad)|actID_type)+(sin(rad)+cos(rad)|sp_type),data=h, REML=FALSE)", side = 3, line = -1.2, cex=0.5,outer = TRUE)	
					
					dev.off()
				}
			
		}
		{# Supplementary Figure 2b-c, Supplementary Table 4
			{# run first - prepare data	
				# limited to species with uniparental data for both sexes	
				hh=h[h$sys=='biparental' & h$type=='uni' & h$sp%in%c('amgp','basa','sesa','wesa'),]
					nrow(hh) # N
					length(unique(hh$act_ID)) # N nests
					length(unique(hh$sp)) # N species
				{# add sex
				nest =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				nest_=nest[nest$circumstances!='temporal',]

				hh$sex=as.factor(nest_$sex[match(hh$act_ID,nest_$act_ID)])
				hh$cols=ifelse(hh$sex=='f', uni_col, bip_uni_col)
				}
				
				# convert time to radians
					hh$hour=as.numeric(hh$hour)
					hh$rad=as.numeric(hh$hour)*pi/12
					hh$sin_=sin(hh$rad)
					hh$cos_=cos(hh$rad)
				# min value for y-axis
					k=-0.05	
			}	
			{# run first - prepare predictions
							m=lmer(att~sin_*sex+cos_*sex+(sin_+cos_|act_ID)+(sin_+cos_|sp),data=hh, REML=FALSE)
							#m=lmer(att~sin(rad)+cos(rad) + sex +sin(rad)*sex+cos(rad)*sex+(sin(rad)+cos(rad)|act_ID)+(sin(rad)+cos(rad)|sp),data=hh, REML=FALSE)	
							#summary(m)
							#plot(allEffects(m))
						
							# simulation		
								nsim <- 5000
								bsim <- sim(m, n.sim=nsim)  
								apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
							
							# coefficients
								v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))	
							# predicted values		
								ll=list()
								for(i in c('f','m')){
									newD=data.frame(hour=seq(0,24,0.25))
										newD$rad=2*pi*newD$hour / 24
										newD$sin_=sin(newD$rad)
										newD$cos_=cos(newD$rad)
										newD$sex=i
										ll[[i]]=newD
										}
								newD=do.call(rbind,ll)		
								
							# exactly the model which was used has to be specified here 
								X <- model.matrix(~ sin_*sex+cos_*sex,data=newD)	
											
							# calculate predicted values and creditability intervals
								newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
										predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
										for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
										newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
										newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
										#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
										#newD=newD[order(newD$t_tundra),]
								p=newD
								p_f=p[p$sex=='f',]
								p_m=p[p$sex=='m',]
				}			
			
			{# Supplementary Figure 2b-c
					#dev.new(width=3.5*0.5,height=1.85)
					{# (b) raw data
						png(paste(out_,"Supplementary_Figure_2b.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						
						par(mar=c(0.0,0.2,0,0.2),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("b")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							lines(c(0,24),c(0.19,0.19)+k,, lty=3, col="grey80")							
						# data
							# aggregate per hour, species and type of incubation		
								hh$nn=1
								gg=ddply(hh,.(sp,cols, sex, hour), summarise,mean_=mean(att),se_=sd(att)/sqrt(length(att)),n=sum(nn))
								gg=gg[order(gg$sex),]							
							#arrows(x0=gg$hour, y0=gg$mean_-gg$se_,x1=gg$hour,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE, fg=col_p) #bg=alpha(col_p,0.1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,fg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE), fg=col_p) #bg=alpha(col_p,0.1)
								
														#points(h$att~h$hour, col=adjustcolor(h$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
							
								text(x=-2+k, y=0.105, pos=4, expression(italic('N')*' cases:'),cex=0.5,las=1,col='grey30') 
								symbols(c(10,15,20),c(0.105,0.105,0.105)+k,circles=sqrt(c(10,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(10,15,20),c(0.007,0.007,0.007)+k,labels=c(10,100,150), xpd=TRUE, cex=0.5,col='grey30') 
						dev.off()															
						
						}
					{# (c) 
						png(paste(out_,"Supplementary_Figure_2c.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							mtext(expression(bold("c")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
						# predictions
							# uniparental species
							polygon(c(p_f$hour, rev(p_f$hour)), c(p_f$lwr, 
								rev(p_f$upr)), border=NA, col=adjustcolor(uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(p_f$hour, p_f$pred, col=uni_col,lwd=1)
							
							# biparental species uniparental incubation
							polygon(c(p_m$hour, rev(p_m$hour)), c(p_m$lwr, 
								rev(p_m$upr)), border=NA, col=adjustcolor(bip_uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(p_m$hour, p_m$pred, col=bip_uni_col,lwd=1)
							
							dev.off()
						}
			}	
			{# Supplementary Table 4
				m=lmer(att~sin(rad)+cos(rad) + sex +sin(rad)*sex+cos(rad)*sex+(sin(rad)+cos(rad)|act_ID)+(sin(rad)+cos(rad)|sp),data=hh, REML=FALSE)	
			 
				pred=c('Intercept (female)','Sin', 'Cos','Sex (male)','Sin x sex(male)', 'Cos x sex(male)')
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
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(l$vcov*100,2), lwr_r=NA, upr_r=NA)
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
				{# model assumptions 
						#dev.new(width=6,height=9)
						m=lmer(att~sin(rad)+cos(rad) + sex +sin(rad)*sex+cos(rad)*sex+(sin(rad)+cos(rad)|act_ID)+(sin(rad)+cos(rad)|sp),data=hh, REML=FALSE)	
						
						png(paste(out_,"model_ass/Supplementary_Table4.png", sep=""), width=6,height=9,units="in",res=600)
						par(mfrow=c(4,4))
						
						scatter.smooth(fitted(m),resid(m),col='grey');abline(h=0, lty=2, col='red')
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='grey')
						
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey') 
						qqline(resid(m))
			 
						qqnorm(unlist(ranef(m)$act_ID[1]), main = " act_ID",col='grey')
						qqline(unlist(ranef(m)$act_ID[1]))
						
						qqnorm(unlist(ranef(m)$act_ID[2]), main = " sin|act_ID",col='grey')
						qqline(unlist(ranef(m)$act_ID[2]))
						
						qqnorm(unlist(ranef(m)$act_ID[3]), main = " cos|act_ID",col='grey')
						qqline(unlist(ranef(m)$act_ID[3]))
						
						qqnorm(unlist(ranef(m)$sp[1]), main = " sp",col='grey')
						qqline(unlist(ranef(m)$sp[1]))
						
						qqnorm(unlist(ranef(m)$sp[2]), main = " sin|sp",col='grey')
						qqline(unlist(ranef(m)$sp[2]))
						
						qqnorm(unlist(ranef(m)$sp[3]), main = " cos|sp",col='grey')
						qqline(unlist(ranef(m)$sp[3]))
						
						scatter.smooth(resid(m)~sin(hh$rad),col='grey');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~cos(hh$rad),col='grey');abline(h=0, lty=2, col='red')
						#scatter.smooth(resid(m)~as.factor(h$types));abline(h=0, lty=2, col='red')
						plot(resid(m)~hh$sex);abline(h=0, lty=2, col='red')
						
						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=hh$lon, y=hh$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
								
								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					mtext("lmer(att~sin(rad)+cos(rad) + sex +sin(rad)*sex+cos(rad)*sex+(sin(rad)+cos(rad)|act_ID)+(sin(rad)+cos(rad)|sp),data=hh, REML=FALSE)", side = 3, line = -1.2, cex=0.5,outer = TRUE)	
					
					dev.off()
				}
			
		}
		
		{# Figure 3 
		  {# load metadata
			{# nests to extract the data for
				nests=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='original',colTypes = c(
									XLC$DATA_TYPE.STRING,									
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.NUMERIC,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.NUMERIC,
									XLC$DATA_TYPE.NUMERIC,
									XLC$DATA_TYPE.NUMERIC,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.NUMERIC,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING))
				nests$on=as.POSIXct(nests$on)
				nests$off=as.POSIXct(nests$off)
				nests$end=as.POSIXct(nests$end)
				nests$start=as.POSIXct(nests$start)
				nests_=nests[which(nests$circumstances!='temporal'),]
				p=readWorksheetFromFile(paste(wd,'populations.xls', sep=""), sheet='populations')
				nests_$bout=p$bout[match(paste(nests_$site,nests_$sp), paste(p$site_abbreviation, p$sp))]
			}
			{# incubation start
				 s=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='inc_start',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING))
				s$inc_start=as.POSIXct(s$inc_start,tz='UTC')			
			}
			{# incubation period
				 ip=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='incubation_period',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING
									))
			}
			{# species
					sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				}
		  }	
		  {# plots
			{# create the two actograms
				for(i in c('biparental_70','uniparental_07')){	
					nest=nests_$nest[nests_$act_ID==i]
					yr= nests_$year[nests_$act_ID==i]
					act_ID=nests_$act_ID[nests_$act_ID==i]
			
					#on_=nests_$on[nests_$act_ID=='biparental_70']
					#off_=nests_$off[nests_$act_ID=='biparental_70']
					#start_=nests_$start[nests_$act_ID=='biparental_70']
					#end_=nests_$end[nests_$act_ID=='biparental_70']
					inc_start=s$inc_start[s$nest==nest & s$year==yr]
					ip_=ip$inc_period[ip$sp==nests_$sp[nests_$act_ID==i]]
					day='TRUEcons'
					
					# get data from database
						conLite = dbConnect(dbDriver("SQLite"),dbname = db)
						dfr_=dbq(conLite,paste("SELECT*FROM", i))
						dfr_=dfr_[which(!is.na(dfr_$datetime_)),]
						dfr=dfr_
						
								# define captions and nest location
									figCap_=data.frame(sp=dfr$sp[1],scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1],stringsAsFactors=FALSE)
									figCap=figCap_
								
									latlon_=data.frame(lat=nests_$lat[nests_$nest==nest],lon=nests_$lon[nests_$nest==nest],stringsAsFactors=FALSE) # for the map
									latlon=latlon_
								
								# generate actograms
									if(i=='biparental_70'){p1=Temperature_actogram(dfr=dfr_,day='TRUEcons')}else{p2=Temperature_actogram(dfr=dfr_,day='TRUEcons')}
				}
			}
			{# paste the two actograms together	
			  {# plot within R	
				dev.new(width = 3.5, height = 3*2)
				grid.arrange(p1,p2)
				grid.text(x=0.04,y=0.99,label=expression(bold("a")), gp=gpar(col="grey50", cex=0.6,fontsize=12, fontfamily="arie"))
				grid.text(x=0.04,y=0.5,label=expression(bold("b")), gp=gpar(col="grey50", cex=0.6,fontsize=12, fontfamily="arie"))
			}
			  {# export to png	
				png(paste(out_, "Figure_5.png",sep="_"), width = 3.5, height = 3*2, units = "in", res = 300)	
					
				grid.arrange(p1,p2)
				grid.text(x=0.04,y=0.99,label=expression(bold("a")), gp=gpar(col="grey50", cex=0.6,fontsize=12, fontfamily="arie"))
				grid.text(x=0.04,y=0.5,label=expression(bold("b")), gp=gpar(col="grey50", cex=0.6,fontsize=12, fontfamily="arie"))
						
				dev.off()
			}	
			}
		  }
		}
	
		{# Supplementary Figure 3
				{# species
					sp_ =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
					#sp_$order=-sp_$order
					sp_=sp_[order(sp_$order),]
			
					h$species=sp_$species[match(h$sp,sp_$sp)]
					h$order=as.factor(sp_$order[match(h$sp,sp_$sp)])
				}	
				{# create order for plotting
					h=h[order(h$order, h$act_ID),]
						xx=data.frame(act_ID=unique(h$act_ID))
						xx$order_=1:nrow(xx)
					h$order_all=xx$order_[match(h$act_ID,xx$act_ID)]	
				}
				{# add sex
					n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
					n=n[!duplicated(n$act_ID),]
					h$sex=NA
					h$sex[h$type=='uni']=n$sex[match(h$act_ID[h$type=='uni'],n$act_ID)]
					h$sex=factor(ifelse(is.na(h$sex),'biparental', ifelse(h$sex=='m', 'uniparental male', 'uniparental female')),levels=c('biparental','uniparental female', 'uniparental male'))#'uniparental \u2640', 'uniparental \u2642'))
					
				}
				
				dev.new(width=7, height=6.5)
				ggplot(h,aes(x=hour,y=att))+
										geom_point(aes(fill=sex),shape=21, size=0.75, col='grey80')+#	#geom_point(aes(shape=sys),alpha=0.3,size=1)+
										
										stat_smooth(aes(col=order, lwd=sex),method='loess', se=FALSE)+#, lwd=0.8)+
										scale_size_manual(name='Incubation', breaks=c('biparental','uniparental female', 'uniparental male'),values=c(0.3,0.7,0.7))+										
										#scale_linetype_manual(name='Incubation type', breaks=c('biparental','uniparental female', 'uniparental male'),values=c("dashed","solid","solid"))+
										scale_fill_manual(name="Incubation",breaks=c('biparental','uniparental female', 'uniparental male'),values=alpha(c("#5eab2b","#FCB42C","#535F7C"),0.6))+
										
										scale_colour_discrete(name="Species", breaks=sp_$order[sp_$order], labels=sp_$species[sp_$order])+		
										guides(lwd = guide_legend(order = 1), fill = guide_legend(order = 1),	colour = guide_legend(order = 2))+
										facet_wrap(~order_all, ncol=8)+#, scales="free_x")+
										xlab("Time of day [hours]")+
										ylab("Nest attendance [proportion]")+
										scale_x_continuous(limits=c(-0.1,24.1),breaks=seq(0,24,by=6), labels=c('0','','12','','24'))+
										scale_y_continuous(limits=c(-0.05,1.05),breaks=seq(0,1,by=0.25), labels=c('0.0','','0.5','','1.0'))+
										theme_light()+
										theme(	
												axis.line=element_line(colour="grey70", size=0.25),
												panel.border=element_rect(colour="grey70", size=0.25),
												panel.grid = element_blank(),
								
												axis.title=element_text(size=7, colour="grey30"),
												axis.title.y = element_text(vjust=1.1),
												axis.title.x = element_text(vjust=0.2),
												axis.text=element_text(size=6),# margin=units(0.5,"mm")),
												axis.ticks.length=unit(0.5,"mm"),
												#axis.ticks.margin,
												
												strip.background = element_blank(),
												strip.text.x = element_blank(),
												#strip.background = element_blank(), 
												#strip.text = element_blank(),
												panel.margin = unit(1, "mm"),
												#legend.position="none"
												#legend.background=element_rect(colour="white"),
												legend.key=element_blank(),
												legend.key.size = unit(0.75, 'lines'),
												legend.text=element_text(size=6, colour="grey30"),
												legend.title=element_text(size=7, colour="grey30")
													)
		
				ggsave(paste(out_,"Supplementary_Figure_2.png", sep=""),width=7, height=6.5, units='in',dpi=600)						
				}
		
		
		
		
		 {# run first - prepare predictions - with att previous
							m2=lmer(att~att_previous+sin_*types+cos_*types+(sin(rad)+cos(rad)|actID_type)+(sin(rad)+cos(rad)|sp_type),data=h[!is.na(h$att_previous),], REML=FALSE)	
							#summary(m)
							#plot(allEffects(m))
						
							# simulation		
								nsim <- 5000
								bsim <- sim(m2, n.sim=nsim)  
								apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
							
							# coefficients
								v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))	
							# predicted values		
								ll=list()
								for(i in c('bip_sp_bip','bip_sp_uni','unip_sp')){
									newD=data.frame(hour=seq(0,24,0.25), att_previous=mean(h$att_previous, na.rm=TRUE))
										newD$rad=2*pi*newD$hour / 24
										newD$sin_=sin(newD$rad)
										newD$cos_=cos(newD$rad)
										newD$types=i
										ll[[i]]=newD
										}
								newD=do.call(rbind,ll)		
								
							# exactly the model which was used has to be specified here 
								X <- model.matrix(~ att_previous+sin_*types+cos_*types,data=newD)	
											
							# calculate predicted values and creditability intervals
								newD$pred <- X%*%v # #newD$fit_b <- plogis(X%*%v) # in case on binomial scaleback
										predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
										for(i in 1:nsim) predmatrix[,i] <- X%*%bsim@fixef[i,]
										newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
										newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
										#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
										#newD=newD[order(newD$t_tundra),]
								p=newD
								pu=p[p$types=='unip_sp',]
								pbb=p[p$types=='bip_sp_bip',]
								pbu=p[p$types=='bip_sp_uni',]
				}			
		  			{# (d) 
						png(paste(out_,"Figure_4d_previous.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,24), ylim=c(0,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							mtext(expression(bold("d")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
						# predictions
							# uniparental species
							polygon(c(pu$hour, rev(pu$hour)), c(pu$lwr, 
								rev(pu$upr)), border=NA, col=adjustcolor(uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pu$hour, pu$pred, col=uni_col,lwd=1)
							
							# biparental species biparental incubation
							polygon(c(pbb$hour, rev(pbb$hour)), c(pbb$lwr, 
								rev(pbb$upr)), border=NA, col=adjustcolor(bip_bip_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pbb$hour, pbb$pred, col=bip_bip_col,lwd=1)
							
							# biparental species uniparental incubation
							polygon(c(pbu$hour, rev(pbu$hour)), c(pbu$lwr, 
								rev(pbu$upr)), border=NA, col=adjustcolor(bip_uni_col ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pbu$hour, pbu$pred, col=bip_uni_col,lwd=1)
							dev.off()
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
				g$success=ifelse(g$state%in%c('s','l','h'),'yes','no')
				g$success_bin=ifelse(g$state%in%c('s','l','h'),1,0)
				{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				}
		}	
		
		{# number of nests with given state and distribution of success/failure across cay and durantion of uniparental
			nrow(g) # number of nests
			summary(factor(g$state)) # distribution of end states
			
			table(g$sp,g$success)  # distribution of successful nests
			
			unique(n_$state)
			
				densityplot(~g$prop_ip)
				densityplot(~g$prop_ip, groups=g$success, auto.key=TRUE)
				
				densityplot(~g$uni_last)
				densityplot(~g$uni_last, groups=g$success, auto.key=TRUE)
				densityplot(~log(g$uni_last))
		}
		{# Figure 4
			{# run first
				sp_=sp[!sp$sp%in%c('pesa','rnph'),]
				sp_$order=-sp_$order
				sp_=sp_[order(sp_$order),]
				
				g$species=sp_$species[match(g$sp,sp_$sp)]
				g$order=sp_$order[match(g$sp,sp_$sp)]
				g$success=factor(g$success, levels=c('yes','no'))
				counts=table(g$success,g$order)
			}	
			{# Figure 4a distribution across nests
			
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
							col=c(female_col,male_col), 
							#legend = rownames(counts),args.legend = list(bty='n', legend=c('\u2642','\u2640')),
							xaxt='n'
							)
												
						axis(1, at=seq(0,30,by=5),labels=c(0,'',10,'',20,"",30),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext('Nests\n[count] ',side=1,line=1, cex=0.6, las=1, col='grey30')
							
						text(y=23.5,x=32, labels='Failed', col=male_col, cex=0.5, pos=2)
						text(y=22.0,x=32, labels='Successful', col=female_col, cex=0.5,pos=2)
						
							#axis(2, at=seq(0,1,by=0.25), labels=c('0.0','','0.5','','1.0'))
						mtext(expression(bold("a")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)
			 dev.off()				
			}
			{# Figure 4b densityplot across day
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
					
					mtext(expression(bold("b")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
		}
			{# Figure 4c densityplot across duration
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
							#mtext("Kernel density",side=2,line=1.2, cex=0.6, las=3, col='grey30')
					lines(dens_suc,col=female_col, xpd=FALSE)
					lines(dens_fail,col=male_col,xpd=FALSE)
					points(y=jitter(points_$y),x=points_$x, pch = 21,cex=0.5, col="gray63",bg=adjustcolor(points_$col_, alpha.f = 0.6))
					#points(y=jitter(points_$y),x=points_$x, col=points_$col_, pch=19, cex=0.5)
					
					mtext(expression(bold("c")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
		}
		}
		{# Figure 5 & Supplementary Table 5 - effect of day and length of uniparental incubation
			{# run first
			
				g$prop_ip=g$prop_ip*100
			}
			{# correlation of predictors
				cor(g$uni_last,g$prop_ip,method = c("pearson"))
				cor(g$uni_last,g$prop_ip,method = c("spearman"))
				cor(log(g$uni_last),g$prop_ip,method = c("pearson"))
				cor(log(g$uni_last),g$prop_ip,method = c("spearman"))
				
				plot(g$uni_last,g$prop_ip)
				plot(log(g$uni_last),g$prop_ip)
								
				nrow(g)
			}	
			

			{# Figure 5
				{# run first - predictions
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
				{# plot
					png(paste(out_,"Figure_3.png", sep=""), width=3.5,height=1.85,units="in",res=600)
					#dev.new(width=3.5,height=1.85)
					{# a
						par(mfrow=c(1,2),mar=c(2.2,2.1,0.5,0.1),  mgp=c(1.2,0.35,0),oma = c(0, 0, 0, 1),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
					
						plot(NA, xlim=c(0,160), ylim=c(0,1), ylab='Nest success [probability]',xlab=NA, xaxt='n', type='n')#yaxt='n',
								axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
								mtext("Start of uniparental\n[% of incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
								mtext(expression(bold("a")),side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.48)
								
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
								mtext(expression(bold("b")),side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.86)
								
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
			}
			{# Supplementary Table 5
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
									{# model assumptions simple
						#dev.new(width=6,height=9)
						png(paste(out_,"model_ass/Supplementary_Table5.png", sep=""), width=6,height=9,units="in",res=600)
						m=glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')
						par(mfrow=c(5,3))
						
						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									 
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
						
						plot(fitted(m), jitter(g$success_bin, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(g$success_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(g$success_bin, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")
							
						plot(fitted(m), jitter(g$success_bin, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(g$success_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(g$success_bin, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")	
			 
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
							qqline(resid(m))
						qqnorm(unlist(ranef(m)$sp[1]), main = " sp",col='red')
						qqline(unlist(ranef(m)$sp[1]))
						
						scatter.smooth(resid(m)~g$prop_ip);abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~g$uni_last);abline(h=0, lty=2, col='red')
												
						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=g$lon, y=g$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
								
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								
								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
					mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)	
				dev.off()
				}
			
		}		
		{# hatching succes 	
				sum(g$success_bin)/nrow(g) # overall
				g$success_bin_=ifelse(g$state%in%c('s','l','h'),1,-1)
				g_=ddply(g,.(sp), summarise, fail=-sum(success_bin_[success_bin_==-1]),hatch=sum(success_bin_[success_bin_==1])) 
				
				g_$prop=g_$hatch/(g_$hatch+g_$fail)
		}		
				{# compare models - not in the MS
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
				{# Figure 5 only start <100% incubation period
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
				{# Figure 5 only SESA
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
			
  }	
  
  {# Supplementary ActogramsACTOGRAMS
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




				dsplit=split(d,d$nest) # if issues than nest has factor levels that are not present
					#x=dsplit$"S312"
				dsplit=lapply(dsplit,function(x) cbind(x,bout_lag=c(NA,x$bout_length[-nrow(x)])))
				d=do.call(rbind, dsplit)
				
				