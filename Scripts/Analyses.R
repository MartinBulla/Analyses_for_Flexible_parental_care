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
		's310' (since Jul 6 18:00)
		's815' (since Jun 30)
	# considered, but have not met UNIPARENTAL definition
		c('l301','l401','l501','s304','s902','w504')
	# we do not know  when the uniparental incubation started (incubation monitoring system was taken of), but we know from visits to the nest that the nest was uniparental before we brought the incubation monitoring system back
		"s807" #act_ID biparental_59
	
# sample sizes
	# 70 cases of uniparental incubation from 
	# 68 nests
	# 66 nests attendance excluded for nest attendance
		s704 - dislocated temperature probe
		s310 - incubation of one egg
		s815 - incubation of one egg
		
		
}

{# RUN FIRST
  {# TOOLS
	{# define time 
	  Sys.setenv(TZ="UTC")	
	}
	
	{# load packages
	   sapply(c('ggplot2', 'ggthemes','grid','gridExtra','plyr','lattice', 'latticeExtra','magrittr','matrixStats','maptools','raster', 'rgeos', 'rgdal', 'RSQLite','XLConnect','zoo'),
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
  {# LOAD DATA
		{# cases of uniparental incubation in biparental species
				n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				n$end=as.POSIXct(n$end)
				n$start=as.POSIXct(n$start)
				n=n[!n$sp%in%c('pesa','rnph'),]
					p=read.csv(paste(wd,'populations.csv', sep=""), stringsAsFactors=FALSE)
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
				n_=n[n$act_ID=='biparental_59'| n$uni_last>2*n$bout,] # act_ID biparental_59 (nest s807) we lack data for when the uniparental incubation started (incubation monitoring system was taken of), but we know from visits to the nest that the nest was uniparental before we brought the incubation monitoring system back
				n_$sex=as.factor(n_$sex)
				n_$sexsp=interaction(n_$sp,n_$sex)
				n_$uni_last=n_$uni_last/24
				n_$prop_day_end=100*(n_$day_j+n_$uni_last)/n_$inc_per_sp
				
				

			}
		{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
		}
		
		{# nest attendance data
				load(paste(wd,'for_analyses.RData',sep="")) 
					{# check whether sample sizes consistent with nest data
						dd=d[-which(d$sys=='uniparental'),]
							#length(unique(dd$act_ID))
						dn=unique(dd$act_ID)
							#length(unique(n_$act_ID))
						nn_=unique(n_$act_ID)
					
						nn_[!nn_%in%dn] # not in attandance: biparental_29 s310 (one egg incubation), biparental_48 s704 (dislocated temperature), biparental_61 s815 (one egg incubation)
						dn[!dn%in%nn_] # same
					}
					
					{# limit to periods with at least 75% of uniparental or biparental incubation
						d=d[which((d$sp=='pesa' & 0.75<(d$n/1440))| (d$sp!='pesa' & 0.75<(d$n/17280))),]
						d=ddply(d,.(act_ID,type), transform, start_j=day_j-min(day_j)+1)
						h=h[which((h$sp=='pesa' & 0.75<(h$n/60))| (h$sp!='pesa' & 0.75<(h$n/720))),]
					}
					{# add day in incubation period (julian) 
						h=ddply(h,.(act_ID,type), transform, start_j=day_j-min(day_j)+1)
						h$hour=as.numeric(h$hour)
					}		
					{# add species # PERHAPS ADD THIS ALWAYS AGAIN WHEN IT IS NEEDED
						sp_ =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
						sp_$order=-sp_$order
						sp_=sp_[order(sp_$order),]
				
						d$species=sp_$species[match(d$sp,sp_$sp)]
						d$order=as.factor(sp_$order[match(d$sp,sp_$sp)])
						
						h$species=sp_$species[match(h$sp,sp_$sp)]
						h$order=as.factor(sp_$order[match(h$sp,sp_$sp)])

						}	
					{# add lat long of the nest
						 n2 =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
						 n2=n2[!n2$circumstances=='temporal',]
						 d$lat=n2$lat[match(d$act_ID,n$act_ID)]
						 h$lat=n2$lat[match(h$act_ID,n$act_ID)]
						 d$lon=n2$lon[match(d$act_ID,n$act_ID)]
						 h$lon=n2$lon[match(h$act_ID,n$act_ID)]
			 
					}
		}
	}
}
	
{# INTRODUCTION - Table 1
	{# number of nests with uniparental incubation in biparental species	
		nrow(n_)# number of cases
		length(unique(n_$act_ID)) # overall number of nests
		summary(factor(n_$sex)) # uniparental females and males
		dd=ddply(n_,.(sp,nest), summarise,nn=1)
		ddply(dd,.(sp), summarise,nn=sum(nn)) # numer of nests per species
		
		dd=ddply(n_,.(sp,nest,sex), summarise,nn=1)
		ddply(dd[dd$sex=='m',],.(sp), summarise,nn=sum(nn)) # numer of nests incubated by male
		#ddply(dd,.(sp,sex), summarise,nn=sum(nn)) # numer of nests per species and sex
		
		# % of nests incubated by male
		m=round(100*ddply(dd[dd$sex=='m',],.(sp), summarise,nn=sum(nn))$nn/ddply(dd,.(sp), summarise,nn=sum(nn))$nn) 
		names(m)=ddply(dd,.(sp), summarise,nn=sum(nn))$sp
			m
		
		
	 } 
	{# number of successful nests
			g=n_[!n_$state%in%c('r','u','w'),]
				g$success=ifelse(g$state%in%c('s','l','h'),'yes','no')
				g$success_bin=ifelse(g$state%in%c('s','l','h'),1,0)
				g$n=1
				ddply(g,.(sp), summarise,nn=sum(n)) # nests with known outcome
				ddply(g,.(sp), summarise,nn=sum(success_bin)) # numer of successful nests per species
				ddply(g,.(sp), summarise,nn=round(100*(sum(success_bin)/sum(n)))) # % of successful from nests with known outcome
				summary(ddply(g,.(sp), summarise,nn=round(100*(sum(success_bin)/sum(n))))) # % of successful from nests with known outcome
				
	}
}

{# METHODS - Extraction of incubation behaviour - sample size
	
		{# number of cases according to desertion type
			summary(factor(n_$circumstances))
				length(n_$circumstances[n_$circumstances%in%c('bpd','unknown','temporal')])# parent naturally disappeared
				length(n_$circumstances[n_$circumstances%in%c('catching')])# parent naturally disappeared
				length(n_$circumstances[n_$circumstances=='removal'])# cases with experimental removal
		
		}
		{# number of cases and nests with uniparental incubation
				nrow(n_) # number of cases
				length(unique(n_$act_ID)) # number of nests
				
		}
		{# number of cases of daily and hourly nest attendance
			nrow(d) # cases of daily nest attendance
			nrow(h) # cases of hourly nest attendance
			
			length(unique(d$act_ID)) # number of nests
			length(unique(h$act_ID)) # number of nests
			
			length(unique(d$sp)) # number of species
			length(unique(h$sp)) # number of species
			
			length(unique(d$act_ID[d$sys=='biparental'])) # number of biparental nests
			length(unique(d$act_ID[d$sys=='uniparental'])) # number of biparental nests
				length(unique(d$act_ID[d$sp=='pesa'])) # number of pectoral sandpiper nests
				length(unique(d$act_ID[d$sp=='rnph'])) # number of red-necked phalarope nests
		}
		
}

{# RESULTS & Supplementary
  {# Abundance of uniparental incubation
	{# number of species with uniparental incubation from number of species studied
		# uniparental found in 
				length(unique(n_$sp))
						
		# studied species
				nn=read.csv(paste(wd,'populations.csv', sep=""), stringsAsFactors=FALSE)
				nn=nn[!nn$sp%in%c('rnph','pesa'),]
				length(unique(nn$sp))
				
		# % of uniparental - distribution 
			x=data.frame(n=c(21,18,13,23,183,9,44,37), p=c(48,44,31,26,20,11,5,4), stringsAsFactors=FALSE)
			weightedMedian(x$p, x$n)
			summary(x$p)
					
	}
	{# cases with female uniparental incubation
		summary(factor(n_$sex)) # uniparental females and males
		nrow(n_)# number of cases
		length(unique(n_$act_ID)) # overall number of nests
		}
	
	{# day in incubation period when uniparental incubation started
			ns_=n_[-which(n_$nest=='s807'),] # exclude because we do not know when the uniparental incubation started
			summary(100*ns_$prop_ip) # distribution of start of uniparental incubation
			ns_[100*ns_$prop_ip>100,]
			nrow(ns_) # number of cases
			length(unique(ns_$nest)) # number of nests
			
			densityplot(~ns_$prop_ip*100)
			
			# % of nests that started before estimated hatching
			nrow(ns_[which(ns_$prop_ip<1),])/nrow(ns_)
			
			# limited to those before the eggs were supposed to hatch
			summary(ns_$prop_ip[ns_$prop_ip<1])
			densityplot(~ns_$prop_ip[ns_$prop_ip<1]*100)
			length(unique(ns_$nest[ns_$prop_ip<1])) # number of nests
			length(ns_$prop_ip[ns_$prop_ip<1]) # number of cases
			length(unique(ns_$sp)) # number of species
			
			# sex specific

				m=lmer(prop_ip~sex+(1|sp),ns_)
								nsim <- 5000
								bsim <- sim(m, n.sim=nsim)  
								apply(bsim@fixef, 2, quantile, prob=c(0.5, 0.025,0.975))*100/0.7035061 # output is in %
								
								l=data.frame(summary(m)$varcor)
								l=l[is.na(l$var2),]
								ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(l$vcov*100/sum(l$vcov),1), lwr_r=NA, upr_r=NA)
								ri
					
				plot(allEffects(m))
				summary(m)
				ggplot(ns_, aes(x=sp, y=prop_ip, col=sex))+geom_point()+geom_boxplot()
										
		}
	{# for how long the uniparental incubation lasted
			ns_=n_[-which(n_$nest=='s807'),] # exclude because we do not know when the uniparental incubation started
			summary(ns_$uni_last)
			ns_[ns_$uni_last>10,]
			densityplot(~ns_$uni_last)
			
			m=lmer(uni_last~sex+(1|sp),ns_)
								nsim <- 5000
								bsim <- sim(m, n.sim=nsim)  
								apply(bsim@fixef, 2, quantile, prob=c(0.5, 0.025,0.975)) # output is in %
								
								l=data.frame(summary(m)$varcor)
								l=l[is.na(l$var2),]
								ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(l$vcov*100/sum(l$vcov),1), lwr_r=NA, upr_r=NA)
								ri
								
				plot(allEffects(m))
				summary(m)
				ggplot(ns_, aes(x=sp, y=uni_last, col=sex))+geom_point()+geom_boxplot()
		}
		{# nests left because field work ended = 10
			length(n_$nest[n_$state=='w'])+ length(n_$nest[n_$state=='u'])-3+1 # 3 nests with uncertainty was about hatching, desertion or depredation
																			   # 1 nest with thee system taken off for over 10 days and then placed back again
		}
	
	{# Figure 1a - lines
		ns_=n_[-which(n_$nest=='s807'),] # exclude because we do not know when the uniparental incubation started
		sp_=sp[!sp$sp%in%c('pesa','rnph'),]
		sp_$order=-sp_$order
		sp_=sp_[order(sp_$order),]
		
		ns_$species=sp_$species[match(ns_$sp,sp_$sp)]
		ns_$order=sp_$order[match(ns_$sp,sp_$sp)]
		ns_$sex=factor(ns_$sex, levels=c('m','f'))
		counts=table(ns_$sex,ns_$order)
		
		# par(mfrow=c(1,3),mar=c(0.0,0,0,0.4),oma = c(1.8, 1.8, 0.2, 0.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
		 #dev.new(width=3.5*0.75,height=1.85)
		 png(paste(out_,"Figure_1a_lines.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
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
					
					lines(x=c(0,30), y=c(3.5,3.5), col="grey90")
					lines(x=c(0,30), y=c(3.5,3.5)+3, col="grey90")
					lines(x=c(0,30), y=c(3.5,3.5)+3*2, col="grey90")
					lines(x=c(0,30), y=c(3.5,3.5)+3*3, col="grey90")
					lines(x=c(0,30), y=c(3.5,3.5)+3*4, col="grey90")
					lines(x=c(0,30), y=c(3.5,3.5)+3*5, col="grey90")
					lines(x=c(0,30), y=c(3.5,3.5)+3*6, col="grey90")
		 dev.off()				
		}
	{# Figure 1bc - lines
		 {# run first	
			ns_=n_[-which(n_$nest=='s807'),] # exclude because we do not know when the uniparental incubation started
			ns_$uni_last=as.numeric(difftime(ns_$end, ns_$start, units='days'))
			# create dummy values for species missing nests for one sex
				n1=ns_[ns_$sp=='blgo',][1,]
				n1$sex='f'
				n1$prop_ip=-50
				n1$uni_last=-2
				n2=ns_[ns_$sp=='dunl',][1,]
				n2$sex='f'
				n2$prop_ip=-50
				n2$uni_last=-2
				n3=ns_[ns_$sp=='lbdo',][1,]
				n3$sex='f'
				n3$prop_ip=-50
				n3$uni_last=-2
				n4=ns_[ns_$sp=='reds',][1,]
				n4$sex='f'
				n4$prop_ip=-50
				n4$uni_last=-2
				ns_=rbind(ns_,n1,n2,n3,n4)
				
		sp_=sp[!is.na(sp$order),]
		sp_$order=-sp_$order
		sp_=sp_[order(sp_$order),]
		
		ns_$species=sp_$species[match(ns_$sp,sp_$sp)]
		ns_$order=sp_$order[match(ns_$sp,sp_$sp)]
		ns_$sex=factor(ns_$sex, levels=c('m','f'))
			ns_$order=ifelse(ns_$sex=='m', 2*ns_$order-0.4, 2*ns_$order+0.4)
			unique(ns_$order)[order(unique(ns_$order))]
			ns_$prop_ip_=ns_$prop_ip*100
		ns_$col_=ifelse(ns_$sex=='f',female_col,male_col)
		table(ns_$sex,ns_$sp)
		
		ns_$order=jitter(ns_$order)
		
		
					
		
			}
		 {# Figure 1b points	
		   #dev.new(width=3.5*0.75,height=1.85)
		    png(paste(out_,"Figure_1b+est_lines.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			#ggplot(ns_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(ns_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = positions_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			plot(ns_$order~ns_$prop_ip_,xlim=c(0,160), ylim=c(-16.5,-1.4), 
						xaxt='n',yaxt='n',type='n',
						#xlab="Cases of uniparental incubation [count]", 
						pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6)
							)
									
					axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext("Start of uniparental incubation\n[% of species' incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
					
					#axis(2, at=seq(-16,-2,2), labels=FALSE)
					abline(h=seq(-16,-4,2)+1, par(xpd=FALSE), col="grey90")
					points(jitter(ns_$order)~ns_$prop_ip_,pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6))
					#mtext('Nest attendance',side=2,line=1, cex=0.6, las=3, col='grey30')
					#text(y=23,x=28, labels='\u2640', col='#FCB42C', cex=0.6)
					#text(y=23.5,x=30, labels='\u2642', col='#535F7C', cex=0.6)
					mtext(expression(bold("b")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)
					# add species specific estimates for sex differences
								ns__=ns_
								ns__$sex=factor(ns__$sex, levels=c('f','m'))
								xx=data.frame(li=c(-2, -10,-14,-16), sp=c('amgp','basa','wesa','sesa'), stringsAsFactors=FALSE)
								for(i in 1:nrow(xx)){
									xi=xx[i,]
									m=lm(prop_ip~sex,ns__[ns__$sp==xi$sp,])
										nsim <- 5000
										bsim <- sim(m, n.sim=nsim)  
										pp=data.frame(round(100*apply(bsim@coef, 2, quantile, prob=c(0.5, 0.025,0.975))/apply(bsim@coef, 2, quantile, prob=c(0.5, 0.025,0.975))[1,1]))# output is in %
										if(i==1){
										text(y=xi$li+0.3,x=167, labels=paste(pp$sexm[1]," (",pp$sexm[2],"-",pp$sexm[3],")", sep=""), col='grey30', cex=0.4, pos=2, offset=0)}else{
										text(y=xi$li+0.3,x=167, labels=paste(pp$sexm[1]," (",pp$sexm[2],"-",pp$sexm[3],")", sep=""), col='grey30', cex=0.4, pos=2, offset=0)}
									}
								
					dev.off()				
		}
		 {# Figure 1c points	
		   #dev.new(width=3.5*0.75,height=1.85)
		    png(paste(out_,"Figure_1c+est_lines.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			#ggplot(ns_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(ns_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = positions_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			plot(ns_$order~ns_$uni_last,xlim=c(0,20), ylim=c(-16.5,-1.4), 
						xaxt='n',yaxt='n',type='n',
						#xlab="Cases of uniparental incubation [count]", 
						pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6)
							)
									
						axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext("Duration of uniparental incubation\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
					
					#axis(2, at=seq(-16,-2,2), labels=FALSE)
					abline(h=seq(-16,-4,2)+1, par(xpd=FALSE), col="grey90")
					points(jitter(ns_$order)~ns_$uni_last,pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6))
						#mtext('Nest attendance',side=2,line=1, cex=0.6, las=3, col='grey30')
					#text(y=23,x=28, labels='\u2640', col='#FCB42C', cex=0.6)
					#text(y=23.5,x=30, labels='\u2642', col='#535F7C', cex=0.6)
					mtext(expression(bold("c")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)	
								ns__=ns_
								ns__$sex=factor(ns__$sex, levels=c('f','m'))
								xx=data.frame(li=c(-2, -10,-14,-16), sp=c('amgp','basa','wesa','sesa'), stringsAsFactors=FALSE)
								for(i in 1:nrow(xx)){
									xi=xx[i,]
									m=lm(uni_last~sex,ns__[ns__$sp==xi$sp,])
										nsim <- 5000
										bsim <- sim(m, n.sim=nsim)  
										pp=data.frame(round(apply(bsim@coef, 2, quantile, prob=c(0.5, 0.025,0.975)),1))# output is in %
										text(y=xi$li+0.3,x=20.9, labels=paste(pp$sexm[1]," (",pp$sexm[2],"-",pp$sexm[3],")", sep=""), col='grey30', cex=0.4, pos=2, offset=0)
									}
					arrows(y0=ns_$order[round(ns_$uni_last,2)==round(18.669045,2)], x0= 18.669045+1.5,x1=18.669045+0.5, length = 0.02, angle = 15, col="#5eab2b", lwd=1.5)
					arrows(y0=ns_$order[round(ns_$uni_last,2)==round(15.373264,2)],x0=15.373264+1.5,  x1=15.373264+0.5, length = 0.02, angle = 15, col="#5eab2b", lwd=1.5)
      
		 dev.off()				
		}
			{# not used Figure 1b points and boxplot
				
		ns_$species=sp_$species[match(ns_$sp,sp_$sp)]
		ns_$order=sp_$order[match(ns_$sp,sp_$sp)]
		ns_$sex=factor(ns_$sex, levels=c('m','f'))
		ns_$col_=ifelse(ns_$sex=='f',female_col,male_col)
		table(ns_$sex,ns_$sp)
					
		 #dev.new(width=3.5*0.75,height=1.85)
		 png(paste(out_,"Figure_1b_boxplot.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
		 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			at_=c(seq(1,15,by=2)+0.15, seq(2,16,2)-0.15)
			at_=at_[order(at_)]
			#ggplot(ns_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(ns_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = positions_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			boxplot(ns_$prop_ip*100~ns_$sex*ns_$order, horizontal=TRUE,
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
			stripchart(ns_$prop_ip*100~ns_$sex*ns_$order, vertical = FALSE, method = "jitter",jitter=0.05, add = TRUE, 
										at=at_,
										pch = 21,cex=0.5, 
										col="gray63",
										#bg=adjustcolor(c(male_col,female_col), alpha.f = 0.4)
										#bg=c(male_col,female_col)
										bg=adjustcolor("gray63", alpha.f = 0.4)
										)
										
			boxplot(ns_$prop_ip*100~ns_$sex*ns_$order,horizontal=TRUE,
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
	
	{# Figure 1a
		ns_=n_[-which(n_$nest=='s807'),] # exclude because we do not know when the uniparental incubation started
		sp_=sp[!sp$sp%in%c('pesa','rnph'),]
		sp_$order=-sp_$order
		sp_=sp_[order(sp_$order),]
		
		ns_$species=sp_$species[match(ns_$sp,sp_$sp)]
		ns_$order=sp_$order[match(ns_$sp,sp_$sp)]
		ns_$sex=factor(ns_$sex, levels=c('m','f'))
		counts=table(ns_$sex,ns_$order)
		
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
			ns_=n_[-which(n_$nest=='s807'),] # exclude because we do not know when the uniparental incubation started
			ns_$uni_last=as.numeric(difftime(ns_$end, ns_$start, units='days'))
			# create dummy values for species missing nests for one sex
				n1=ns_[ns_$sp=='blgo',][1,]
				n1$sex='f'
				n1$prop_ip=-50
				n1$uni_last=-2
				n2=ns_[ns_$sp=='dunl',][1,]
				n2$sex='f'
				n2$prop_ip=-50
				n2$uni_last=-2
				n3=ns_[ns_$sp=='lbdo',][1,]
				n3$sex='f'
				n3$prop_ip=-50
				n3$uni_last=-2
				n4=ns_[ns_$sp=='reds',][1,]
				n4$sex='f'
				n4$prop_ip=-50
				n4$uni_last=-2
				ns_=rbind(ns_,n1,n2,n3,n4)
				
		sp_=sp[!is.na(sp$order),]
		sp_$order=-sp_$order
		sp_=sp_[order(sp_$order),]
		
		ns_$species=sp_$species[match(ns_$sp,sp_$sp)]
		ns_$order=sp_$order[match(ns_$sp,sp_$sp)]
		ns_$sex=factor(ns_$sex, levels=c('m','f'))
			ns_$order=ifelse(ns_$sex=='m', 2*ns_$order-0.4, 2*ns_$order+0.4)
			unique(ns_$order)[order(unique(ns_$order))]
			ns_$prop_ip_=ns_$prop_ip*100
		ns_$col_=ifelse(ns_$sex=='f',female_col,male_col)
		table(ns_$sex,ns_$sp)
		
		ns_$order=jitter(ns_$order)
		
		
					
		
			}
		 {# Figure 1b points	
		   #dev.new(width=3.5*0.75,height=1.85)
		    png(paste(out_,"Figure_1b+est.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			#ggplot(ns_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(ns_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = positions_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			plot(ns_$order~ns_$prop_ip_,xlim=c(0,160), ylim=c(-16.5,-1), 
						xaxt='n',yaxt='n',type='n',
						#xlab="Cases of uniparental incubation [count]", 
						pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6)
							)
									
					axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext("Start of uniparental incubation\n[% of species' incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
					
					#axis(2, at=seq(-16,-2,2), labels=FALSE)
					abline(h=seq(-16,-2,2), par(xpd=FALSE), col="grey90")
					points(jitter(ns_$order)~ns_$prop_ip_,pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6))
					#mtext('Nest attendance',side=2,line=1, cex=0.6, las=3, col='grey30')
					#text(y=23,x=28, labels='\u2640', col='#FCB42C', cex=0.6)
					#text(y=23.5,x=30, labels='\u2642', col='#535F7C', cex=0.6)
					mtext(expression(bold("b")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)
					# add species specific estimates for sex differences
								ns__=ns_
								ns__$sex=factor(ns__$sex, levels=c('f','m'))
								xx=data.frame(li=c(-2, -10,-14,-16), sp=c('amgp','basa','wesa','sesa'), stringsAsFactors=FALSE)
								for(i in 1:nrow(xx)){
									xi=xx[i,]
									m=lm(prop_ip~sex,ns__[ns__$sp==xi$sp,])
										nsim <- 5000
										bsim <- sim(m, n.sim=nsim)  
										pp=data.frame(round(100*apply(bsim@coef, 2, quantile, prob=c(0.5, 0.025,0.975))/apply(bsim@coef, 2, quantile, prob=c(0.5, 0.025,0.975))[1,1]))# output is in %
										if(i==1){
										text(y=xi$li+0.5,x=167, labels=paste(pp$sexm[1]," (",pp$sexm[2],"-",pp$sexm[3],")", sep=""), col='grey30', cex=0.4, pos=2, offset=0)}else{
										text(y=xi$li+0.5,x=167, labels=paste(pp$sexm[1]," (",pp$sexm[2],"-",pp$sexm[3],")", sep=""), col='grey30', cex=0.4, pos=2, offset=0)}
									}
								
					dev.off()				
		}
		 {# Figure 1c points	
		   #dev.new(width=3.5*0.75,height=1.85)
		    png(paste(out_,"Figure_1c+est.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
			par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			#ggplot(ns_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(ns_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = positions_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			plot(ns_$order~ns_$uni_last,xlim=c(0,20), ylim=c(-16.5,-1), 
						xaxt='n',yaxt='n',type='n',
						#xlab="Cases of uniparental incubation [count]", 
						pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6)
							)
									
						axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
						mtext("Duration of uniparental incubation\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
					
					#axis(2, at=seq(-16,-2,2), labels=FALSE)
					abline(h=seq(-16,-2,2), par(xpd=FALSE), col="grey90")
					points(jitter(ns_$order)~ns_$uni_last,pch = 21,cex=0.5, col="gray63",bg=adjustcolor(ns_$col_, alpha.f = 0.6))
						#mtext('Nest attendance',side=2,line=1, cex=0.6, las=3, col='grey30')
					#text(y=23,x=28, labels='\u2640', col='#FCB42C', cex=0.6)
					#text(y=23.5,x=30, labels='\u2642', col='#535F7C', cex=0.6)
					mtext(expression(bold("c")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.95)	
								ns__=ns_
								ns__$sex=factor(ns__$sex, levels=c('f','m'))
								xx=data.frame(li=c(-2, -10,-14,-16), sp=c('amgp','basa','wesa','sesa'), stringsAsFactors=FALSE)
								for(i in 1:nrow(xx)){
									xi=xx[i,]
									m=lm(uni_last~sex,ns__[ns__$sp==xi$sp,])
										nsim <- 5000
										bsim <- sim(m, n.sim=nsim)  
										pp=data.frame(round(apply(bsim@coef, 2, quantile, prob=c(0.5, 0.025,0.975)),1))# output is in %
										text(y=xi$li+0.5,x=20.9, labels=paste(pp$sexm[1]," (",pp$sexm[2],"-",pp$sexm[3],")", sep=""), col='grey30', cex=0.4, pos=2, offset=0)
									}
					arrows(y0=ns_$order[round(ns_$uni_last,2)==round(18.669045,2)], x0= 18.669045+1.5,x1=18.669045+0.5, length = 0.02, angle = 15, col="#5eab2b", lwd=1.5)
					arrows(y0=ns_$order[round(ns_$uni_last,2)==round(15.373264,2)],x0=15.373264+1.5,  x1=15.373264+0.5, length = 0.02, angle = 15, col="#5eab2b", lwd=1.5)
      
		 dev.off()				
		}
			{# not used Figure 1b points and boxplot
				
		ns_$species=sp_$species[match(ns_$sp,sp_$sp)]
		ns_$order=sp_$order[match(ns_$sp,sp_$sp)]
		ns_$sex=factor(ns_$sex, levels=c('m','f'))
		ns_$col_=ifelse(ns_$sex=='f',female_col,male_col)
		table(ns_$sex,ns_$sp)
					
		 #dev.new(width=3.5*0.75,height=1.85)
		 png(paste(out_,"Figure_1b_boxplot.png", sep=""), width=3.5*0.75,height=1.85,units="in",res=600)
		 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0.5, 0.2, 5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)
			at_=c(seq(1,15,by=2)+0.15, seq(2,16,2)-0.15)
			at_=at_[order(at_)]
			#ggplot(ns_,aes(x=species, y=prop_ip*100, fill=sex))+geom_boxplot() +coord_flip()+coord_cartesian(ylim = c(0, 160))
			#ggplot(ns_,aes(x=species, y=prop_ip*100, col=sex))+geom_point(position = positions_jitter(w = 0.3, h = 0.3)) +coord_flip()+coord_cartesian(ylim = c(0, 160))
			boxplot(ns_$prop_ip*100~ns_$sex*ns_$order, horizontal=TRUE,
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
			stripchart(ns_$prop_ip*100~ns_$sex*ns_$order, vertical = FALSE, method = "jitter",jitter=0.05, add = TRUE, 
										at=at_,
										pch = 21,cex=0.5, 
										col="gray63",
										#bg=adjustcolor(c(male_col,female_col), alpha.f = 0.4)
										#bg=c(male_col,female_col)
										bg=adjustcolor("gray63", alpha.f = 0.4)
										)
										
			boxplot(ns_$prop_ip*100~ns_$sex*ns_$order,horizontal=TRUE,
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
	{# Supplementary Table 1 - uniparental incubation according to sex and circumstances of disertion
		table(n_$sex,n_$circumstances)
	}
  }

  {# Change in nest attendance
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
					png(paste(out_,"Figure_2a.png", sep=""), width=0.51+3.5*0.5,height=1.85,units="in",res=600)
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
						mtext("Daily nest attendance\n[proportion]",side=1,line=1, cex=0.6, las=1, col='grey30')
						
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
		
		{# Figure 2b and Supplementary Table 2
			{# run first
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
					png(paste(out_,"Figure_2b.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,1.6), ylim=c(0,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,1.6,by=0.4),labels=seq(0,1.6,by=0.4),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Species' incubation period\n[proportion]",side=1,line=1, cex=0.6, las=1, col='grey30')
							
							axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("b")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							#lines(c(0,0),c(0,16.5), lty=3, col="red")							
						# data
							points(d$att~d$prop_ip, col=adjustcolor(d$cols, alpha.f = 0.3), pch=20, cex=0.3)	
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
			
			{# Supplementary Table 1
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
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
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
		{# Supplementary Figure 2a, Supplementary Table 3
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
							points(dd$att~dd$prop_ip, col=adjustcolor(dd$cols, alpha.f = 0.3), pch=20, cex=0.3)	
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
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
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
		
		{# Figure 2c-d &  Supplementary Table 4
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
						png(paste(out_,"Figure_2c.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
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
						png(paste(out_,"Figure_2d.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
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
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
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
		{# Supplementary Figure 2b-c, Supplementary Table 5
			{# run first - prepare data	
				# limited to species with uniparental data for both sexes	
				hh=h[h$sys=='biparental' & h$type=='uni' & h$sp%in%c('amgp','basa','sesa','wesa'),]
					nrow(hh) # N 5784
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
							m=lmer(att~sin_*sex+cos_*sex+(sin_+cos_|act_ID)+(sin_+cos_|sp),data=hh, REML=FALSE) # no worries about convergence as the same model below runs well and gives same results
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
			
			gg_color_hue <- function(n) {
				  hues = seq(15, 375, length = n + 1)
				  hcl(h = hues, l = 65, c = 100)[1:n]
					}
			n = 10
			cols = gg_color_hue(n)[c(1,5,7,9)]
			
			dev.new(width = 4, height = 4)
			plot(1:length(cols), pch = 16, cex = 2, col = cols)		
			
					#dev.new(width=3.5*0.5,height=1.85)
					{# (b) raw data
						png(paste(out_,"Supplementary_Figure_2b_loess.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						
						par(mar=c(0.0,0.4,0,0),oma = c(2.1, 2.3, 0.2, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
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
							
								s= data.frame(sp=rep(c("amgp", "basa" ,"sesa", "wesa"),2), sex=c(rep('f',4), rep('m',4)), 
											  col_=rep(cols,2), #rep(c("purple", "pink" ,"deepskyblue", "deepblue"),2), 
											  lwd_=c(rep(1,4), rep(2,4)), stringsAsFactors=FALSE)
								
								for( i in 1:nrow(s)){	
													ha=hh[hh$sp==s$sp[i] & hh$sex==s$sex[i],]
													xx=loess(att~hour,ha)
													j=order(ha$hour)
													lines(ha$hour[j], xx$fitted[j], col=s$col_[i], lwd=s$lwd_[i])
												}	
							
								text(x=-2+k, y=0.05, pos=4, expression(italic('N')*' cases:'),cex=0.5,las=1,col='grey30') 
								
								symbols(c(10,15,20),c(0.105,0.105,0.105)+k,circles=sqrt(c(10,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(10,15,20),c(0.007,0.007,0.007)+k,labels=c(10,100,150), xpd=TRUE, cex=0.5,col='grey30') 
								
								mtext(side=2, 'American\ngolden\nplover',cex=0.4,las=1,col=cols[1], line=0.3,at=0.9) 
								mtext(side=2, "Baird's\nsandpiper",cex=0.4,las=1,col=cols[2], line=0.3,at=0.7) 
								mtext(side=2, "Western\nsandpiper",cex=0.4,las=1,col=cols[3], line=0.3,at=0.54) 
								mtext(side=2, "Semipalmated\nsandpiper",cex=0.4,las=1,col=cols[4], line=0.3,at=0.36) 
								
								lines(x=c(-10,-6)+3.5, y=c(0.15,0.15)+k, xpd=NA,lwd=2,col=col_p)
								lines(x=c(-10,-6)+3.5, y=c(0.07,0.07)+k, xpd=NA,lwd=1,col=col_p)
								symbols(x=c(-8,-8)+3.5,c(0.15,0.07)+k,circles=c(0.1,0.1),bg="white", fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								symbols(x=c(-8,-8)+3.5,c(0.15,0.07)+k,circles=c(0.1,0.1),bg=c("#535F7C33", "#FCB42C33"), fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								text(x=c(-7.9,-8), y=c(0.17,0.07)+k, labels=c('\u2642','\u2640'),xpd=NA,cex=0.6,col=c('#535F7C','#FCB42C'))
								
								
								#text(c(-2,-2,-2,-2),c(1,0.6,0.4,0.2),labels=c('American\ngolden\nplover',"Baird's\nsandpiper", "Western\nsandpiper","Semipalmated\nsandpiper"),xpd=TRUE, cex=0.4,col=cols, pos=3, outer=TRUE) 
								
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
			{# Supplementary Figure 2b-c - changed order
			
			gg_color_hue <- function(n) {
				  hues = seq(15, 375, length = n + 1)
				  hcl(h = hues, l = 65, c = 100)[1:n]
					}
			n = 10
			cols = gg_color_hue(n)[c(1,5,7,9)]
			
			dev.new(width = 4, height = 4)
			plot(1:length(cols), pch = 16, cex = 2, col = cols)		
			
					#dev.new(width=3.5*0.5,height=1.85)
					{# (c) raw data
						png(paste(out_,"Supplementary_Figure_2c_loess_left.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0, 0.2, 2.3),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
												
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("c")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							lines(c(0,24),c(0.19,0.19)+k,, lty=3, col="grey80")							
						# data
							# aggregate per hour, species and type of incubation		
								hh$nn=1
								gg=ddply(hh,.(sp,cols, sex, hour), summarise,mean_=mean(att),se_=sd(att)/sqrt(length(att)),n=sum(nn))
								gg=gg[order(gg$sex),]							
							#arrows(x0=gg$hour, y0=gg$mean_-gg$se_,x1=gg$hour,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							gf=gg[gg$sex=='f',]
							symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE, fg=col_p) 
							symbols(gf$hour,gf$mean_, circles=sqrt(gf$n/pi),inches=0.14/1.75,bg=adjustcolor(gf$cols,alpha.f = 0.2),add=TRUE, fg=col_p) #bg=alpha(col_p,0.1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,fg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE), fg=col_p) #bg=alpha(col_p,0.1)
								
														#points(h$att~h$hour, col=adjustcolor(h$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
							
								s= data.frame(sp=rep(c("amgp", "basa" ,"sesa", "wesa"),2), sex=c(rep('f',4), rep('m',4)), 
											  col_=rep(cols,2), #rep(c("purple", "pink" ,"deepskyblue", "deepblue"),2), 
											  lwd_=c(rep(1,4), rep(2,4)), stringsAsFactors=FALSE)
								
								for( i in 1:nrow(s)){	
													ha=hh[hh$sp==s$sp[i] & hh$sex==s$sex[i],]
													xx=loess(att~hour,ha)
													j=order(ha$hour)
													lines(ha$hour[j], xx$fitted[j], col=s$col_[i], lwd=s$lwd_[i])
												}	
							
								text(x=-2+k, y=0.05, pos=4, expression(italic('N')*' cases:'),cex=0.5,las=1,col='grey30') 
								
								symbols(c(10,15,20),c(0.105,0.105,0.105)+k,circles=sqrt(c(10,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(10,15,20),c(0.007,0.007,0.007)+k,labels=c(10,100,150), xpd=TRUE, cex=0.5,col='grey30') 
								
								mtext(side=4, 'American\ngolden\nplover',cex=0.4,las=1,col=cols[1], line=0.3,at=0.9) 
								mtext(side=4, "Baird's\nsandpiper",cex=0.4,las=1,col=cols[2], line=0.3,at=0.7) 
								mtext(side=4, "Western\nsandpiper",cex=0.4,las=1,col=cols[3], line=0.3,at=0.54) 
								mtext(side=4, "Semipalmated\nsandpiper",cex=0.4,las=1,col=cols[4], line=0.3,at=0.36) 
								
								lines(x=c(-10,-6)-3.5+40, y=c(0.15,0.15)+k, xpd=NA,lwd=2,col=col_p)
								lines(x=c(-10,-6)-3.5+40, y=c(0.07,0.07)+k, xpd=NA,lwd=1,col=col_p)
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg="white", fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg=c("#535F7C33", "#FCB42C33"), fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								text(x=c(-7.9,-8)+40, y=c(0.17,0.07)+k, labels=c('\u2642','\u2640'),xpd=NA,cex=0.6,col=c('#535F7C','#FCB42C'))
								
								
								#text(c(-2,-2,-2,-2),c(1,0.6,0.4,0.2),labels=c('American\ngolden\nplover',"Baird's\nsandpiper", "Western\nsandpiper","Semipalmated\nsandpiper"),xpd=TRUE, cex=0.4,col=cols, pos=3, outer=TRUE) 
								
						dev.off()															
						
						}
					{# (cf) raw data
						png(paste(out_,"Supplementary_Figure_2c_loess_left_female.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0, 0.2, 2.3),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
												
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("c")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							lines(c(0,24),c(0.19,0.19)+k,, lty=3, col="grey80")							
						# data
							# aggregate per hour, species and type of incubation		
								hh$nn=1
								gg=ddply(hh,.(sp,cols, sex, hour), summarise,mean_=mean(att),se_=sd(att)/sqrt(length(att)),n=sum(nn))
								gg=gg[order(gg$sex),]							
							#arrows(x0=gg$hour, y0=gg$mean_-gg$se_,x1=gg$hour,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							gf=gg[gg$sex=='f',]
							gx=gg[gg$n==max(gg$n),]
							gx$mean_=-1
							gf=rbind(gf,gx)

							symbols(gf$hour,gf$mean_, circles=sqrt(gf$n/pi),inches=0.14/1.75,bg=adjustcolor(gf$cols,alpha.f = 0.2),add=TRUE, fg=col_p) #bg=alpha(col_p,0.1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,fg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE), fg=col_p) #bg=alpha(col_p,0.1)
								
														#points(h$att~h$hour, col=adjustcolor(h$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
							
								s= data.frame(sp=c("amgp", "basa" ,"sesa", "wesa"), sex=rep('f',4), 
											  col_=cols, #rep(c("purple", "pink" ,"deepskyblue", "deepblue"),2), 
											  lwd_=rep(1,4), stringsAsFactors=FALSE)
								
								hf=hh[hh$sex=='f',]
								for( i in 1:nrow(s)){	
													ha=hf[hf$sp==s$sp[i] & hf$sex==s$sex[i],]
													xx=loess(att~hour,ha)
													j=order(ha$hour)
													lines(ha$hour[j], xx$fitted[j], col=s$col_[i], lwd=s$lwd_[i])
												}	
							
								text(x=-2+k, y=0.05, pos=4, expression(italic('N')*' cases:'),cex=0.5,las=1,col='grey30') 
								
								symbols(c(10,15,20),c(0.105,0.105,0.105)+k,circles=sqrt(c(10,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(10,15,20),c(0.007,0.007,0.007)+k,labels=c(10,100,150), xpd=TRUE, cex=0.5,col='grey30') 
								
								mtext(side=4, 'American\ngolden\nplover',cex=0.4,las=1,col=cols[1], line=0.3,at=0.9) 
								mtext(side=4, "Baird's\nsandpiper",cex=0.4,las=1,col=cols[2], line=0.3,at=0.7) 
								mtext(side=4, "Western\nsandpiper",cex=0.4,las=1,col=cols[3], line=0.3,at=0.54) 
								mtext(side=4, "Semipalmated\nsandpiper",cex=0.4,las=1,col=cols[4], line=0.3,at=0.36) 
								
								lines(x=c(-10,-6)-3.5+40, y=c(0.15,0.15)+k, xpd=NA,lwd=2,col=col_p)
								lines(x=c(-10,-6)-3.5+40, y=c(0.07,0.07)+k, xpd=NA,lwd=1,col=col_p)
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg="white", fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg=c("#535F7C33", "#FCB42C33"), fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								text(x=c(-7.9,-8)+40, y=c(0.17,0.07)+k, labels=c('\u2642','\u2640'),xpd=NA,cex=0.6,col=c('#535F7C','#FCB42C'))
								
								
								#text(c(-2,-2,-2,-2),c(1,0.6,0.4,0.2),labels=c('American\ngolden\nplover',"Baird's\nsandpiper", "Western\nsandpiper","Semipalmated\nsandpiper"),xpd=TRUE, cex=0.4,col=cols, pos=3, outer=TRUE) 
								
						dev.off()															
						
						}
					{# (dm) raw data
						png(paste(out_,"Supplementary_Figure_2d_loess_left_male.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0, 0.2, 2.3),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
												
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("d")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
							lines(c(0,24),c(0.19,0.19)+k,, lty=3, col="grey80")							
						# data
							# aggregate per hour, species and type of incubation		
								hh$nn=1
								gg=ddply(hh,.(sp,cols, sex, hour), summarise,mean_=mean(att),se_=sd(att)/sqrt(length(att)),n=sum(nn))
								gg=gg[order(gg$sex),]							
							#arrows(x0=gg$hour, y0=gg$mean_-gg$se_,x1=gg$hour,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							gf=gg[gg$sex=='m',]
							
							symbols(gf$hour,gf$mean_, circles=sqrt(gf$n/pi),inches=0.14/1.75,bg=adjustcolor(gf$cols,alpha.f = 0.2),add=TRUE, fg=col_p) #bg=alpha(col_p,0.1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,fg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE), fg=col_p) #bg=alpha(col_p,0.1)
								
														#points(h$att~h$hour, col=adjustcolor(h$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
							
								s= data.frame(sp=c("amgp", "basa" ,"sesa", "wesa"), sex=rep('m',4), 
											  col_=cols, #rep(c("purple", "pink" ,"deepskyblue", "deepblue"),2), 
											  lwd_=rep(2,4), stringsAsFactors=FALSE)
								
								hf=hh[hh$sex=='m',]
								for( i in 1:nrow(s)){	
													ha=hf[hf$sp==s$sp[i] & hf$sex==s$sex[i],]
													xx=loess(att~hour,ha)
													j=order(ha$hour)
													lines(ha$hour[j], xx$fitted[j], col=s$col_[i], lwd=s$lwd_[i])
												}	
							
								text(x=-2+k, y=0.05, pos=4, expression(italic('N')*' cases:'),cex=0.5,las=1,col='grey30') 
								
								symbols(c(10,15,20),c(0.105,0.105,0.105)+k,circles=sqrt(c(10,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(10,15,20),c(0.007,0.007,0.007)+k,labels=c(10,100,150), xpd=TRUE, cex=0.5,col='grey30') 
								
								mtext(side=4, 'American\ngolden\nplover',cex=0.4,las=1,col=cols[1], line=0.3,at=0.9) 
								mtext(side=4, "Baird's\nsandpiper",cex=0.4,las=1,col=cols[2], line=0.3,at=0.7) 
								mtext(side=4, "Western\nsandpiper",cex=0.4,las=1,col=cols[3], line=0.3,at=0.54) 
								mtext(side=4, "Semipalmated\nsandpiper",cex=0.4,las=1,col=cols[4], line=0.3,at=0.36) 
								
								lines(x=c(-10,-6)-3.5+40, y=c(0.15,0.15)+k, xpd=NA,lwd=2,col=col_p)
								lines(x=c(-10,-6)-3.5+40, y=c(0.07,0.07)+k, xpd=NA,lwd=1,col=col_p)
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg="white", fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg=c("#535F7C33", "#FCB42C33"), fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								text(x=c(-7.9,-8)+40, y=c(0.17,0.07)+k, labels=c('\u2642','\u2640'),xpd=NA,cex=0.6,col=c('#535F7C','#FCB42C'))
								
								
								#text(c(-2,-2,-2,-2),c(1,0.6,0.4,0.2),labels=c('American\ngolden\nplover',"Baird's\nsandpiper", "Western\nsandpiper","Semipalmated\nsandpiper"),xpd=TRUE, cex=0.4,col=cols, pos=3, outer=TRUE) 
								
						dev.off()															
						
						}
					{# (cf) raw data col
						png(paste(out_,"Supplementary_Figure_2c_loess_left_female_col_.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0, 0.2, 2.3),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
												
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("c")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							mtext('\u2640',side=3,line=-.7/2, cex=0.6,  col='#FCB42C', outer=TRUE)
							#lines(c(0,24),c(0.19,0.19)+k,, lty=3, col="grey80")							
						# data
							# aggregate per hour, species and type of incubation		
								hh$nn=1
								gg=ddply(hh,.(sp,cols, sex, hour), summarise,mean_=mean(att),se_=sd(att)/sqrt(length(att)),n=sum(nn))
								gg=gg[order(gg$sex),]							
							#arrows(x0=gg$hour, y0=gg$mean_-gg$se_,x1=gg$hour,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							gf=gg[gg$sex=='f',]
							gx=gg[gg$n==max(gg$n),]
							gx$mean_=-1
							gf=rbind(gf,gx)
							
							s= data.frame(sp=c("amgp", "basa" ,"sesa", "wesa"), sex=rep('f',4), 
											  col_=cols, #rep(c("purple", "pink" ,"deepskyblue", "deepblue"),2), 
											  lwd_=rep(2,4), stringsAsFactors=FALSE)
							gf$cols=s$col_[match(gf$sp,s$sp)]				  

							symbols(gf$hour,gf$mean_, circles=sqrt(gf$n/pi),inches=0.14/1.75,bg=adjustcolor(gf$cols,alpha.f = 0.2),add=TRUE, fg=gf$cols) #bg=alpha(col_p,0.1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,fg=adjustcolor(gg$cols,alpha.f = 0.2),add=TRUE), fg=col_p) #bg=alpha(col_p,0.1)
								
														#points(h$att~h$hour, col=adjustcolor(h$cols, alpha.f = 0.3), pch=20, cex=0.2)	
							#points(inc$inc_eff~inc$bout_start_j_c, col=inc$col_,bg=adjustcolor(inc$col_, alpha.f = 0.4), pch=21, cex=0.5)	
							
								
								hf=hh[hh$sex=='f',]
								for( i in 1:nrow(s)){	
													ha=hf[hf$sp==s$sp[i] & hf$sex==s$sex[i],]
													xx=loess(att~hour,ha)
													j=order(ha$hour)
													lines(ha$hour[j], xx$fitted[j], col=s$col_[i], lwd=s$lwd_[i])
												}	
							
								#text(x=-2+k, y=0.05, pos=4, expression(italic('N')*' cases:'),cex=0.5,las=1,col='grey30') 
								#symbols(c(10,15,20),c(0.105,0.105,0.105)+k,circles=sqrt(c(10,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) 
								#text(c(10,15,20),c(0.007,0.007,0.007)+k,labels=c(10,100,150), xpd=TRUE, cex=0.5,col='grey30')
								
								#bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								
							 
								
								mtext(side=4, 'American\ngolden\nplover',cex=0.4,las=1,col=cols[1], line=0.3,at=0.9) 
								mtext(side=4, "Baird's\nsandpiper",cex=0.4,las=1,col=cols[2], line=0.3,at=0.7) 
								mtext(side=4, "Western\nsandpiper",cex=0.4,las=1,col=cols[3], line=0.3,at=0.54) 
								mtext(side=4, "Semipalmated\nsandpiper",cex=0.4,las=1,col=cols[4], line=0.3,at=0.36) 
								
								lines(x=c(-10,-6)-3.5+40, y=c(0.15,0.15)+k, xpd=NA,lwd=2,col=col_p)
								lines(x=c(-10,-6)-3.5+40, y=c(0.07,0.07)+k, xpd=NA,lwd=1,col=col_p)
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg="white", fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg=c("#535F7C33", "#FCB42C33"), fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								text(x=c(-7.9,-8)+40, y=c(0.17,0.07)+k, labels=c('\u2642','\u2640'),xpd=NA,cex=0.6,col=c('#535F7C','#FCB42C'))
								
								
								#text(c(-2,-2,-2,-2),c(1,0.6,0.4,0.2),labels=c('American\ngolden\nplover',"Baird's\nsandpiper", "Western\nsandpiper","Semipalmated\nsandpiper"),xpd=TRUE, cex=0.4,col=cols, pos=3, outer=TRUE) 
								
						dev.off()															
						
						}
					{# (dm) raw data col
						png(paste(out_,"Supplementary_Figure_2d_loess_left_male_col.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 0, 0.2, 2.3),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
												
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							
							#axis(2, at=seq(0,1,by=0.25), labels=TRUE)
							#mtext("Nest attendance [proportion]",side=2,line=1.3, cex=0.6, las=3, col='grey30')
							
							mtext(expression(bold("d")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							mtext('\u2642',side=3,line=-.7/2, cex=0.6,  col='#535F7C', outer=TRUE)
							
							lines(c(0,24),c(0.19,0.19)+k,, lty=3, col="grey80")							
						# data
							# aggregate per hour, species and type of incubation		
								hh$nn=1
								gg=ddply(hh,.(sp,cols, sex, hour), summarise,mean_=mean(att),se_=sd(att)/sqrt(length(att)),n=sum(nn))
								gg=gg[order(gg$sex),]							
							#arrows(x0=gg$hour, y0=gg$mean_-gg$se_,x1=gg$hour,y1=gg$mean_+gg$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
							#symbols(gg$hour,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
							gf=gg[gg$sex=='m',]
							s= data.frame(sp=c("amgp", "basa" ,"sesa", "wesa"), sex=rep('m',4), 
											  col_=cols, #rep(c("purple", "pink" ,"deepskyblue", "deepblue"),2), 
											  lwd_=rep(2,4), stringsAsFactors=FALSE)
							gf$cols=s$col_[match(gf$sp,s$sp)]				  

							symbols(gf$hour,gf$mean_, circles=sqrt(gf$n/pi),inches=0.14/1.75,bg=adjustcolor(gf$cols,alpha.f = 0.2),add=TRUE, fg=gf$cols) #bg=alpha(col_p,0.1)
							
								
								
								hf=hh[hh$sex=='m',]
								for( i in 1:nrow(s)){	
													ha=hf[hf$sp==s$sp[i] & hf$sex==s$sex[i],]
													xx=loess(att~hour,ha)
													j=order(ha$hour)
													lines(ha$hour[j], xx$fitted[j], col=s$col_[i], lwd=s$lwd_[i])
												}	
							
								text(x=-2+k, y=0.05, pos=4, expression(italic('N')*' cases:'),cex=0.5,las=1,col='grey30') 
								
								symbols(c(10,15,20),c(0.105,0.105,0.105)+k,circles=sqrt(c(10,100,150)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								#symbols(c(23,23,23),c(0.77,0.65,0.5),circles=sqrt(c(10,100,300)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
								text(c(10,15,20),c(0.007,0.007,0.007)+k,labels=c(10,100,150), xpd=TRUE, cex=0.5,col='grey30') 
								
								mtext(side=4, 'American\ngolden\nplover',cex=0.4,las=1,col=cols[1], line=0.3,at=0.9) 
								mtext(side=4, "Baird's\nsandpiper",cex=0.4,las=1,col=cols[2], line=0.3,at=0.7) 
								mtext(side=4, "Western\nsandpiper",cex=0.4,las=1,col=cols[3], line=0.3,at=0.54) 
								mtext(side=4, "Semipalmated\nsandpiper",cex=0.4,las=1,col=cols[4], line=0.3,at=0.36) 
								
								#lines(x=c(-10,-6)-3.5+40, y=c(0.15,0.15)+k, xpd=NA,lwd=2,col=col_p)
								#lines(x=c(-10,-6)-3.5+40, y=c(0.07,0.07)+k, xpd=NA,lwd=1,col=col_p)
								#symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg="white", fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								#symbols(x=c(-8,-8)-3.5+40,c(0.15,0.07)+k,circles=c(0.1,0.1),bg=c("#535F7C33", "#FCB42C33"), fg=col_p,add=TRUE, xpd=NA, inches=0.03) 
								#text(x=c(-7.9,-8)+40, y=c(0.17,0.07)+k, labels=c('\u2642','\u2640'),xpd=NA,cex=0.6,col=c('#535F7C','#FCB42C'))
								
								
								#text(c(-2,-2,-2,-2),c(1,0.6,0.4,0.2),labels=c('American\ngolden\nplover',"Baird's\nsandpiper", "Western\nsandpiper","Semipalmated\nsandpiper"),xpd=TRUE, cex=0.4,col=cols, pos=3, outer=TRUE) 
								
						dev.off()															
						
						}
					
					{# (b) 
						png(paste(out_,"Supplementary_Figure_2b_fit.png", sep=""), width=3.5*0.5,height=1.85,units="in",res=600)
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0.2),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(NA,pch=19,xlim=c(0,24), ylim=c(k,1), xlab=NA, ylab=NA, yaxt='n',xaxt='n', type='n')
											
							axis(1, at=seq(0,24,by=6),labels=seq(0,24,by=6),cex.axis=0.5,mgp=c(0,-0.20,0))
								mtext("Time of day [hours]",side=1,line=1/2, cex=0.6, las=1, col='grey30')
							mtext(expression(bold("b")),side=3,line=-.7/2, cex=0.6,  col='grey30', outer=TRUE, adj=0.48*2)
							
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
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
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
				nests=read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				nests$on=as.POSIXct(nests$on)
				nests$off=as.POSIXct(nests$off)
				nests$end=as.POSIXct(nests$end)
				nests$start=as.POSIXct(nests$start)
				nests_=nests[which(nests$circumstances!='temporal'),]
				p=read.csv(paste(wd,'populations.csv', sep=""), stringsAsFactors=FALSE)
				nests_$bout=p$bout[match(paste(nests_$site,nests_$sp), paste(p$site_abbreviation, p$sp))]
			}
			{# incubation start
				 s=read.csv(paste(wd,'inc_start.csv', sep=""), stringsAsFactors=FALSE)
				 s$inc_start=as.POSIXct(s$inc_start,tz='UTC')			
			}
			{# incubation period
				 ip=read.csv(paste(wd,'inc_period.csv', sep=""), stringsAsFactors=FALSE)
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
									if(i=='biparental_70'){p1=Temperature_actogram(dfr=dfr_,day='TRUEcons',sex=TRUE)}else{p2=Temperature_actogram(dfr=dfr_,day='TRUEcons', sex=TRUE)}
									
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
				png(paste(out_, "Figure_5_sex.png",sep="_"), width = 3.5, height = 3*2, units = "in", res = 300)	
					
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
		{# run first - prepare predictions - with AR1 control
							m2=glmmPQL(att ~sin(rad)+cos(rad) + types +sin(rad)*types+cos(rad)*types, random = ~ sin(rad)+cos(rad) | sp_type/actID_type,data = h, family='gaussian',
							correlation=corAR1(0.2, form = ~ 1 | sp_type/actID_type))
				
							#summary(m)
							#plot(allEffects(m2))
						
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
				g=n_[!n_$state%in%c('r','u','w'),]
				g$success=ifelse(g$state%in%c('s','l','h'),'yes','no')
				g$success_bin=ifelse(g$state%in%c('s','l','h'),1,0)
				g$prop_ip=g$prop_ip*100
				g$n=1
				{# add median daily nest attendance - CHECK WHETHER HERE AND IN OTHER DATASETS WE USE THE RIGHT BIRDS
								dd=d[which(d$type=='uni'),]
								d_=ddply(dd,.(act_ID), summarise, att_med=median(att, na.rm=TRUE))							
								g$att_med=d_$att_med[match(g$act_ID,d_$act_ID)]
								g2=g[-which(is.na(g$att_med)),]# biparental_24  48 - dislocated temperature probe; 61 - one egg nest
				
				}
				g3=g2[-which(g2$nest=='s807'),] 
				
		}	
		
		{# number of nests with given end state and distribution of success/failure across day and durantion of uniparental
			nrow(g) # number of nests
			mean(g$success_bin) # proportion of successful nests
			nrow(g[g$success_bin==1,]) # number of successful nests
			length(unique(g$sp[g$success_bin==1]))
			
			summary(factor(g$state)) # distribution of end states	
			
			table(g$sp,g$success)  # distribution of successful nests
			g$n=1
			ddply(g,.(sp), summarise,nn=round(100*(sum(success_bin)/sum(n)))) # % of successful from nests with known outcome

			unique(n_$state)
				
				# distribution of incubation start
				densityplot(~g$prop_ip)
				densityplot(~g$prop_ip, groups=g$success, auto.key=TRUE)
				
				# distribution of how long the uni incubation lasted
				densityplot(~g$uni_last)
				densityplot(~g$uni_last, groups=g$success, auto.key=TRUE)
				densityplot(~log(g$uni_last))
				
				# distribution of incubation end
				densityplot(~g$prop_day_end)
				densityplot(~g$prop_day_end/100, groups=g$success, auto.key=TRUE)
				summary(g$prop_day_end[g$success=='yes'])
				densityplot(~log(g$uni_last))
		}
		{# Supplementary Table 6 | Cases of uniparental incubation according to desertion circumstances and nest faith. 
			table(g$success,g$circumstances)
		}
  
		{# Supplementary Figure 3 | Distribution of descriptors of uniparental incubation from biparental species according to nest faith
			{# run first
				sp_=sp[!sp$sp%in%c('pesa','rnph'),]
				sp_$order=-sp_$order
				sp_=sp_[order(sp_$order),]
				
				g$species=sp_$species[match(g$sp,sp_$sp)]
				g$order=sp_$order[match(g$sp,sp_$sp)]
				g$success=factor(g$success, levels=c('yes','no'))
				counts=table(g$success,g$order)
				
				g3=g[-which(g$nest=='s807'),] 
			}	
			{# Supplementary Figure 3a densityplot across start
				dens_suc <- density(g3$prop_ip[g3$success=='yes']/100)
				dens_fail <- density(g3$prop_ip[g3$success=='no']/100)
				range(dens_fail$y, dens_suc$y)
				g3$col_=ifelse(g3$state%in%c('s','l','h'),female_col,male_col)
				
				points_=data.frame( x=g3$prop_ip/100, success=g3$success, col_=g3$col_, stringsAsFactors=FALSE)
				points_$y=ifelse(points_$success=='yes', -0.125,-0.25)
			#dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_4a_.png", sep=""), width=3.5*0.7,height=1.85,units="in",res=600)
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
					
					text(y=2.8,x=1.60, pos=2, labels='Hatched', col='#FCB42C', cex=0.5)
					text(y=2.5,x=1.60, pos=2, labels='Failed', col='#535F7C', cex=0.5)
					
					mtext(expression(bold("a")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
		}
			{# Supplementary Figure 3b densityplot across duration
				dens_suc <- density(g3$uni_last[g3$success=='yes'])
				dens_fail <- density(g3$uni_last[g3$success=='no'])
				range(dens_fail$y, dens_suc$y)
				summary(g3$uni_last)
				g3$col_=ifelse(g3$state%in%c('s','l','h'),female_col,male_col)
				
				points_=data.frame( x=g3$uni_last, success=g3$success, col_=g3$col_, stringsAsFactors=FALSE)
				points_$y=ifelse(points_$success=='yes', -0.26/2*0.25/3,-0.26*0.25/3)
			#dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_4b_.png", sep=""), width=3.5*0.7,height=1.85,units="in",res=600)
			 par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2, 0.2, 3.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) # 0.6 makes font 7pt, 0.7 8pt
				
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
					
					mtext(expression(bold("b")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
		}
			{# Supplementary Figure 3c densityplot across end
				dens_suc <- density(g$prop_day_end[g$success=='yes']/100)
				dens_fail <- density(g$prop_day_end[g$success=='no']/100)
				range(dens_fail$y, dens_suc$y)
				g$col_=ifelse(g$state%in%c('s','l','h'),female_col,male_col)
				
				points_=data.frame( x=g$prop_day_end/100, success=g$success, col_=g$col_, stringsAsFactors=FALSE)
				points_$y=ifelse(points_$success=='yes', -0.26/2*5.5/3,-0.26*5.5/3)
			#dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_4c_.png", sep=""), width=3.5*0.7,height=1.85,units="in",res=600)
			par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2, 0.2, 3.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) # 0.6 makes font 7pt, 0.7 8pt
				
				plot(NA,xlim=c(0.2,1.80), ylim=c(-0.26*5.5/3,5.5),ylab='Kernel density',
							xaxt='n'#yaxt='n',
							#xlab="Cases of uniparental incubation [count]", 
							#pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6)
								)
					
						axis(1, at=seq(0.2,1.80,by=0.20),labels=c(20,"",60,"",100,"",140,"",180),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("End of uniparental incubation\n[% of species' incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
						lines(dens_suc,col=female_col, xpd=FALSE)
						lines(dens_fail,col=male_col,xpd=FALSE)
					points(y=jitter(points_$y),x=points_$x, pch = 21,cex=0.5, col="gray63",bg=adjustcolor(points_$col_, alpha.f = 0.6))
					#points(y=jitter(points_$y),x=points_$x, col=points_$col_, pch=19, cex=0.5)
					
					mtext(expression(bold("c")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
			
		}
			{# Supplementary Figure 3d densityplot median daily nest attendance
				dens_suc <- density(g2$att_med[g2$success=='yes'])
				dens_fail <- density(g2$att_med[g2$success=='no'])
				range(dens_fail$y, dens_suc$y)
				g2$col_=ifelse(g2$state%in%c('s','l','h'),female_col,male_col)
				
				points_=data.frame( x=g2$att_med, success=g2$success, col_=g2$col_, stringsAsFactors=FALSE)
				points_$y=ifelse(points_$success=='yes', -0.26/2*4/3,-0.26*4/3)

			#dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_4d_.png", sep=""), width=3.5*0.7,height=1.85,units="in",res=600)
				par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2, 0.2, 3.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) # 0.6 makes font 7pt, 0.7 8pt
				
				plot(NA,xlim=c(0.2,1), ylim=c(-0.26*4/3,4),ylab='Kernel density',
							xaxt='n'#yaxt='n',
							#xlab="Cases of uniparental incubation [count]", 
							#pch = 21,cex=0.5, col="gray63",bg=adjustcolor(n_$col_, alpha.f = 0.6)
								)
					
						axis(1, at=seq(0.2,1,by=0.10),labels=c(0.2,"",0.4,"",0.6,"",0.8,"",1.0),cex.axis=0.5,mgp=c(0,-0.2,0))
							mtext("Median nest attendance\n[proportion]",side=1,line=1, cex=0.6, las=1, col='grey30')
						lines(dens_suc,col=female_col, xpd=FALSE)
						lines(dens_fail,col=male_col,xpd=FALSE)
					points(y=jitter(points_$y),x=points_$x, pch = 21,cex=0.5, col="gray63",bg=adjustcolor(points_$col_, alpha.f = 0.6))
					#points(y=jitter(points_$y),x=points_$x, col=points_$col_, pch=19, cex=0.5)
					
					mtext(expression(bold("d")),side=3,line=-.35, cex=0.6,  col='grey30', outer=TRUE, adj=0.9)	
				dev.off()
			
		}
		
			{# not used Supplementary Figure 3a distribution across nests
			
			# par(mfrow=c(1,3),mar=c(0.0,0,0,0.4),oma = c(1.8, 1.8, 0.2, 0.5),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
			 #dev.new(width=3.5*0.75,height=1.85)
			 png(paste(out_,"Figure_4a.png", sep=""), width=3.5*0.7,height=1.85,units="in",res=600)
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
			
		
		}
		{# Figure 4 | Nest success in nests of biparental species with biparental and uniparental incubation
			{# run first
				u =read.csv(paste("C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Comparative/Submission/Supplementary/Data/",'Supplementary Data 4 - Nests metadata.csv', sep=""), stringsAsFactors=FALSE)
				u=u[which(paste(u$sp,u$breeding_site)%in%c('AMGP barr', 'BASA barr', 'BLGO isla','DUNL barr', 'LBDO barr', 'REDS neth', 'SESA barr', 'WESA barr')),]
				
				u1=ddply(u[u$end_state%in%c('fl','h','d','p'),],.(sp), summarise, s=100*length(end_state[end_state%in%c('fl','h')])/length(end_state[end_state%in%c('fl','h','d','p')]),n=length(end_state), type='bip')

				
				u2=ddply(g,.(sp=toupper(sp)), summarise,s=round(100*(sum(success_bin)/sum(n))), n=sum(n),type='uni') #
				uuu_=rbind(u1,u2)
			}
			{# plot - ADD COLOR
						png(paste(out_,"Bip_unip_success_size.png", sep=""), width=1.85,height=1.85,units="in",res=600)
						#dev.new(width=1.85,height=1.85)
						uuu_$type_=ifelse(uuu_$type=='bip', 2,6)
						uuu_$type_j=jitter(uuu_$type_)						
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(uuu_$s~uuu_$type_, xlim=c(0,8), ylim=c(0,100),xaxt='n',  ylab = "Successful nests [%]",,xlab = NULL,type='n')
						
												
						axis(1, at=c(2,6), label=c('Biparental', 'Uniparental'), mgp=c(0,-0.20,0))
						mtext("Type of incubation\nin biparental species",side=1,line=1, cex=0.6, las=1, col='grey30')
											
						axis(2, at=seq(0,100,by=20))
						mtext("Successful nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(toupper(uuu_$sp)))){
									lines(uuu_$type_j[toupper(uuu_$sp)==unique(uuu_$sp)[i]], uuu_$s[toupper(uuu_$sp)==unique(uuu_$sp)[i]], col='grey80')
									}
						
						symbols(uuu_$type_j, uuu_$s, circles=sqrt(uuu_$n/pi),inches=0.14/1.75,bg='white',add=TRUE, fg='white') #
						symbols(uuu_$type_j, uuu_$s, circles=sqrt(uuu_$n/pi),inches=0.14/1.75,bg=adjustcolor(col_p,alpha.f = 0.2),add=TRUE, fg=col_p) #
						
						dev.off()
					
			}
			
			
			
			
			# overall nest success
			summary(ddply(u[u$end_state%in%c('fl','h','d','p'),],.(sp), summarise, s=length(end_state[end_state%in%c('fl','h')])/length(end_state[end_state%in%c('fl','h','d','p')])))
			uu=ddply(u[u$end_state%in%c('fl','h','d','p'),],.(sp), summarise, s=length(end_state[end_state%in%c('fl','h')]),al=length(end_state[end_state%in%c('fl','h','d','p')]),n=length(end_state), type='bip')
			uu$p=uu$s/uu$al
			plot(p~n,uu)
	
			u1=ddply(u[u$end_state%in%c('fl','h','d','p'),],.(sp), summarise, s=100*length(end_state[end_state%in%c('fl','h')])/length(end_state[end_state%in%c('fl','h','d','p')]),n=length(end_state), type='bip')
		
		
			u2=ddply(g,.(sp), summarise,s=round(100*(sum(success_bin)/sum(n))), n=sum(n),type='uni') #
			uuu=rbind(u1,u2)
			uuu_=rbind(u1[u1$sp%in%toupper(u2$sp),],u2)
		
			boxplot(s~type,uuu)
			ggplot(uuu,aes(x=type,y=s, fill=type))+geom_violin(fill='white')+ geom_boxplot(width=0.1)
		
			ggplot(uuu,aes(x=type,y=s, fill=type, col=type))+geom_violin()+ geom_boxplot(width=0.1,fill='white', col='grey50')+labs(x="Type of incubation\nin biparental species", y = "Successful nests per species [%]")+guides(fill=FALSE, col=FALSE)
		
			ggplot(uuu,aes(x=type,y=s, fill=type, col=type))+geom_violin()+ labs(x="Type of incubation\nin biparental species", y = "Successful nests per species [%]")+guides(fill=FALSE, col=FALSE)+ geom_jitter(shape=16, position=position_jitter(0.2), col='black')
		
			ggplot(uuu_,aes(x=type,y=s, fill=type, col=type))+geom_violin()+ labs(x="Type of incubation\nin biparental species", y = "Successful nests per species [%]")+guides(fill=FALSE, col=FALSE)+ geom_jitter(shape=16, position=position_jitter(0.2), col='black')
				dev.new()
		
						png(paste(out_,"Bip_unip_success_xy.png", sep=""), width=1.85,height=1.85,units="in",res=600)
						#dev.new(width=1.85,height=1.85)
												
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(uuu_$s[uuu_$type=='uni']~uuu_$s[uuu_$type=='bip'], xlim=c(0,100), ylim=c(0,100), ylab=NULL, xlab = NULL,xaxt='n', type='n')
						
						axis(1, at=seq(0,100,by=20), ,mgp=c(0,-0.20,0))
						mtext("Successful nests [%]\n biparental incubation",side=1,line=1, cex=0.6, las=1, col='grey30')
											
						axis(2, at=seq(0,100,by=20))
						mtext("Successful nests [%]\n uniparental incubation",side=2,line=1, cex=0.6, las=3, col='grey30')
						
							lines(c(0,100),c(0,100), lty=3)
							
						points(	uuu_$s[uuu_$type=='uni']~uuu_$s[uuu_$type=='bip'], pch=19)
						
						dev.off()
						
						png(paste(out_,"Bip_unip_success_size.png", sep=""), width=1.85,height=1.85,units="in",res=600)
						#dev.new(width=1.85,height=1.85)
						uuu_$type_=ifelse(uuu_$type=='bip', 2,6)
						uuu_$type_j=jitter(uuu_$type_)						
						par(mar=c(0.0,0,0,0.4),oma = c(2.1, 2.1, 0.2, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(uuu_$s~uuu_$type_, xlim=c(0,8), ylim=c(0,100),xaxt='n',  ylab = "Successful nests [%]",,xlab = NULL,type='n')
						
												
						axis(1, at=c(2,6), label=c('Biparental', 'Uniparental'), mgp=c(0,-0.20,0))
						mtext("Type of incubation\nin biparental species",side=1,line=1, cex=0.6, las=1, col='grey30')
											
						axis(2, at=seq(0,100,by=20))
						mtext("Successful nests [%]",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(toupper(uuu_$sp)))){
									lines(uuu_$type_j[toupper(uuu_$sp)==unique(uuu_$sp)[i]], uuu_$s[toupper(uuu_$sp)==unique(uuu_$sp)[i]], col='grey80')
									}
						
						symbols(uuu_$type_j, uuu_$s, circles=sqrt(uuu_$n/pi),inches=0.14/1.75,bg='white',add=TRUE, fg='white') #
						symbols(uuu_$type_j, uuu_$s, circles=sqrt(uuu_$n/pi),inches=0.14/1.75,bg=adjustcolor(col_p,alpha.f = 0.2),add=TRUE, fg=col_p) #
						
						dev.off()
						
	
}

		
		{# Figure 5 & Supplementary Table 7 | Effects of start and duration of uniparental incubation on the probability of hatching
			{# correlation of predictors
				cor(g3$uni_last,g3$prop_ip,method = c("pearson"))
				cor(g3$uni_last,g3$prop_ip,method = c("spearman"))
				cor(g3$uni_last,g3$att_med,method = c("pearson"))
				cor(g3$uni_last,g3$att_med,method = c("spearman"))
				cor(g3$prop_ip,g3$att_med,method = c("pearson"))
				cor(g3$prop_ip,g3$att_med,method = c("spearman"))
							
				plot(g3$uni_last,g3$prop_ip)
				plot(g3$uni_last,g3$att_med)
				plot(g3$prop_ip,g3$att_med)
				
				nrow(g3)
			}	
			
			g3$sex=as.factor(g3$sex)
				
				# center within before and after
				g3$bef_aft=ifelse(g3$prop_ip>100, 1,-1)
				bef=mean(g3$prop_ip[g3$bef_aft==-1],na.rm = TRUE)
				aft=mean(g3$prop_ip[g3$bef_aft==1],na.rm = TRUE)
				g3$prop_ip_c=ifelse(g3$bef_aft==-1,g3$prop_ip-bef, g3$prop_ip-aft)
			
			
			m=glmer(success_bin~sex+(1|sp),data=g3,family='binomial')
			plot(allEffects(m))
			{# Figure 5
			   {# predictions	
				{# for day of incubation
					# model
					m=glmer(success_bin~prop_ip+uni_last+att_med+(1|sp),data=g3,family='binomial')
					#m2=glmer(success_bin~poly(prop_ip,2):uni_last+(1|sp),data=g,family='binomial')
											  
					# simulation		
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
					apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				
					# coefficients
					v <-apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
					
					# specify dataset
						newD=data.frame(prop_ip=seq(min(g3$prop_ip),max(g3$prop_ip),length.out=200),
										att_med=mean(g3$att_med),
										uni_last=mean(g3$uni_last)
										)
						
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last+att_med,data=newD)	
										
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
					# model
										
					m=glmer(success_bin~prop_ip+uni_last+att_med+(1|sp),data=g3,family='binomial')
																  
				    # simulation		
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
					apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					
					# coefficients
					v <-apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
					# specify dataset
						newD=data.frame(prop_ip=mean(g3$prop_ip),
										att_med=mean(g3$att_med),
										uni_last=seq(min(g3$uni_last),max(g3$uni_last),length.out=200)
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last+att_med,data=newD)	
										
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
				{# median daily nest attendance
					# model
						m=glmer(success_bin~prop_ip+uni_last+att_med+(1|sp),data=g3,family='binomial')
											  
					# simulation		
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
					apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				
					# coefficients
					v <-apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
					
					# specify dataset
						newD=data.frame(prop_ip=mean(g3$prop_ip),
										att_med=seq(min(g3$att_med),max(g3$att_med),length.out=200),
										uni_last=mean(g3$uni_last)
										)
									
					# exactly the model which was used has to be specified here 
							X = model.matrix(~ prop_ip+uni_last+att_med,data=newD)	
										
						# calculate predicted values and creditability intervals
								newD$pred = plogis(X%*%v) # in case on binomial scaleback
								
								predmatrix = matrix(nrow=nrow(newD), ncol=nsim)
									for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
									newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
									newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
									#newD$other <- apply(predmatrix, 1, quantile, prob=0.5)
									#newD=newD[order(newD$t_tundra),]
								pa=newD
								pa$predictor=pa$att_med
				
				}
			   }			
			   {# plot
					png(paste(out_,"Figure_5_.png", sep=""), width=3.5*1.5,height=1.85,units="in",res=600)
					#dev.new(width=3.5*1.5,height=1.85)
					par(mfrow=c(1,3),mar=c(2.2,2.1,0.5,0.1),  mgp=c(1.2,0.35,0),oma = c(0, 0, 0, 1),ps=12, cex=1,las=1,  tcl=-0.15,bty="n",xpd=TRUE)
					#par(mfrow=c(1,3),mar=c(2.2,2.1,0.5,0.1),  mgp=c(1.2,0.35,0),oma = c(0, 0, 0, 1),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") # 0.6 makes font 7pt, 0.7 8pt
					{# a
						par(mar=c(2.2,2.1,0.5,0.1),  mgp=c(1.2,0.35,0),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") 
					
						
						plot(NA, xlim=c(0,160), ylim=c(0,1), ylab='Nest success [probability]',xlab=NA, xaxt='n', type='n')#yaxt='n',
								axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
								mtext("Start of uniparental\n[% of incubation period]",side=1,line=1, cex=0.6, las=1, col='grey30')
								mtext(expression(bold("a")),side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.32)
								
								polygon(c(pp$predictor, rev(pp$predictor)), c(pp$lwr, 
										rev(pp$upr)), border=NA, col=col_lb)#adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
								lines(pp$predictor, pp$pred, col=col_l,lwd=1)			
								
									# prepare points for plotting
										g3$prop_cut=as.character(cut(g3$prop_ip, 10))
										g3$n=1
										gg=ddply(g3,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(prop_ip), n=sum(n))
											# add fake point to match sizes in (a) and (b)
													gg_=gg[1,]
													gg_$mean_=-1
													gg_$se_=0
													gg_$n=21
													gg=rbind(gg,gg_)
								
								#arrows(x0=gg3$prop_cut_m, y0=gg3$mean_-gg3$se_,x1=gg3$prop_cut_m,y1=gg3$mean_+gg3$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
								symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
								symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
									
						}
					{# b
						par(mar=c(2.2,1.1,0.5,1.1),  mgp=c(1.2,0.35,0),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") 
											
						plot(NA, xlim=c(0,20), ylim=c(0,1), ylab=NA,xlab=NA, xaxt='n', type='n',yaxt='n')
								#axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
								axis(1, at=seq(0,20,by=5),labels=seq(0,20,by=5),cex.axis=0.5,mgp=c(0,-0.2,0))
								mtext("Duration of uniparental\n[days]",side=1,line=1, cex=0.6, las=1, col='grey30')
								mtext(expression(bold("b")),side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.61)
								
								polygon(c(pd$predictor, rev(pd$predictor)), c(pd$lwr, 
										rev(pd$upr)), border=NA, col=col_lb)#col=adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
								lines(pd$predictor, pd$pred, col=col_l,lwd=1)			
									
									# prepare points for plotting
										g3$prop_cut=as.character(cut(g3$uni_last, 10))
										g3$n=1
										gg=ddply(g3,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(uni_last), n=sum(n))
								
								#arrows(x0=gg3$prop_cut_m, y0=gg3$mean_-gg3$se_,x1=gg3$prop_cut_m,y1=gg3$mean_+gg3$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
								symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
								symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
								
						}
					{# c
						par(mar=c(2.2,0.2,0.5,2),  mgp=c(1.2,0.35,0),ps=12, las=1, cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.15,bty="n",xpd=TRUE, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70") 
										
						plot(NA, xlim=c(0.2,1), ylim=c(0,1), ylab=NA,xlab=NA, type='n',xaxt='n', yaxt='n')
								#axis(1, at=seq(0,160,by=20),labels=c(0,"",40,"",80,"",120,"",160),cex.axis=0.5,mgp=c(0,-0.2,0))
								axis(1, at=seq(0.2,1,by=0.2),labels=seq(0.2,1,by=0.2),cex.axis=0.5,mgp=c(0,-0.2,0))
								mtext("Median daily nest attendance\n[proportion]",side=1,line=1, cex=0.6, las=1, col='grey30')
								mtext(expression(bold("c")),side=3,line=-.7, cex=0.6,  col='grey30', outer=TRUE, adj=0.92)
								
								polygon(c(pa$predictor, rev(pa$predictor)), c(pa$lwr, 
										rev(pa$upr)), border=NA, col=col_lb)#col=adjustcolor(col_l ,alpha.f = 0.1)) #0,0,0 black 0.5 is transparents RED
								lines(pa$predictor, pa$pred, col=col_l,lwd=1)			
									
									# prepare points for plotting
										g3$prop_cut=as.character(cut(g3$att_med, 10))
										g3$n=1
										gg=ddply(g3,.(prop_cut), summarise,mean_=mean(success_bin),se_=sd(success_bin)/sqrt(length(success_bin)), prop_cut_m=median(att_med), n=sum(n))
													gg_=gg[1,]
													gg_$mean_=-1
													gg_$se_=0
													gg_$n=21
													gg=rbind(gg,gg_)
								#arrows(x0=gg3$prop_cut_m, y0=gg3$mean_-gg3$se_,x1=gg3$prop_cut_m,y1=gg3$mean_+gg3$se_,code = 0, col =col_p , angle = 90, length = .025, lwd=1, lty=1)
								symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg='white', add=TRUE) 
								symbols(gg$prop_cut_m,gg$mean_, circles=sqrt(gg$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #bg=alpha(col_p,0.1)
								
						}
								
					{# legend
								mtext(expression(italic('N')*' nests:'),side = 4,line=0.2, padj=-6.5,cex=0.5,las=1,col='grey30', xpd=TRUE) 
									symbols(c(1.15,1.15,1.15),c(0.77,0.65,0.5),circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
									text(c(1.15,1.15,1.15)+2.7/23,c(0.77,0.66,0.5),labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
					}			
					dev.off()
				}
			}
			{# Supplementary Table 7 | Effects of start and duration of uniparental incubation on the probability of hatching
				m=glmer(success_bin~scale(prop_ip)+scale(uni_last)+scale(att_med)+(1|sp),data=g3,family='binomial')
			 
				pred=c('Intercept','Incubation period', 'Duration', 'Attendance')
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
						ri=data.frame(model='1', type='random (var)',effect=l$grp, estimate_r=l$vcov, lwr_r=NA, upr_r=NA)
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
		{# hatching succes descriptive
				sum(g$success_bin)/nrow(g) # overall
				g$success_bin_=ifelse(g$state%in%c('s','l','h'),1,-1)
				g_=ddply(g,.(sp), summarise, fail=-sum(success_bin_[success_bin_==-1]),hatch=sum(success_bin_[success_bin_==1])) 
				
				g_$prop=g_$hatch/(g_$hatch+g_$fail)
				# in relation to type of desertion (not in MS)
				ggplot(g,aes(x=circumstances, y=success_bin))+geom_violin()+ geom_jitter(shape=16, position=position_jitter(height=0.2), col='black')
				table(g$success, g$circumstances)
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
  
  {# Supplementary Actograms
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
{# DISCUSSION

	{# proportion of nests that started after expected hatching
		{# run first
		    n =read.csv(paste(wd,'nests.csv', sep=""), stringsAsFactors=FALSE)
				n$end=as.POSIXct(n$end)
				n$start=as.POSIXct(n$start)
				n=n[!n$sp%in%c('pesa','rnph'),]
					p=read.csv(paste(wd,'populations.csv', sep=""), stringsAsFactors=FALSE)
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
				n_$sex=as.factor(n_$sex)
				n_$sexsp=interaction(n_$sp,n_$sex)
				n_$uni_last=n_$uni_last/24
				
				{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				}
				length(n_$prop_ip[n_$prop_ip>1])/nrow(n_)
		}	
		
	
	{# number and propotion of nests with uniparental incubation
		sum(c(10,8,4,6,36,1,2,1))
		sum(c(10,8,4,6,36,1,2,1))/398
		
	}	
}

}



