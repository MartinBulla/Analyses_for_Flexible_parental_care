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
	   sapply(c('lattice','plyr','RSQLite'),
			function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))
	}
	
	{# define working and output directories
	    # metadata
		  wd="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Analyses/Data/"	
		# scripts
		  wds="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Analyses/Scripts/"	  
		# SQLite database
		  wd2="C:/Users/mbulla/Documents/ownCloud/BIPtoUNIP/"
	}

	{# establish database connections
	   dbq=dbGetQuery
	  
	  # name of sqlite database
	    db=paste(wd2,"bip_to_unip.sqlite",sep="")
	    db2=paste(wd2,"bip_to_unip_metadata.sqlite",sep="")
	}
}

{# CREATE DATASET
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
		}
		{# birds
				birds=read.csv(paste(wd,'birds.csv', sep=""), stringsAsFactors=FALSE)
				birds=birds[which(!is.na(birds$tag_ID)),]	
		}			
		{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
		}
	}
	
	{# created data for analyses
	lday=list()
	lhour=list()
	lmin_max=list()
	
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
						a=dbq(conLite,paste("SELECT*FROM", act_ID))
						dbDisconnect(conLite)
						
						if(a$sp[1]!='pesa'){a=a[which(!is.na(a$t_nest)),]}
						
						#a=a[which(!duplicated(a$datetime_)),] ############### REMOVE LATER
					# nest attendance by by day and hour			
					  tstu=nrow(a[a$type=='uni',])
					  tstb=nrow(a[a$type=='bip',])
					
					  if(tstu>0 | tstb>0 & tstu>0){
							a$datetime_=as.POSIXct(a$datetime_)
							a$day = as.Date(trunc(a$datetime_, "day"))
							a$time_ = as.numeric(difftime(a$datetime_, trunc(a$datetime_,"day"), units = "hours"))
							a$hour=sub("\\..*$", "", a$time_)
							a$n=1
									
							if(tstu>0 & tstb>0){
								u=ddply(a[a$type=='uni',],.(act_ID,sys,sp,site,year,t_method,nest,day, type), summarise, att=mean(inc),n=sum(n))
								b=ddply(a[a$type=='bip',],.(act_ID,sys,sp,site,year,t_method,nest,day, type), summarise, att=mean(inc),n=sum(n))
								aa=rbind(b,u)
								lday[[i]]=aa[order(aa$day,aa$type),]
								
								u=ddply(a[a$type=='uni',],.(act_ID,sys,sp,site,year,t_method,nest,day,hour, type), summarise, att=mean(inc),n=sum(n))
								b=ddply(a[a$type=='bip',],.(act_ID,sys,sp,site,year,t_method,nest,day,hour, type), summarise, att=mean(inc),n=sum(n))
								aa=rbind(b,u)
								lhour[[i]]=aa[order(aa$day,aa$hour,aa$type),]
														
								lmin_max[[i]]=ddply(a[a$type=='uni',],.(act_ID,sys,sp,site,year,nest,type), summarise, uni_start=min(datetime_), uni_end=max(datetime_))
								
								print(paste(nest, i, 'bu',sep=" "))
								}
							if(tstu>0 & tstb==0){
								lday[[i]]=ddply(a[a$type=='uni',],.(act_ID,sys,sp,site,year,t_method,nest,day, type), summarise, att=mean(inc),n=sum(n))
								lhour[[i]]=ddply(a[a$type=='uni',],.(act_ID,sys,sp,site,year,t_method,nest,day,hour,type), summarise, att=mean(inc),n=sum(n))
								lmin_max[[i]]=ddply(a[a$type=='uni',],.(act_ID,sys,sp,site,year,nest,type), summarise, uni_start=min(datetime_), uni_end=max(datetime_))
								print(paste(nest, i, 'u',sep=" "))	
								}
								
					}else{print(paste(nest, i, 'no data',sep=" "))}
					}	
					
	d=do.call(rbind,lday)
	h=do.call(rbind,lhour)
	se=do.call(rbind,lmin_max)
		
	{# add day in incubation + create proportion of species incubation period
			d$inc_start=s$inc_start[match(paste(d$year,d$nest),paste(s$year,s$nest))]
			d$day_j=as.numeric(format(d$day ,"%j")) - as.numeric(format(as.Date(trunc(d$inc_start, "day")),"%j"))+1
				d[d$day_j<1,]# exclude data that are from day before the incubation actually started
			d$inc_per_sp=ip$inc_period[match(d$sp,ip$sp)]
			d$prop_ip=d$day_j/d$inc_per_sp
			
			h$inc_start=s$inc_start[match(paste(h$year,h$nest),paste(s$year,s$nest))]
			h$day_j=as.numeric(format(h$day ,"%j")) - as.numeric(format(as.Date(trunc(h$inc_start, "day")),"%j"))+1
			
			h$inc_per_sp=ip$inc_period[match(h$sp,ip$sp)]
			h$prop_ip=h$day_j/h$inc_per_sp
			
			se$inc_start=s$inc_start[match(paste(se$year,se$nest),paste(s$year,s$nest))]
			se$inc_per_sp=ip$inc_period[match(se$sp,ip$sp)]
			
	}
		
	save(d,h,se,file=paste(wd,'for_analyses.RData',sep="")) # d - per day aggregates, h per hour aggregates, se - start and end of unip incubation
	}
}
	

	
	
{# DONE CHECK whether number of 5s readings per day is not higher than it should be - 60*60*24/5
		load(paste(wd,'for_analyses.RData',sep="")) # d - per day aggregates, h per hour aggregates
		densityplot(~d$n)
		
		nrow(d[d$n>60*60*24/5 & !d$sp%in%c('pesa'),]) # ok
		nrow(d[d$n>60*60*24/4 & d$sp%in%c('pesa'),]) # pesa ok
			#nrow(d$nest[d$n>60*60*24/5 & !d$sp%in%c('pesa')])
			#unique(d$t_method[d$n>60*60*24/5 & !d$sp%in%c('pesa')])
			#head(d[d$n>60*60*24/5 & !d$sp%in%c('pesa'),],n=10)
			#d[d$n>60*60*24/5 & !d$sp%in%c('pesa') & d$t_method=='msr',]
			#head(d, n=10)
		unique(d$sp[which(d$n>60*60*24/5)])
		unique(d$sp)
		
		
	
		if(nest%in%c("a301","a302","b501","s111","s409","s502","s506","s602","s610","s627","s807","w801")){
		
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
						a=dbq(conLite,paste("SELECT*FROM", act_ID))
						dbDisconnect(conLite)
					
						a$datetime_=as.POSIXct(a$datetime_)
						a$datetime_after=c(a$datetime_[-1], a$datetime_[nrow(a)]+5)	
						a$dif=as.character(difftime(a$datetime_after,a$datetime_, 'secs'))
						head(a[a$dif!=5,])
						nrow(a[a$dif!=5,])
						}
}
  