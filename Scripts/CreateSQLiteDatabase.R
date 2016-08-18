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
	{# load packages
	   sapply(c('data.table', 'ggplot2', 'ggthemes','grid','plyr','lattice', 'latticeExtra','magrittr','maptools','raster', 'rgeos', 'rgdal', 'RMySQL','RSQLite','XLConnect','zoo'),
			function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))
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
	  con=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
	  conMy=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
	  conE=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='extract_2013')
	  dbq=dbGetQuery
	  
	  # name of sqlite database
	    db=paste(wd2,"bip_to_unip.sqlite",sep="")
	    db2=paste(wd2,"bip_to_unip_metadata.sqlite",sep="")
	}
	
	{# load functions & constants
		source(paste(wds, 'Functions_&_constants.R',sep=""))
	}
}

{# make database
	
	{# RNPH
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
				nests_=nests[nests$sp%in%c('rnph'),]
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
				s=s[s$sp=='rnph',]
				s$inc_start=as.POSIXct(s$inc_start,tz='UTC')			
			}
			{# incubation period
				 ip=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='incubation_period',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING
									))
			}
			{# end states
					  n=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='nest_states',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING))
				
						n$datetime_=as.POSIXct(n$datetime_, tz="UTC")
						n=n[n$sp=='rnph',]
						n$datetime_=as.POSIXct(n$datetime_)
			}
			{# species
					sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				}
		}
		
		{# extract and plot
			f=list.files(path=paste(wd,"rnph_MSR/",sep=""),pattern='.csv', recursive=TRUE,full.names=TRUE)
			f=f[order(substring(f,nchar(f)-8))]
			f2=list.files(path=paste(wd,"rnph_MSR/",sep=""),pattern='.csv', recursive=TRUE,full.names=FALSE)
			f2=substring(f2,nchar(f2)-7,nchar(f2)-4)
			f2=f2[order(f2)]
			
		   for (i in (1:length(f))) {
			hasLight = read.table(f[i], skip = 18, sep = ",",nrows = 1)
            if (length(hasLight) > 5){ 
                varnames = c("datetime_", "humidity", "t_surface","t_nest", "light", "BV")}else{
				varnames = c("datetime_", "humidity", "t_surface","t_nest","BV")}
           
			d = read.table(f[i], skip = 38, sep = ";", col.names = varnames, fill = TRUE, stringsAsFactors = FALSE)
		    d$datetime_ = as.POSIXct(d$datetime_)
			d$nest = f2[i]
            d$foo = NULL
			d$light=NULL
			d$humidity=NULL
			
			d$datetime_=  as.POSIXct(d$datetime_)+ (nests_$local_plus[nests_$nest==d$nest[1]]*60*60) # bring to longitudanal time
			
			a=d[,c('datetime_','t_surface','t_nest','nest')]
			a=a[order(a$datetime_),]
			{# extratction of incubation/non-incubation based on nest and surface temperature
					
						b=a
						
						b$t_nest_prior=c(b$t_nest[1],b$t_nest[-nrow(b)])
						b$t_difference = as.numeric(b$t_nest - b$t_nest_prior) 
						
						b$t_nest_run_med=rollmedian(b$t_nest, 17279 , fill="extend") # 1/2 day 8639, whole day 17279
							
						b$t_diff_run=rollmean(b$t_difference, 24, fill="extend") #2 minutes (for median neads to be odd)	
						
						b$inc_t = 	ifelse(is.na(b$t_surface),
											(ifelse(b$t_nest_run_med<20,
												(ifelse(b$t_nest > b$t_nest_run_med+3, 1, 0)),#))
												ifelse(b$t_nest > b$t_nest_run_med-3, 1,0))),#))
											ifelse(b$t_nest_run_med<20,
												(ifelse(b$t_nest > b$t_nest_run_med+3, 1, 
													ifelse(b$t_nest > b$t_surface+12.5, 1,0))),#))
												ifelse(b$t_nest > b$t_nest_run_med-3, 1, 
													ifelse(b$t_nest > b$t_surface+12.5, 1,0))))#))
						
						
						b$inc = 	ifelse(is.na(b$t_surface),
											(ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run < 0, 0,
													ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run > 0, 
														1, b$inc_t))),
										(ifelse(b$t_nest<20 &(b$t_surface-abs(b$t_nest))>(-3) & b$t_nest<b$t_nest_run_med+3, b$inc_t,
												(ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run < 0, 0,
												ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run > 0,1,
																	b$inc_t)))))	)	
						
						a=b
					
				}
			{# define uniparental periods, no = not used
				 tst=ifelse(nests_$state[nests_$nest==a$nest[1]]=='s',6, ifelse(nests_$state[nests_$nest==a$nest[1]]%in%c('h','l'),24,0)) # if start of hatching then end -6h, if hatching or left nest that -24h, else 0
				a$type=ifelse(a$datetime_>=a$datetime_[1]+60*60*2 & a$datetime_<nests_$end[nests_$nest==a$nest[1]]-tst*60*60 ,'uni','no')    
				a=a[which(a$datetime_<=max(n$datetime_[n$nest==a$nest[1]])+60*60),]
			}
			{# add variables
					act_ID=nests_$act_ID[nests_$nest==a$nest[1]]
					a$sp='rnph'
					a$year=2015
					a$site='chuk'
					a$who='m'
					a$signal=NA
					a$sys='uniparental' # breeding system
					a$act_ID=nests_$act_ID[nests_$nest==a$nest[1]]
					a$t_method='MSR'
					a$disturb=NA
					a$bird_ID=paste(a$nest,a$who,sep="_")
					a$tag=NA
					a$datetime_= as.character(a$datetime_)
				}	
				aa=a[,c('act_ID','sys','sp','site','year','t_method','nest','bird_ID','tag','who','datetime_',"disturb",'t_surface','t_nest',"signal",'inc','type' )] 
				
				nest=aa$nest[1]
				yr=aa$year[1]
				inc_start=s$inc_start[s$nest==nest & s$year==yr]
				ip_=ip$inc_period[ip$sp==nests_$sp[i]]
					
			{# load to database				
				
					conLite = dbConnect(dbDriver("SQLite"),dbname = db)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",act_ID))
					dbWriteTable(conn=conLite, name = act_ID, value = aa,row.names = FALSE)
					dbDisconnect(conLite)
				}
				
			{# save as txt
				 write.table(aa, paste(wd3,nest,".txt",sep=""), sep=",", row.names=FALSE)
				}
				
			{# plot
					
					dfr_=aa[which(!is.na(aa$datetime_)),]
					dfr=dfr_
					
					# define captions and nest location
						figCap_=data.frame(sp=dfr$sp[1],scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1], ID=dfr$act_ID[i],stringsAsFactors=FALSE)
						figCap=figCap_
								
						latlon_=data.frame(lat=nests_$lat[nests_$nest==nest],lon=nests_$lon[nests_$nest==nest],stringsAsFactors=FALSE) # for the map
						latlon=latlon_
								
					# generate actograms
						RFID.temperature_actogram(dfr=dfr_,type="PNG", UTC=TRUE, UTC_met=FALSE,day=FALSE) #type="SAVE")# 		
					
			}
			print(paste(nest, i,sep=" "))
			}
	
		}
		
	}

	{# PESA # careful some nests are in both years
		{# cut off value for incubation non/incubation from Lesku et al 2012
			# script to get it
				#tr=dbq(con,"SELECT*from loggersatbarrow.cv_inttags where tagType=0")
				#densityplot(~sqrt(tr$CV))
				#quantile(tr$CV, 0.99,na.rm = TRUE)
				#quantile(sqrt(tr$CV), 0.99,na.rm = TRUE)
				#tr_=quantile(tr$CV, 0.99,na.rm = TRUE)
			tr_=0.01085508#+0.009 
		}
		{# load matadata
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
				nests_=nests[nests$sp%in%c('pesa'),]
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
				s=s[s$sp=='pesa',]
				s$inc_start=as.POSIXct(s$inc_start,tz='UTC')			
			}
			{# incubation period
				 ip=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='incubation_period',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING
									))
			}
			{# end states
					  n=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='nest_states',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING))
				
						n$datetime_=as.POSIXct(n$datetime_, tz="UTC")
						n=n[n$sp=='pesa',]
						n$nest_year=paste(n$nest,n$year)
						nt=n[n$state=='tag_loss',]
						n2=n[which(!n$nest_year%in%nt$nest_year),]
						n2_=n2[n2$state!='p',]
						n=rbind(nt,n2_)
			}
			{# birds
					birds=readWorksheetFromFile(paste(wd,'birds.xls', sep=""), sheet='birds')
					birds=birds[which(birds$sp=='pesa'),]	
				}			
			{# species
					sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
				}
		}
		{# extract and plot
			for (i in (1:nrow(nests_))) {
			#for (i in c(25, 55,65)) {
				nest=nests_$nest[i]
				yr= nests_$year[i]
				act_ID=nests_$act_ID[i]
				
				tag_ID= birds$tag_ID[birds$nest==nest & birds$year_==yr]
				bird_ID=birds$bird_ID[birds$nest==nest & birds$year_==yr]
				on_=nests_$on[i]
				off_=nests_$off[i]
				start_=nests_$start[i]
				end_=nests_$end[i]
				inc_start=s$inc_start[s$nest==nest & s$year==yr]
				ip_=ip$inc_period[ip$sp==nests_$sp[i]]
				print(paste(tag_ID, i,sep=" "))
				a=dbq(con,paste("SELECT tag_ID as tag, CV as cv, datetime_ from loggersatbarrow.cv_inttags where tagType=1 and tag_ID='",tag_ID,"'", sep=""))
				
				# define incubation
					#if((nest=='p101' & yr==2008) | (nest=='p202' & yr==2009)){a$inc=ifelse(a$cv<(tr_+0.009),1,0)}else{
																								a$inc=ifelse(a$cv>tr_,0,1)
					#																			}
				
				#a$inc=ifelse(a$cv<tr_,1,0) 
									
			   {# add variables
					a$nest=nest
					a$sp='pesa'
					a$year=yr
					a$site='barr'
					a$who='f'
					a$sys='uniparental' # breeding system
					a$act_ID=act_ID
					a$t_method='radio'
					a$disturb=NA
					a$bird_ID=bird_ID
					a$datetime_z=a$datetime_
					a$datetime_=  as.POSIXct(a$datetime_z)+ (nests_$local_plus[i]*60*60) # bring to longitudanal time
					a$datetime_z= as.character(a$datetime_z)
					a$type=ifelse(a$datetime_>=start_ & a$datetime_<end_,'uni', 'no') # define uniparental period used for the analyses (no need to limit data prior to hatching as hatching happend in the incubator
					a$t_surface=a$t_nest=NA
					a=a[a$datetime_>=on_ & a$datetime_<=off_,] # limit data
					
				}	
				a$datetime_=as.character(a$datetime_)		
				#aa=a[,c('act_ID','sys','sp','site','year','t_method','nest','bird_ID','tag','who','datetime_z','datetime_',"disturb",'t_surface','t_nest',"cv",'inc','type' )] # state not used
				aa=a[,c('act_ID','sys','sp','site','year','t_method','nest','bird_ID','tag','who','datetime_',"disturb",'t_surface','t_nest',"cv",'inc','type' )] # state not used
			
			 {# load to database				
				
					conLite = dbConnect(dbDriver("SQLite"),dbname = db)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",act_ID))
					dbWriteTable(conn=conLite, name = act_ID, value = aa,row.names = FALSE)
					dbDisconnect(conLite)
				}
							
			 {# save as txt
				 write.table(aa, paste(wd3,yr,nest,".txt",sep=""), sep=",", row.names=FALSE)
				}
			
   			 {# plot
					dfr_=aa[which(!is.na(aa$datetime_)),]
					dfr=dfr_
					
					# define captions and nest location
						figCap_=data.frame(sp=dfr$sp[1],scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1], ID=dfr$act_ID[i],stringsAsFactors=FALSE)
						figCap=figCap_
								
						latlon_=data.frame(lat=nests_$lat[i],lon=nests_$lon[i],stringsAsFactors=FALSE) # for the map
						latlon=latlon_
								
					# generate actograms
						RFID.temperature_actogram(dfr=dfr_,type="PNG", UTC=TRUE, UTC_met=FALSE,min_=-.05, max_=0.22, signal=TRUE,day=FALSE) #type="SAVE")# 		
					
				}
			}	
		}
	}
	
	{# biparental species
		{# data extraction function BARROW
				RFID.temperature_actogram_data = function (con, nest, yr, start_,end_) {
			#on.exit(mysqlCloseConnection(con))
			  	nest_l=tolower(nest)
				nest_ =shQuote(tolower(nest))
				
					rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, coalesce(who, a.sex) who from
											(select  distinct  year_,lower(r.nest) nest, FUNCTIONS.RoundTime(COALESCE(datetime_corrected,datetime_),'S',5) datetime_,
												transp, NULL who
												from LOGGERSatBARROW.RFID r
												where transp IS NOT NULL AND lower(r.nest) =",nest_,") x  
												LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from AVESatBARROW.CAPTURES c
															 LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
														   UNION SELECT transponder , 'fieldteam' sex from  AVESatBARROW.AUTHORS) a
											   on x.transp = a.transponder	
										  where x.year_ = ",yr)) 
					
				rfids$who[is.na(rfids$who)] = 'unknown'
				rfids$who[which(rfids$who%in%c(2,"f","F"))] = 'f'
				rfids$who[which(rfids$who%in%c(1,"m","M"))] = 'm'
				
				#table(rfids$who, rfids$transp)
				#rfids$who[which(rfids$who%in%c("f","m") & rfids$transp%in%bird$logger[which(tolower(paste(bird$year,bird$nest))==paste(nests$year[i],nests$nest[i]))])]=bird$sex[match(tolower(rfids$transp[which(rfids$who%in%c("f","m") & rfids$transp%in%bird$logger[which(tolower(paste(bird$year,bird$nest))==paste(nests$year[i],nests$nest[i]))])]), tolower(bird$logger[bird$nest==nest & bird$year==yr]))]
				
				rfids$who[which(rfids$who%in%c("m","f") & !tolower(rfids$transp)%in%tolower(birds$tag_ID[which(tolower(paste(birds$year_,birds$nest))==tolower(paste(yr,nest)))]))] = 'stranger'
				
				# remove duplicated lines caused by start up (in the lab) of rfid overlaps with the data from RFID running in the field (later
					x=rfids[duplicated(rfids$datetime_),]
					if(nrow(x)>0){ rfids=rfids[which(!(rfids$datetime_==x$datetime_ & rfids$who=='fieldteam')),] }
					if(nest=='s627'){rfids=rfids[which(!rfids$datetime_=='2013-06-26 01:41:55'),]}	
					if(nest=='s807'){rfids=rfids[which(!rfids$datetime_=='2012-07-06 15:59:35'),]}					
			# w508 2013 - has MSR and TT so TT is not used
			if(!nest%in%c('s111','s506','s610','s712','w508')){
				if(yr==2011 & nest%in%c('a301','a302','b501','s602')){
					tts   = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, 
												T_egg as t_nest,'tt+avg' as t_method
										 from LOGGERSatBARROW.TinyTag
										 where T_egg is not NULL and T_egg > -20 and lower(nest) =",nest_," and year_ =",yr))
					hobo   = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, T_ambient as t_surface from LOGGERSatBARROW.HOBO where lower(nest) =",nest_))
					tts$t_surface=hobo$t_surface[match(tts$datetime_, hobo$datetime_)]
					tts$t_method=ifelse(is.na(tts$t_surface), tts$t_method, 'tt+hobo')
					
					#tts   = dbq(con, paste("SELECT LOWER(t.nest) nest, FUNCTIONS.RoundTime(t.datetime_,'M',1)  datetime_, h.t_surface, t.T_egg as t_nest,'tt+hobo' as t_method from LOGGERSatBARROW.TinyTag t
					#							LEFT JOIN (SELECT FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, T_ambient as t_surface from LOGGERSatBARROW.HOBO WHERE LOWER(nest) =",nest_,") h
					#								on t.datetime_ = h.datetime_	
					#								WHERE t.T_egg is not NULL and t.T_egg > -20 and LOWER(t.nest) =",nest_," and t.year_ =",yr))
													
													
					}else{
						tts   = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, 
												T_egg as t_nest,'tt+avg' as t_method
										 from LOGGERSatBARROW.TinyTag
										 where T_egg is not NULL and T_egg > -20 and lower(nest) =",nest_," and year_ =",yr))
						}
			}else{tts=data.frame(nest=NULL, datetime_=NULL, t_egg=NULL, t_method=NULL)}
			
			MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5)  datetime_,
			                                    T_egg as t_nest,T_ambient as t_surface,'msr' as 't_method'
										 from LOGGERSatBARROW.MSR
										 where lower(nest) =",nest_,"and year_ =",yr))
			
			# visits			
				if(yr==2011){
						visits=  dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5) datetime_
												from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,"and year_ =",yr,"GROUP BY FUNCTIONS.RoundTime(datetime_,'S',5)"))
												
						capture=dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(COALESCE(caught_date_time,release_time,start_capture_date_time),'S',5) datetime_ , 'capture' disturb
												from AVESatBARROW.CAPTURES where lower(nest) = ",nest_,"and year_ =",yr))
						
						capture=capture[which(!duplicated(capture$datetime_)),]
						if(nrow(capture)>0){
												capture$datetime_=as.POSIXct(capture$datetime_)
												visits$datetime_=as.POSIXct(visits$datetime_)
												visits$disturb=capture$disturb[match(visits$datetime_, capture$datetime_)]
												visits$disturb=ifelse(is.na(visits$disturb), 'nest visit', visits$disturb)
												if(nrow(visits[visits$disturb=='capture',])==0){visits=rbind(capture,visits)}# in case capture has different datetime
												}
						
						}else{
							v=  dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5) datetime_,FUNCTIONS.RoundTime(datetime_left,'S',5) datetime_left,
												 'nest visit' disturb
												from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,"and year_ =",yr))
							
							v$datetime_left=ifelse(is.na(v$datetime_left)| v$datetime_==v$datetime_left, as.character(as.POSIXct(v$datetime_)+30), v$datetime_left)	
							v=v[which(!duplicated(v$datetime_)),]
							
						l=list()
						for( j in 1:nrow(v)) {
							v_i = v[j, ]
							
							#xi=x[x$nest=='S305',]
							l[[j]] = data.frame(nest = v_i$nest, datetime_ = seq(from = as.POSIXct(v_i$datetime_), 
									to = as.POSIXct(v_i$datetime_left), by = "5 secs"))
							#print(v_i)
							}
						visits= do.call(rbind,l)

							capture=dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(COALESCE(caught_date_time,release_time,start_capture_date_time),'S',5) datetime_ , 'capture' disturb
												from AVESatBARROW.CAPTURES where lower(nest) = ",nest_,"and year_ =",yr))
							
							capture=capture[which(!duplicated(capture$datetime_)),]
							
							if(nrow(capture)>0){
												capture$datetime_=as.POSIXct(capture$datetime_)
												visits$disturb=capture$disturb[match(visits$datetime_, capture$datetime)]
												visits$disturb=ifelse(is.na(visits$disturb), 'nest visit', visits$disturb)
												}else{visits$disturb='nest visit'}
						
						}	
				
		 # merge the data
		 if (nrow(rfids) > 0) { 
			
			rfids$datetime_=as.POSIXct(rfids$datetime_, tz="UTC")
		
			#adds loggers  
					if (nrow(tts)>0) {
							
							tts$datetime_ = as.POSIXct(tts$datetime_, tz="UTC")
							# expand the data to per 5 second
							tts=tts[tts$datetime_>=start_ & tts$datetime_<=end_,]
							tts$datetime_lag=c(tts$datetime_[-1],tts$datetime_[nrow(tts)])-5
							tts$datetime_lag[nrow(tts)]=tts$datetime_[nrow(tts)]
							tts$pkk=1:nrow(tts)
								tts=data.table(tts)
								y = tts[, .(datetime_ = seq(datetime_, datetime_lag, by = "5 secs")), by = pkk]
								tts$datetime_=tts$datetime_lag=NULL
								tts=merge(tts, y, by = 'pkk')
								tts$pkk=NULL
						if(yr==2011 & nest%in%c('a301','a302','b501','s602')){
							
							 rfids = merge(rfids, tts, all = TRUE)
						}else{
						  
						  if(yr==2012 & nest=="a802"){tts=tts[tts$datetime_<as.POSIXct("2012-07-01 17:03:10"),]} # a802 2012 has MSR and TT for part of data so TT is notused where MSR present
						  tts$t_surface = NA
						  rfids = merge(rfids, tts, all = TRUE)
						  }
					  }
					if (nrow(MSRs)>0) {  
					  MSRs$datetime_ = as.POSIXct(MSRs$datetime_, tz="UTC")
					  rfids = merge(rfids, MSRs, all = TRUE)
					  }
					 if (nrow(MSRs)==0 & nrow(tts)==0) rfids$t_nest  =  rfids$t_surface = rfids$t_method = NA
			
			#adds disturb  		
					if (nrow(visits)>0) {
					  visits$datetime_ = as.POSIXct(visits$datetime_, tz="UTC")
					  rfids = merge(rfids, visits, all.x = TRUE)
					  }
					 if (nrow(visits)==0) rfids$disturb=NA 
					rfids$disturb=ifelse(is.na(rfids$disturb) & rfids$who%in%c('fieldteam','unknown','stranger'), 'nest visit', rfids$disturb)
			names(rfids)[names(rfids) =="transp"] <- "tag"
		
		  # limit to active nest data
					rfids=rfids[which(!is.na(rfids$datetime_)),]
			 		rfids=rfids[rfids$datetime_>=start_,]
					rfids=rfids[rfids$datetime_<=end_,]
		  # add average temperature where needed
				if(nrow(rfids[which(rfids$t_method=='tt+avg'),])>0){
					if(yr==2011){ avg=dbq(con, paste("select*from _sandbin.mb_avgt 
															where datetime_>='",rfids$datetime_[which(rfids$t_method=='tt+avg')][1],"' and 
															datetime_<='",rfids$datetime_[which(rfids$t_method=='tt+avg')][nrow(rfids[which(rfids$t_method=='tt+avg'),])],"'"))
								
								 rfids$t_surface[which(rfids$t_method=='tt+avg')]=avg$t_amb_avg[match(as.character(rfids$datetime_[which(rfids$t_method=='tt+avg')]), avg$datetime_)]
								 }
								 
					if(yr==2012){ avg=dbq(con, paste("select datetime_1m as datetime_, t_amb_avg from _sandbin.mb_avgt_2012_1m 
															where datetime_1m>='",rfids$datetime_[which(rfids$t_method=='tt+avg')][1],"' and 
															datetime_1m<='",rfids$datetime_[which(rfids$t_method=='tt+avg')][nrow(rfids[which(rfids$t_method=='tt+avg'),])],"'"))
								
								 rfids$t_surface[which(rfids$t_method=='tt+avg')]=avg$t_amb_avg[match(as.character(rfids$datetime_[which(rfids$t_method=='tt+avg')]), avg$datetime_)]
								 }
					if(yr==2013){ avg=dbq(con, paste("select datetime_1m as datetime_, t_amb_avg from extract_2013.mb_avgt_2013_1m 
															where datetime_1m>='",rfids$datetime_[which(rfids$t_method=='tt+avg')][1],"' and 
															datetime_1m<='",rfids$datetime_[which(rfids$t_method=='tt+avg')][nrow(rfids[which(rfids$t_method=='tt+avg'),])],"'"))
								
								 rfids$t_surface[which(rfids$t_method=='tt+avg')]=avg$t_amb_avg[match(as.character(rfids$datetime_[which(rfids$t_method=='tt+avg')]), avg$datetime_)]
								 }
					}				
				
		rfids=rfids[order(rfids$datetime_),]
		rfids$pk=c(1:nrow(rfids))
		return(rfids) 
				}
			
		 }
		 }
		{# data extraction function non-BARROW
			RFID.temperature_actogram_data_non_barrow = function (con, nest, yr, start_,end_) {
			#on.exit(mysqlCloseConnection(con))
			  	nest_l=tolower(nest)
				nest_ =shQuote(tolower(nest))
				
					rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, coalesce(who, a.sex) who from
											(select  distinct  lower(r.nest) nest, FUNCTIONS.RoundTime(COALESCE(datetime_corrected,datetime_),'S',5) datetime_,
												transp, NULL who
												from extra_incubation.RFID r
												where transp IS NOT NULL AND lower(r.nest) =",nest_,") x  
												LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from extra_incubation.CAPTURES c
															 LEFT JOIN extra_incubation.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
														   UNION SELECT transponder , 'fieldteam' sex from  extra_incubation.AUTHORS) a
											   on x.transp = a.transponder	
										 ")) 
					
				if(nest=='re133_nl2014'){rfids=rfids[which(!(rfids$transp=='FFD524B43A6F0001' & rfids$who=='fieldteam')),]}
				rfids$who[is.na(rfids$who)] = 'unknown'
				rfids$who[which(rfids$who%in%c(2,"f","F"))] = 'f'
				rfids$who[which(rfids$who%in%c(1,"m","M"))] = 'm'
				
				#table(rfids$who, rfids$transp)
				#rfids$who[which(rfids$who%in%c("f","m") & rfids$transp%in%bird$logger[which(tolower(paste(bird$year,bird$nest))==paste(nests$year[i],nests$nest[i]))])]=bird$sex[match(tolower(rfids$transp[which(rfids$who%in%c("f","m") & rfids$transp%in%bird$logger[which(tolower(paste(bird$year,bird$nest))==paste(nests$year[i],nests$nest[i]))])]), tolower(bird$logger[bird$nest==nest & bird$year==yr]))]
				
				rfids$who[which(rfids$who%in%c("m","f") & !tolower(rfids$transp)%in%tolower(birds$tag_ID[which(tolower(paste(birds$year_,birds$nest))==tolower(paste(yr,nest)))]))] = 'stranger'
				
					x=rfids[duplicated(rfids$datetime_),]
					if(nrow(x)>0){ rfids=rfids[which(!(rfids$datetime_==x$datetime_ & rfids$who=='fieldteam')),] }			
			
			MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5)  datetime_,
			                                    T_egg as t_nest,T_ambient as t_surface,'msr' as 't_method'
										 from extra_incubation.MSR
										 where lower(nest) =",nest_))
			
			{# visits and capture		
					vv=readWorksheetFromFile(paste(wd,'nl_is_visits.xlsx', sep=""), sheet='Sheet1',colTypes = c(
									XLC$DATA_TYPE.STRING,									
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING,
									XLC$DATA_TYPE.STRING))
						
						vv$datetime_left=as.POSIXct(paste(substring(vv$datetime_,1,10),substring(vv$end_,12,20),sep=" "))
						vv$datetime_=as.POSIXct(paste(substring(vv$datetime_,1,10),substring(vv$start_,12,20),sep=" "))
						v_=vv[vv$nest==nest,c('nest','datetime_','datetime_left')]
						v_$disturb='nest visit'
						v=v_
						if(nest_%in%c('re213_nl2014')){
									v=  dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5) datetime_,FUNCTIONS.RoundTime(datetime_left,'S',5) datetime_left,
												 'nest visit' disturb
												from extra_incubation.visits where lower(nest) = ",nest_,"and year_ =",yr))
							
									v$datetime_left=ifelse(is.na(v$datetime_left)| v$datetime_==v$datetime_left, as.character(as.POSIXct(v$datetime_)+30), v$datetime_left)	
									v=v[which(!duplicated(v$datetime_)),]
									v=rbind(v,v_)
											}
						l=list()
						for( j in 1:nrow(v)) {
							v_i = v[j, ]
							
							#xi=x[x$nest=='S305',]
							l[[j]] = data.frame(nest = v_i$nest, datetime_ = seq(from = as.POSIXct(v_i$datetime_), 
									to = as.POSIXct(v_i$datetime_left), by = "5 secs"),disturb='nest visit')
							#print(v_i)
							}
						visits= do.call(rbind,l)
						
						if(nest%in%c('re133_nl2014','ka1411_is2014')){
							capture=dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(COALESCE(caught_date_time,release_time,start_capture_date_time),'S',5) datetime_ , 'capture' disturb
												from extra_incubation.CAPTURES where lower(nest) = ",nest_,"and year_ =",yr))
							capture=capture[which(!duplicated(capture$datetime_)),]
							if(nrow(capture)>0){capture$datetime_=as.POSIXct(capture$datetime_)
												visits$disturb=NULL
												visits$disturb=capture$disturb[match(visits$datetime_, capture$datetime)]
												visits$disturb=ifelse(is.na(visits$disturb), 'nest visit', visits$disturb)
												}else{visits$disturb='nest visit'}
						
						}	
				}				
		 # merge the data
		 if (nrow(rfids) > 0) { 
			
			rfids$datetime_=as.POSIXct(rfids$datetime_, tz="UTC")
		
			#adds loggers  
					 if (nrow(MSRs)>0) {  
					  MSRs$datetime_ = as.POSIXct(MSRs$datetime_, tz="UTC")
					  rfids = merge(rfids, MSRs, all = TRUE)
					  }
					 if (nrow(MSRs)==0) rfids$t_nest  =  rfids$t_surface = rfids$t_method = NA
			
			#adds disturb  		
					if (nrow(visits[!is.na(visits$datetime_),])>0) {
					  visits$datetime_ = as.POSIXct(visits$datetime_, tz="UTC")
					  rfids = merge(rfids, visits, all.x = TRUE)
					  }
					if (nrow(visits[!is.na(visits$datetime_),])==0) rfids$disturb=NA 
					rfids$disturb=ifelse(is.na(rfids$disturb) & rfids$who%in%c('fieldteam','unknown','stranger'), 'nest visit', rfids$disturb)
				
			names(rfids)[names(rfids) =="transp"] <- "tag"
		
		  # limit to active nest data
					rfids=rfids[which(!is.na(rfids$datetime_)),]
			 		rfids=rfids[rfids$datetime_>=start_,]
					rfids=rfids[rfids$datetime_<=end_,]
		  			
		rfids=rfids[order(rfids$datetime_),]
		rfids$pk=c(1:nrow(rfids))
		return(rfids) 
				}
		}
		 }
			
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
				nests=nests[!nests$sp%in%c('rnph','pesa'),]
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
			{# end states
				  n=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='nest_states',colTypes = c(
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING))
			
					n$datetime_=as.POSIXct(n$datetime_, tz="UTC")
			}
			{# birds
				birds=readWorksheetFromFile(paste(wd,'birds.xls', sep=""), sheet='birds')
				birds=birds[which(!is.na(birds$tag_ID)),]	
			}			
			{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
			}
		}
   		{# extract and plot
		   for (i in 1:nrow(nests_)) {# 66:67) { #(1:nrow(nests_))) {#
			#for (i in c(25, 55,65)) {
				nest=nests_$nest[i]
				yr= nests_$year[i]
				act_ID=nests_$act_ID[i]
		
				on_=nests_$on[i]
				off_=nests_$off[i]
				start_=nests_$start[i]
				end_=nests_$end[i]
				inc_start=s$inc_start[s$nest==nest & s$year==yr]
				ip_=ip$inc_period[ip$sp==nests_$sp[i]]
				
								
				print(paste(nest, i,sep=" "))
				if(nest%in%c('ka1411_is2014','re133_nl2014','re213_nl2014')){
					a=RFID.temperature_actogram_data_non_barrow(con = conMy, nest = nests_$nest[i],yr = nests_$year[i], start_ = on_, end_ = off_+60) 
					if(nest=='ka1411_is2014'){a$disturb[1]='capture'}	
					}else{
					a = RFID.temperature_actogram_data(con = conMy, nest = nests_$nest[i],yr = nests_$year[i], start_ = on_, end_ = off_+60)
					}
						
				{# extratction of incubation/non-incubation based on nest and surface temperature
						
						b=a[which(!is.na(a$t_nest)),]
						
						b$t_nest_prior=c(b$t_nest[1],b$t_nest[-nrow(b)])
						b$t_difference = as.numeric(b$t_nest - b$t_nest_prior) 
						
						b$t_nest_run_med=rollmedian(b$t_nest, 17279 , fill="extend") # 1/2 day 8639, whole day 17279
							
						b$t_diff_run=rollmean(b$t_difference, 24, fill="extend") #2 minutes (for median neads to be odd)	
						
						b$inc_t = 	ifelse(is.na(b$t_surface),
											(ifelse(b$t_nest_run_med<20,
												(ifelse(b$t_nest > b$t_nest_run_med+3, 1, 0)),#))
												ifelse(b$t_nest > b$t_nest_run_med-3, 1,0))),#))
											ifelse(b$t_nest_run_med<20,
												(ifelse(b$t_nest > b$t_nest_run_med+3, 1, 
													ifelse(b$t_nest > b$t_surface+12.5, 1,0))),#))
												ifelse(b$t_nest > b$t_nest_run_med-3, 1, 
													ifelse(b$t_nest > b$t_surface+12.5, 1,0))))#))
						
						
						b$inc_t = 	ifelse(is.na(b$t_surface),
											(ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run < 0, 0,
													ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run > 0, 
														1, b$inc_t))),
										(ifelse(b$t_nest<20 &(b$t_surface-abs(b$t_nest))>(-3) & b$t_nest<b$t_nest_run_med+3, b$inc_t,
												(ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run < 0, 0,
												ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run > 0,1,
																	b$inc_t)))))	)	
						
						a$inc=b$inc_t[match(a$pk, b$pk)]
					
				}
				{# corrections
					if(nest=='a802'){ a$inc=ifelse(a$datetime_>as.POSIXct('2012-07-15 12:59:30') & a$datetime_<as.POSIXct('2012-07-15 17:13:30'), 0,a$inc)}
					if(nest=='a305'){ a$inc=ifelse(a$datetime_>as.POSIXct('2012-07-15 16:40:250'), 1,
											ifelse(a$datetime_>as.POSIXct('2012-07-15 08:12:05') & a$datetime_<as.POSIXct('2012-07-15 16:39:25'),0,a$inc))}
					if(nest=='a802'){ a$inc=ifelse(a$datetime_>as.POSIXct('2012-07-15 12:59:30') & a$datetime_<as.POSIXct('2012-07-15 17:13:30'), 0,a$inc)}
					if(nest=='s609'){ a$inc=ifelse(a$datetime_>as.POSIXct('2012-07-05 13:31:25') & a$datetime_<as.POSIXct('2012-07-05 20:33:30'), 0,a$inc)}
					if(nest=='s708'){ a$inc=ifelse(a$datetime_>as.POSIXct('2012-06-26 12:00:00'), 0,a$inc)}
					if(nest=='s804'){ a$inc=ifelse(a$datetime_>as.POSIXct('2012-06-22 23:00:00') & a$datetime_<as.POSIXct('2012-06-24 01:00:00'), 0,a$inc)}
					if(nest=='w505'){ a$inc=ifelse(a$datetime_>as.POSIXct('2013-07-06 13:17:45') & a$datetime_<as.POSIXct('2013-07-06 15:08:05'), 0,a$inc)}
					
				}
				{# define uniparental and biparental periods, no = not used
				   tst=ifelse(nests_$state[nests_$nest==nest]=='s',6, ifelse(nests_$state[nests_$nest==nest]%in%c('h','l'),24,0)) # if start of hatching then end -6h, if hatching or left nest that -24h, else 0
				   tst2=ifelse(nests_$circumstances[nests_$nest==nest]%in%c('bpd','catching','removal','temporal'),nests_$bout[nests_$nest==nest],0) # start of uniparental after median bout of the species
				   if(nests_$one_since[i]=='always'){ # for nests that were uniparental ever since we have recorded 
										a$type=ifelse(a$datetime_>=nests_$start[nests_$nest==nest & nests_$year==yr] & a$datetime_<nests_$end[nests_$nest==nest & nests_$year==yr]-tst*60*60,'uni','no') 
													}
				   if(nests_$one_since[i]=='unknown'){ # for nests that were uniparental ever since we have recorded 
										a$type=ifelse(a$datetime_>=nests_$start[nests_$nest==nest & nests_$year==yr] & a$datetime_<nests_$end[nests_$nest==nest & nests_$year==yr]-tst*60*60,'uni',
										ifelse(a$datetime_>=nests_$on[nests_$nest==nest & nests_$year==yr]+60*60*2 & a$datetime_<nests_$start[nests_$nest==nest & nests_$year==yr],'bip','no')) 
													}
				   if(!nests_$one_since[i]%in%c('unknown','always')){
									a$type=ifelse(a$datetime_>=nests_$on[nests_$nest==nest & nests_$year==yr]+60*60*2 & a$datetime_<nests_$one_since[nests_$nest==nest & nests_$year==yr],'bip', # biparental incubation from 2 hours after equipment was placed until a parent deserted (after catching or naturally)
												ifelse(a$datetime_>=nests_$start[nests_$nest==nest & nests_$year==yr]+tst2*60*60 & a$datetime_<nests_$end[nests_$nest==nest & nests_$year==yr]-tst*60*60,'uni', # uniparental incubation from the time the widowed bird arrived to the nest until the end of incubation
												'no'))			
													}
				   
				   if(nrow(a[a$type=='uni',])>0){
				   if(as.numeric(difftime(max(a$datetime_[a$type=='uni']),min(a$datetime_[a$type=='uni']),units='hours'))<=2*nests_$bout[nests_$nest==nest]){a$type[a$type=='uni']='no'} # excludes nests with unip inc shorter than 2 med bouts of population
				   }
				   
				   if(length(nests$circumstances[which(nests$nest==nest & nests$circumstances=='temporal')])==1){ # for nests witho temporal desertion present 
										a$type=ifelse(a$datetime_>=nests$start[nests$nest==nest & nests$year==yr & nests$circumstances=='temporal']+tst2*60*60 & a$datetime_<nests$end[nests$nest==nest & nests$year==yr & nests$circumstances=='temporal'],'uni',a$type) 
										}
					a$type=ifelse(a$datetime_<inc_start,'no',a$type)
					if(nest%in%c('s815','s704')){a$type='no'}
					if(nest=='s804'){a$type=ifelse(a$datetime_<as.POSIXct('2012-06-22 20:25:00',tz='UTC') | a$datetime_>as.POSIXct('2012-07-03 09:25:05',tz='UTC'), a$type, 'no')}
					if(nest=='w504'){a$type=ifelse(a$datetime_>as.POSIXct('2013-07-09 10:08:05',tz='UTC'), a$type, 'no')}
				}
				
				{# add variables
					a$nest=nests_$nest[i]
					a$sp=nests_$sp[i]
					a$year=nests_$year[i]
					a$site=nests_$site[i]
					a$who=tolower(a$who)
					a$datetime_z=a$datetime_
					a$datetime_=  as.character(as.POSIXct(a$datetime_z)+ (nests_$local_plus[i]*60*60)) # bring to longitudanal time
						birds_=birds[paste(birds$year_, birds$nest)==paste( nests_$year[i], nests_$nest[i]),]
					
					a$bird_ID=birds_$bird_ID[match(a$who, birds_$sex)]
					a$signal=NA
					a$sys='biparental' # breeding system
					a$act_ID=act_ID
				}	
				
				aa=a[,c('act_ID','sys','sp','site','year','t_method','nest','bird_ID','tag','who','datetime_','disturb','t_surface','t_nest','signal','inc','type')] # state not used
				#aa=a[,c('act_ID','sys','sp','site','year','t_method','nest','bird_ID','tag','who','datetime_z','datetime_','disturb','t_surface','t_nest','signal','inc','type')] # state not used
							
				{# load to database				
				
					conLite = dbConnect(dbDriver("SQLite"),dbname = db)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",act_ID))
					dbWriteTable(conn=conLite, name = act_ID, value = aa,row.names = FALSE)
					#dbWriteTable(conn=conLite, name = paste(wd3,nest,".txt",sep=""), value = aa,row.names = FALSE)
					#dbq(conLite,"CREATE INDEX act_ID ON rfid (act_ID)")
					#}else{dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE, append=TRUE)}
					dbDisconnect(conLite)
				}
				{# save as txt
				  write.table(aa, paste(wd3,nest,".txt",sep=""), sep=",", row.names=FALSE)
				}
				
				{# plot
						# prepare for plotting
									#conLite = dbConnect(dbDriver("SQLite"),dbname = db)
									#dfr_=dbq(conLite,paste("SELECT*FROM", nest))
									dfr_=aa[which(!is.na(aa$datetime_)),]
									dfr=dfr_
								
								# define captions and nest location
									figCap_=data.frame(sp=dfr$sp[1],scinam=sp$scinam[match(dfr$sp[1], sp$sp)], species=sp$species[match(dfr$sp[1], sp$sp)], year=dfr$year[1],nest=dfr$nest[1],site=dfr$site[1], ID=dfr$act_ID[i],stringsAsFactors=FALSE)
									figCap=figCap_
								
									latlon_=data.frame(lat=nests_$lat[nests_$nest==nest],lon=nests_$lon[nests_$nest==nest],stringsAsFactors=FALSE) # for the map
									latlon=latlon_
								
								# define incubation start and end
									inc_start=s$inc_start[s$nest==nest & s$year==yr]
									ip_=ip$inc_period[ip$sp==nests_$sp[i]]
								# generate actograms
									RFID.temperature_actogram(dfr=dfr_,type="PNG", UTC=TRUE,UTC_met=TRUE,day=FALSE) #type="SAVE")# 
				}
				print(paste(nest, i,sep=" "))
				}
		} 
	}
	
	{# load metadata into DB
			{# nests to extract the data for
				nests=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='all_lon_time',colTypes = c(
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
				
					conLite = dbConnect(dbDriver("SQLite"),dbname = db2)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",'nests'))
					dbWriteTable(conn=conLite, name = 'nests', value = nests,row.names = FALSE)
					dbDisconnect(conLite)
					
					write.csv(nests, file = paste(wd,"nests.csv", sep=""),row.names = FALSE)
				}
			{# incubation start
				 s=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='inc_start_lon_time',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING))
				
					conLite = dbConnect(dbDriver("SQLite"),dbname = db2)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",'inc_start'))
					dbWriteTable(conn=conLite, name = 'inc_start', value = s,row.names = FALSE)
					dbDisconnect(conLite)	
					
					write.csv(s, file = paste(wd,"inc_start.csv", sep=""),row.names = FALSE)
			}
			{# incubation period
				 ip=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='incubation_period',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING
									))
					conLite = dbConnect(dbDriver("SQLite"),dbname = db2)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",'inc_period'))
					dbWriteTable(conn=conLite, name = 'inc_period', value = ip,row.names = FALSE)
					dbDisconnect(conLite)		

					write.csv(ip, file = paste(wd,"inc_period.csv", sep=""),row.names = FALSE)
			}
			{# end states
				  n=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='nest_states_lon_time',colTypes = c(
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING))
			
					conLite = dbConnect(dbDriver("SQLite"),dbname = db2)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",'end_states'))
					dbWriteTable(conn=conLite, name = 'end_states', value = n,row.names = FALSE)
					dbDisconnect(conLite)
					
					write.csv(n, file = paste(wd,"end_states.csv", sep=""),row.names = FALSE)
			}
			{# birds
				birds=readWorksheetFromFile(paste(wd,'birds.xls', sep=""), sheet='birds')
					birds$datetime_=NULL
					conLite = dbConnect(dbDriver("SQLite"),dbname = db2)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",'birds'))
					dbWriteTable(conn=conLite, name = 'birds', value = birds,row.names = FALSE)
					dbDisconnect(conLite)	
					
					write.csv(birds, file = paste(wd,"birds.csv", sep=""),row.names = FALSE)
				
			}			
			{# species
				sp =read.csv(paste(wd,'species.csv', sep=""), stringsAsFactors=FALSE)
					conLite = dbConnect(dbDriver("SQLite"),dbname = db2)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",'species'))
					dbWriteTable(conn=conLite, name = 'species', value = sp,row.names = FALSE)
					dbDisconnect(conLite)
					
					#write.csv(sp, file = paste(wd,"species.csv", sep=""),row.names = FALSE)
			}
			{# populations
				p=readWorksheetFromFile(paste(wd,'populations.xls', sep=""), sheet='populations')
					conLite = dbConnect(dbDriver("SQLite"),dbname = db2)
					dbq(conLite,paste( "DROP TABLE IF EXISTS",'populations'))
					dbWriteTable(conn=conLite, name = 'populations', value = p,row.names = FALSE)
					dbDisconnect(conLite)
					
					write.csv(p, file = paste(wd,"populations.csv", sep=""),row.names = FALSE)
			}
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

{# create datasets
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
			se$day_j=as.numeric(format(se$day ,"%j")) - as.numeric(format(as.Date(trunc(se$inc_start, "day")),"%j"))+1
			
			se$inc_per_sp=ip$inc_period[match(se$sp,ip$sp)]
			se$prop_ip=se$day_j/se$inc_per_sp
	}
		
	save(d,h,se,file=paste(wd,'for_analyses.RData',sep="")) # d - per day aggregates, h per hour aggregates, se - start and end of unip incubation
	}
	{# check whether number of 5s readings per day is not higher than it should be - 60*60*24/5
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
					
						aa=
						a$datetime_=as.POSIXct(a$datetime_)
						a$datetime_after=c(a$datetime_[-1], a$datetime_[nrow(a)]+5)	
						a$dif=as.character(difftime(a$datetime_after,a$datetime_, 'secs'))
						head(a[a$dif!=5,])
						nrow(a[a$dif!=5,])
						}
}
  
}


{# DONE METADATA PREPARATION 			
{# create datafile with on_nest and off_nest - DONE
		# load metadata
		m=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='Sheet1',colTypes = c(XLC$DATA_TYPE.NUMERIC,
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
		{# 2011
		x =  dbq(con, "SELECT lower(a.nest) nest, a.year_, min(datetime_onNest) start, b.species 
							FROM LOGGERSatBARROW.Device_filestatus a
						LEFT JOIN
							(SELECT nest, lower(species) species FROM AVESatBARROW.NESTS where year_ = 2011) b
								on a.nest = b.nest							
								WHERE a.year_= 2011 and a.nest in 
									('a301','a302','b301','b501','s602')
									GROUP BY a.nest ")
		x$stop=m$end[match(paste(x$year_, x$nest,x$species), paste(m$year,m$nest, m$sp))]							
		}
		{# 2012
			y =  dbq(con, "SELECT lower(a.nest) nest, a.year_, min(datetime_onNest) start, b.stop, b.species 
							FROM LOGGERSatBARROW.Device_filestatus a
						LEFT JOIN
							(SELECT nest, nest_endstate_date stop, lower(species) species FROM AVESatBARROW.NESTS where year_ = 2012) b
								on a.nest = b.nest							
								WHERE a.nest in 	('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902')
								and a.file_ID NOT in (SELECT file_ID FROM 	LOGGERSatBARROW.Device_filestatus where year_=2012 and remarks is not NULL)
								and a.year_=2012
								GROUP BY a.nest
										")
										
			y[y$nest=='b303',]$stop= '2012-07-12 07:00:00'
			y[y$nest=='b304',]$stop= '2012-07-15 12:00:00'
			y[y$nest=='b402',]$stop= '2012-07-13 07:39:00'
			y[y$nest=='b403',]$stop= '2012-07-19 08:06:00'
			y[y$nest=='s201',]$stop= '2012-07-03 10:10:00'
			y[y$nest=='s313',]$stop= '2012-07-13 16:15:00'
			y[y$nest=='s502',]$stop= '2012-07-04 13:30:00'
			y[y$nest=='s509',]$stop= '2012-06-28 16:00:00'
			y[y$nest=='s716',]$stop= '2012-07-08 08:45:00'
			
			
			y[y$nest=='l302',]$start= '2012-07-08 12:09:00'
			}
		{# 2013	
				uu=readWorksheetFromFile(paste(wd,'endstates_actogram_based_2016-06-15.xls', sep=""), sheet="temp")
				uu=uu[!is.na(uu$barr_time),2:5]
				uu$bird_incubate=NULL
				uu$nest=tolower(uu$nest)
				uu$datetime_=as.POSIXct(strptime(uu$datetime_,'%Y-%b-%d %H:%M:%S'))
				#unique(uu$state)
						ui=uu[uu$state=="fl",] #3
						u2=uu[uu$state=="hd" & !uu$nest%in%ui$nest,] #7
						u3=uu[-which(uu$state%in%c("fl","hd") | uu$nest%in%c(ui$nest,u2$nest)),]
						u3i=ddply(u3,. (nest),summarise, datetime_=max(datetime_))
						u3i$state=u3$state[match(paste(u3i$nest,u3i$datetime_),paste(u3$nest,u3$datetime_))]
						
				u=rbind(ui,u2,u3i)	
				u=u[order(u$nest),]
					nrow(u)
				colnames(u)[colnames(u)=='datetime_']='stop'
						
				w=dbq(con, "select datetime_ , nest, RFID_ID, RFID FROM barrow_2013.visits where RFID = 'on' and nest in ('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804')")
				
				mm=ddply(w,. (nest),summarise, start=min(datetime_))
				
				w2=dbq(con, "select datetime_ , nest, RFID_ID, RFID FROM barrow_2013.visits where RFID = 'off' and nest in ('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804')")
				mm2=ddply(w2,. (nest),summarise, stop=max(datetime_))
				
				z=merge(mm,u, by='nest',all.x=TRUE)
				z$species=ifelse(substring(z$nest,1,1)=='s','sesa',ifelse(substring(z$nest,1,1)=='l','lbdo',ifelse(substring(z$nest,1,1)=='w','wesa',NA)))
				#z[z$start>z$stop,]
				z$year_=2013	
				
				z$stop2=mm2$stop[match(z$nest, mm2$nest)]
				z$stop=ifelse(is.na(z$stop), as.character(z$stop2),as.character(z$stop))
				z=z[,c('year_','species','nest', 'start','stop')] #
				
				z[z$nest=='w509',]$start= '2013-07-01 03:01:00'
				
				z[z$nest=='l501',]$stop= '2013-07-08 06:40:00'
				z[z$nest=='l502',]$stop= '2013-07-09 11:30:00'
				z[z$nest=='l801',]$stop= '2013-07-03 02:20:00'
				z[z$nest=='s520',]$stop= '2013-07-08 18:00:00'
				z[z$nest=='s627',]$stop= '2013-07-01 02:30:00'
				z[z$nest=='w505',]$stop= '2013-07-06 15:55:00'
				z[z$nest=='w509',]$stop= '2013-07-07 16:00:00'
				z[z$nest=='w511',]$stop= '2013-07-11 02:30:00'
				z[z$nest=='w702',]$stop= '2013-07-08 01:30:00'
				z[z$nest=='w804',]$stop= '2013-07-10 06:55:00'
		
		}	
		{# combine
			se=rbind(x,y,z)
			m$on=se$start[match( paste(m$year,m$nest, m$sp), paste(se$year_, se$nest,se$species))]
			m$off=se$stop[match( paste(m$year,m$nest, m$sp), paste(se$year_, se$nest,se$species))]
			
			writeWorksheetToFile(m,file=paste(wd,'nests.xls',sep=""),sheet='new')
		}
}
{# create datafile with birds - DONE
	{# 2011
	xf =  dbq(con, "SELECT a.year_, lower(a.species) sp, lower(a.nest) nest, IDfemale bird_ID, b.tag_ID FROM AVESatBARROW.NESTS a
						LEFT JOIN
							(SELECT ID, tag_ID FROM AVESatBARROW.captures where tag_ID is not null) b
								on a.IDfemale = b.ID							
								WHERE a.year_= 2011 and a.nest in 
									('a301','a302','b301','b501','s602')
									GROUP BY a.nest ")
	xf$sex='f'
	xm =  dbq(con, "SELECT a.year_, lower(a.species) sp, lower(a.nest) nest, IDmale bird_ID, b.tag_ID FROM AVESatBARROW.NESTS a
						LEFT JOIN
							(SELECT ID, tag_ID FROM AVESatBARROW.captures where tag_ID is not null) b
								on a.IDmale = b.ID							
								WHERE a.year_= 2011 and a.nest in 
									('a301','a302','b301','b501','s602')
									GROUP BY a.nest ")	
	xm$sex='m'	
	x=rbind(xf,xm)
	}
	{# 2012
	yf =  dbq(con, "SELECT a.year_, lower(a.species) sp, lower(a.nest) nest, IDfemale bird_ID, b.tag_ID FROM AVESatBARROW.NESTS a
						LEFT JOIN
							(SELECT ID, tag_ID FROM AVESatBARROW.captures where tag_ID is not null) b
								on a.IDfemale = b.ID							
								WHERE a.year_= 2012 and a.nest in 
									('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902')
									GROUP BY a.nest ")
	yf$sex='f'
	ym =  dbq(con, "SELECT a.year_, lower(a.species) sp, lower(a.nest) nest, IDmale bird_ID, b.tag_ID FROM AVESatBARROW.NESTS a
						LEFT JOIN
							(SELECT ID, tag_ID FROM AVESatBARROW.captures where tag_ID is not null) b
								on a.IDmale = b.ID							
								WHERE a.year_= 2012 and a.nest in 
									('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902')
									GROUP BY a.nest ")	
	ym$sex='m'	
	y=rbind(yf,ym)
	}
	{# 2013
	zf =  dbq(con, "SELECT a.year_, lower(a.species) sp, lower(a.nest) nest, IDfemale bird_ID, b.tag_ID FROM AVESatBARROW.NESTS a
						LEFT JOIN
							(SELECT ID, tag_ID FROM AVESatBARROW.captures where tag_ID is not null) b
								on a.IDfemale = b.ID							
								WHERE a.year_= 2013 and a.nest in 
									('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804')
									GROUP BY a.nest ")
	zf$sex='f'
	zm =  dbq(con, "SELECT a.year_, lower(a.species) sp, lower(a.nest) nest, IDmale bird_ID, b.tag_ID FROM AVESatBARROW.NESTS a
						LEFT JOIN
							(SELECT ID, tag_ID FROM AVESatBARROW.captures where tag_ID is not null) b
								on a.IDmale = b.ID							
								WHERE a.year_= 2013 and a.nest in 
									('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804')
									GROUP BY a.nest ")	
	zm$sex='m'	
	z=rbind(zf,zm)
	}
		{# combine
			birds=rbind(x,y,z)
			
			writeWorksheetToFile(birds,file=paste(wd,'birds.xls',sep=""),sheet='birds')
		}
}	
{# add latitude and longitude to nest table - DONE
	m=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='Sheet1',colTypes = c(XLC$DATA_TYPE.NUMERIC,
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
	x =  dbq(con, "SELECT lower(nest) nest, year_, latit as lat, longit as lon FROM AVESatBARROW.NESTS where year_ = 2011 and nest in ('a301','a302','b301','b501','s602')
					UNION SELECT lower(nest) nest, year_, latit as lat, longit as lon FROM AVESatBARROW.NESTS where year_ = 2012 and nest in ('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902')
					UNION SELECT lower(nest) nest, year_, latit as lat, longit as lon FROM AVESatBARROW.NESTS where year_ = 2013 and nest in ('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804')
					UNION SELECT lower(nest) nest, year_, latit as lat, longit as lon FROM extra_incubation.nests where nest in ('ka1411_is2014','re133_nl2014','re213_nl2014')
					")		
	m$lat=x$lat[match(m$nest,x$nest)]
	m$lon=x$lon[match(m$nest,x$nest)]
	m$utc_plus=ifelse(m$site=="isla",0, 
									ifelse(m$site=="czrp", -2,
										ifelse(m$site== "neth" , -2,
											ifelse(m$site== "frie" , -2,
												ifelse(m$site== "barr" , +8,
												ifelse(m$site== "hofo", +2, NA)))))) 
	
	m$local_plus=m$utc_plus+m$lon/15 # amount of time (hours) used to transform the time to local time based on latitude (was calculated as amount of time to bring the times to UTC + longitue/15
	
	m$utc_plus=NULL
	writeWorksheetToFile(m,file=paste(wd,'nests.xls',sep=""),sheet='new2')
	
}
{# check which nests have more than one temperature logger
	{# 2011
	xm =  dbq(con, "SELECT distinct device, lower(nest) nest FROM loggersatbarrow.device_filestatus 
							WHERE year_= 2011 and nest in ('a301','a302','b301','b501','s602') and device = 'MSR'
									 ")
	xt =  dbq(con, "SELECT distinct device as tiny, lower(nest) nest FROM loggersatbarrow.device_filestatus 
							WHERE year_= 2011 and nest in ('a301','a302','b301','b501','s602') and device = 'TinyTag'
									 ")
	merge(xm,xt,all=TRUE)								 
	}
	{# 2012
	xm =  dbq(con, "SELECT distinct device, lower(nest) nest FROM loggersatbarrow.device_filestatus 
							WHERE year_= 2012 and nest in ('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902') and device = 'MSR'
									 ")
	xt =  dbq(con, "SELECT distinct device as tiny, lower(nest) nest FROM loggersatbarrow.device_filestatus 
							WHERE year_= 2012 and nest in ('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902') and device = 'TinyTag'
									 ")
	x=merge(xm,xt,all=TRUE)	
	x[!is.na(x$device) & ! is.na(x$tiny),]	
	}
	{# 2013
	xm =  dbq(con, "SELECT distinct device, lower(nest) nest FROM loggersatbarrow.device_filestatus 
							WHERE year_= 2013 and nest in ('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804') and device = 'MSR'
									 ")
	xt =  dbq(con, "SELECT distinct device as tiny, lower(nest) nest FROM loggersatbarrow.device_filestatus 
							WHERE year_= 2013 and nest in ('l501','l502','l801','l805','s409','s510','s516','s520','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804') and device = 'TinyTag'
									 ")
	x=merge(xm,xt,all=TRUE)	
	x[!is.na(x$device) & ! is.na(x$tiny),]	
	}
}

{# add bout length to populations
	
	load("C:\\Users\\mbulla\\Documents\\Dropbox\\Science\\Projects\\MS\\Comparative\\Data_Scripts\\Supplementary\\Data\\comparative_all.Rdata")
	p=readWorksheetFromFile(paste(wd,'populations.xls', sep=""), sheet='Sheet1')
	p$sp=tolower(p$sp)
	zz$sp=tolower(zz$sp)
	p$bout=zz$med_[match(paste(p$site_abbreviation, p$sp),paste(zz$breeding_site, zz$sp))]
	writeWorksheetToFile(p,file=paste(wd,'populations.xls',sep=""),sheet='populations')
}

{# inc_start
	{# a
		m=readWorksheetFromFile(paste(wd,'nests.xls', sep=""),sheet='inc_start')
		load("M:\\Science\\Projects\\PHD\\STATS\\DATASETS\\inc_start_est_2011_SESA_NON-SESA.Rdata")
		e$year=2011
		e1=e
		load("M:\\Science\\Projects\\PHD\\STATS\\DATASETS\\inc_start_est_2012_SESA_NON-SESA.Rdata")
		e$year=2012
		e2=e
		load("M:\\Science\\Projects\\PHD\\STATS\\DATASETS\\inc_start_est_2013_SESA_NON-SESA.Rdata")
		e$year=2013
		e=rbind(e1,e2,e)
		m$inc_start_=e$inc_est[match(tolower(paste(m$year,m$nest)), tolower(paste(e$year,e$nest)))]
		m$method_=e$method[match(tolower(paste(m$year,m$nest)), tolower(paste(e$year,e$nest)))]
		m$inc_start_
			writeWorksheetToFile(m,file=paste(wd,'nests.xls',sep=""),sheet='inc_start_new')
		
	}	
	{#b m=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='original')
	
	load("C:\\Users\\mbulla\\Documents\\Dropbox\\Science\\Projects\\MS\\Bip_to_uni\\Data\\inc_start_estimation_2012_new_2016-02-25.Rdata")
	e$nest=tolower(e$nest)
	e$year=2012
	e$inc_start=e$inc_est
	e1=e[c('nest','year', 'inc_start')]
	load("C:\\Users\\mbulla\\Documents\\Dropbox\\Science\\Projects\\MS\\Bip_to_uni\\Data\\inc_start_estimation_2013_new.Rdata")
	e$nest=tolower(e$nest)
	e$year=2013
	e2=e[c('nest','year', 'inc_start')]
	e2$inc_start=as.character(e2$inc_start)
	e=rbind(e1,e2)
	m$inc_start=e$inc_start[match(paste(m$year,m$nest), paste(e$year,e$nest))]
	
	m2=m[is.na(m$inc_start),]
	m2$inc_start=as.character(a$incubation_start_datetime[match(paste(m2$year,m2$nest),tolower(paste(a$year,a$nest)))])
	
	m2[,c('year','nest','inc_start')]
 
	m$inc_start[is.na(m$inc_start)]=m2$inc_start[match(paste(m$year[is.na(m$inc_start)],m$nest[is.na(m$inc_start)]),tolower(paste(m2$year,m2$nest)))]

	writeWorksheetToFile(m,file=paste(wd,'nests.xls',sep=""),sheet='inc_start_')
	}
}

{# old pesa
load(paste(wd,'pesa_nests.RData',sep=""))
			nests_$on=NA
			nests_$off=NA
			nests_$end=NA
			nests_$start=NA
			#nrow(nests_)	
			nests_$temp[is.na(nests_$nest)]= 1:length(nests_$nest[is.na(nests_$nest)])
			nests_$nest[is.na(nests_$nest)]=as.character(ifelse(nchar(nests_$temp[is.na(nests_$nest)])==1, paste('P00',nests_$temp[is.na(nests_$nest)],sep=""), paste('P0',nests_$temp[is.na(nests_$nest)],sep="")))
			#unique(nests_$nest)
			nests_=nests_[order(nests_$year_,nests_$tag_ID),]
			nests_$local_plus=8+nests_$lon/15 
			nests_$sp='pesa'
			nests_$nest=tolower(nests_$nest)
			nests_=nests_[nests_$nest%in%c('p304','p302','p202','p101','p801','p301','p205','p802','p301','p202','p201','p309'),] #'p203' not used, second, clutch, deserted and not clear when + readings are funny
			
			nests_$nest=paste(nests_$year_, nests_$nest,sep="_")
			nests_=nests_[order(nests_$nest),]
				
				nests_$state[nests_$nest=='p101']='tag_lost'
				nests_$state[nests_$nest=='p201']='tag_lost' 
				nests_$state[nests_$nest=='p301']='tag_lost' 
				nests_$state[nests_$nest=='p309']='tag_lost'  # vypada jak sesa
				
				nests_$state[nests_$nest=='p101']='' # 	?p203
				nests_$state[nests_$nest=='p202']='tag_lost' 
				nests_$state[nests_$nest=='p203']='tag_lost' # 	?p203
				nests_$state[nests_$nest=='p205']='' 
				nests_$state[nests_$nest=='p301']='tag_lost' # only three days
				nests_$state[nests_$nest=='p302']='tag_lost'
				nests_$state[nests_$nest=='p304']='tag_lost' # capture
				nests_$state[nests_$nest=='p801']='tag_lost'
				nests_$state[nests_$nest=='p802']='' 
				
				
				
				
				
			
				nests_$state[nests_$nest=='p304']='capture'  # vypada jak sesa
			
				nests_$end[nests_$nest=='p202']='2008-06-28 11:00:00'


}

{# bring biparental metadata to longitudinal time
			
			{# nests 
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
				nests$one_since=as.POSIXct(nests$one_since)
				n=nests[!nests$sp%in%c('rnph','pesa'),]
				n$on=  as.character(as.POSIXct(n$on)+ (n$local_plus*60*60)) # bring to longitudanal time
				n$off=  as.character(as.POSIXct(n$off)+ (n$local_plus*60*60)) # bring to longitudanal time
				n$end=  as.character(as.POSIXct(n$end)+ (n$local_plus*60*60)) # bring to longitudanal time
				n$start=  as.character(as.POSIXct(n$start)+ (n$local_plus*60*60)) # bring to longitudanal time
				n$one_since[!n$one_since%in%c('always','unknown')]=as.character(as.POSIXct(n$one_since[!n$one_since%in%c('always','unknown')])+ (n$local_plus[!n$one_since%in%c('always','unknown')]*60*60)) # bring to longitudanal time
				n2=nests[nests$sp%in%c('rnph','pesa'),]
				
				writeWorksheetToFile(rbind(n,n2),file=paste(wd,'nests.xls',sep=""),sheet='all_lon_time')
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
				s$local_plus=n$local_plus[match(s$nest,n$nest)]
				s$inc_start[!s$sp%in%c('rnph','pesa')]=as.character(as.POSIXct(s$inc_start[!s$sp%in%c('rnph','pesa')])+ s$local_plus[!s$sp%in%c('rnph','pesa')]*60*60)
				s$laying_date[!s$sp%in%c('rnph','pesa')]=as.character(as.POSIXct(s$laying_date[!s$sp%in%c('rnph','pesa')])+ s$local_plus[!s$sp%in%c('rnph','pesa')]*60*60)
				s$local_plus=NULL
				writeWorksheetToFile(s,file=paste(wd,'nests.xls',sep=""),sheet='inc_start_lon_time')
			}
			{# nest states
				 s=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='nest_states',colTypes = c(
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING,
									 XLC$DATA_TYPE.STRING))
				s$local_plus=n$local_plus[match(s$nest,n$nest)]
				s$datetime_[!s$sp%in%c('rnph','pesa')]=as.character(as.POSIXct(s$datetime_[!s$sp%in%c('rnph','pesa')])+ s$local_plus[!s$sp%in%c('rnph','pesa')]*60*60)
				s$local_plus=NULL
				writeWorksheetToFile(s,file=paste(wd,'nests.xls',sep=""),sheet='nest_states_lon_time')
			}
		}
 } 		

