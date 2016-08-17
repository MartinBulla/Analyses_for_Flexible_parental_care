{# RFID
		
	# who:	identifies the RFID (f = female, m = male, misread = someone on the nest, but tag wrongly read, fieldteam = fiel worker at the nest, NA - not applicable, as the reading refers to temperature)
	{# MPIO data - MSR 30s aggregate
		{# Barrow 
		  {# define constants
			# define database
				#con = dbcon(user='mbulla',host='scidb.orn.mpg.de', password='Kvetak25',database='LOGGERSatBARROW')
				conMy = dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='LOGGERSatBARROW')
								
			# define nests for actos
				nests=d[!duplicated(paste(d$year,d$sp,d$nest)) & d$breeding_site=="barr" & d$method%in%c("rfid+msr","rfid","rfid+tt"),c('year','sp','nest','nn','breeding_site','lon')]
				nests$nest=tolower(nests$nest)
				# set minimum and maximum time for which to use the RFID data (first egg -1 and end_date+1 day)
				nn=n[n$breeding_site=="barr" & n$year>2010,]
				nests$start_=nn$first_egg_laid[match(tolower(paste(nests$year,nests$sp,nests$nest)),tolower(paste(nn$year,nn$sp,nn$nest)))]-1*24*60*60
				nests$end_=nn$end_datetime[match(tolower(paste(nests$year,nests$sp,nests$nest)),tolower(paste(nn$year,nn$sp,nn$nest)))]+1*24*60*60
				
				nests$pk=c(1:nrow(nests))
				nests$utc_plus=ifelse(nests$breeding_site=="isla",0, 
									ifelse(nests$breeding_site=="czrp", -2,
										ifelse(nests$breeding_site== "neth" , -2,
											ifelse(nests$breeding_site== "frie" , -2,
												ifelse(nests$breeding_site== "barr" , +8,
												ifelse(nests$breeding_site== "hofo", +2, NA)))))) 
				nests[is.na(nests$end_),]
				nests[is.na(nests$start_),]
				
				# brings in also birds that are not in the extracted dataset, but are in the raw data
					bird1=d[!duplicated(paste(d$year,d$sp,d$nest, d$logger, d$sex)) & d$breeding_site=="barr" & d$method%in%c("rfid+msr","rfid","rfid+tt"),c('year','sp','nest','sex','lon',"logger")]
					bird2=bb[bb$method=="pit_tag" ,c('year', 'sp','nest','sex','lon','logger')]
					bird=rbind(bird1,bird2)
					bird$logger=tolower(bird$logger)
					bird=bird[!duplicated(paste(bird$year,bird$sp,bird$nest, bird$logger, bird$sex)),]
				
					
				
		  }			
		  {# data extraction functions
				RFID.temperature_actogram_data = function (con, nest, yr, start_,end_) {
			#on.exit(mysqlCloseConnection(con))
			  	nest_l=tolower(nest)
				nest_ =shQuote(tolower(nest))
				
				rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, coalesce(who, a.sex) who from
											(select  distinct  year_,lower(r.nest) nest, FUNCTIONS.RoundTime(COALESCE(datetime_corrected,datetime_),'S',5) datetime_,
												transp, NULL who
												from RFID r
												where transp IS NOT NULL AND lower(r.nest) =",nest_,
											" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'S',5) datetime_,
												NULL transp, 'hatching start' who
											 	from AVESatBARROW.NESTS where lower(nest) = ",nest_,
											" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5)  datetime_,
												NULL transp, 'nest visit' who
												from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,") x  
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
				
				rfids$who[which(rfids$who%in%c("m","f") & !tolower(rfids$transp)%in%tolower(bird$logger[which(tolower(paste(bird$year,bird$nest))==tolower(paste(nests$year[i],nests$nest[i])))]))] = 'stranger'
		
				#rfids$act[which(rfids$who=="female")] = 15
			   # dfr$act[which(dfr$who=="female")] = 15
			rfids$t_method=NA
			
			tts   = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, 
												T_egg as t_nest_tt,'tt' as t_method
										 from TinyTag
										 where T_egg is not NULL and T_egg > -20 and lower(nest) =",nest_," and year_ =",yr))
			
			if(yr==2011){
				hobos   = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, 
												T_ambient as t_surface,'hobo' as 't_method'
										 from HOBO
										 where lower(nest) =",nest_))
						}else{hobos=data.frame(nest=NULL, datetime_=NULL, t_surface=NULL, t_method=NULL)}				 
			
			MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',30)  datetime_,
			                                    T_egg as t_nest_msr,T_ambient as t_surface,'msr' as 't_method'
										 from MSR
										 where lower(nest) =",nest_,"and year_ =",yr,"group by FUNCTIONS.RoundTime(datetime_,'S',30) ,nest, year_"))
										 
			
			
		 
		  if (nrow(rfids) > 0) { 
			## last state
			#	rfids$laststate = dbq(con, paste("select nest_endstate 
			#								  FROM AVESatBARROW.NESTS  where year_ = ",yr," and nest =",nest_))$nest_endstate
			#	if (is.null(rfids$laststate)) rfids$laststate = NA									  
			#								  
			rfids$datetime_=as.POSIXct(rfids$datetime_, tz="UTC")
			
				#rfids=rfids[order(rfids$datetime_),]		 
			
			#adds loggers  
					if (nrow(tts)>0) {
					  tts$datetime_ = as.POSIXct(tts$datetime_, tz="UTC")
					  if(yr==2011 & nest=="s320"){tts=tts[tts$datetime_<as.POSIXct("2011-07-03 07:46:00"),]}
					  tts$t_surface = NA
					  tts$t_nest_msr = NA
					  rfids = merge(rfids, tts, all = TRUE)
					  
					  }
					if (yr==2011){if(nrow(hobos)>0) {
					  hobos$datetime_ = as.POSIXct(hobos$datetime_, tz="UTC")
					  hobos$t_nest_tt = NA
					  hobos$t_nest_msr = NA
					 
					  rfids = merge(rfids, hobos, all = TRUE)
					  }}
					if (nrow(MSRs)>0) {  
					  MSRs$datetime_ = as.POSIXct(MSRs$datetime_, tz="UTC")
					  MSRs$t_nest_tt = NA
					  
					  rfids = merge(rfids, MSRs, all = TRUE)
					  }
					if (nrow(MSRs)==0 & nrow(tts)==0 & nrow(hobos)==0) rfids$t_nest_tt  = rfids$t_nest_msr  = rfids$t_surface = rfids$t_method = NA
					
				names(rfids)[names(rfids) =="transp"] <- "tag"
				# limit to active nest data
				rfids=rfids[which(!is.na(rfids$datetime_)),]
				if(yr==2013){st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(min(datetime_),'M',1)  datetime_
									 from barrow_2013.visits
									 where RFID = 'on' and lower(nest) =",nest_))
							}else{
							st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(coalesce(datetime_onNest,datetime_start),'S',30) datetime_
												 from LOGGERSatBARROW.Device_filestatus
												 where device = 'RFID' and coalesce(datetime_onNest,datetime_start)>'2009-01-01 00:00:00' and lower(nest) =",nest_," and year_ =",yr,"
												 "))
												 }
						
						if(nrow(st[!is.na(st$datetime_),])==0){print(paste("time RFID on nest missing ",nest_l, yr))
										rfids=rfids[rfids$datetime_>=start_,]
										}else{
										st$datetime_ = as.POSIXct(st$datetime_)
										st=min(st$datetime_)
										st = as.POSIXct(trunc(st, "day"))
										if(st>start_){rfids=rfids[rfids$datetime_>=st,]}else{rfids=rfids[rfids$datetime_>=start_,]}
										}
						
						if(yr==2013){et = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(max(datetime_),'M',1)  datetime_
									 from barrow_2013.visits
									 where RFID = 'off' and lower(nest) =",nest_))
							}else{
							et = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(coalesce(datetime_offNest,datetime_stop),'S',30)  datetime_
												 from LOGGERSatBARROW.Device_filestatus
												 where device = 'RFID' and coalesce(datetime_offNest,datetime_stop)>'2009-01-01 00:00:00'  and lower(nest) =",nest_," and year_ =",yr,"
												 "))
												 }
						if(nrow(et[!is.na(et$datetime_),])==0){print(paste("time RFID off nest missing ",nest_l, yr))
										rfids=rfids[rfids$datetime_<=end_,]
										}else{
										et$datetime_ = as.POSIXct(et$datetime_)
										et=max(et$datetime_)
										if(et<end_){rfids=rfids[rfids$datetime_<=et+20*60,]}else{rfids=rfids[rfids$datetime_<=end_+20*60,]}
										}		
				
				return(rfids) 
				}
			
		 }
		 }
		
		  {# extract data
		   
			for (i in (1:nrow(nests))) {
			#for (i in c(25, 55,65)) {
				a = RFID.temperature_actogram_data(con = conMy, nest = nests$nest[i],yr = nests$year[i], start_ = nests$start_[i], end_ = nests$end_[i])
				#gps=GPS.temperature_actogram_data (con = con, nest = nests$nest[i],yr = nests$yr[i])
				#RFID.temperature_actogram(dfr,gps,yr=yr,type=type, utc=TRUE)
				a$nest=toupper(nests$nest[i])
				a$nn=nests$nn[i]
				a$sp=toupper(nests$sp[i])
				a$year=tolower(nests$year[i])
				a$breeding_site=tolower(nests$breeding_site[i])
				a$who=tolower(a$who)
				a$datetime_utc=a$datetime_+nests$utc_plus[i]*60*60
				
				a$datetime_=  as.character(a$datetime_utc + (nests$lon[i]/15)*60*60)
				#a$method="rfid"
				z=d[tolower(d$breeding_site)=="barr",]
				z1=z[!(duplicated(paste(z$nest,z$year,z$bird_ID, z$sex))),]
				a$bird_ID=toupper(z1$bird_ID[match(tolower(paste(a$year, a$nest, a$who)), tolower(paste(z1$year, z1$nest,z1$sex)))])
								
				aa=a[,c('sp','breeding_site','year','t_method','nest','bird_ID','tag','who','datetime_','t_surface','t_nest_tt',"t_nest_msr")] # state not used
				aa$act_ID=paste("rfid",ifelse(nchar(i)==1,paste("00",i,sep=""),ifelse(nchar(i)==2,paste("0",i,sep=""),i)) ,sep="_")
				
				print(paste(nests$nest[i],nests$year[i]))
				print(paste(max(c(aa$t_surface),na.rm=TRUE), min(aa$t_surface,na.rm=TRUE)))
				print(paste(max(c(aa$t_nest_tt,aa$t_nest_msr),na.rm=TRUE), min(c(aa$t_nest_tt,aa$t_nest_msr),na.rm=TRUE)))
				
				conLite = dbConnect(dbDriver("SQLite"),dbname = db)
				#dbRemoveTable(conLite,name = "rfid_try")
				if(i==1){
					wet(conLite,"DROP TABLE IF EXISTS rfid ")
					dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE)
					wet(conLite,"CREATE INDEX act_ID ON rfid (act_ID)")
					}else{dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE, append=TRUE)}
				dbDisconnect(conLite)
					
				print(aa[1,])
			 } 
			}
		}
	## check reds_neth_2014_re123_nl2014 reds          neth 2014 re123_nl2014
		{# non-Barrow - datetime correction happens hear 
			#CHANGE START FOR HOFO2015
			###### hofo mihght have wrong datatime_ because it was initially spelled out in m as hocf
			# check 2012 godwit nest are (utc +1) except for 1200504314 UTC+2
			# check re114_nl2014"
			
			{# define constants
				#load("\\\\ds\\grpkempenaers\\Martin\\prep_for_ACTOS.Rdata")
				   conMy = dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='EXTRA_INCUBATION')
						
				# define nests for actos
					nests=d[!duplicated(paste(d$year,d$sp,d$nest)) & d$breeding_site!="barr" & d$method%in%c("rfid+msr","rfid","rfid+tt") & !tolower(d$sp)%in%c("snpl", "kepl", "sand"),c('year','sp','nest','nn','breeding_site','lon')]
					nests$nest=tolower(nests$nest)
					# set minimum and maximum time for which to use the RFID data (first egg -1 and end_date+1 day)
					nn=n[n$breeding_site%in%unique(nests$breeding_site) & n$year>2011,]
					
					nests$start_=nn$first_egg_laid[match(tolower(paste(nests$year,nests$sp,nests$nest)),tolower(paste(nn$year,nn$sp,nn$nest)))]-1*24*60*60
					nests$end_=nn$end_datetime[match(tolower(paste(nests$year,nests$sp,nests$nest)),tolower(paste(nn$year,nn$sp,nn$nest)))]+2.5*24*60*60
					
					nests[is.na(nests$end_),]
					nests[is.na(nests$start_),]
					nests$start_[is.na(nests$start_)]=as.POSIXct("2015-07-23 20:00:00")
					nests$pk=c(1:nrow(nests))
					nests$utc_plus=ifelse(nests$breeding_site=="isla",0, 
									ifelse(nests$breeding_site=="czrp", -2,
										ifelse(nests$breeding_site== "neth" , -2,
											ifelse(nests$breeding_site== "frie" , -2,
												ifelse(nests$breeding_site== "barr" , +8,
													ifelse(nests$breeding_site== "hofo", +2, NA)))))) 
					
					
					
					# brings in also birds that are not in the extracted dataset, but are in the raw data
						bird1=d[!duplicated(paste(d$year,d$sp,d$nest, d$logger, d$sex)) & d$breeding_site!="barr" & d$method%in%c("rfid+msr","rfid","rfid+tt"),c('year','sp','nest','sex','lon',"logger")]
						bird2=bb[bb$method=="pit_tag"  ,c('year', 'sp','nest','sex','lon','logger')]
						
						bird=rbind(bird1,bird2)
						bird$logger=tolower(bird$logger)
						bird=bird[!duplicated(paste(bird$year,bird$sp,bird$nest, bird$logger, bird$sex)),]
	}
		    {# data extraction functions
				RFID.temperature_actogram_data = function (con, nest, yr, start_,end_) {
				#on.exit(mysqlCloseConnection(con))
					if(tolower(nest)=='oycx_is2014'){
												nest_l=tolower('oyc10_is2014')
												nest_ =shQuote(tolower('oyc10_is2014'))
												}else{		nest_l=tolower(nest)
															nest_ =shQuote(tolower(nest))
															}
					if(tolower(nest)=='bv02_gl2015'){
							rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, coalesce(who, a.sex) who from
											(select lower(r.nest) nest, datetime_,
													transp, NULL who
													from RFID r
														where transp IS NOT NULL AND lower(r.nest) =",nest_,
														") x  
														LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from EXTRA_INCUBATION.CAPTURES c
																 LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
															   UNION SELECT transponder , 'fieldteam' sex from  EXTRA_INCUBATION.AUTHORS) a
														on x.transp = a.transponder"	
														))
										rfids$who=ifelse(rfids$transp=="001CE7D001AF0001","m",	ifelse(rfids$transp=="CFECE7D001AF0001","f",rfids$who))	
																				
										}else{
											if(tolower(nest)=='re114_nl2014'){
															rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, coalesce(who, a.sex) who from
																(select lower(r.nest) nest, datetime_,
																	transp, NULL who
																	from RFID r
																		where transp IS NOT NULL AND lower(r.nest) =",nest_,
																		") x  
																		LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from EXTRA_INCUBATION.CAPTURES c
																				 LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
																			   UNION SELECT transponder , 'fieldteam' sex from  EXTRA_INCUBATION.AUTHORS) a
																		on x.transp = a.transponder"	
																		))
															hs_ = dbq(con, paste0("SELECT lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'S',5) datetime_,
																	NULL transp, 'hatching start' who
																	from extra_incubation.nests where lower(nest) = ",nest_))
															vis_ = dbq(con, paste0("SELECT lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5)  datetime_,
																	NULL transp, 'nest visit' who
																	from extra_incubation.visits where lower(nest) = ",nest_))	
															rfids=rbind(rfids,hs_,vis_)		
																					
														}else{
											
										rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, coalesce(who, a.sex) who from
												(select lower(r.nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5)  datetime_,
													transp, NULL who
													from RFID r
														where transp IS NOT NULL AND lower(r.nest) =",nest_,
												" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'S',5) datetime_,
													NULL transp, 'hatching start' who
													from extra_incubation.nests where lower(nest) = ",nest_,
												" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5)  datetime_,
													NULL transp, 'nest visit' who
													from extra_incubation.visits where lower(nest) = ",nest_,") x  
													LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from extra_incubation.captures c
																 LEFT JOIN extra_incubation.sex s on c.ID = s.ID where tag_ID IS NOT NULL
															   UNION SELECT transponder , 'fieldteam' sex from  extra_incubation.authors) a
												   on x.transp = a.transponder"	
											)) 		
												}
												}
		
				rfids$who[is.na(rfids$who)] = 'unknown'
				rfids$who[which(rfids$who%in%c(2,"f","F"))] = 'f'
				rfids$who[which(rfids$who%in%c(1,"m","M"))] = 'm'
				#table(rfids$who, rfids$transp)
				#rfids$who[which(rfids$who%in%c("f","m") & rfids$transp%in%bird$logger[which(tolower(paste(bird$year,bird$nest))==paste(nests$year[i],nests$nest[i]))])]=bird$sex[match(tolower(rfids$transp[which(rfids$who%in%c("f","m") & rfids$transp%in%bird$logger[which(tolower(paste(bird$year,bird$nest))==paste(nests$year[i],nests$nest[i]))])]), tolower(bird$logger[bird$nest==nest]))]
				
				rfids$who[which(rfids$who%in%c("m","f") & !tolower(rfids$transp)%in%tolower(bird$logger[which(tolower(paste(bird$year,bird$nest))==paste(nests$year[i],nests$nest[i]))]))] = 'stranger'
					#rfids$act[which(rfids$who=="female")] = 15
				   # dfr$act[which(dfr$who=="female")] = 15
				rfids$t_method=NA
				rfids=rfids[!is.na(rfids$datetime_),]
								
				tts   = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1) datetime_, 
													T_egg as t_nest_tt,'tt' as 't_method'
											 from EXTRA_INCUBATION.tinytag
											 where T_egg is not NULL and T_egg > -20 and lower(nest) =",nest_))
				
				hobos   = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1) datetime_, 
													T_ambient as t_surface,'hobo' as 't_method'
											 from EXTRA_INCUBATION.HOBO
											 where lower(nest) =",nest_))
				
				MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',30) datetime_,
													T_egg as t_nest_msr,T_ambient as t_surface,'msr' as 't_method'
											 from EXTRA_INCUBATION.MSR
											 where lower(nest) =",nest_,"group by FUNCTIONS.RoundTime(datetime_,'S',30) ,nest"))
											 
			   if (nrow(rfids) > 0) { 
				## last state
				#	rfids$laststate = dbq(con, paste("select nest_endstate 
				#								  FROM AVESatBARROW.NESTS  where year_ = ",yr," and nest =",nest_))$nest_endstate
				#	if (is.null(rfids$laststate)) rfids$laststate = NA									  
				#								  
				rfids$datetime_=as.POSIXct(rfids$datetime_, tz="UTC")
							 
				# adjusts wrong times
					if(unique(rfids$nest)=="wh303_is2013"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+14449.1511*24*60*60}
					if(unique(rfids$nest)=="re304_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]-3341.420833*24*60*60}
					if(unique(rfids$nest)=="re114_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+18662.15788*24*60*60}
					if(unique(rfids$nest)=="oyc10_is2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+3630.301319*24*60*60}
					#if(unique(rfids$nest)=="oycx_is2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+3630.301319*24*60*60}
					if(unique(rfids$nest)=="re123_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+0.257604167*24*60*60}
					if(unique(rfids$nest)=="bv02_gl2015"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+as.numeric(difftime(as.POSIXct("2015-07-23 19:32:59"),as.POSIXct("1961-04-03 03:31:06"), units = c( "secs")))}
				 
				#adds loggers  
					if (nrow(tts)>0) {
					  tts$datetime_ = as.POSIXct(tts$datetime_, tz="UTC")
					  tts$t_surface = NA
					  tts$t_nest_msr = NA
					  #tts$who=NA
					  rfids = merge(rfids, tts, all = TRUE)
					  
					  }
					if (nrow(hobos)>0) {
					  hobos$datetime_ = as.POSIXct(hobos$datetime_, tz="UTC")
					  hobos$t_nest_tt = NA
					  hobos$t_nest_msr = NA
					  #hobos$who=NA
					  rfids = merge(rfids, hobos, all = TRUE)
					  }
					if (nrow(MSRs)>0) {  
					  MSRs$datetime_ = as.POSIXct(MSRs$datetime_, tz="UTC")
					  MSRs$t_nest_tt = NA
					  #MSRs$who=NA
					  rfids = merge(rfids, MSRs, all = TRUE)
					  }
					if (nrow(MSRs)==0 & nrow(tts)==0 & nrow(hobos)==0) rfids$t_nest_tt  = rfids$t_nest_msr  = rfids$t_surface = rfids$t_method = NA
					
						if(tolower(nest)=='oycx_is2014'){rfids$nest="oycx_is2014"}
						if(tolower(nest)=='oycx_is2014'){nest_l=tolower(nest)
														nest_ =shQuote(tolower(nest))
																}
					
					names(rfids)[names(rfids) =="transp"] <- "tag"
					
				# limit to active nest data
					rfids=rfids[!is.na(rfids$datetime_),]
					st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(coalesce(datetime_onNest,datetime_start),'S',30)  datetime_
					  from EXTRA_INCUBATION.Device_filestatus
					  where device = 'RFID' and coalesce(datetime_onNest,datetime_start)>'2009-01-01 00:00:00' and lower(nest) =",nest_))
						if(nrow(st[!is.na(st$datetime_),])==0){print(paste("time RFID on nest missing ",nest_l, yr))
										rfids=rfids[rfids$datetime_>=start_,]
										}else{
										st$datetime_ = as.POSIXct(st$datetime_)
										st=min(st$datetime_)
										st = as.POSIXct(trunc(st, "day"))
										if(st>start_){rfids=rfids[rfids$datetime_>=st,]}else{rfids=rfids[rfids$datetime_>=start_,]}
										
										}
					et = dbq(con,  paste("select lower(nest) nest,FUNCTIONS.RoundTime(coalesce(datetime_offNest,datetime_stop),'S',30)  datetime_
					  from EXTRA_INCUBATION.Device_filestatus
					  where device = 'RFID' and coalesce(datetime_offNest,datetime_stop)>'2009-01-01 00:00:00'  and lower(nest) =",nest_))
						if(nrow(et[!is.na(et$datetime_),])==0){print(paste("time RFID off nest missing ",nest_l, yr))
										rfids=rfids[rfids$datetime_<=end_,]
										}else{
										et$datetime_ = as.POSIXct(et$datetime_)
										et=max(et$datetime_)
										if(et<end_){rfids=rfids[rfids$datetime_<=et+20*60,]}else{rfids=rfids[rfids$datetime_<=end_+20*60,]}
										}		
					
					return(rfids) 
					}
				
			 }
			}	 
		    {# extract data
			for (i in (1:nrow(nests))) {
				#for (i in c(1,25,70 )) {
					a = RFID.temperature_actogram_data(con = conMy, nest = nests$nest[i],yr = nests$year[i], start_ = nests$start_[i], end_ = nests$end_[i])
					#gps=GPS.temperature_actogram_data (con = con, nest = nests$nest[i],yr = nests$yr[i])
					#RFID.temperature_actogram(dfr,gps,yr=yr,type=type, utc=TRUE)
					a$nest=toupper(nests$nest[i])
					a$nn=nests$nn[i]
					a$sp=toupper(nests$sp[i])
					a$year=tolower(nests$year[i])
					a$breeding_site=tolower(nests$breeding_site[i])
					a$who=tolower(a$who)
					a$datetime_utc=a$datetime_+nests$utc_plus[i]*60*60
					
					a$datetime_= as.character(a$datetime_utc + (nests$lon[i]/15)*60*60)
					#a$method="rfid"
					z=d[tolower(d$breeding_site)!="barr" & d$method%in%c("rfid+msr","rfid","rfid+tt") & !tolower(d$sp)%in%c("snpl", "kepl", "sand"),]
					z1=z[!(duplicated(paste(z$nest,z$year,z$bird_ID, z$sex))),]
					a$bird_ID=toupper(z1$bird_ID[match(tolower(paste(a$year, a$nest, a$who)), tolower(paste(z1$year, z1$nest,z1$sex)))])
					
					aa=a[,c('sp','breeding_site','year','t_method','nest','bird_ID','tag','who','datetime_','t_surface','t_nest_tt',"t_nest_msr")] # state not used
					aa$act_ID=paste("rfid",i+115,sep="_")
					print(paste(nests$nest[i],nests$year[i]))
					print(paste(max(c(aa$t_surface),na.rm=TRUE), min(aa$t_surface,na.rm=TRUE)))
					print(paste(max(c(aa$t_nest_tt,aa$t_nest_msr),na.rm=TRUE), min(c(aa$t_nest_tt,aa$t_nest_msr),na.rm=TRUE)))
					
					conLite = dbConnect(dbDriver("SQLite"),dbname = db)
					#dbRemoveTable(conLite,name = "rfid_try")
					dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE, append=TRUE)
					dbDisconnect(conLite)
					
					print(aa[1,])
				 } 
				}
	


		}
	}
	
	{# kepl - ddll - rfid # state not used 
		a = read.csv("C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/KEPL_ddll_rfid.csv", stringsAsFactors=FALSE)
			a=a[tolower(paste(a$year,a$nest))%in%tolower(unique(paste(d$year[d$breeding_site=="ddll"],d$nest[d$breeding_site=="ddll"]))),]
			
			a$date=ifelse(nchar(a$date)==17, paste(substring(a$date,6,9),"-0",substring(a$date,4,4),"-",substring(a$date,1,2)," ",sep=""),paste(substring(a$date,5,8),"-0",substring(a$date,3,3),"-0",substring(a$date,1,1)," ",sep=""))
			a$time=ifelse(nchar(a$time)==19, substring(a$time,12), paste("0",substring(a$time,12),sep=""))
			a$datetime_l= as.POSIXct(paste(a$date,a$time,sep=""), tz="UTC")
			a$datetime_utc=a$datetime_l-2*60*60
			a$year=format(a$datetime_utc,"%Y")
			a=a[order(a$year,a$nest,a$datetime_utc),]
			
			a$nest=toupper(a$nest)
			#summary(factor(a$sex))
			a$inc=tolower(a$sex)
			a$sex=a$inc
			a$bird_ID=toupper(a$bird_ID)
			a$tag=a$bird_ID
			a$sp="KEPL"
			a$method='rfid'
			a$t_method=NA
			a$state=0
			
			z=d[tolower(d$breeding_site)=="ddll",]
			zz=z[!(duplicated(paste(z$nest,z$year))),]
			a$nn=tolower(unique(zz$nn[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))]))
			a$breeding_site=tolower(zz$breeding_site[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			a$lon=tolower(zz$lon[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			
			
			a$datetime_= as.character(a$datetime_utc + (as.numeric(a$lon)/15)*60*60)
			a$datetime_utc= as.character(a$datetime_utc)		
			
			a$who=a$inc
			a$t_surface=a$t_nest_tt=a$t_nest_msr=a$gpsdist=NA
			
			aa=a[,c('sp','breeding_site','year','t_method','nest','bird_ID','tag','who','datetime_','t_surface','t_nest_tt',"t_nest_msr")] # state not used
			xx=data.frame(id=unique(paste(aa$year,aa$nest)),ii=c((115+85+1):(115+85+length(unique(paste(aa$year,aa$nest))))))
			aa$act_ID=xx$ii[match(paste(aa$year,aa$nest), xx$id)]
			aa$act_ID=paste("rfid",aa$act_ID,sep="_")
			a1=aa
			#conLite = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE)#, append=TRUE
			#dbDisconnect(conLite)
	
	}
	{# sand - zack - rfid+tt # state not used
		{# s30 # the old graphs have time that is 2h less then it should be
			r=read.csv("C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/sand_zack/S30.07_rfid.csv", stringsAsFactors=FALSE)
				names(r)[names(r) =="date.and.time"] <- "datetime_utc"
				names(r)[names(r) =="transponder"] <- "tag"
				names(r)[names(r) =="Sex"] <- "who"
				r=r[!r$datetime_utc=="",]
				r$datetime_utc= as.POSIXct(r$datetime_utc, tz="UTC")##,tz = "America/Anchorage") 
				r=r[,c("datetime_utc", "who", "tag")]
					r$who=tolower(r$who)
					#r$inc=1
					r$t_method=r$t_nest_tt=NA
					
			tt=read.csv("C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/sand_zack/S30.07_tt.csv", stringsAsFactors=FALSE)
					names(tt)[names(tt) =="date...time"] <- "datetime_utc"
					names(tt)[names(tt) =="temp"] <- "t_nest_tt"
					tt$datetime_utc=as.POSIXct(as.character(tt$datetime_utc),tz="UTC")
					tt=tt[!is.na(tt$t_nest_tt),c("datetime_utc","t_nest_tt")]
					tt$who=NA
					tt$tag=NA
					tt$t_method="tt"
					
			a=rbind(r,tt)
					
			a$year=substring(a$datetime_utc[1], 1,4)
			a$nest=toupper("s30")
			a$sp=toupper("sand")
			a$method='rfid'
			a$state=0
			
			z=d[tolower(d$breeding_site)=="zack",]
			z1=z[!(duplicated(paste(z$nest,z$year,z$bird_ID, z$sex))),]
			zz=z[!(duplicated(paste(z$nest,z$year))),]
			
			a$bird_ID=toupper(z1$bird_ID[match(tolower(paste(a$year, a$nest, a$who)), tolower(paste(z1$year, z1$nest,z1$sex)))])
			a$nn=tolower(unique(zz$nn[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))]))
			a$breeding_site=tolower(zz$breeding_site[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			a$lon=tolower(zz$lon[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			
			a$datetime_= as.character(a$datetime_utc + (as.numeric(a$lon)/15)*60*60)
			a$datetime_utc= as.character(a$datetime_utc)		
			a$t_surface=a$t_nest_msr=a$gpsdist=NA
			
			aa=a[,c('sp','breeding_site','year','t_method','nest','bird_ID','tag','who','datetime_','t_surface','t_nest_tt',"t_nest_msr")]
			aa$act_ID=234+1
			aa$act_ID=paste("rfid",aa$act_ID,sep="_")
			a2=aa
			
			#conLite = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE)#, append=TRUE
			#dbDisconnect(conLite)		
				}				
		{# s24 # the old graphs have time that is 2h less then it should be
			r=read.csv("C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/sand_zack/S24.07_rfid.csv", stringsAsFactors=FALSE)
				names(r)[names(r) =="date.and.time"] <- "datetime_utc"
				names(r)[names(r) =="transponder"] <- "tag"
				names(r)[names(r) =="Sex"] <- "who"
				
				r$datetime_utc= as.POSIXct(r$datetime_utc,tz="UTC")##,tz = "America/Anchorage") 
				r=r[,c("datetime_utc", "who", "tag")]
					r$who=tolower(r$who)
					#r$inc=1
					r$t_method=r$t_nest_tt=NA
					
			tt=read.csv("C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/sand_zack/S24.07_tt.csv", stringsAsFactors=FALSE)
					names(tt)[names(tt) =="Date...time"] <- "datetime_utc"
					names(tt)[names(tt) =="temp"] <- "t_nest_tt"
					tt$datetime_utc=as.POSIXct(ifelse(substring(tt$datetime_utc,1,4)=="1999", as.character(paste("2007",substring(tt$datetime_utc,5), sep="")), as.character(tt$datetime_utc)),tz="UTC")
					tt=tt[!is.na(tt$t_nest_tt),c("datetime_utc","t_nest_tt")]
					tt$who=NA
					tt$tag=NA
					tt$t_method="tt"
					
			a=rbind(r,tt)
					
			a$year=substring(a$datetime_utc[1], 1,4)
			a$nest=toupper("s24")
			a$sp=toupper("sand")
			a$method='rfid'
			a$state=0
			
			z=d[tolower(d$breeding_site)=="zack",]
			z1=z[!(duplicated(paste(z$nest,z$year,z$bird_ID, z$sex))),]
			zz=z[!(duplicated(paste(z$nest,z$year))),]
			
			a$bird_ID=toupper(z1$bird_ID[match(tolower(paste(a$year, a$nest, a$who)), tolower(paste(z1$year, z1$nest,z1$sex)))])
			a$nn=tolower(unique(zz$nn[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))]))
			a$breeding_site=tolower(zz$breeding_site[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			a$lon=tolower(zz$lon[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			
			a$datetime_= as.character(a$datetime_utc + (as.numeric(a$lon)/15)*60*60)
			a$datetime_utc= as.character(a$datetime_utc)		
			a$t_surface=a$t_nest_msr=a$gpsdist=NA
			
			aa=a[,c('sp','breeding_site','year','t_method','nest','bird_ID','tag','who','datetime_','t_surface','t_nest_tt',"t_nest_msr")]
			aa$act_ID=234+2
			aa$act_ID=paste("rfid",aa$act_ID,sep="_")
			a3=aa
			
			#conLite = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE)#, append=TRUE
			#dbDisconnect(conLite)		
				}				
		{# s09	# the old graphs have time that is 2h less then it should be / CHECDK WHETHER TINY TAG TIME NEED THE CURRENT +1H
			r=read.csv("C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/sand_zack/S09.07_rfid.csv", stringsAsFactors=FALSE)
				names(r)[names(r) =="date.and.time"] <- "datetime_utc"
				names(r)[names(r) =="transponder"] <- "tag"
				names(r)[names(r) =="Sex"] <- "who"
				r=r[!r$datetime_utc=="",]
				r$datetime_utc= as.POSIXct(r$datetime_utc, tz="UTC")##,tz = "America/Anchorage")
				r=r[,c("datetime_utc", "who", "tag")]
					r$who=tolower(r$who)
					#r$inc=1
					r$t_method=r$t_nest_tt=NA
			tt=read.csv("C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/sand_zack/S09tt.csv", stringsAsFactors=FALSE)
					tt$t_nest_tt=as.numeric(substring(tt$t_nest, 1,4))
					tt$datetime_utc=as.POSIXct(tt$datetime_utc,tz="UTC")+60*60	# had wrong time
									
					tt=tt[!is.na(tt$t_nest_tt),c("datetime_utc","t_nest_tt")]
					tt$who=NA
					tt$tag=NA
					tt$t_method="tt"
					
			a=rbind(r,tt)
					
			a$year=substring(a$datetime_utc[1], 1,4)
			a$nest=toupper("s09")
			a$sp=toupper("sand")
			a$method='rfid'
			a$state=0
			
			z=d[tolower(d$breeding_site)=="zack",]
			z1=z[!(duplicated(paste(z$nest,z$year,z$bird_ID, z$sex))),]
			zz=z[!(duplicated(paste(z$nest,z$year))),]
			
			a$bird_ID=toupper(z1$bird_ID[match(tolower(paste(a$year, a$nest, a$who)), tolower(paste(z1$year, z1$nest,z1$sex)))])
			a$nn=tolower(unique(zz$nn[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))]))
			a$breeding_site=tolower(zz$breeding_site[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			a$lon=tolower(zz$lon[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			
			a$datetime_= as.character(a$datetime_utc + (as.numeric(a$lon)/15)*60*60)
			a$datetime_utc= as.character(a$datetime_utc)		
			a$t_surface=a$t_nest_msr=a$gpsdist=NA
			
			aa=a[,c('sp','breeding_site','year','t_method','nest','bird_ID','tag','who','datetime_','t_surface','t_nest_tt',"t_nest_msr")]
			aa$act_ID=234+3
			aa$act_ID=paste("rfid",aa$act_ID,sep="_")
			a4=aa
			
			#conLite = dbConnect(dbDriver("SQLite"),dbname = db)
			#dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE)#, append=TRUE
			#dbDisconnect(conLite)		
		}				
	}			
	{# snpl - ceut - rfid
		f=list.files(path="C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/snpl_ceut/", 
							pattern='.csv', recursive=TRUE,full.names=TRUE)
		f2=list.files(path="C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/RFID/snpl_ceut/", 
							pattern='.csv', recursive=TRUE,full.names=FALSE)
	
			z=d[tolower(d$breeding_site)=="ceut",]
			z1=z[!(duplicated(paste(z$nest,z$year,z$bird_ID, z$sex))),]
			zz=z[!(duplicated(paste(z$nest,z$year))),]
		
	    for (i in (1:length(f))) { 
			a = read.table(f[i],sep=",", col.names=c("check","sec","min", "hour", "day", "month", "year", "juldate", "c1","c2", "tag", "sex"),header=TRUE,fill=TRUE,stringsAsFactors=FALSE)		
			a$check=a$juldate=a$c1=a$c2=NULL
			a$min=as.character(ifelse(nchar(a$min)==1, paste("0",a$min,sep=""), a$min))
			a$sec=as.character(ifelse(nchar(a$sec)==1, paste("0",a$sec,sep=""), a$sec))
			a$hour=as.character(ifelse(nchar(a$hour)==1, paste("0",a$hour,sep=""), a$hour))
			a$day=as.character(ifelse(nchar(a$day)==1, paste("0",a$day,sep=""), a$day))
			a$month=as.character(ifelse(nchar(a$month)==1, paste("0",a$month,sep=""), a$month))
			a$year=2006
			a$datetime_l=as.POSIXct(strptime(paste(paste("2006",a$month,a$day,sep="-"), paste(a$hour,a$min, a$sec, sep=":"), sep=" "),format="%Y-%m-%d %H:%M:%S"),tz="UTC")
			a$datetime_utc=a$datetime_l+6*60*60
			a$sec=a$hour=a$day=a$min=a$month=NULL
			a$who=tolower(a$sex)
			a$who[!a$who%in%c("f","m")]="misread" # error readings
			#summary(factor(a$tag)) # misread
			#a$tag[!a$who%in%c("f","m")]="misread"
			a$t_surface=a$t_nest_tt=a$t_nest_msr=a$gpsdist=NA
				
			a$nest=toupper(substring(f2[i],1, nchar(f2[i])-4))
			a$sp="SNPL"
			a$method='rfid'
			a$t_method=NA			
			a$bird_ID=toupper(z1$bird_ID[match(tolower(paste(a$year, a$nest, a$who)), tolower(paste(z1$year, z1$nest,z1$sex)))])
			a$nn=tolower(unique(zz$nn[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))]))
			a$breeding_site=tolower(zz$breeding_site[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			a$lon=tolower(zz$lon[match(tolower(paste(a$year, a$nest)), tolower(paste(zz$year, zz$nest)))])
			
			a$datetime_= as.character(a$datetime_utc + (as.numeric(a$lon)/15)*60*60)
			a$datetime_utc= as.character(a$datetime_utc)		
						
			aa=a[,c('sp','breeding_site','year','t_method','nest','bird_ID','tag','who','datetime_','t_surface','t_nest_tt',"t_nest_msr")]
			aa$act_ID=234+3+i
			aa$act_ID=paste("rfid",aa$act_ID,sep="_")
			if(i==1){write.table(aa,paste(wd2,'raw_data_rfid_ceut.txt', sep=""), row.names=FALSE,append =FALSE, sep=",")
				}else{write.table(aa,paste(wd2,'raw_data_rfid_ceut.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")}
			
			print(paste(zz$year[i],zz$nest[i], sep="_"))
			}
			
			
			#conLite = dbConnect(dbDriver("SQLite"),dbname = db)
			a5=read.table(paste(wd2,'raw_data_rfid_ceut.txt', sep=""), sep=",", header=TRUE, stringsAsFactors=FALSE)
	
			#dbWriteTable(conLite, name = "rfid", value = a5, row.names = FALSE, header=TRUE)
			#dbDisconnect(conLite)
		}		
		
		h=rbind(a1,a2,a3,a4,a5)
		conLite = dbConnect(dbDriver("SQLite"),dbname = db)
		#dbRemoveTable(conLite,name = "rfid")
		dbWriteTable(conLite, name = "rfid", value=h , row.names = FALSE, header=TRUE, append=TRUE)
		#wet(conLite,"CREATE INDEX idx_act_ID ON rfid (act_ID)")
		dbDisconnect(conLite)	
		
		

}
