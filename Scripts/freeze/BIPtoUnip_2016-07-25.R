{# INFO
 # data are in local time
 # data used in the analyses are from 2 hours after the equipment was placed on the nest, and 6 hours before the hatching start or 12 hours before the nest hatched
 # NESTS
	#BARRROW
		#nests=c('a301','a302','b301','b501','s602') # deserted 2011
		#nests=c('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902') # deserted 2012 (s712 right during hatching, it may even be no desertion)
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
		
	# 	
}
{# TOOLS
	# define working and output directory
		wd="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Data/"	
		
	
	require(plyr)
	require(RMySQL)
	require(XLConnect)
	con=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
	conMY=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
	conE=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='extract_2013')
	dbq=dbGetQuery

  # define time and load packages
	Sys.setenv(TZ="UTC")	

}
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
{# add latitude and longitude to nest table
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
{# prepare database

	{# Barrow data
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
			}else{tts=data.frame(nest=NULL, datetime_=NULL, t_nest_tt=NULL, t_method=NULL)}
			
			MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5)  datetime_,
			                                    T_egg as t_nest,T_ambient as t_surface,'msr' as 't_method'
										 from MSR
										 where lower(nest) =",nest_,"and year_ =",yr))
			
			# visits			
				if(yr==2011){
						visits=  dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5) datetime_,
												 'nest visit' disturb
												from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,"and year_ =",yr,"GROUP BY FUNCTIONS.RoundTime(datetime_,'S',5)"))
												
						capture=dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(COALESCE(caught_date_time,release_time,start_capture_date_time),'S',5) datetime_ , 'capture' disturb
												from AVESatBARROW.CAPTURES where lower(nest) = ",nest_,"and year_ =",yr))
						
						visits=rbind(visits,capture)
						}else{
							v=  dbq(con, paste("SELECT lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',5) datetime_,FUNCTIONS.RoundTime(datetime_left,'S',5) datetime_left,
												 'nest visit' disturb
												from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,"and year_ =",yr))
							v$datetime_left=ifelse(is.na(v$datetime_left), as.character(as.POSIXct(v$datetime_)+30), v$datetime_left)			
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
							
							
							if(nrow(capture)>0){
												capture$datetime_=as.POSIXct(capture$datetime_)
												visits$disturb=capture$disturb[match(visits$datetime_, capture$datetime)]
												visits$disturb=ifelse(is.na(visits$disturb), 'nest visit', visits$disturb)
												}
						
						}	
				
		 # merge the data
		 if (nrow(rfids) > 0) { 
			
			rfids$datetime_=as.POSIXct(rfids$datetime_, tz="UTC")
		
			#adds loggers  
					if (nrow(tts)>0) {
						if(yr==2011 & nest%in%c('a301','a302','b501','s602')){
							 tts$datetime_ = as.POSIXct(tts$datetime_, tz="UTC")
							 rfids = merge(rfids, tts, all = TRUE)
						}else{
						  tts$datetime_ = as.POSIXct(tts$datetime_, tz="UTC")
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
					  rfids = merge(rfids, visits, all = TRUE)
					  }
					 if (nrow(visits)==0) rfids$disturb=NA 
				
			names(rfids)[names(rfids) =="transp"] <- "tag"
		
		  # limit to active nest data
					rfids=rfids[which(!is.na(rfids$datetime_)),]
			 		rfids=rfids[rfids$datetime_>=start_,]
					rfids=rfids[rfids$datetime_<=end_,]
						
				return(rfids) 
				}
			
		 }
		 }
		{# define constants
			# nests to extract the data for
			nests=readWorksheetFromFile(paste(wd,'nests.xls', sep=""), sheet='original',colTypes = c(
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
								 XLC$DATA_TYPE.NUMERIC,
                                 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
								 XLC$DATA_TYPE.STRING,
                                 XLC$DATA_TYPE.STRING))
			nests$on=as.POSIXct(nests$on)
			nests$off=as.POSIXct(nests$off)
			nests=nests[!nests$sp%in%c('blgo','reds'),]
			nests_=nests[!duplicated(nests$nest),]
			
			# birds
				birds=readWorksheetFromFile(paste(wd,'birds.xls', sep=""), sheet='birds')
				birds=birds[which(!is.na(birds$tag_ID)),]	
				
			
		  }			
		
		for (i in (1:nrow(nests))) {
			#for (i in c(25, 55,65)) {
				a = RFID.temperature_actogram_data(con = conMy, nest = nests_$nest[i],yr = nests_$year[i], start_ = nests_$on[i], end_ = nests_$off[i]+60)
				#gps=GPS.temperature_actogram_data (con = con, nest = nests$nest[i],yr = nests$yr[i])
				#RFID.temperature_actogram(dfr,gps,yr=yr,type=type, utc=TRUE)
				a$nest=nests_$nest[i]
				a$sp=nests_$sp[i]
				a$year=nests_$year[i]
				a$site=nests_$site[i]
				a$who=tolower(a$who)
				a$datetime_z=a$datetime_
				
				a$datetime_=  as.character(a$datetime_z+ (nests_$local_plus[i]*60*60))
					birds_=birds[paste(birds$year_, birds$nest)==paste( nests_$year[i], nests_$nest[i]),]
					a$bird_ID=birds_$bird_ID[match(a$who, birds_$sex)]
					a$signal=NA
					a$sys='bip' # breeding system
				{# add average surface temperature where missing
				 #### select from sandbing for each year
				}
				{# extratction of incubation/non-incubation based on nest and surface temperature
						
						b=a[order(a$datetime_),]
						
						b$t_nest_prior=c(b$t_nest[1],b$t_nest[-nrow(b)])
						b$t_difference = as.numeric(b$t_nest - b$t_nest_prior) 
						
						b$t_nest_run_med=rollmedian(b$t_nest, 17279 , fill="extend") # 1/2 day 8639, whole day 17279
							
						b$t_diff_run=rollmean(b$t_difference, 24, fill="extend") #2 minutes (for median neads to be odd)	
						
							# in nests where t.ambient was missing it is set in previous sections to avg.t
						
						b$inc_t = 	ifelse(is.na(b$t_ambient),
											(ifelse(b$t_nest_run_med<20,
												(ifelse(b$t_nest > b$t_nest_run_med+3, 1, 0)),#))
												ifelse(b$t_nest > b$t_nest_run_med-3, 1,0))),#))
											ifelse(b$t_nest_run_med<20,
												(ifelse(b$t_nest > b$t_nest_run_med+3, 1, 
													ifelse(b$t_nest > b$t_ambient+12.5, 1,0))),#))
												ifelse(b$t_nest > b$t_nest_run_med-3, 1, 
													ifelse(b$t_nest > b$t_ambient+12.5, 1,0))))#))
						
						
						b$inc_t = 	ifelse(is.na(b$t_ambient),
						
										(ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run < 0, 0,
													ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run > 0, 
														1, b$inc_t))),
						
						(ifelse(b$t_nest<20 &(b$t_ambient-abs(b$t_nest))>(-3) & b$t_nest<b$t_nest_run_med+3, b$inc_t,
												(ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run < 0, 0,
												ifelse(abs(b$t_diff_run) >= 0.02 & b$t_diff_run > 0,1,
																	b$inc_t)))))	)	
				}
				aa=b[,c('sys','sp','site','year','t_method','nest','bird_ID','tag','who','datetime_z','datetime_','t_surface','t_nest',"t_nest","signal","disturb")] # state not used
				aa$act_ID=paste("bip",ifelse(nchar(i)==1,paste("0",i,sep=""),i),sep="_")
				
				
				print(paste(nests$nest[i],nests$year[i]))
				print(paste(max(c(aa$t_surface),na.rm=TRUE), min(aa$t_surface,na.rm=TRUE)))
				print(paste(max(c(aa$t_nest_tt,aa$t_nest_msr),na.rm=TRUE), min(c(aa$t_nest_tt,aa$t_nest_msr),na.rm=TRUE)))
				
				conLite = dbConnect(dbDriver("SQLite"),dbname = db)
				#dbRemoveTable(conLite,name = "rfid_try")
				if(i==1){
					dbq(conLite,"DROP TABLE IF EXISTS rfid ")
					dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE)
					dbq(conLite,"CREATE INDEX act_ID ON rfid (act_ID)")
					}else{dbWriteTable(conLite, name = "rfid", value = aa, row.names = FALSE, header=TRUE, append=TRUE)}
				dbDisconnect(conLite)
					
				print(aa[1,])
			 } 
			}
		}






		