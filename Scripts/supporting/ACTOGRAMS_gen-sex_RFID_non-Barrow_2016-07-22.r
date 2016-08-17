# GPS still needs to be added
# add to select TRANSP that are read more than
# WHATCH first_egg is deducted twice (check it) - in mysql and then later
# create the functions first!
{# TOOLS
require(sdb)
require(plyr)
require(grid)
require(sp)
require(lattice)
require(colorRamps)
require(RColorBrewer)
require(utilsMPIO)
require(maptools)

	Sys.setenv(TZ="UTC")
	require(plyr)
	require(gdata)
	require(lattice)
	require(grid)
	require(XLConnect)
	require(maptools)
	require(stringr)
	require(utilsMPIO)
	require(diptest)
	require(CircStats)
	require(sdb)

#require(tcltk2)

}

{# define constants
#load("\\\\ds\\grpkempenaers\\Martin\\prep_for_ACTOS.Rdata")

#con = dbcon(user='mbulla',host='scidb.orn.mpg.de', password='Kvetak25',database='EXTRA_INCUBATION')
con = dbcon(user='root',host='127.0.0.1', password='',database='EXTRA_INCUBATION')
actogram.cols = data.frame(who  = c("sun < -6°","sun < 0°",'video', 'one bird removed','heating','male','female','unknown','fieldteam','nest visit','capture','captive released', "first egg", "incubation start", "hatching start", "incubation end", "ambient temperature", "egg temperature"),
                           cols = c("grey35","grey66","darkolivegreen2","darkviolet","yellow2",transpcol("deepskyblue", newalpha=150),transpcol("red", newalpha=150),"orange","deeppink4","pink","red","darkviolet",'#F6E8C3','#DFC27D','#BF812D','#8C510A',"dark blue", "light blue"),
						   stringsAsFactors = FALSE
                           )
						   
						
type   = 'PNG'	# or PDF

#outdir = "\\\\ds\\RAW_DATA_KEMP\\FIELD\\Incubation\\RFID_ACTOGRAMS"			
outdir = "C:/Users/mbulla/Documents/ownCloud/ACTOS_Bip_unip_local_time"			

}

#FUNCTIONS-RUN THIS BIT FIRST! 
	# check 2012 godwit nest are (utc +1) except for 1200504314 UTC+2



{# meta data and gps extraction function 
meta_nest.data=function(con=con, nest){
 if (missing(nest)) 
     nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from EXTRA_INCUBATION.Device_filestatus where device='RFID' order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	m = dbq(con, paste("select species sp, country, lower(nest) nest,year_, IDfemale, IDmale, latit lat, longit lon, loggers 
								from EXTRA_INCUBATION.NESTS
									where lower(nest) =",nest_))
			
			m$scinam=ifelse(m$sp=="DUNL","Calidris alpina", 
							ifelse(m$sp=="BLGO", "Limosa limosa",
								ifelse(m$sp== "EUGP", "Pluvialis apricaria",
									ifelse(m$sp== "LRPL", "Charadrius dubius", 
										ifelse(m$sp=="OYST",  "Haematopus ostralegus", 
											ifelse(m$sp=="REDS", "Tringa totanus", 
												ifelse(m$sp=="RIPL", "Charadrius hiaticula", 
													ifelse(m$sp=="WHIM", "Numenius phaeopus", NA)))))))) 
			
			m$utc_plus=ifelse(m$country=="IS",0, 
							ifelse(m$country=="CZ", +2,
								ifelse(m$country== "NL" , +2,
									ifelse(m$country== "GL", -2, NA)))) 
			
			m$breeding_site=ifelse(m$country=="IS","isla", 
							ifelse(m$country=="CZ", "czrp",
								ifelse(m$country== "NL", "neth",
									ifelse(m$country== "GL", "hocf", NA)))) 
			m$sampling="5s"
				
	return(m)
}
meta_birds.data=function(con=con, nest){
 if (missing(nest)) 
       nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from EXTRA_INCUBATION.Device_filestatus where device='RFID' order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	m = dbq(con, paste("select species sp, country, lower(nest) nest,year_, IDfemale, IDmale, latit lat, longit lon, loggers 
							from EXTRA_INCUBATION.NESTS
								where lower(nest) =",nest_,""))
	m$IDfemale=shQuote(m$IDfemale)
	m$IDmale=shQuote(m$IDmale)
	n=c(m$IDfemale,m$IDmale)
	n=n[!is.na(n)]
	if(length(n)==1){
	s_b = dbq(con, paste("select*from EXTRA_INCUBATION.CAPTURES c
										LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID
										where c.ID in (",n,")"))
										}else{
										s_b = dbq(con, paste("select*from EXTRA_INCUBATION.CAPTURES c
										LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID
										where c.ID in (",m$IDfemale,",",m$IDmale,")"))
										}
	
	return(s_b)
}
GPS.temperature_actogram_data = function (con = con, nest, yr) {
  if (missing(yr))
	 yr = pickOne(c(2013,2012,2011))
    if (missing(nest)) 
     nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	# RFID placed on the nest (to limit the data lateR)
		# does not work yet for 2013, because RFID on off are not available for 2013
		st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_onNest,'S',30)  datetime_
									 from LOGGERSatBARROW.Device_filestatus
									 where device = 'RFID' and lower(nest) =",nest_," and year_ =",yr,"
									 "))
		st$datetime_ = as.POSIXct(st$datetime_)
		if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])==0) {print(paste("on missing ",nest_l))} else{
		if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])>0) 
				{st=min(s$datetime_[s$nest==nest_l & s$yr==yr])} else { st=min(st$datetime_)}}			
		stt=st
		
  # calculate disturbance distance to nest
	  gps      = dbq(con,paste("SELECT * from LOGGERSatBARROW.GPS where datetime_ >",shQuote(stt)," and datetime_<",shQuote(max(dfr$datetime_, na.rm = TRUE))))
	 
	  nestcoords  = dbq(con,paste("SELECT longit, latit from AVESatBARROW.NESTS where nest =",nest_," and year_=",yr))
	  nestcoords  = SpatialPoints(nestcoords[,c("longit","latit")])
	  gps         = SpatialPointsDataFrame(gps[,c("longit","latit")],gps)
	  gps         = cbind(gps@data,gpsdist = spDistsN1(gps,nestcoords,longlat=TRUE)*1000)
	  gps         = gps[gps$gpsdist<200,]   
	  #Ramp        = colorRampPalette(brewer.pal(9, 'Greys'), space = 'Lab') 
	  #cols        = Ramp(201)
	  #gps$col     = cols[201-round(gps$gpsdist,0)]
	  #gps$act     = 50
	gps$datetime_ = as.POSIXct(gps$datetime_)
	gps$day=as.Date(trunc(gps$datetime_,"day"))
	gps$time_=difftime(gps$datetime_,trunc(gps$datetime_,"day"),units="hours")
  return(gps) # IS THIS CORRECT HERE
 }
 
}

{# RFID extraction function 30s
RFID.temperature_actogram_data = function (con = con, nest) {
#on.exit(mysqlCloseConnection(con))
     if (missing(nest)) 
     nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from EXTRA_INCUBATION.Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	# selects distinct 
		rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
                            (select  distinct  lower(r.nest) nest, FUNCTIONS.RoundTime(COALESCE(datetime_corrected,datetime_),'S',30) datetime_,
                                     transp, boutID, 10 act,  NULL who
										from RFID r
											where transp IS NOT NULL AND lower(r.nest) =",nest_,
									" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(coalesce(start_capture_date_time,caught_date_time,release_time),'S',30) datetime_, NULL transp, NULL boutID, 50 act, 'capture' who 
										from EXTRA_INCUBATION.CAPTURES where lower(nest) = ",nest_,
									" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(DATE_SUB(incubation_start, INTERVAL 3 DAY),'S',30) datetime_,NULL transp, NULL boutID, 50 act, 'first egg' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(incubation_start,'S',30) datetime_,NULL transp, NULL boutID, 50 act, 'incubation start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'S',30) datetime_,NULL transp, NULL boutID, 50 act, 'hatching start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(nest_endstate_date,'S',30) datetime_, NULL transp, NULL boutID, 50 act, 'incubation end' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
								") x  
								LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from EXTRA_INCUBATION.CAPTURES c
								LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
									UNION SELECT transponder , 'fieldteam' sex from  EXTRA_INCUBATION.AUTHORS) a
									on x.transp = a.transponder
							"))
 
 
		tts   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'S',30)  datetime_, T_egg as T_egg_TT,'TT' as 'logger'
									from EXTRA_INCUBATION.TinyTag
  										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'S',30) ,nest"))
 
		hobos   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'S',30)  datetime_, T_ambient,'HOBO' as 'logger'
									from EXTRA_INCUBATION.HOBO
										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'S',30) ,nest"))
     
		MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',30)  datetime_, T_egg as T_egg_MSR,T_ambient,'MSR' as 'logger'
									from EXTRA_INCUBATION.MSR
										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'S',30) ,nest"))
 
	# select metadata
		
	# RFID placed on the nest (to limit the data lateR)
	st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(coalesce(datetime_onNest,datetime_start),'S',30)  datetime_
					  from EXTRA_INCUBATION.Device_filestatus
					  where device = 'RFID' and lower(nest) =",nest_))
	st$datetime_ = as.POSIXct(st$datetime_)
	#if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])==0) {print(paste("on missing ",nest_l))} else{
	 #if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])>0) 
	 #{st=min(s$datetime_[s$nest==nest_l & s$yr==yr])} else { st=min(st$datetime_)}}
	st$datetime_ = as.Date(trunc(st$datetime_, "day"))
	st=min(st$datetime_, na.rm=TRUE)
  if (nrow(rfids) > 0) { 
     # last state
		rfids$laststate = dbq(con, paste("select nest_endstate FROM EXTRA_INCUBATION.NESTS  where nest =",nest_))$nest_endstate
		  if (is.null(rfids$laststate)) rfids$laststate = NA  
   
         rfids$datetime_=as.POSIXct(rfids$datetime_)
         rfids$datetime_=as.POSIXct(ifelse(!is.na(rfids$who) & rfids$who=="first egg", as.character(rfids$datetime_-60*60*24*3), as.character(rfids$datetime_)))
      
	 #adds loggers  
		if (nrow(tts)>0) {
			tts$datetime_ = as.POSIXct(tts$datetime_)
			tts$T_ambient = NA
			tts$T_egg_MSR = NA
			rfids = merge(rfids, tts, all = TRUE)
		}
		if (nrow(hobos)>0) {
			hobos$datetime_ = as.POSIXct(hobos$datetime_)
			hobos$T_egg_TT = NA
			hobos$T_egg_MSR = NA
			rfids = merge(rfids, hobos, all = TRUE)
		}
		if (nrow(MSRs)>0) {  
			MSRs$datetime_ = as.POSIXct(MSRs$datetime_)
			MSRs$T_egg_TT = NA
			rfids = merge(rfids, MSRs, all = TRUE)
		}
		if (nrow(MSRs)==0 & nrow(tts)==0 & nrow(hobos)==0) rfids$T_egg_TT  = rfids$T_egg_MSR  = rfids$T_ambient = rfids$logger = NA
 
 
		rfids$who[which(rfids$who%in%c(2,'f','F'))] = 'female'
		rfids$act[which(rfids$who=="female")] = 15
        rfids$who[which(rfids$who%in%c(1,'m','M'))] = 'male'
		rfids$who[is.na(rfids$who)] = 'unknown'
        rfids$day = as.Date(trunc(rfids$datetime_, "day"))
		rfids$time = as.numeric(difftime(rfids$datetime_, trunc(rfids$datetime_,"day"), units = "hours"))
		#days = data.frame(nest=rfids$nest[1], datetime_=NA, transp = NA, boutID = NA, act=1,who = 'nodata',laststate = NA, T_egg_TT = NA,T_egg_MSR=NA, T_ambient = NA, logger=NA, day = c(st:max(rfids$day, na.rm = TRUE)),time = 0.0,stringsAsFactors = FALSE)
        #days$day = structure(days$day, class='Date')
		rfids=rfids[rfids$day>=st,]
		#rfids = rbind(rfids,days) #						  
        
        }  
        return(rfids) 
		}
}
{# RFID extraction function 1 min
RFID.temperature_actogram_data = function (con = con, nest) {
#on.exit(mysqlCloseConnection(con))
     if (missing(nest)) 
     nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from EXTRA_INCUBATION.Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	# selects distinct 
	#DATE_ADD(datetime_,INTERVAL 14449.1511 DAY)
	#COALESCE(datetime_corrected,datetime_)
		if(nest=="re114_nl2014"){
			rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
                            (select  distinct  lower(r.nest) nest,  DATE_SUB(FUNCTIONS.RoundTime(DATE_ADD(datetime_,INTERVAL 10 YEAR),'M',1), INTERVAL 10 YEAR)  datetime_,
                                     transp, boutID, 10 act,  NULL who
										from RFID r
											where transp IS NOT NULL AND lower(r.nest) =",nest_,
									" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(coalesce(start_capture_date_time,caught_date_time,release_time),'M',1) datetime_, NULL transp, NULL boutID, 50 act, 'capture' who 
										from EXTRA_INCUBATION.CAPTURES where lower(nest) = ",nest_,
									" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(DATE_SUB(incubation_start, INTERVAL 3 DAY),'M',1) datetime_,NULL transp, NULL boutID, 50 act, 'first egg' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(incubation_start,'M',1) datetime_,NULL transp, NULL boutID, 50 act, 'incubation start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'M',1) datetime_,NULL transp, NULL boutID, 50 act, 'hatching start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(nest_endstate_date,'M',1) datetime_, NULL transp, NULL boutID, 50 act, 'incubation end' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
								") x  
								LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from EXTRA_INCUBATION.CAPTURES c
								LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
									UNION SELECT transponder , 'fieldteam' sex from  EXTRA_INCUBATION.AUTHORS) a
									on x.transp = a.transponder
							"))
							}else{
							rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
                            (select  distinct  lower(r.nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1) datetime_,
                                     transp, boutID, 10 act,  NULL who
										from RFID r
											where transp IS NOT NULL AND lower(r.nest) =",nest_,
									" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(coalesce(start_capture_date_time,caught_date_time,release_time),'M',1) datetime_, NULL transp, NULL boutID, 50 act, 'capture' who 
										from EXTRA_INCUBATION.CAPTURES where lower(nest) = ",nest_,
									" UNION SELECT lower(nest) nest, FUNCTIONS.RoundTime(DATE_SUB(incubation_start, INTERVAL 3 DAY),'M',1) datetime_,NULL transp, NULL boutID, 50 act, 'first egg' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(incubation_start,'M',1) datetime_,NULL transp, NULL boutID, 50 act, 'incubation start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'M',1) datetime_,NULL transp, NULL boutID, 50 act, 'hatching start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, FUNCTIONS.RoundTime(nest_endstate_date,'M',1) datetime_, NULL transp, NULL boutID, 50 act, 'incubation end' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
								") x  
								LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from EXTRA_INCUBATION.CAPTURES c
								LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
									UNION SELECT transponder , 'fieldteam' sex from  EXTRA_INCUBATION.AUTHORS) a
									on x.transp = a.transponder
							"))
 
							
							
							
							
							}
 
	
		tts   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, T_egg as T_egg_TT,'TT' as 'logger'
									from EXTRA_INCUBATION.TinyTag
  										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
 
		hobos   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, T_ambient,'HOBO' as 'logger'
									from EXTRA_INCUBATION.HOBO
										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
     
		MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, T_egg as T_egg_MSR,T_ambient,'MSR' as 'logger'
									from EXTRA_INCUBATION.MSR
										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
 
	# select metadata
		
	# RFID placed on the nest (to limit the data lateR)
	st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(coalesce(datetime_onNest,datetime_start),'M',1)  datetime_ 
							from EXTRA_INCUBATION.Device_filestatus
								where device = 'RFID' and lower(nest) =",nest_))
	st$datetime_ = as.POSIXct(st$datetime_)
	#if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])==0) {print(paste("on missing ",nest_l))} else{
	 #if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])>0) 
	 #{st=min(s$datetime_[s$nest==nest_l & s$yr==yr])} else { st=min(st$datetime_)}}
	st$datetime_ = as.Date(trunc(st$datetime_, "day"))
	st=min(st$datetime_, na.rm=TRUE)
  if (nrow(rfids) > 0) { 
     # last state
		rfids$laststate = dbq(con, paste("select nest_endstate FROM EXTRA_INCUBATION.NESTS  where nest =",nest_))$nest_endstate
		  if (is.null(rfids$laststate)) rfids$laststate = NA  
   
         rfids$datetime_=as.POSIXct(rfids$datetime_)
         #rfids$datetime_=as.POSIXct(ifelse(!is.na(rfids$who) & rfids$who=="first egg", as.character(rfids$datetime_-60*60*24*3), as.character(rfids$datetime_)))
			if(unique(rfids$nest)=="wh303_is2013"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+14449.1511*24*60*60}
			if(unique(rfids$nest)=="re304_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]-3341.420833*24*60*60}
			if(unique(rfids$nest)=="re114_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+18662.15788*24*60*60}
			if(unique(rfids$nest)=="oyc10_is2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+3630.301319*24*60*60}
			if(unique(rfids$nest)=="oycx_is2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+3630.301319*24*60*60}
			if(unique(rfids$nest)=="re123_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+0.257604167*24*60*60}
		 #rfids$datetime_[13829]+14449.1511*24*60*60
      
	 #adds loggers  
		if (nrow(tts)>0) {
			tts$datetime_ = as.POSIXct(tts$datetime_)
			tts$T_ambient = NA
			tts$T_egg_MSR = NA
			rfids = merge(rfids, tts, all = TRUE)
		}
		if (nrow(hobos)>0) {
			hobos$datetime_ = as.POSIXct(hobos$datetime_)
			hobos$T_egg_TT = NA
			hobos$T_egg_MSR = NA
			rfids = merge(rfids, hobos, all = TRUE)
		}
		if (nrow(MSRs)>0) {  
			MSRs$datetime_ = as.POSIXct(MSRs$datetime_)
			MSRs$T_egg_TT = NA
			rfids = merge(rfids, MSRs, all = TRUE)
		}
		if (nrow(MSRs)==0 & nrow(tts)==0 & nrow(hobos)==0) rfids$T_egg_TT  = rfids$T_egg_MSR  = rfids$T_ambient = rfids$logger = NA
 
 
		rfids$who[which(rfids$who%in%c(2,'f','F'))] = 'female'
		rfids$act[which(rfids$who=="female")] = 15
        rfids$who[which(rfids$who%in%c(1,'m','M'))] = 'male'
		rfids$who[is.na(rfids$who)] = 'unknown'
        rfids$day = as.Date(trunc(rfids$datetime_, "day"))
		rfids$time = as.numeric(difftime(rfids$datetime_, trunc(rfids$datetime_,"day"), units = "hours"))
		#days = data.frame(nest=rfids$nest[1], datetime_=NA, transp = NA, boutID = NA, act=1,who = 'nodata',laststate = NA, T_egg_TT = NA,T_egg_MSR=NA, T_ambient = NA, logger=NA, day = c(st:max(rfids$day, na.rm = TRUE)),time = 0.0,stringsAsFactors = FALSE)
        #days$day = structure(days$day, class='Date')
		#rfids=rfids[rfids$day>=st,]
		#rfids = rbind(rfids,days) #						  
        
		if(m$nest=="oycx_is2014"){
			rfids=rfids[rfids$day> as.Date("2014-04-19") & rfids$day<as.Date("2014-05-18"),]
				rfids$nest="oycx_is2014" }else{
				if(m$nest=="oyc10_is2014"){rfids=rfids[rfids$day> as.Date("2014-05-18"),]}else{
					rfids=rfids[rfids$day>=st,]}}
	    }  
        return(rfids) 
		}
}
{# RFID extraction function raw
RFID.temperature_actogram_data = function (con = con, nest) {
#on.exit(mysqlCloseConnection(con))
     if (missing(nest)) 
     nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from EXTRA_INCUBATION.Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	# selects distinct 

		rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
                            (select  distinct  lower(r.nest) nest, datetime_,
                                     transp, boutID, 10 act,  NULL who
										from RFID r
											where transp IS NOT NULL AND lower(r.nest) =",nest_,
									" UNION SELECT lower(nest) nest, coalesce(start_capture_date_time,caught_date_time,release_time) datetime_, NULL transp, NULL boutID, 50 act, 'capture' who 
										from EXTRA_INCUBATION.CAPTURES where lower(nest) = ",nest_,
									" UNION SELECT lower(nest) nest,DATE_SUB(incubation_start, INTERVAL 3 DAY) datetime_,NULL transp, NULL boutID, 50 act, 'first egg' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, incubation_start  datetime_,NULL transp, NULL boutID, 50 act, 'incubation start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, hatch_start datetime_,NULL transp, NULL boutID, 50 act, 'hatching start' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
									" UNION SELECT  lower(nest) nest, nest_endstate_date datetime_, NULL transp, NULL boutID, 50 act, 'incubation end' who
										from EXTRA_INCUBATION.NESTS where lower(nest) = ",nest_,
								") x  
								LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from EXTRA_INCUBATION.CAPTURES c
								LEFT JOIN EXTRA_INCUBATION.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
									UNION SELECT transponder , 'fieldteam' sex from  EXTRA_INCUBATION.AUTHORS) a
									on x.transp = a.transponder
							"))
 
 
		tts   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, T_egg as T_egg_TT,'TT' as 'logger'
									from EXTRA_INCUBATION.TinyTag
  										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
 
		hobos   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, T_ambient,'HOBO' as 'logger'
									from EXTRA_INCUBATION.HOBO
										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
     
		MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',30)  datetime_, T_egg as T_egg_MSR,T_ambient,'MSR' as 'logger'
									from EXTRA_INCUBATION.MSR
										where lower(nest) =",nest_,"
										group by FUNCTIONS.RoundTime(datetime_,'S',30) ,nest"))
 
	# select metadata
		
	# RFID placed on the nest (to limit the data lateR)
	st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(coalesce(datetime_onNest,datetime_start),'M',1)  datetime_
					  from EXTRA_INCUBATION.Device_filestatus
					  where device = 'RFID' and lower(nest) =",nest_,""))
	st$datetime_ = as.POSIXct(st$datetime_)
	#if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])==0) {print(paste("on missing ",nest_l))} else{
	 #if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])>0) 
	 #{st=min(s$datetime_[s$nest==nest_l & s$yr==yr])} else { st=min(st$datetime_)}}
	st$datetime_ = as.Date(trunc(st$datetime_, "day"))
	st=min(st$datetime_, na.rm=TRUE)
  if (nrow(rfids) > 0) { 
     # last state
		rfids$laststate = dbq(con, paste("select nest_endstate FROM EXTRA_INCUBATION.NESTS  where nest =",nest_))$nest_endstate
		  if (is.null(rfids$laststate)) rfids$laststate = NA  
   
         rfids$datetime_=as.POSIXct(rfids$datetime_)
        # rfids$datetime_=as.POSIXct(ifelse(!is.na(rfids$who) & rfids$who=="first egg", as.character(rfids$datetime_-60*60*24*3), as.character(rfids$datetime_)))
			if(unique(rfids$nest)=="wh303_is2013"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+14449.1511*24*60*60}
			if(unique(rfids$nest)=="re304_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]-3341.420833*24*60*60}
			if(unique(rfids$nest)=="re114_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+18662.15788*24*60*60}
			if(unique(rfids$nest)=="oyc10_is2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+3630.301319*24*60*60}
			if(unique(rfids$nest)=="oycx_is2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+3630.301319*24*60*60}
			if(unique(rfids$nest)=="re123_nl2014"){rfids$datetime_[!is.na(rfids$transp)]=rfids$datetime_[!is.na(rfids$transp)]+0.257604167*24*60*60}
	 #adds loggers  
		if (nrow(tts)>0) {
			tts$datetime_ = as.POSIXct(tts$datetime_)
			tts$T_ambient = NA
			tts$T_egg_MSR = NA
			rfids = merge(rfids, tts, all = TRUE)
		}
		if (nrow(hobos)>0) {
			hobos$datetime_ = as.POSIXct(hobos$datetime_)
			hobos$T_egg_TT = NA
			hobos$T_egg_MSR = NA
			rfids = merge(rfids, hobos, all = TRUE)
		}
		if (nrow(MSRs)>0) {  
			MSRs$datetime_ = as.POSIXct(MSRs$datetime_)
			MSRs$T_egg_TT = NA
			rfids = merge(rfids, MSRs, all = TRUE)
		}
		if (nrow(MSRs)==0 & nrow(tts)==0 & nrow(hobos)==0) rfids$T_egg_TT  = rfids$T_egg_MSR  = rfids$T_ambient = rfids$logger = NA
 
 
		rfids$who[which(rfids$who%in%c(2,'f','F'))] = 'female'
		rfids$act[which(rfids$who=="female")] = 15
        rfids$who[which(rfids$who%in%c(1,'m','M'))] = 'male'
		rfids$who[is.na(rfids$who)] = 'unknown'
        rfids$day = as.Date(trunc(rfids$datetime_, "day"))
		rfids$time = as.numeric(difftime(rfids$datetime_, trunc(rfids$datetime_,"day"), units = "hours"))
		#days = data.frame(nest=rfids$nest[1], datetime_=NA, transp = NA, boutID = NA, act=1,who = 'nodata',laststate = NA, T_egg_TT = NA,T_egg_MSR=NA, T_ambient = NA, logger=NA, day = c(st:max(rfids$day, na.rm = TRUE)),time = 0.0,stringsAsFactors = FALSE)
        #days$day = structure(days$day, class='Date')
		
		if(m$nest=="oycx_is2014"){
			rfids=rfids[rfids$day> as.Date("2014-04-19") & rfids$day<as.Date("2014-05-18"),]
				rfids$nest="oycx_is2014" }else{
				if(m$nest=="oyc10_is2014"){rfids=rfids[rfids$day> as.Date("2014-05-18"),]}else{
					rfids=rfids[rfids$day>=st,]}}
		#rfids = rbind(rfids,days) #						  
        
        }  
        return(rfids) 
		}
}

{# actogram function	
RFID.temperature_actogram = function(dfr, type, utc=TRUE, show=FALSE) {
		
        if (missing(dfr)) dfr = RFID.temperature_actogram_data()
		#if (missing(gps)) gps = GPS.temperature_actogram_data()
		
        if (type =='PDF') {
           tf = paste0(outdir,'/',dfr$nest[1], ".pdf")
           pdf(tf, paper = "a4", width = 8, height = 11.6929134)
		}
		 if (type =='PNG') { # ASK ANNE HOW THIS WORKS
          tf = paste0(outdir,'/',paste(dfr$nest[1],spe="_"), "_%03d.png") # %03d - three position digit file name
          png(tf,, width = 210, height = 297, units = "mm", res = 600)	
	
		dfr=dfr[!is.na(dfr$datetime_),]
		dfr$datetime_utc=dfr$datetime_-m$utc_plus*60*60
		gps$datetime_utc=gps$datetime_-m$utc_plus*60*60
		if(utc==TRUE){
		 dfr$datetime_= dfr$datetime_utc + (m$lon/15)*60*60
         dfr$day = as.Date(trunc(dfr$datetime_, "day"))
		 dfr$time = as.numeric(difftime(dfr$datetime_, trunc(dfr$datetime_,"day"), units = "hours"))
		 gps$datetime_= gps$datetime_utc + (m$lon/15)*60*60
		 gps$day= as.Date(trunc(gps$datetime_, "day"))
         gps$time_ = as.numeric(difftime(gps$datetime_, trunc(gps$datetime_,"day"), units = "hours"))
		  }
		
		 
   		  clr = actogram.cols
		  mf = ddply(dfr,.(transp, who),summarize, ntransp = length(transp))
		  mf = mf[order(mf$who, mf$ntransp),]
		  mm=mf$transp[mf$who=='male'][1]
		  ff=mf$transp[mf$who=='female'][1]
		 # mm=dfr$transp[dfr$who=='male' & nchar(dfr$transp)>12][1]
		  #ff=dfr$transp[dfr$who=='female' & nchar(dfr$transp)>12][1]
		  nest_=unique(dfr$nest[!is.na(dfr$nest)]	)      
          nest_ =shQuote(tolower(nest_))
		  
		  dfr     = merge(dfr,clr,all.x=TRUE)	
          
		  transplist = unique(dfr$transp[which(!is.na(dfr$transp))])
		  trCol      = data.frame(transp = transplist, col2 = rainbow(length(transplist)),stringsAsFactors = FALSE)
		  dfr        = merge(dfr,trCol,all.x=TRUE)
		  dfr$cols[which(is.na(dfr$cols))] = dfr$col2[which(is.na(dfr$cols))]
		   unknowns = trCol[which(trCol$transp %in% dfr$transp[which(dfr$who=='unknown')]),]
          
		  dfr = dfr[order(dfr$day,dfr$time), ]
          sl = unique(dfr$day)
		  
				dfr$sunrise=sunriset(matrix(c(m$lon,m$lat),nrow=1),dfr$datetime_utc+(m$lon/15)*60*60, direction=c("sunrise"),POSIXct.out=TRUE)$time+(m$lon/15)*60*60
				dfr$sunset=sunriset(matrix(c(m$lon,m$lat),nrow=1),dfr$datetime_utc+(m$lon/15)*60*60, direction=c("sunset"),POSIXct.out=TRUE)$time+(m$lon/15)*60*60 
						
				dfr$c_dawn=crepuscule(matrix(c(m$lon,m$lat),nrow=1),dfr$datetime_utc+(m$lon/15)*60*60, solarDep=6,direction=c("dawn"),POSIXct.out=TRUE)$time+(m$lon/15)*60*60
				dfr$c_dusk=crepuscule(matrix(c(m$lon,m$lat),nrow=1),dfr$datetime_utc+(m$lon/15)*60*60, solarDep=6,direction=c("dusk"),POSIXct.out=TRUE)$time+(m$lon/15)*60*60
					
				dfr$twilight=ifelse(is.na(dfr$c_dawn), ifelse(dfr$datetime_<dfr$sunrise & dfr$time>=0, 1,NA), ifelse(dfr$datetime_<dfr$sunrise & dfr$datetime_>=dfr$c_dawn, 1,NA))
				dfr$twilight=ifelse(is.na(dfr$twilight), ifelse(is.na(dfr$c_dusk), ifelse(dfr$datetime_>dfr$sunset & dfr$time<=24, 1, NA), ifelse(dfr$datetime_>dfr$sunset & dfr$datetime_<=dfr$c_dusk, 1, NA)), dfr$twilight)
					  
				dfr$night=ifelse(is.na(dfr$c_dawn), NA, ifelse(dfr$datetime_<dfr$c_dawn & dfr$time>=0, 1,NA))
				dfr$night=ifelse(is.na(dfr$c_dusk), dfr$night, ifelse(dfr$datetime_>dfr$c_dusk & dfr$time<=24, 1, dfr$night))
				
				
            
		  strip.left = function(which.panel, ...) {
                LAB = format(sl[which.panel], "%b-%d")
                grid.rect(gp = gpar(fill = "grey"))
                ltext(0.5, 0.5, cex = 0.8, LAB)
            }
			
		   #scales = list(x = list(at = 0:24, labels = rep(format(seq.POSIXt(trunc(Sys.time(), 
            #"day"), trunc(Sys.time(), "day") + 23 * 3600, "hours"), 
            #"%H:%M"), len = 49), rot = 90, cex = 0.7, limits = c(0, 
            #24),alternating=3), y = list(limits = c(0, 50),at =c(10,30), alternating=2, cex=0.6, tck=0.4,))
			
		   scales = list(x = list(at=c(0,6,12,18,24),labels=c('00:00','06:00','12:00','18:00','24:00') , #cex = 0.7, 
							limits = c(0,24),alternating=3), y = list(limits = c(0, 50),at =c(10,30), alternating=2, cex=0.6, tck=0.4))	
		
			panel = function(...) {	
				panel.abline(v=c(1:5,7:11,13:17,19:23),col="light grey")
				panel.abline(v=c(6,12,18))
				panel.abline(v=dfr$time[dfr$day==sl[panel.number()] & dfr$twilight==1],col="grey66")
						   panel.abline(v=dfr$time[dfr$day==sl[panel.number()] & dfr$night==1],col="grey35")	
				panel.xyplot(...)#temperatures
				d = dfr[which(dfr$day == sl[panel.number()]),]
				panel.xyplot(d$time, d$act, col = d$cols, 
                  type = "h")
				}
             
			
            # fate and loggers for title
			fate = dfr$laststate[!(is.na(dfr$laststate))][1]
				if(length(fate)==0) {fate='not in database'}else{
				fate = ifelse(fate == "p", "predated", ifelse(fate == 
                "hd", "hatched", ifelse(fate == "ud", "undetermined", 
                ifelse(fate == "d", "deserted", 'to be determined'))))
				}
			maintitle = paste(paste(m$scinam, m$country, m$year_, m$nest, sep=", "), ", last state = ", fate, sep="")
			

			{# legend 
				clr_a=clr[clr$who%in%c('female','male','fieldteam','unknown',"ambient temperature", "egg temperature"),] 
				clr_a$who=ifelse(clr_a$who=='female', paste('\u2640 incubation',ff), ifelse(clr_a$who=='male', paste('\u2642 incubation',mm), clr_a$who))
				#clr_a=rbind(clr_a, unknowns)
		
				clr_b=clr[clr$who%in%c("sun < -6°","sun < 0°","first egg", "incubation start", "hatching start", "incubation end"),]
				clr_d=data.frame(who=c("nest visit","capture","someone between 100 & 200 m","someone between 50 & 100 m","someone between 25 & 50 m","someone between 10 & 25 m","someone within 10 m"), cols=c("pink","red","gray97","gray85","gray70","gray55","gray40"), stringsAsFactors=FALSE)
				
				if(nrow(clr_a)<nrow(clr_d)){
											clr_x=data.frame(who=rep("",nrow(clr_d)-nrow(clr_a)), cols=rep("white",nrow(clr_d)-nrow(clr_a)), stringsAsFactors=FALSE)
											clr_a=rbind(clr_a,clr_x)
											}
				if(nrow(clr_b)<nrow(clr_d)){
											clr_x=data.frame(who=rep("",nrow(clr_d)-nrow(clr_b)), cols=rep("white",nrow(clr_d)-nrow(clr_b)), stringsAsFactors=FALSE)
											clr_b=rbind(clr_b,clr_x)
											}
				
				key = list(
					text = list(clr_a$who,cex=0.7), 
					points = list(pch = 19, col = clr_a$cols),
					text = list(clr_b$who,cex=0.7), 
					points=list(pch=15,col=c(clr_b$cols),cex=0.9),
					text = list(clr_d$who,cex=0.7),
					points=list(pch=15,col=c(clr_d$cols),cex=0.9)		
					)
				}
			
			par(pin = c(8.26771654, 11.6929134)) # what does it do?
            
			rfidsplot = xyplot(T_ambient + T_egg_TT + T_egg_MSR ~ time | day, 
                data = dfr, col = c("dark blue", "light blue", "light blue"), 
                cex = 0.1, strip.left = strip.left, layout = c(1,30),#ifelse(length(sl) > 25, 25, length(sl))), 
				as.table = TRUE, 
                panel = panel, aspect = "fill", strip = FALSE, 
                distribute.type = TRUE, cex.title=0.5,main = maintitle,  scales = scales, 
				ylab='Date',
				ylab.right='Temperature [°C]',
				xlab='Time [h]',
				key=key,
                lattice.options = list(layout.widths = list(strip.left = list(x = 3))))
            
			print(rfidsplot)
			#dev.off()
        if(type %in% c('PNG','PDF')) {
        dev.off()
        if (show) shell.exec(tf)
			}
		save(dfr,rfidsplot,strip.left, scales, key, panel, maintitle,sl, m,s_b,file= paste0(outdir,'/',paste(dfr$nest[1],sep="_"), ".Rdata"))	
		
		print(maintitle)
}
}
}
	

{# to make multiple actograms (otherwise just run RFID.temperature_actogram(con=con) :
nests=c('ka1411_is2014','re117_nl2014','re133_nl2014','re213_nl2014')

nests=c('o502_is2013','o503_is2013','o504_is2013','o505_is2013','oyc09_is2014','oyc32_is2014')
nests=c('lr504_cz2014','lr512_cz2014','lr903_cz2014','lr904_cz2014','lr905_cz2014','lr906_cz2014','lr909_cz2014','lr910_cz2014','lr913_cz2014','lr914_cz2014','lr916_cz2014')
nests=c('ri102_is2013','ri103_is2013')
nests=c('re901_is2013','re902_is2013')
	nests = dbq(con, paste("select distinct lower(nest) nest from EXTRA_INCUBATION.Device_filestatus where device='RFID' and nest not in ('S629','f666') order by nest"))$nest # S629 is excluded

nests=c('re304_nl2014','re114_nl2014','oyc10_is2014','wh303_is2013')
S629
nests=c('wh303_is2013')
nests=c('oyc10_is2014')
nests=c('oycx_is2014')
nests=c('re212_nl2014','re214_nl2014')
nests=c('re107_nl2014','re005_nl2012','re006_nl2012','re007_nl2012','re104_nl2014','re110_nl2014','re112_nl2014','re123_nl2014','re209_nl2014', 're302_nl2014' )
	
for (i in length(nests)){#c(1:4,21)){{c(3,4,8,12,17,32,33,36,42,45,48,57,58,74,76,81,83,85,88,102,120,121,122,123,124,127)){#1:length(nests)) {#(93:117)){#length(nests))) {
	m=meta_nest.data(con=con,nest = nests[i])
	s_b=meta_birds.data(con=con,nest = nests[i])
	#if(i==129 & is.na(m$lat)){m$lat=63.7885; m$lon=-20.19052}
	dfr = RFID.temperature_actogram_data(con=con,nest = nests[i])
   	#gps=GPS.temperature_actogram_data (con=con, nest=nests[i],yr=yr) 
	RFID.temperature_actogram(dfr,type=type, utc=FALSE)
  } 

  } 
 
{# to make one actogram 
	nest='oyc32_is2014' 
	# sets working directory
	m=meta.data(con=con,nest = nest)
	dfr=RFID.temperature_actogram_data (con=con, nest=nest) 
	#gps=GPS.temperature_actogram_data (con=con, nest=nest) 	
	RFID.temperature_actogram (dfr,type=type, show=FALSE)
	
}	

