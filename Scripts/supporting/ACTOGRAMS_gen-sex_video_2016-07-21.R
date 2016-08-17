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
require(tcltk2)

Sys.setenv(TZ="UTC")

} 


{# !!! already done, just load it in CONSTANTS
{# get video from field DB
# vid 2013
con = dbConnect(dbDriver("MySQL"), user='root', dbname = 'barrow_2013', host = '127.0.0.1', port=3306)
v=dbGetQuery(con,"select lower(nest) nest, datetime_left datetime_, recorder, recorder_ID from barrow_2013.visits where recorder is not null and recorder_ID <> ('999')")

v$status=ifelse(v$recorder%in%c('on','newBatt44','newBatt31','newBat24','newBat12','newBat36'),'on', ifelse(v$recorder=='off', 'off', NA))
vv=v[-which(is.na(v$status)),]
vv=vv[order(vv$nest,vv$datetime_),]

#vv=ddply(vv,.(nest), transform, after=c(status[-1],NA))
vv=ddply(vv,.(nest), transform, before=c(NA,status[-length(status)]))
vv=vv[-which(vv$status=='on' & vv$before=='on'),]
vv=vv[-which(vv$nest=='s611'),]

v=vv[,c('nest','datetime_','status'),]
v$video=20
v$yr=2013
v$logger='video'
v$datetime_=as.character(v$datetime_)
# vid 2012
#db="\\\\ds\\raw_data_kemp\\FIELD\\Iceland\\2013\\DATA\\Database\\iceland_2013.sqlite"
db="M:\\PROJECTS\\PHD\\FIELD_METHODS\\2012\\DATA\\Database\\Barrow_2012.sqlite"
con = dbConnect(dbDriver("SQLite"),dbname = db)
d=dbGetQuery(con,"select lower(nest) nest,  (case when (datetime_left is null) THEN datetime_ else datetime_left end) as datetime_ , recorder as status from VISITS where recorder is not null")
x=data.frame(nest=c('s601','s802','s812'),datetime_=c('2012-06-15 12:07:00','2012-06-21 11:00:00','2012-06-22 13:19:00'),status=0)
v2=rbind(d,x)
v2$datetime_ = as.POSIXct(v2$datetime_)
v2=v2[order(v2$nest,v2$datetime_),]
v2$video=20
v2$yr=2012
v2$logger='video'
v2$status=ifelse(v2$status==1, "on", "off")
v2=v2[-which(v2$nest=='s1003' & v2$datetime_==as.POSIXct('2012-06-26 18:13:00')),]
v2=v2[-which(v2$nest=='s313' & v2$datetime_==as.POSIXct('2012-06-28 11:10:00')),]
v2=v2[-which(v2$nest=='s322' & v2$datetime_==as.POSIXct('2012-07-05 19:38:00')),]
v2=v2[-which(v2$nest=='s403' & v2$datetime_==as.POSIXct('2012-06-15 19:02:00')),]
v2=v2[-which(v2$nest=='s506' & v2$datetime_==as.POSIXct('2012-06-21 20:37:00')),]
v2=v2[-which(v2$nest=='s511' & v2$datetime_==as.POSIXct('2012-07-05 19:29:00')),]
v2$datetime_[v2$nest=='s511' & v2$datetime_==as.POSIXct('2012-07-06 08:27:00')]= '2012-07-04 19:29:00'
v2=v2[-which(v2$nest=='s609' & v2$datetime_==as.POSIXct('2012-06-26 17:59:01')),]
v2=v2[-which(v2$nest=='s705' & v2$datetime_==as.POSIXct('2012-06-15 19:16:00')),]
v2=v2[-which(v2$nest=='s705' & v2$datetime_==as.POSIXct('2012-06-17 15:29:00')),]
v2=v2[-which(v2$nest=='s705' & v2$datetime_==as.POSIXct('2012-06-20 10:35:00')),]
v2=v2[-which(v2$nest=='s706' & v2$datetime_==as.POSIXct('2012-06-20 12:57:00')),]
v2=v2[-which(v2$nest=='s708' & v2$datetime_==as.POSIXct('2012-06-18 13:28:00')),]
v2=v2[-which(v2$nest=='s712' & v2$datetime_==as.POSIXct('2012-06-25 19:38:00')),]
v2=v2[-which(v2$nest=='s801' & v2$datetime_==as.POSIXct('2012-06-21 19:22:00 ')),]
v2$datetime_=as.character(v2$datetime_)

v=rbind(v,v2)

}
{# treatment 2013
con = dbcon(user='root',host='127.0.0.1', password='',database='barrow_2013')

mf=dbGetQuery(con,"select lower(nest) as nest, (case when (treatment='Tm') THEN male_ID else female_ID end) as ring_num from barrow_2013.nests where treatment in ('Tm', 'Tf')")
ts=dbGetQuery(con,"select ring_num, datetime_ , phase from barrow_2013.captivity where phase='start'")
tr=dbGetQuery(con,"select ring_num, datetime_ , phase from barrow_2013.captivity where phase='released'")

tt=rbind(ts,tr)
tt=tt[order(tt$ring_num),]

tr=merge(tt,mf,by='ring_num',all.x=TRUE)
tr$nest[tr$ring_num==255174365]='S519'
tr$logger='bird removed'
tr$yr=2013
con = dbConnect(dbDriver("MySQL"), username = 'mbulla', password='Kvetak25', dbname = '', host = 'scidb.orn.mpg.de')
se=dbGetQuery(con,"select ID as ring_num, (case when (sex=1) then 'male' else 'female' end) sex from AVESatBARROW.SEX")

tr=merge(tr,se, by='ring_num', all.x=TRUE)
# nest with dead birds 257188566, 257188570
}
{# start of the RFID on the nest from field DB
con = dbConnect(dbDriver("MySQL"), user='root', dbname = 'barrow_2013', host = '127.0.0.1', port=3306)

s=dbGetQuery(con,"select '2013' yr, lower (nest) nest, (case when (nest='S607') THEN datetime_ else datetime_left end) as datetime_ from barrow_2013.visits where RFID='on'")


con = dbConnect(dbDriver("MySQL"), username = 'mbulla', password='Kvetak25', dbname = '', host = 'scidb.orn.mpg.de')
 u=dbGetQuery(con,"SELECT year_ yr, lower(nest) nest, datetime_onNest as datetime_ FROM LOGGERSatBARROW.Device_filestatus where year_ in ('2011','2012') and device='RFID'")
  #UNION SELECT lower(nest) nest, datetime_offNest as datetime_ FROM LOGGERSatBARROW.Device_filestatus where year_ in ('2011','2012') and device='RFID'")
 
 u$datetime_[u$yr==2011 & u$nest=='l401'] = '2011-06-30 16:30:00'
 
s=rbind(u,s) 
s$datetime_ = as.POSIXct(s$datetime_)
 }
save(v,tr,s, file = "\\\\ds\\grpkempenaers\\Martin\\prep_for_ACTOS.Rdata")
 }
 
{# define constants
load("\\\\ds\\grpkempenaers\\Martin\\prep_for_ACTOS.Rdata")

#con = dbcon(user='mbulla',host='scidb.orn.mpg.de', password='Kvetak25',database='LOGGERSatBARROW')
con = dbcon(user='root',host='127.0.0.1', password='',database='LOGGERSatBARROW')
actogram.cols = data.frame(who  = c('video', 'one bird removed','heating','male','female','fieldteam','unknown','nest visit','capture','captive released', "first egg", "incubation start", "hatching start", "incubation end"),
                           cols = c("darkolivegreen2","darkviolet","yellow2",transpcol("dodgerblue", newalpha=150),transpcol("orange", newalpha=150),"black","deeppink4","pink","red","darkviolet",'#F6E8C3','#DFC27D','#BF812D','#8C510A'),
						   stringsAsFactors = FALSE)
						   
vid_col="darkolivegreen2"
type   = 'PNG'	# or PDF
yr=2013
#outdir = "M:\\PROJECTS\\PHD\\STATS\\DATASETS\\comparative\\drop\\rfid\\BARROW"

if(yr==2013){
outdir ="C:/Users/mbulla/Documents/ownCloud/ACTOS_2013_barrow-time"
#"\\\\ds\\RAW_DATA_KEMP\\FIELD\\Barrow\\2013\\post-Season\\actograms\\2015_Dec"			

} else {if (yr==2012){outdir ="C:/Users/mbulla/Documents/ownCloud/ACTOS_Bip_unip_local_time"
#outdir = "\\\\ds\\RAW_DATA_KEMP\\FIELD\\Barrow\\2012\\post-Season\\DOCS\\actograms\\PNG\\2015_Feb_raw"			
} else {if (yr==2011){
outdir = "\\\\ds\\RAW_DATA_KEMP\\FIELD\\Barrow\\2011\\DOCS\\actograms\\2015_Feb"	
 }}}
}


#FUNCTIONS for 1 min averages-RUN THIS BIT FIRST! - # for 2012-2013 deateitme_ for 2011 also corrected datetime_
{# data extraction function
meta_nest.data=function(con=con, nest){
 if (missing(nest)) 
       nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	m = dbq(con, paste("select species sp, lower(nest) nest,year_, IDfemale, IDmale, latit lat, longit lon, loggers 
						from AVESatBARROW.NESTS
									where lower(nest) =",nest_,"and year_ = ",yr,""))
			
			m$scinam=ifelse(m$sp=="BASA","Calidris bairdii ", 
							ifelse(m$sp=="AMGP", "Pluvialis dominica",
								ifelse(m$sp== "DUNL", "Calidris alpina",
									ifelse(m$sp== "SESA", "Calidris pusilla", 
										ifelse(m$sp=="LBDO",  "Limnodromus scolopaceus", 
											ifelse(m$sp=="RUTU", "Arenaria interpres", 
												ifelse(m$sp=="SEPL", "Charadrius semipalmatus", 
													ifelse(m$sp=="WESA", "Calidris mauri", NA)))))))) 
			
			m$utc_plus=8
			
			m$breeding_site="barr"
									
	return(m)
	
}
meta_birds.data=function(con=con, nest){
 if (missing(nest)) 
       nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	m = dbq(con, paste("select species sp, lower(nest) nest,year_, IDfemale, IDmale, latit lat, longit lon, loggers 
						from AVESatBARROW.NESTS
									where lower(nest) =",nest_,"and year_ = ",yr,""))
	n=c(m$IDfemale,m$IDmale)
	n=n[!is.na(n)]
	if(length(n)==1){
	s_b = dbq(con, paste("select*from AVESatBARROW.CAPTURES c
										LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID
										where c.ID in (",n,")"))
										}else{
										s_b = dbq(con, paste("select*from AVESatBARROW.CAPTURES c
										LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID
										where c.ID in (",m$IDfemale,",",m$IDmale,")"))
										}
	
	return(s_b)
}
RFID.temperature_actogram_data = function (con = con, nest, yr) {
#on.exit(mysqlCloseConnection(con))
  if (missing(yr))
	 yr = pickOne(c(2013,2012,2011))
    if (missing(nest)) 
     nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	# selects distinct
    if (yr==2011 | yr==2013 & nest_l%in%c('s622','s629')) {
		rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
	                                (select  distinct  year_,lower(r.nest) nest, FUNCTIONS.RoundTime(COALESCE(datetime_corrected,datetime_),'M',1) datetime_,
	                                    transp, boutID, 10 act,  NULL who
								        from RFID r
								        where transp IS NOT NULL AND lower(r.nest) =",nest_,
								    " UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(coalesce(start_capture_date_time,caught_date_time,release_time),'M',1)  datetime_,
								        NULL transp, NULL boutID, 50 act, 'capture' who
								        from AVESatBARROW.CAPTURES where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(laying_date,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'first egg' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,	
									 " UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(incubation_start,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'incubation start' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'hatching start' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(nest_endstate_date,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'incubation end' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,										
									" UNION SELECT year_, lower(nest) nest,  FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_,
								        NULL transp, NULL boutID, 50 act, 'nest visit' who
								        from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,") x  
										LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from AVESatBARROW.CAPTURES c
								                     LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
												   UNION SELECT transponder , 'fieldteam' sex from  AVESatBARROW.AUTHORS) a
								       on x.transp = a.transponder	
                                  where x.year_ = ",yr)) 
			if(unique(rfids$nest)=="s629"){ rfids=rfids[-which(as.character(rfids$datetime_)=='2013-06-24 18:56:00' & rfids$who=='fieldteam'),]}
			
			}else{
			rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
	                                (select  distinct  year_,lower(r.nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1) datetime_,
	                                    transp, boutID, 10 act,  NULL who
								        from RFID r
								        where transp IS NOT NULL AND lower(r.nest) =",nest_,
								    " UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(coalesce(start_capture_date_time,caught_date_time,release_time),'M',1)  datetime_,
								        NULL transp, NULL boutID, 50 act, 'capture' who
								        from AVESatBARROW.CAPTURES where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(laying_date,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'first egg' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,	
								    " UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(incubation_start,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'incubation start' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'hatching start' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, FUNCTIONS.RoundTime(nest_endstate_date,'M',1) datetime_,
								        NULL transp, NULL boutID, 50 act, 'incubation end' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,	
								    " UNION SELECT year_, lower(nest) nest,  FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_,
								        NULL transp, NULL boutID, 50 act, 'nest visit' who
								        from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,") x  
										LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from AVESatBARROW.CAPTURES c
								                     LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
												   UNION SELECT transponder , 'fieldteam' sex from  AVESatBARROW.AUTHORS) a
								       on x.transp = a.transponder	
                                  where x.year_ = ",yr)) 
				}
	
	ars =  dbq(con, paste("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
	                                (SELECT  year_, lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_,
	                                    ID transp, NULL boutID, ((50*`signal`)/255) act , NULL who
							            from LOGGERSatBARROW.ARTS where lower(nest)=",nest_,") x    
										LEFT JOIN (select distinct c.ID as trans, COALESCE(s.sex,c.sex_observed) sex from AVESatBARROW.CAPTURES c
								   			LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID where c.ID IS NOT NULL) a
								       on x.transp = a.trans	
                                  where x.year_ = ",yr)) 
			
	tts   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, 
	                                    T_egg as T_egg_TT,'TT' as 'logger'
								 from TinyTag
								 where lower(nest) =",nest_," and year_ =",yr," 
								  group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
	
	hobos   = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_, 
	                                    T_ambient,'HOBO' as 'logger'
								 from HOBO
								 where lower(nest) =",nest_,"
								  group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
    
	MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'M',1)  datetime_,
	                                    T_egg as T_egg_MSR,T_ambient,'MSR' as 'logger'
								 from MSR
								 where lower(nest) =",nest_,"and year_ =",yr," 
								 group by FUNCTIONS.RoundTime(datetime_,'M',1) ,nest"))
								 
	# vidoe data
	#vide=v[v$yr==yr & v$nest==nest_,c('nest','datetime_','video','logger')]		
	min_=v$datetime_[v$nest==nest_l & v$yr==yr & v$status=='on']
	max_=v$datetime_[v$nest==nest_l & v$yr==yr & v$status=='off']
	
	if(length(min_)>0){   
		l=list()
		for( i in 1:length(min_)) {
						a = min_[i]
						b= max_[i]
						#xi=x[x$nest=='S305',]
						x  = data.frame(nest = nest_l,video=2,logger='video', datetime_ = seq(from = trunc(as.POSIXct(a), "min"), to = as.POSIXct(b), by = "1 min")    )
						l[[i]]= x
					}
		vid=do.call(rbind,l)
		} else { vid=data.frame()	}			
					
		
	# RFID placed on the nest (to limit the data lateR)
		# does not work yet for 2013, because RFID on off are not available for 2013
		st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_onNest,'M',1)  datetime_
									 from LOGGERSatBARROW.Device_filestatus
									 where device = 'RFID' and lower(nest) =",nest_," and year_ =",yr,"
									 "))
		st$datetime_ = as.POSIXct(st$datetime_)
		if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])==0) {print(paste("on missing ",nest_l))} else{
		if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])>0) 
				{st=min(s$datetime_[s$nest==nest_l & s$yr==yr])} else { st=min(st$datetime_)}}			
		st = as.Date(trunc(st, "day"))
	
	# prepares lines for the period captive bird was hold in (2013)
	if(yr==2013 & nest_l%in%unique(tr$nest)){
	min_=min(tr$datetime_[tr$nest==nest_l & tr$yr==yr & tr$phase=='start'])
	if(unique(tr$ring_num[tr$nest==nest_l & tr$yr==yr])%in%c(257188566,257188570)) {max_=max(rfids$datetime_[!is.na(rfids$datetime_)])}else{
		max_=max(tr$datetime_[tr$nest==nest_l & tr$yr==yr & tr$phase=='released'])}
	
	if(length(min_)==0) {tr=data.frame()} else{  
		l=list()
		for( i in 1:length(min_)) {
						a = min_[i]
						b= max_[i]
						#xi=x[x$nest=='S305',]
						x  = data.frame(nest = nest_l,tr=5,who='one bird removed', datetime_ = seq(from = trunc(as.POSIXct(a), "min"), to = as.POSIXct(b), by = "1 min")    )
						l[[i]]= x
					}
		trr=do.call(rbind,l)	
		}
		} else{trr=data.frame()}	

 
  if (nrow(rfids) > 0) { 
    
	
	# last state
    	rfids$laststate = dbq(con, paste("select nest_endstate 
									  FROM AVESatBARROW.NESTS  where year_ = ",yr," and nest =",nest_))$nest_endstate
		if (is.null(rfids$laststate)) rfids$laststate = NA									  
	rfids$datetime_=as.POSIXct(rfids$datetime_)								  
        
        
     
	# adds line for relase of captive bird in 2013
	tr13=tr$datetime_[tr$nest==nest_l & tr$yr==yr & tr$phase=='released']	
	if(length(tr13)>0){ tr13=data.frame(nest=nest_l, datetime_= tr13, transp= NA, boutID = NA, laststate=NA, act = 50, who='captive released')
	rfids=rbind(rfids,tr13)
	}
	 # adjust datetime for s269
		
			
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
		if (nrow(ars)>0) {  
		  ars$datetime_ = as.POSIXct(ars$datetime_)
		  ars$logger="ARS"
		  ars$T_egg_MSR = NA
		  ars$T_egg_TT = NA
		  ars$T_ambient=NA
          rfids = merge(rfids, ars, all = TRUE)
		  }  
		
		
		 if (nrow(vid)>0) {  
          vid$datetime_ = as.POSIXct(vid$datetime_)
          rfids = merge(rfids, vid, all = TRUE)
		  }  
		   if (nrow(vid)==0) rfids$video=NA
		 
		if (nrow(trr)>0) {  
          trr$datetime_ = as.POSIXct(trr$datetime_)
          rfids = merge(rfids, trr, all = TRUE)
		  }  
		   if (nrow(trr)==0) rfids$tr=NA		 
		  
        rfids$who[which(rfids$who==2)] = 'female'
        rfids$who[which(rfids$who==1)] = 'male'
		rfids$who[is.na(rfids$who)] = 'unknown'
        rfids$day = as.Date(trunc(rfids$datetime_, "day"))
		rfids$time = as.numeric(difftime(rfids$datetime_, trunc(rfids$datetime_, 
            "day"), units = "hours"))
			days = data.frame(nest=rfids$nest[1], datetime_=NA, transp = NA, boutID = NA, act=1,who = 'nodata',laststate = NA, T_egg_TT = NA,T_egg_MSR=NA, T_ambient = NA, video=NA, tr=NA, logger=NA, day = c(st:max(rfids$day, na.rm = TRUE)),
						  time = 0.0,stringsAsFactors = FALSE)
        days$day = structure(days$day, class='Date')						  
        rfids=rfids[rfids$day>=st,]
		rfids = rbind(rfids,days)						  
        
        }#added    
        return(rfids) 
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
		st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_onNest,'M',1)  datetime_
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
{# actogram function	
RFID.temperature_actogram = function(dfr,gps,yr, type, show=FALSE,utc=TRUE) {
		
        if (missing(dfr)) dfr = RFID.temperature_actogram_data()
		if (missing(gps)) gps = GPS.temperature_actogram_data()
		
        if (type =='PDF') {
           tf = paste0(outdir,'/',dfr$nest[1], ".pdf")
           pdf(tf, paper = "a4", width = 8, height = 11.6929134)
		}
		if (type =='PNG') { # ASK ANNE HOW THIS WORKS
		 if(utc==TRUE){tf = paste0(outdir,'/',paste(yr,dfr$nest[1],sep="_"), "_true-local_%03d.png")}else{ # %03d - three position digit file name)
          tf = paste0(outdir,'/',paste(yr,dfr$nest[1],sep="_"), "_%03d.png")} # %03d - three position digit file name
          png(tf, width = 210, height = 297, units = "mm", res = 600)	
	
		}
			dfr=dfr[!is.na(dfr$datetime_),]
			dfr$datetime_utc=dfr$datetime_+m$utc_plus*60*60
		if(utc==TRUE){
		 dfr$datetime_= dfr$datetime_utc + (m$lon/15)*60*60
         }
		 dfr$day = as.Date(trunc(dfr$datetime_, "day"))
		 dfr$time = as.numeric(difftime(dfr$datetime_, trunc(dfr$datetime_,"day"), units = "hours"))
		 
         
		 clr = actogram.cols
		  mm=dfr$transp[dfr$who=='male' & nchar(dfr$transp)>12][1]
		  ff=dfr$transp[dfr$who=='female' & nchar(dfr$transp)>12][1]
		  nest_=unique(dfr$nest[!is.na(dfr$nest)]	)      
          nest_ =shQuote(tolower(nest_))
		  
		  dfr     = merge(dfr,clr,all.x=TRUE)	
          if ("ARS"%in%unique(dfr$logger[!is.na(dfr$logger)])){dfr$cols[!is.na(dfr$logger) & dfr$logger=="ARS"] = transpcol(dfr$cols[!is.na(dfr$logger) & dfr$logger=="ARS"],newalpha = 15)} # makes transparent for ARS data
		  
		  transplist = unique(dfr$transp[which(!is.na(dfr$transp))])
		  trCol      = data.frame(transp = transplist, col2 = rainbow(length(transplist)),stringsAsFactors = FALSE)
		  dfr        = merge(dfr,trCol,all.x=TRUE)
		  dfr$cols[which(is.na(dfr$cols))] = dfr$col2[which(is.na(dfr$cols))]
		  dfr$cols[dfr$who=='captive released']=clr$cols[clr$who=='captive released']
		  dfr$cols[dfr$logger=='video']=vid_col
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
            #24),alternating=3), y = list(limits = c(0, 50), alternating=2))
			
			 scales = list(x = list(at=c(0,6,12,18,24),labels=c('00:00','06:00','12:00','18:00','24:00') , #cex = 0.7, 
							limits = c(0,24),alternating=3), y = list(limits = c(0, 50),at =c(10,30), alternating=2, cex=0.6, tck=0.4))	
			
            panel = function(...) {
                panel.abline(v = c(1:23), col = "light grey")
				panel.abline(v=c(1:12)*2, col = "dark grey", lwd = 1.1)
				panel.abline(v=dfr$time[dfr$day==sl[panel.number()] & dfr$twilight==1],col="grey66")
						   panel.abline(v=dfr$time[dfr$day==sl[panel.number()] & dfr$night==1],col="grey35")
				panel.abline(v=gps$time_[gps$day==sl[panel.number()]&gps$gpsdist<200 & gps$gpsdist>100],col="gray97")
				panel.abline(v=gps$time_[gps$day==sl[panel.number()]&gps$gpsdist<100 & gps$gpsdist>50],col="gray85")
				panel.abline(v=gps$time_[gps$day==sl[panel.number()]&gps$gpsdist<50 & gps$gpsdist>25],col="gray70")
				panel.abline(v=gps$time_[gps$day==sl[panel.number()]&gps$gpsdist<25 & gps$gpsdist>10],col="gray55")
				panel.abline(v=gps$time_[gps$day==sl[panel.number()]&gps$gpsdist<10],col="gray40")
				d = dfr[which(dfr$day == sl[panel.number()]),]
                panel.xyplot(...)                    #temperatures+video
                panel.xyplot(d$time, d$act, col = d$cols, 
                  type = "h")
				panel.xyplot(d$time, d$heating, col = d$cols, 
                  type = "h")
				panel.xyplot(d$time, d$tr, col = d$cols, 
                  type = "h")
				panel.xyplot(d$time, d$video, col = d$cols, 
                  type = "h")				  
				  
            }
            # fate and loggers for title
			
			fate = dfr$laststate[-which(is.na(dfr$laststate))][1]
            if(length(fate)==0) {fate='not in database'}else{
				fate = ifelse(fate == "p", "predated", ifelse(fate == 
                "hd", "hatched", ifelse(fate == "ud", "undetermined", 
                ifelse(fate == "d", "deserted", 'to be determined'))))
				}
				
			logger=unique(dfr$logger)
			logger= logger[-which(is.na(logger))]
			video=logger[logger=='video']
			remov=dfr$tr[-which(is.na(dfr$tr))]
			ars=logger[logger=='ARS']
			{# creates title loggers
			if (length(video)==0 ){
				if (length(logger)==0){maintitle = paste(dfr$nest[1], ", last state = ", fate, ", loggers = none", sep="")}
				if (length(logger)==1){maintitle = paste(dfr$nest[1], ", last state = ", fate, ", loggers = ",logger, sep="")}
				if (length(logger)==2){maintitle = paste(dfr$nest[1], ", last state = ", fate, ", loggers = MSR & TT", sep="")}
				}else { 
				logger=logger[logger!='video']
				if (length(logger)==0){maintitle = paste(dfr$nest[1], ", last state = ", fate, ", loggers =",video, sep=""	)}
				if (length(logger)==1){
					loggers=paste(logger,video,sep=", ")
					maintitle = paste(dfr$nest[1], ", last state = ", fate, ", loggers = ",loggers, sep="")
					} 
				if (length(logger)==2){maintitle = paste(dfr$nest[1], ", last state = ", fate, ", loggers = MSR & TT, ",video, sep="")
					} 
				}
			if(length(ars)>0){ maintitle=paste(maintitle,' & ARS',sep="")}
			if(length(remov)>0){
				bird=unique(tr$sex[tr$nest==unique(dfr$nest[!(is.na(dfr$nest))])])
				maintitle=paste(maintitle,', ',bird, ' removed', sep="")}
			}	
			# adjust title and add treatment				
			if(yr=='2012' & substring(dfr$nest[1],1,1)=="s"){
				# add heating
				dfr$heating=NA
					treat = dfr[dfr$boutID%in%c(4,6,8,10),]
					if(nrow(treat)>0){
					treat$act=0
					treat$heating=5
					treat$cols='yellow2'
					
					dfr=rbind(dfr,treat)
					
					treatment=unique(dfr$who[!is.na(dfr$heating) & dfr$who%in%c('male','female')])
					if(length(treatment)==2){treatment='both'}
					maintitle=paste(maintitle,', ',treatment,' heated',sep="")
				
				} else {treatment ='none'}				
				
				} else {dfr$heating=NA}
			heat=dfr$heating[-which(is.na(dfr$heating))]
			
			{# legend (different for 2012 and other years
				clr_a=clr[clr$who%in%c('female','male','fieldteam','unknown'),] 
				clr_a$who=ifelse(clr_a$who=='female', paste('\u2640 incubation',ff), ifelse(clr_a$who=='male', paste('\u2642 incubation',mm), clr_a$who))
				#clr_a=rbind(clr_a, unknowns)
				clr_c=data.frame( who=c("ambient temperature", "egg temperature",clr$who[clr$who%in%c('video','heating','female','male','one bird removed')]),
								  cols=c("dark blue", "light blue", clr$cols[clr$who%in%c('video','heating','female','male','one bird removed')]),stringsAsFactors=FALSE)	
					clr_c$cols[clr_c$who%in%c('female','male')]=transpcol(clr_c$cols[clr_c$who%in%c('female','male')],newalpha=30)
					clr_c$who=ifelse(clr_c$who=='female', paste('\u2640 nearby'), ifelse(clr_c$who=='male', paste('\u2642 nearby'), clr_c$who))
				clr_b=clr[clr$who%in%c("first egg", "incubation start", "hatching start", "incubation end","captive released"),]
				clr_d=data.frame(who=c("nest visit","capture","someone between 100 & 200 m","someone between 50 & 100 m","someone between 25 & 50 m","someone between 10 & 25 m","someone within 10 m"), cols=c("pink","red","gray97","gray85","gray70","gray55","gray40"), stringsAsFactors=FALSE)
				
				if(length(video)==0 ){ clr_c=clr_c[!clr_c$who=='video',]}
				if(length(remov)==0 ){ 	clr_c=clr_c[!clr_c$who%in%c('one bird removed'),]; clr_b=clr_b[!clr_b$who=="captive released",]}
				if(length(ars)==0 ){ clr_c=clr_c[!clr_c$who%in%c('\u2640 nearby','\u2642 nearby'),]}
				if(length(heat)==0 ){ clr_c=clr_c[!clr_c$who=='heating',]}
				
				if(nrow(clr_a)<nrow(clr_d)){
											clr_x=data.frame(who=rep("",nrow(clr_d)-nrow(clr_a)), cols=rep("white",nrow(clr_d)-nrow(clr_a)), stringsAsFactors=FALSE)
											clr_a=rbind(clr_a,clr_x)
											}
				if(nrow(clr_b)<nrow(clr_d)){
											clr_x=data.frame(who=rep("",nrow(clr_d)-nrow(clr_b)), cols=rep("white",nrow(clr_d)-nrow(clr_b)), stringsAsFactors=FALSE)
											clr_b=rbind(clr_b,clr_x)
											}
				
				if(nrow(clr_c)<nrow(clr_d)){
											clr_x=data.frame(who=rep("",nrow(clr_d)-nrow(clr_c)), cols=rep("white",nrow(clr_d)-nrow(clr_c)), stringsAsFactors=FALSE)
											clr_c=rbind(clr_c,clr_x)
											}
			
				key = list(
					text = list(clr_a$who,cex=0.7), 
					points = list(pch = 19, col = clr_a$cols),
					text = list(clr_c$who,cex=0.7),
					points = list(pch = 19, col = clr_c$cols),
					text = list(clr_b$who,cex=0.7), 
					points=list(pch=15,col=c(clr_b$cols),cex=0.9),
					text = list(clr_d$who,cex=0.7),
					points=list(pch=15,col=c(clr_d$cols),cex=0.9)		
					)
				}
			
			par(pin = c(8.26771654, 11.6929134)) # what does it do?
            
			rfidsplot = xyplot(T_ambient + T_egg_TT + T_egg_MSR ~ time | day, 
                data = dfr, col = c("dark blue", "light blue", "light blue"), 
                cex = 0.1, strip.left = strip.left, layout = c(1,25),#ifelse(length(sl) > 25, 25, length(sl))), 
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
		save(dfr,gps,rfidsplot,strip.left, scales, key, panel, maintitle, sl, m,s_b,file= paste0(outdir,'/',paste(yr,dfr$nest[1],sep="_"), ".Rdata"))	
    	print(maintitle)
}
}

{# RFID data extraction function for all 5s data
RFID.temperature_actogram_data = function (con = con, nest, yr) {
#on.exit(mysqlCloseConnection(con))
  if (missing(yr))
	 yr = pickOne(c(2013,2012,2011))
    if (missing(nest)) 
     nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
   
	nest_l=tolower(nest)
	nest_ =shQuote(tolower(nest))
	# selects distinct

		rfids = dbq(con, paste0("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
	                                (select  distinct  year_,lower(r.nest) nest, COALESCE(datetime_corrected,datetime_) datetime_,
	                                    transp, boutID, 10 act,  NULL who
								        from RFID r
								        where transp IS NOT NULL AND lower(r.nest) =",nest_,
								    " UNION SELECT year_, lower(nest) nest, coalesce(start_capture_date_time,caught_date_time,release_time)  datetime_,
								        NULL transp, NULL boutID, 50 act, 'capture' who
								        from AVESatBARROW.CAPTURES where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, laying_date datetime_,
								        NULL transp, NULL boutID, 50 act, 'first egg' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,	
									 " UNION SELECT year_, lower(nest) nest, incubation_start datetime_,
								        NULL transp, NULL boutID, 50 act, 'incubation start' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, hatch_start datetime_,
								        NULL transp, NULL boutID, 50 act, 'hatching start' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,
									" UNION SELECT year_, lower(nest) nest, nest_endstate_date datetime_,
								        NULL transp, NULL boutID, 50 act, 'incubation end' who
								        from AVESatBARROW.NESTS where lower(nest) = ",nest_,										
									" UNION SELECT year_, lower(nest) nest, datetime_,
								        NULL transp, NULL boutID, 50 act, 'nest visit' who
								        from EXTRA_AVESatBARROW.NESTVISITS where lower(nest) = ",nest_,") x  
										LEFT JOIN (select tag_ID transponder, COALESCE(s.sex,c.sex_observed) sex from AVESatBARROW.CAPTURES c
								                     LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID where tag_ID IS NOT NULL
												   UNION SELECT transponder , 'fieldteam' sex from  AVESatBARROW.AUTHORS) a
								       on x.transp = a.transponder	
                                  where x.year_ = ",yr)) 
			
	
	
	ars =  dbq(con, paste("select x.nest, x.datetime_, x.transp, x.boutID, x.act, coalesce(who, a.sex) who from
	                                (SELECT  year_, lower(nest) nest, datetime_,
	                                    ID transp, NULL boutID, ((50*`signal`)/255) act , NULL who
							            from LOGGERSatBARROW.ARTS where lower(nest)=",nest_,") x    
										LEFT JOIN (select distinct c.ID as trans, COALESCE(s.sex,c.sex_observed) sex from AVESatBARROW.CAPTURES c
								   			LEFT JOIN AVESatBARROW.SEX s on c.ID = s.ID where c.ID IS NOT NULL) a
								       on x.transp = a.trans	
                                  where x.year_ = ",yr)) 
			
	tts   = dbq(con, paste("select lower(nest), datetime_, 
	                                    T_egg as T_egg_TT,'TT' as 'logger'
								 from TinyTag
								 where lower(nest) =",nest_," and year_ =",yr))
	
	hobos   = dbq(con, paste("select lower(nest) nest, datetime_, 
	                                    T_ambient,'HOBO' as 'logger'
								 from HOBO
								 where lower(nest) =",nest_))
    
	#MSRs  = dbq(con, paste("select lower(nest) nest, datetime_,
	#                                    T_egg as T_egg_MSR,T_ambient,'MSR' as 'logger'
	#							 from MSR
	#							 where lower(nest) =",nest_,"and year_ =",yr))
								 
	MSRs  = dbq(con, paste("select lower(nest) nest, FUNCTIONS.RoundTime(datetime_,'S',30)  datetime_,
	                                    T_egg as T_egg_MSR,T_ambient,'MSR' as 'logger'
								 from MSR
								 where lower(nest) =",nest_,"and year_ =",yr," 
								 group by FUNCTIONS.RoundTime(datetime_,'S',30) ,nest"))							 
								 
	# vidoe data
	#vide=v[v$yr==yr & v$nest==nest_,c('nest','datetime_','video','logger')]		
	min_=v$datetime_[v$nest==nest_l & v$yr==yr & v$status=='on']
	max_=v$datetime_[v$nest==nest_l & v$yr==yr & v$status=='off']
	
	if(length(min_)>0){   
		l=list()
		for( i in 1:length(min_)) {
						a = min_[i]
						b= max_[i]
						#xi=x[x$nest=='S305',]
						x  = data.frame(nest = nest_l,video=2,logger='video', datetime_ = seq(from = trunc(as.POSIXct(a), "min"), to = as.POSIXct(b), by = "1 min")    )
						l[[i]]= x
					}
		vid=do.call(rbind,l)
		} else { vid=data.frame()	}			
					
		
	# RFID placed on the nest (to limit the data lateR)
		# does not work yet for 2013, because RFID on off are not available for 2013
		st = dbq(con, paste("select lower(nest) nest,FUNCTIONS.RoundTime(datetime_onNest,'M',1)  datetime_
									 from LOGGERSatBARROW.Device_filestatus
									 where device = 'RFID' and lower(nest) =",nest_," and year_ =",yr,"
									 "))
		st$datetime_ = as.POSIXct(st$datetime_)
		if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])==0) {print(paste("on missing ",nest_l))} else{
		if(is.na(min(st$datetime_)) & length(s$datetime_[s$nest==nest_l & s$yr==yr])>0) 
				{st=min(s$datetime_[s$nest==nest_l & s$yr==yr])} else { st=min(st$datetime_)}}			
		st = as.Date(trunc(st, "day"))
	
	# prepares lines for the period captive bird was hold in (2013)
	if(yr==2013 & nest_l%in%unique(tr$nest)){
	min_=min(tr$datetime_[tr$nest==nest_l & tr$yr==yr & tr$phase=='start'])
	if(unique(tr$ring_num[tr$nest==nest_l & tr$yr==yr])%in%c(257188566,257188570)) {max_=max(rfids$datetime_[!is.na(rfids$datetime_)])}else{
		max_=max(tr$datetime_[tr$nest==nest_l & tr$yr==yr & tr$phase=='released'])}
	
	if(length(min_)==0) {tr=data.frame()} else{  
		l=list()
		for( i in 1:length(min_)) {
						a = min_[i]
						b= max_[i]
						#xi=x[x$nest=='S305',]
						x  = data.frame(nest = nest_l,tr=5,who='one bird removed', datetime_ = seq(from = trunc(as.POSIXct(a), "min"), to = as.POSIXct(b), by = "1 min")    )
						l[[i]]= x
					}
		trr=do.call(rbind,l)	
		}
		} else{trr=data.frame()}	

 
  if (nrow(rfids) > 0) { 
    # last state
    	rfids$laststate = dbq(con, paste("select nest_endstate 
									  FROM AVESatBARROW.NESTS  where year_ = ",yr," and nest =",nest_))$nest_endstate
		if (is.null(rfids$laststate)) rfids$laststate = NA									  
									  
        rfids$datetime_=as.POSIXct(rfids$datetime_)
        
     
	# adds line for relase of captive bird in 2013
	tr13=tr$datetime_[tr$nest==nest_l & tr$yr==yr & tr$phase=='released']	
	if(length(tr13)>0){ tr13=data.frame(nest=nest_l, datetime_= tr13, transp= NA, boutID = NA, laststate=NA, act = 50, who='captive released')
	rfids=rbind(rfids,tr13)
	}
	 
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
		if (nrow(ars)>0) {  
		  ars$datetime_ = as.POSIXct(ars$datetime_)
		  ars$logger="ARS"
		  ars$T_egg_MSR = NA
		  ars$T_egg_TT = NA
		  ars$T_ambient=NA
          rfids = merge(rfids, ars, all = TRUE)
		  }  
		
		
		 if (nrow(vid)>0) {  
          vid$datetime_ = as.POSIXct(vid$datetime_)
          rfids = merge(rfids, vid, all = TRUE)
		  }  
		   if (nrow(vid)==0) rfids$video=NA
		 
		if (nrow(trr)>0) {  
          trr$datetime_ = as.POSIXct(trr$datetime_)
          rfids = merge(rfids, trr, all = TRUE)
		  }  
		   if (nrow(trr)==0) rfids$tr=NA		 
		  
        rfids$who[which(rfids$who==2)] = 'female'
        rfids$act[which(rfids$who=="female")] = 15
       # dfr$act[which(dfr$who=="female")] = 15
		rfids$who[which(rfids$who==1)] = 'male'
	
		rfids$who[is.na(rfids$who)] = 'unknown'
        rfids$day = as.Date(trunc(rfids$datetime_, "day"))
		rfids$time = as.numeric(difftime(rfids$datetime_, trunc(rfids$datetime_, 
            "day"), units = "hours"))
			days = data.frame(nest=rfids$nest[1], datetime_=NA, transp = NA, boutID = NA, act=1,who = 'nodata',laststate = NA, T_egg_TT = NA,T_egg_MSR=NA, T_ambient = NA, video=NA, tr=NA, logger=NA, day = c(st:max(rfids$day, na.rm = TRUE)),
						  time = 0.0,stringsAsFactors = FALSE)
        days$day = structure(days$day, class='Date')						  
        rfids=rfids[rfids$day>=st,]
		rfids = rbind(rfids,days)						  
        
        }#added    
        return(rfids) 
		}
}


{# to make multiple actograms (otherwise just run RFID.temperature_actogram(con=con) :
nests=c('a301','a302','b301','b501','s201','s506','s602') # deserted 2011
nests=c('a303','a305
a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s417','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s813','s815','s829','s901','s902') # deserted 2012
nests=c('l501','l502','l801','l805','s520','s623','s627','s707','w403','w504','w505','w507','w508','w509','w510','w511','w701','w702','w801','w802','w804	') # deserted 2013
# add the experimental ones
# add script for lines for unip start and inc end

yr=2013
if(yr==2013){ 
	nests = dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and nest not in ('F666') and year_ =",yr," order by nest"))$nest # S629 is excluded
	#nests = dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and substring(nest,1,1) in ('S') and nest not in ('S629','F666') and year_ =",yr," order by nest"))$nest # S629 is excluded
	}else{ nests = dbq(con, paste("select distinct lower(nest) nest from Device_filestatus where device='RFID' and substring(nest,1,1) not in ('S')  and year_ =",yr," order by nest"))$nest # S629 is excluded
	}
for (i in (1:length(nests))) {
	m=meta_nest.data(con=con,nest = nests[i])
	s_b=meta_birds.data(con=con,nest = nests[i])
	dfr = RFID.temperature_actogram_data(con=con,nest = nests[i],yr=yr)
   	gps=GPS.temperature_actogram_data (con=con, nest=nests[i],yr=yr) 
	RFID.temperature_actogram(dfr,gps,yr=yr,type=type, utc=FALSE)
  } 

 } 
 
{# to make one actogram 
	nest='s622' 
	yr=2013
	# sets working directory
	if(yr==2013){
outdir = "\\\\ds\\RAW_DATA_KEMP\\FIELD\\Barrow\\2013\\post-Season\\actograms\\2014_Dec"			
} else {if (yr==2012){outdir = "\\\\ds\\RAW_DATA_KEMP\\FIELD\\Barrow\\2012\\post-Season\\DOCS\\actograms\\PNG\\2014_Dec"			
} else {if (yr==2011){
outdir = "\\\\ds\\RAW_DATA_KEMP\\FIELD\\Barrow\\2011\\DOCS\\actograms\\2014_Dec"	
 }}}

	m=meta_nest.data(con=con,nest = nest)
	s_b=meta_birds.data(con=con,nest = nest)
	dfr=RFID.temperature_actogram_data (con=con, nest=nest, yr=yr) 
	gps=GPS.temperature_actogram_data (con=con, nest=nest, yr=yr) 	
	RFID.temperature_actogram (dfr, gps, yr=yr, type=type, show=FALSE, utc=FALSE)
	
}	

dfr[which(dfr$datetime_>as.POSIXct('2013-06-24 13:18:00') & dfr$datetime_<as.POSIXct('2013-06-24 13:50:00') & is.na(dfr$logger)),1:7]
dfr[which(dfr$datetime_>as.POSIXct('2013-06-24 13:18:00') & dfr$datetime_<as.POSIXct('2013-06-24 14:55:00') & dfr$who=='nest visit'),1:7]
dfr[which(dfr$datetime_>as.POSIXct('2013-06-24 13:49:00') & dfr$datetime_<as.POSIXct('2013-06-24 13:52:00') & dfr$logger=='MSR'),1:9]
unique(dfr$who[which(dfr$datetime_>as.POSIXct('2013-06-24 13:18:00') & dfr$datetime_<as.POSIXct('2013-06-24 14:55:00'))])
head(dfr[83400:83480,1:7])