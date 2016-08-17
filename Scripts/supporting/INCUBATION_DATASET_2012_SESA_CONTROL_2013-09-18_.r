{# TOOLS & SETTINGS

require(maptools) 
require(gregmisc) 
require(actogram)
require(zoo)
require(sp)
require(caTools)

require(lattice)
require(grid)
require(plyr)

backtoPOSIXct= function (timevar) structure(timevar, class = c("POSIXt", "POSIXct"))

#require(scidb)
#dbcon("_SANDBIN")

#wet=scidbQuery
}
# if scidb does not work:
require(RMySQL)
Driver = dbDriver("MySQL")
password = readline("enter password:")
Kvetak25
con = dbConnect(dbDriver("MySQL"), username = 'mbulla', password=password, dbname = '_SANDBIN', host = 'scidb.orn.mpg.de')
wet=dbGetQuery


{# CREATE RFID, T, envi tables
	{# RFID table (corrected, uncorrected times in one table)
		wet(con, "DROP TABLE IF EXISTS _SANDBIN.MB_RFID_2012_5s")
		
		wet(con, "CREATE TABLE _SANDBIN.MB_RFID_2012_5s AS 
							SELECT nest, 
								if(datetime_corrected IS NULL, mysql.RoundTime(datetime_,'S',5),mysql.RoundTime(datetime_corrected,'S',5)) as datetime_5s, 
							transp as tag_ID,
								boutID as bout_ID_tr
								FROM LOGGERSatBARROW.RFID 
								where year_= 2012
								") #where nest in ('s101', 's201')
								
		wet(con, " ALTER TABLE _SANDBIN.MB_RFID_2012_5s
								CHANGE COLUMN `datetime_5s` `datetime_5s` DATETIME NULL DEFAULT NULL AFTER `nest`")
		
		
		wet(con, "ALTER TABLE `MB_RFID_2012_5s`
							ADD INDEX `datetime_5s` (`datetime_5s`),
							ADD INDEX `nest` (`nest`)")
		}						
	{# MSR table
		wet(con, "DROP TABLE IF EXISTS _SANDBIN.MB_MSR_2012_5s ")

		wet(con, "CREATE TABLE _SANDBIN.MB_MSR_2012_5s AS
							SELECT nest, mysql.RoundTime(datetime_,'S',5) as datetime_5s, T_egg as t_nest, 
							T_ambient as t_ambient, humidity as h 
								FROM LOGGERSatBARROW.MSR
								WHERE year_=2012"
								)
		
		wet(con, "ALTER TABLE `MB_MSR_2012_5s`
							ADD INDEX `datetime_5s` (`datetime_5s`),
							ADD INDEX `nest` (`nest`)")		
		}
	{# TT table
		wet(con, "DROP TABLE IF EXISTS _SANDBIN.MB_TT_2012_2m ")
		
		wet(con, "CREATE TABLE _SANDBIN.MB_TT_2012_2m AS
							SELECT nest, mysql.RoundTime(datetime_,'M', 1) as datetime_1m, mysql.RoundTime(datetime_,'M', 2) as datetime_2m,
							T_egg as t_nest 
								FROM LOGGERSatBARROW.TinyTag 
								WHERE year_=2012
								") # where nest in ('s101', 's201')
							
		wet(con, "ALTER TABLE `MB_TT_2012_2m`
							ADD INDEX `datetime_2m` (`datetime_2m`),
							ADD INDEX `nest` (`nest`)")	
		}					
	{# average t_ambient/1min (from all MSRs running at that time # LIMIT TO DATA WHEN ON NEST (not LAB)
			
			#1min
			wet(con, "DROP TABLE IF EXISTS _SANDBIN.MB_avgT_2012_1m")
		
			wet(con, "CREATE TABLE _SANDBIN.MB_avgT_2012_1m as
								SELECT mysql.RoundTime(datetime_,'M',1) datetime_1m, 
								AVG(T_ambient) as t_amb_avg, AVG(light) as l_avg, AVG(humidity) as h_avg FROM LOGGERSatBARROW.MSR where year_=2012
											GROUP BY datetime_1m")

			wet(con, "ALTER TABLE `MB_avgT_2012_1m`
							ADD INDEX `datetime_1m` (`datetime_1m`)")	
											
			#5s		
			wet(con, "DROP TABLE IF EXISTS _SANDBIN.MB_avgT_2012_5s")			
			wet(con, "CREATE TABLE _SANDBIN.MB_avgT_2012_5s as
								SELECT mysql.RoundTime(datetime_,'S',5) datetime_, 
								AVG(T_ambient) as t_amb_avg, AVG(light) as l_avg, AVG(humidity) as h_avg  FROM LOGGERSatBARROW.MSR where year_=2012
											GROUP BY datetime_")
				
			wet(con, "ALTER TABLE `MB_avgT_2012_5s`
							ADD INDEX `datetime_` (`datetime_`)")			
	}
	{# visit table
	wet(con, "DROP TABLE if EXISTS MB_visit_2012")
	
	wet(con,	"CREATE TABLE _SANDBIN.MB_visit_2012 (
							`pk` INT(11) NOT NULL AUTO_INCREMENT,
							
							`nest` VARCHAR(5) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`exper` VARCHAR(1) NULL DEFAULT NULL COMMENT 't = treatment or c= control nest' COLLATE 'latin1_bin',
							`datetime_` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
										PRIMARY KEY (`pk`),
									INDEX `nest` (`nest`),
									INDEX `datetime_` (`datetime_`)

						)")
		
	
	x =  	wet(con, "SELECT nest, mysql.RoundTime(datetime_,'S',5) as datetime_5s,  mysql.RoundTime(datetime_left,'S',5) as datetime_left_5s FROM EXTRA_AVESatBARROW.NESTVISITS where year_= 2012
								") 

	x$datetime_5s=ifelse(substr(x$datetime_5s,12,nchar(x$datetime_5s))=='00:00:00', NA, x$datetime_5s) # length(which(is.na(x$datetime_5s)))
	x$datetime_left_5s=ifelse(substr(x$datetime_left_5s,12,nchar(x$datetime_left_5s))=='00:00:00', NA, x$datetime_left_5s) # length(which(is.na(x$datetime_left_5s)))
	x=x[-which(is.na(x$datetime_5s) & is.na(x$datetime_left_5s)),]
	x$datetime_5s=ifelse(is.na(x$datetime_5s), x$datetime_left_5s,x$datetime_5s)
	x$datetime_left_5s=ifelse(is.na(x$datetime_left_5s), x$datetime_5s,x$datetime_left_5s)
	
	x$datetime_5s=as.POSIXct(x$datetime_5s)
	x$datetime_left_5s=as.POSIXct(x$datetime_left_5s)
	
	
	
	
	

	x=x[!x$datetime_5s==as.POSIXct('1970-01-01 01:00:00'),]
	x=x[-which(x$datetime_left_5s==as.POSIXct('1970-01-01 01:00:00')),]
	x[x$datetime_left_5s<x$datetime_5s,]
	
	x$datetime_left_5s[x$datetime_left_5s==as.POSIXct('2012-06-22 10:23:00') & x$nest=='S1005']='2012-06-22 10:29:00'
	x$datetime_5s[x$datetime_5s==as.POSIXct('2012-06-22 10:29:00') & x$nest=='S1005']='2012-06-22 10:23:00'
	
	x$datetime_left_5s[x$datetime_left_5s==as.POSIXct('2012-06-12 17:14:45') & x$nest=='S503']='2012-06-12 17:20:45'
	x$datetime_5s[x$datetime_5s==as.POSIXct('2012-06-12 17:20:45') & x$nest=='S503']='2012-06-12 17:14:45'	
	
	x$datetime_left_5s[x$datetime_left_5s==as.POSIXct('2012-06-13 11:51:30') & x$nest=='S704']='2012-06-13 11:51:35'
	x$datetime_5s[x$datetime_5s==as.POSIXct('2012-06-13 11:51:35') & x$nest=='S704']='2012-06-13 11:51:30'	
	
	x$datetime_left_5s[x$datetime_left_5s==as.POSIXct('2012-06-13 15:59:15') & x$nest=='S705']='2012-06-13 16:20:15'
	x$datetime_5s[x$datetime_5s==as.POSIXct('2012-06-13 16:20:15') & x$nest=='S705']='2012-06-13 15:59:15'
	
	x$datetime_left_5s[x$datetime_left_5s==as.POSIXct('2012-06-10 19:30:00') & x$nest=='A701']='2012-06-10 19:45:00'
	x$datetime_5s[x$datetime_5s==as.POSIXct('2012-06-10 19:45:00') & x$nest=='A701']='2012-06-10 19:30:00'

	x[x$datetime_left_5s<x$datetime_5s,]

	for( i in 1:nrow(x)) {
						xi = x[i, ]
						if(xi$datetime_left_5s==xi$datetime_5s) {
								d  = data.frame(nest = xi$nest, datetime_ = xi$datetime_5s)
								}else{
						#xi=x[x$nest=='S305',]
						d  = data.frame(nest = xi$nest, datetime_ = seq(from = as.POSIXct(xi$datetime_5s), 
								to = as.POSIXct(xi$datetime_left_5s), by = "5 secs")    )
									}
						dbWriteTable(con, "MB_visit_2012", d, append = TRUE, row.names = FALSE)
		
						#print(xi)
							}

		}				
}
{# move below MAIN FRAME when generating a new precipitation table	# there are some missing data at some days
		wet(con, "DROP TABLE IF EXISTS _SANDBIN.MB_precip_2012")
		
			wet(con, "CREATE TABLE _SANDBIN.MB_temp_p AS 
							SELECT datetime_, mysql.RoundTime(datetime_,'M',5) as datetime_5m, precip from _SANDBIN.MB_incubation_2012
								GROUP BY datetime_
								") 
			
			wet(con, "CREATE TABLE _SANDBIN.MB_precip_2012  
								SELECT datetime_, datetime_5m, if (datetime_ > datetime_5m, DATE_ADD(`datetime_5m`, 
									INTERVAL 5 MINUTE),datetime_5m) as datetime_new, precip FROM _SANDBIN.MB_temp_p
									")
			
			wet(con, " ALTER TABLE `MB_precip_2012`
								CHANGE COLUMN `datetime_new` `datetime_new` DATETIME NULL DEFAULT NULL AFTER `datetime_5m`")
												
			wet(con, "DROP TABLE IF EXISTS _SANDBIN.MB_temp_p")

			wet(con, "UPDATE _SANDBIN.MB_precip_2012 
								SET 	precip = NULL
											")		

			wet(con, "ALTER TABLE _SANDBIN.`MB_precip_2012`
								ADD INDEX `datetime_new` (`datetime_new`),
								ADD INDEX `precip` (`precip`)")
											
									# devides the precipitation by 60 (we have 12 5s intervals in one min and precip is pro 5 min)
			wet(con, "UPDATE _SANDBIN.MB_precip_2012 b, LOGGERSatBARROW.ENVIRONMENTAL e
								SET 	b.precip = e.precipitation/60
											WHERE b.datetime_new = e.datetime_ and e.precipitation is not NULL")	
			
			#check weather precip data are correctly in
				t= wet(con, "SELECT distinct datetime_new FROM _SANDBIN.MB_precip_2012 where precip is NULL")
				print(t)			
}


{# MAIN FRAME
	wet(con, "DROP TABLE if EXISTS MB_incubation_2012")
	
	wet(con,	"CREATE TABLE _SANDBIN.MB_incubation_2012 (
							`pk` INT(11) NOT NULL AUTO_INCREMENT,
							`species` VARCHAR(4) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`nest` VARCHAR(5) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`exper` VARCHAR(1) NULL DEFAULT NULL COMMENT 't = treatment or c= control nest' COLLATE 'latin1_bin',
							`datetime_` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							`tag_ID` VARCHAR(17) NULL DEFAULT NULL COMMENT 'transponder' COLLATE 'latin1_bin',
							`capture` VARCHAR(1) NULL DEFAULT NULL COMMENT 'c when capture happend' COLLATE 'latin1_bin',
							`bird_ID` INT(10) NULL DEFAULT NULL COMMENT 'ring number',
							`sex` VARCHAR(1) NULL DEFAULT NULL COMMENT 'm = male, f = female' COLLATE 'latin1_bin',
							`inc_t` INT(2) NULL DEFAULT NULL COMMENT 'incubation (1) /non-incubation (0) derived from temperature data',
							`incubation` INT(1) NULL DEFAULT NULL COMMENT 'incubation (1) /non-incubation (0) derived from RFID and supplemented by temperature data' COLLATE 'latin1_bin',
							`bout_ID` INT(11) NULL DEFAULT NULL COMMENT 'ID of all incubation and all nonincubation spells > 3h',
							`sex_inc_filled` VARCHAR(16) NULL DEFAULT NULL COMMENT 'm/f assigned to all bout_IDs also those with incubation =NA; sex assigned also to exchange gaps' COLLATE 'latin1_bin',
							`exchange_gap` INT(1) NULL DEFAULT NULL COMMENT '0 = inefficiency of the exchange: bird leaves the nest, other comes late - no incubation',
							`bout_ID_tr` INT(11) NULL DEFAULT NULL COMMENT 'ID of all incubation bouts after the treatment was started',
							`treat` VARCHAR(2) NULL DEFAULT NULL COMMENT 'status of the nest at the time, n=natural, in=bout where fake egg was put in, b=fake egg in, tt= fake egg on, tn= non treatement bird, a=fake egg off ' COLLATE 'latin1_bin',
							`t_nest` FLOAT NULL DEFAULT NULL COMMENT 'based on TT or MSR',
							`t_nest_type` VARCHAR(4) NULL DEFAULT NULL COMMENT 'TT or MSR',
							`t_ambient` FLOAT NULL DEFAULT NULL COMMENT 'based on HOBO or MSR',
							`t_ambient_type` VARCHAR(4) NULL DEFAULT NULL COMMENT 'HOBO or MSR',
							`t_amb_avg` FLOAT NULL DEFAULT NULL COMMENT 'average t_ambient (from all hobos and MSRs running at that time)',
							`switch_check` INT(1) NULL DEFAULT NULL COMMENT '1 = start of bout that has not bird_ID for more than 3hours, is used to adjust bout_ID',
							`datetime_1m` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 1m',
							`datetime_2m` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 2m',
							`bird_ID_filled` INT(10) NULL DEFAULT NULL COMMENT 'fills bird_ID for males and females for each nest, where bird_Id known and where sex_inc_fill_2 is not NULL',
							`ar` INT(10) NULL DEFAULT NULL COMMENT 'signal strength of the tag',
							precip FLOAT NULL DEFAULT NULL COMMENT 'precipitation in mm (original data per 5min)',
									PRIMARY KEY (`pk`),
									INDEX `species` (`species`),
									INDEX `nest` (`nest`),
									INDEX `datetime_` (`datetime_`),
									INDEX `datetime_2m` (`datetime_2m`),
									INDEX `datetime_1m` (`datetime_1m`),
									INDEX `incubation` (`incubation`),
									INDEX `capture` (`capture`),
									INDEX `t_nest` (`t_nest`),
									INDEX `tag_ID` (`tag_ID`),
									INDEX `treat` (`treat`),
									INDEX `exper` (`exper`),
									INDEX `bout_ID` (`bout_ID`),
									INDEX `bout_ID_tr` (`bout_ID_tr`),
									INDEX `bird_ID` (`bird_ID`),
									INDEX `sex` (`sex`),
									INDEX `sex_inc_filled` (`sex_inc_filled`),
									INDEX `bird_ID_filled` (`bird_ID_filled`),
									INDEX `exchange_gap` (`exchange_gap`)

						)")
		
	
	x =  wet(con, "SELECT a.nest, a.year_, min(datetime_onNest) start, b.stop, b.species 
							FROM LOGGERSatBARROW.Device_filestatus a
						LEFT JOIN
							(SELECT nest, nest_endstate_date stop, species FROM AVESatBARROW.NESTS where year_ = 2012) b
								on a.nest = b.nest							
								WHERE a.nest in 	('S1001','S1003','S110','S201','S310','S313','S402','S405','S409','S413','S507','S510','S511','S514','S602','S603','S704','S706','S707','S717','S718','S804','S809','S810','S811','S815')
								and a.file_ID NOT in (SELECT file_ID FROM 	LOGGERSatBARROW.Device_filestatus where year_=2012 and remarks is not NULL)
								and a.year_=2012
								and b.species='SESA'
								GROUP BY a.nest
										")

										
		# nests with eggs in, but not used for experiment								
			# short inc ('S304','S422','S502','S701',
			# long inc	('S1002','S303','S506','S509', 'S705','S902')
										
										
	#x=x[x$year_==2012,]

	x[x$nest=='S602',]$start= '2012-06-10 07:00:00'
	x[x$nest=='S602',]$stop= '2012-06-27 21:00:00'
	x[x$nest=='S507',]$stop= '2012-07-01 21:00:00'
	#x[x$nest=='S602',]
	#x[x$nest=='S209',]$start= '2012-06-24 21:12:10'
	#x[x$nest=='S520',]$stop= '2012-07-15 18:28:55'
	
	#x$stop=as.POSIXct(strptime(x$stop, format="%Y-%m-%d %H:%M:%S"))
	#summary(x$stop)	
	

	# (hatch_start IS NULL, nest_endstate_date ,hatch_start)
	
	#dbWriteTable(con, "_SANDBIN.MB_max_min_runTime2012Loggers_temp", x, row.names = FALSE)
	# remove undesired x = x[ -c(98,14, grep("_", x$nest) ), ]
	# y=x[x$nest=='S101' | x$nest=='S201',]	
	
	for( i in 1:nrow(x)) {
						xi = x[i, ]
						#xi=x[x$nest=='S305',]
						d  = data.frame(nest = xi$nest, species=xi$species, datetime_ = seq(from = trunc(as.POSIXct(xi$start), "min"), 
								to = as.POSIXct(xi$stop), by = "5 secs")    )
		
						dbWriteTable(con, "MB_incubation_2012", d[d$datetime_ >= as.POSIXct(xi$start), ] ,append = TRUE, row.names = FALSE)
		
						print(xi)
							}
	
	wet(con, " UPDATE _SANDBIN.MB_incubation_2012 
					SET datetime_1m = mysql.RoundTime(datetime_,'M', 1),
					datetime_2m = mysql.RoundTime(datetime_,'M', 2)
					")
					
}							

{# JOIN THE RFID, T_NEST, t_ambient DATA 
	
		wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_RFID_2012_5s r
						SET b.tag_ID = r.tag_ID,
							b.bout_ID_tr=r.bout_ID_tr
							WHERE r.nest = b.nest and b.datetime_=r.datetime_5s")

				#wet(con, "UPDATE _SANDBIN.MB_incubation_2012 
				#			SET t_nest = NULL,
				#				t_nest_type= NULL,
				#				t_ambient = NULL,
				#				t_ambient_type= NULL
				#				")
		
		wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_MSR_2012_5s  m
						SET b.t_nest = m.t_nest,
							b.t_nest_type= 'MSR',
							b.t_ambient = m.t_ambient,
							b.t_ambient_type= 'MSR'
								WHERE b.nest = m.nest and b.datetime_= m.datetime_5s")
			
		
		tt=	wet(con,"SELECT distinct nest FROM LOGGERSatBARROW.TinyTag where year_=2012") 
		m=	wet(con,"SELECT distinct nest FROM LOGGERSatBARROW.MSR where year_=2012")
		mt=m[m$nest%in%tt$nest,]
		
		
		wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_TT_2012_2m t
						SET b.t_nest = t.t_nest,
							b.t_nest_type= 'TT'
							WHERE b.nest = t.nest and b.datetime_2m = t.datetime_2m and b.t_nest is NULL")
		
		
		#wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_TT_1m_2m t
		#					SET b.t_nest = t.t_nest
		#						WHERE b.nest=t.nest and t.nest 
		#							and b.datetime_1m = t.datetime_1m")

								
		# for nests where t_ambient missing (sets t_ambient to average t-ambient at the time)
			wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_avgT_2012_5s h
						SET b.t_ambient = h.t_amb_avg,
							b.t_ambient_type='AVG'
								WHERE b.datetime_ = h.datetime_ and h.t_amb_avg is not NULL and b.t_ambient is NULL")	
								
		# where TT was not working put NAA
			wet(con, "UPDATE _SANDBIN.MB_incubation_2012 
							SET t_nest = NULL
								WHERE t_nest<-20 and t_nest_type='TT'")	
								
}

{# ADD Captures, Bird_ID, SEX # add captivity
	wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
									SET bird_ID = NULL,
										sex = NULL,
										capture=NULL
										 ")
										 ) 
	
	# add capture
	d=wet(con, "SELECT tag_ID, ID, nest, mysql.RoundTime(caught_date_time,'S', 5) caught_date_time, 
							mysql.RoundTime(release_time,'S', 5) release_time, age 
								FROM AVESatBARROW.CAPTURES where year_=2012") 
								#and tag_ID is not NULL
								
	d$tag_ID=ifelse(d$tag_ID=='E9E7C4B43A6F0001', 'E9E6C4B43A6F0001', d$tag_ID)
	d$ID=ifelse(d$nest=='B302' & d$tag_ID=='662524B43A6F0001', 255174742, d$ID)
	d$ID=ifelse(d$nest=='S809' & d$ID==229154791, 229194791, d$ID)
	d= d[!(d$age=='L' & !is.na(d$age)),]
		#d[which(d$ID==255174748),] 	
		#d[which(d$ID==229194730) ,]							
	# d[which(d$nest=='S111'),]	
		# date_time variables		
			d$caught_date_time= as.POSIXct(strptime(d$caught_date_time, format="%Y-%m-%d %H:%M:%S"))							
			d$release_time= as.POSIXct(strptime(d$release_time, format="%Y-%m-%d %H:%M:%S"))

			# creates caught_date_time where it is missing based on average differance between caught_date_time and release-time
						# check 
						mean(d$caught_date_time-d$release_time, na.rm = TRUE)
					d$caught_date_time= ifelse(is.na(d$caught_date_time),d$release_time + as.numeric(mean(d$caught_date_time-d$release_time, na.rm = TRUE)),d$caught_date_time)

					backtoPOSIXct= function (timevar) structure(timevar, class = c("POSIXt", "POSIXct"))
					d$caught_date_time=backtoPOSIXct(d$caught_date_time)
						# check 
						mean(d$caught_date_time-d$release_time, na.rm = TRUE)

						z=5
						d$caught_date_time=strptime(paste(format(d$caught_date_time,"%Y-%m-%d %H:%M:"),round(as.numeric(format(d$caught_date_time,"%S"))/z)*z,sep=""),format="%Y-%m-%d %H:%M:%S") 
						
	dbWriteTable(con, "MB_temp_2012", d, row.names = FALSE)
	

	
	wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b, _SANDBIN.MB_temp_2012 t
												SET b.tag_ID = t.tag_ID,
													b.capture = 'c',
													b.bird_ID = t.ID
														WHERE b.datetime_= t.caught_date_time
															and b.nest=t.nest")
															)
	
	wet(con, "DROP TABLE MB_temp_2012")
												
	# add bird ID
	d=wet(con, "SELECT tag_ID, ID, nest, mysql.RoundTime(caught_date_time,'S', 5) caught_date_time, 
							mysql.RoundTime(release_time,'S', 5) release_time, age, recapture 
								FROM AVESatBARROW.CAPTURES where year_ in (2011,2012) and tag_ID is not null") 
	
		# checks
			#b=d[d$ID%in%d$ID[duplicated(d$ID)],]
			#b=d[d$ID%in%d$ID[duplicated(d$ID)],]
			#"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0  
			#bb=d[d$ID%!in%d$ID[duplicated(d$ID)] & d$tag_ID%in%d$tag_ID[duplicated(d$tag_ID)],]
	
	d$ID=ifelse(d$nest=='B302' & d$tag_ID=='662524B43A6F0001', 255174742, d$ID)
	d$tag_ID=ifelse(d$tag_ID=='E9E7C4B43A6F0001', 'E9E6C4B43A6F0001', d$tag_ID)
	
	dbWriteTable(con, "MB_temp_2012", d, row.names = FALSE)
	wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b, _SANDBIN.MB_temp_2012 t
												SET b.bird_ID = t.ID
													WHERE b.tag_ID = t.tag_ID
														and t.tag_ID is not NULL")
															)

										
			wet(con, "DROP TABLE MB_temp_2012")
	
	# correct where foreign bird walks over the nest
		#S815 foreign bird walks over the nest
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
												SET bird_ID = NULL
													WHERE bird_ID in (229194795,255174792)
															and nest='S815'
														")
															)
	
	# bring in the SEX data	
			wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b, AVESatBARROW.SEX s
										SET b.sex = if( s.sex = 1, 'm', if(s.sex=2, 'f', NULL))
											WHERE b.bird_ID = s.ID
												")
												)
			
	}

{# INCUBATION 0,1 creation 
	{# only for data where t_nest not NULL
		nests = wet(con, "SELECT distinct nest FROM MB_incubation_2012 where nest!='S809'")$nest
									#where nest not in ('S403', 'S204')
		
		#nests=data.frame(nest=c('S507', 'S302'), t_nest_type=c('TT','MSR'))						
		
		for  (i in 1:length(nests)) {	
		
						
						b=wet(con, paste("SELECT pk, nest, datetime_, t_nest, t_nest_type, t_ambient, bird_ID 
							FROM MB_incubation_2012 where t_nest is not NULL and nest ='", nests[i],"'",sep="" ) )
						
						b$datetime_= as.POSIXct(strptime(b$datetime_, format="%Y-%m-%d %H:%M:%S"))	
						b=b[order(b$datetime_),]
						
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
						
						z=b[,c("pk", "inc_t")]	
				
						dbWriteTable(con, "MB_temp_2012", z, row.names = FALSE)
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
								SET b.inc_t = t.inc_t,
									b.incubation = t.inc_t
										WHERE b.pk= t.pk"))
				
						wet(con, "DROP TABLE MB_temp_2012") # or dbRemoveTable(con, "MB_temp_2012")		
				
						print(b[1,])
						

				}
						
	}	
	{# corrections
		{# S110
				nests='S110'
				i=1
				
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012
										SET incubation = 0,
											inc_t= 0
												WHERE datetime_>'2012-06-16 17:21:50' and datetime_<'2012-06-16 17:59:50' and 
												nest ='", nests[i],"'",sep="" ) )
												
												
												
						
						
				print(nests)
			}	
		{#S314
		nests='S314'
				i=1
				
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012
										SET incubation = 0,
											inc_t= 0
												WHERE datetime_>'2012-06-28 19:04:00' and datetime_<'2012-06-28 19:04:35' and 
												nest ='", nests[i],"'",sep="" ) )
												
												
												
						
						
				print(nests)
		}
		{# S405
				nests='S405'
				i=1
				
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012
										SET incubation = 0,
											inc_t= 0
												WHERE datetime_>'2012-06-16 10:45:05' and datetime_<'2012-06-16 12:01:55' and 
												nest ='", nests[i],"'",sep="" ) )
												
												
												
						
						
				print(nests)
			}	
		{# S510
		nests='S510'
				i=1
				
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012
										SET incubation = 1,
											inc_t= 1
												WHERE datetime_>'2012-07-04 14:00:00' and datetime_<'2012-07-04 16:00:00' and nest ='", nests[i],"'",sep="" ) )
												
												
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012
										SET incubation = 1,
											inc_t= 1
												WHERE datetime_>'2012-07-03 08:00:00' and datetime_<'2012-07-03 11:00:00' and nest ='", nests[i],"'",sep="" ) )						
												
						
						
				print(nests)
			}	
		{# S511
		nests='S511'
				i=1
				
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012
										SET incubation = 1,
											inc_t= 1
												WHERE datetime_>'2012-07-06 09:00:00' and datetime_<'2012-07-06 12:00:00' and nest ='", nests[i],"'",sep="" ) )
												
												
								
				print(nests)
			}	
		{# S804
				nests='S804'
				i=1
				
						wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012
										SET incubation = 0,
											inc_t= 0
												WHERE datetime_>'2012-06-22 20:24:55' and datetime_<'2012-06-24 01:40:20' and nest ='", nests[i],"'",sep="" ) )
												
															
				print(nests)
				}
		
		}
	{# BASED ON bird_ID (also for data where t_nest is null)
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET incubation = 1
									WHERE bird_ID is not NULL")) #and nest='S809'
					}	
}

{# BOUT ID 
	{# create bout BOUT IDs for original RFID and merge it with the incubation table
		nests = wet(con, "SELECT distinct nest FROM MB_incubation_2012")$nest
		
		for  (i in 1:length(nests)) {
				
				wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = NULL,
									switch_check  = NULL
									WHERE nest ='", nests[i],"'",sep="" ))
				
				b=wet(con, paste("SELECT pk, datetime_, capture, bird_ID FROM _SANDBIN.MB_incubation_2012 
											WHERE bird_ID is not NULL and nest ='", nests[i],"'",sep="" )) # it was Tag_ID before
											
						
				b$datetime_= as.POSIXct(strptime(b$datetime_, format="%Y-%m-%d %H:%M:%S"))	
				
				b=b[order(b$datetime_),]
								
				b$bird_ID_after=c(b$bird_ID[-1],b$bird_ID[nrow(b)])  # last time is repeated
				b$bird_ID_prior=c(b$bird_ID[1],b$bird_ID[-nrow(b)])	# first time is repeated

				b$datetime_after=c(b$datetime_[-1], b$datetime_[nrow(b)])
				b$datetime_prior=c(b$datetime_[1],b$datetime_[-nrow(b)])
				
				b$datetime_diff_af = as.numeric(difftime(b$datetime_after, b$datetime_, units = "min"))
				b$datetime_diff_pr = as.numeric(difftime(b$datetime_, b$datetime_prior, units = "min"))
				
				b$capture_pr = c(b$capture[1],b$capture[-nrow(b)])
				
						#densityplot(~b$datetime_diff_af)
				
						#histogram(~b$datetime_diff_af, type="count", 
						#		xlab= "reading interval (min)", 
						#		ylab = "frequency", data=b, subset =b$datetime_diff_af>60)
				
				
				# exchange gap assigned to exchanging bird 
				b$switch= ifelse(b$bird_ID!=b$bird_ID_after, 1,
								#b$bird_ID!=b$bird_ID_after,1
								ifelse(b$datetime_diff_pr>180,1,					
								ifelse(b$datetime_diff_af>180,1,0))
								)	
				b$switch = ifelse(!is.na(b$capture) & b$capture=='c' & b$datetime_diff_pr>180, 0,b$switch)
				b$switch =	ifelse(!is.na(b$capture) & b$capture=='c' & b$datetime_diff_pr>180 & b$bird_ID!=b$bird_ID_after & b$datetime_diff_af<180,
										1, b$switch)	# jinak by to vytvorilo nove bout_ID kde ve skutecnosti zadne nove neni (S709)
				b$switch = ifelse(!is.na(b$capture) & b$capture=='c' & b$datetime_diff_af>180, 1,b$switch)  # nove, mozna nefunguje
		
				
				b$switch_check = ifelse(b$datetime_diff_af > 180,1,0)
				
				b$switch_check = ifelse(b$bird_ID!=b$bird_ID_after, 1, b$switch_check)
				
				b$bout_ID=cumsum(b$switch)
								
				t=b[,c("pk", "bout_ID", "switch_check")]	
				
				dbWriteTable(con, "MB_temp_2012", t, row.names = FALSE)
				
				wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012  b,_SANDBIN.MB_temp_2012 t
								SET b.bout_ID = t.bout_ID,
									b.switch_check = t.switch_check
										WHERE b.pk= t.pk
												")) 
				
				wet(con, "DROP TABLE MB_temp_2012")

				# fill in missing bout IDs in incubation table from first to last point	
				z=wet(con, paste("SELECT nest, pk, bout_ID, switch_check, bird_ID, inc_t 
											FROM _SANDBIN.MB_incubation_2012 
												where nest ='", nests[i],"'",sep="" ) ) 
				
				z=z[order(z$pk),]
				#if (is.na(z$bout_ID [1])) { 
				#			z$bout_ID=z$bout_ID+1
				#			z$bout_ID[1]=1
				#			}
							
				z$bout_ID=na.locf(z$bout_ID, na.rm = FALSE, fromLast=FALSE, maxgap = Inf, rule = 2)
				z$bout_ID=ifelse(is.na(z$bout_ID), 0, z$bout_ID) # changed from -1 to 0
						
				# correct the wrong assingment of the bout_ID due to prior "after statement" 
				# first bout_ID of the bout that has no after tag for 3 hours belong to prior bout
					z$bout_ID = 	ifelse(!is.na(z$switch_check),
									ifelse(z$switch_check == 1, z$bout_ID-z$switch_check, z$bout_ID),z$bout_ID)
				
				dbWriteTable(con, "MB_temp_2012", z, row.names = FALSE)
				wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
								SET b.bout_ID = t.bout_ID
									WHERE b.pk= t.pk" )) 
				wet(con, "DROP TABLE MB_temp_2012")
				
				
				print(z[1,])
				}
				}
	{# delete first rows where t_nest is NA and and bout_ID=1 (except for S809 where t is missing)			
			wet(con, "DELETE FROM _SANDBIN.MB_incubation_2012 
							where nest not in ('S809') and bout_ID<2 and t_nest = NULL
								")
	}								
	{# adjust bout_ID for 
		{#S310 
	
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S310' and datetime_='2012-07-08 09:42:20'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S310' and datetime_>'2012-07-07 12:16:50' and datetime_<'2012-07-08 18:04:10'"))
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S310' and datetime_='2012-07-09 02:22:50'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S310' and datetime_>'2012-07-09 16:33:20' and datetime_<'2012-07-09 22:47:25'"))
		
		wet(con, paste("DELETE from _SANDBIN.MB_incubation_2012 
										WHERE nest='S310' and datetime_>'2012-07-09 22:47:20'"))
		
		}
		{#S510			
		#wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
		#						SET bout_ID = bout_ID+1
		#							WHERE nest='S510' and datetime_='2012-07-03 10:39:40'"))
		
		#wet(con, paste("DELETE FROM _SANDBIN.MB_incubation_2012 
		#						WHERE nest='S510' and datetime_='2012-07-07 04:27:40'"))
		}
		{#S511

		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S511' and datetime_='2012-06-28 08:56:35'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S511' and datetime_>'2012-06-28 08:55:05' and datetime_<'2012-06-28 08:56:35'"))
		
		#wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 SET bout_ID = bout_ID+1 WHERE nest='S511' and datetime_='2012-07-01 11:01:00'"))
		
		#wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
		#						SET bout_ID = bout_ID+1
		#							WHERE nest='S511' and datetime_>'2012-07-06 10:36:45' and datetime_<'2012-07-06 10:39:50'"))
		
		
		}
		{#S514
		
		#wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
		#						SET bout_ID = bout_ID+1
		#							WHERE nest='S514' and datetime_>'2012-06-24 10:10:20' and datetime_<'2012-06-24 10:12:35'"))
		
		wet(con, paste("DELETE from _SANDBIN.MB_incubation_2012 
										WHERE nest='S514' and datetime_>'2012-07-14 14:58:05'"))
		
		
		}
		{#S704 
	
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S704' and datetime_='2012-07-03 07:07:05'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S704' and datetime_>'2012-07-03 07:07:05' and datetime_<'2012-07-04 09:56:00'"))
		
		}
		{#S707
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S707' and datetime_='2012-06-12 03:28:50'"))
		bout_ID$bout_ID=bout_ID$bout_ID-1
	
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S707' and datetime_<'2012-06-12 03:27:25'"))
			
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
									WHERE nest='S707' and datetime_='2012-06-14 06:15:40'"))
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S707' and datetime_>'2012-06-14 06:11:05' and datetime_<'2012-06-14 06:15:40'"))
		
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
									WHERE nest='S707' and datetime_='2012-06-25 20:19:15'"))
		
			wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S707' and datetime_>'2012-06-25 20:19:10' and datetime_<'2012-06-26 09:41:40'"))
		
		}
		{#S717
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
			WHERE nest='S717' and datetime_='2012-07-02 12:39:20'"))
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S717' and datetime_>'2012-07-02 12:16:50' and datetime_<'2012-07-03 03:54:15'"))
		
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
			WHERE nest='S717' and datetime_='2012-07-05 22:07:05'"))
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
									SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S717' and datetime_>'2012-07-05 21:59:25' and datetime_<'2012-07-06 10:21:25'"))
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
			WHERE nest='S717' and datetime_='2012-07-07 22:22:00'"))
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S717' and datetime_>'2012-07-07 22:22:00' and datetime_<'2012-07-08 02:04:35'"))
		
	
		}
		{#S718

		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
									WHERE nest='S718' and datetime_='2012-07-01 06:57:15'"))
		
			
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S718' and datetime_>'2012-07-01 06:52:45' and datetime_<'2012-07-01 06:57:15'"))
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
									WHERE nest='S718' and datetime_='2012-07-02 07:19:30'"))
		
				wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S718' and datetime_>'2012-07-02 07:18:05' and datetime_<'2012-07-02 07:19:30'"))
		
		}
		{#S815 - needs more adjustments to be perfect
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = bout_ID-1
									WHERE nest='S815' and datetime_>'2012-06-26 17:24:35' and datetime_<'2012-06-26 18:14:40'"))
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S815' and datetime_='2012-06-22 17:52:25'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S804' and datetime_>'2012-06-22 21:27:25' and datetime_<'2012-06-23 05:17:55'"))
		
		
		
		}
		{#S804 
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S804' and datetime_='2012-06-14 14:19:10'"))
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S804' and datetime_>'2012-06-14 08:12:20' and datetime_<'2012-06-15 03:25:45'"))
		
	
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S804' and datetime_='2012-06-15 23:19:10'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S804' and datetime_>'2012-06-15 23:27:40' and datetime_<'2012-06-16 05:52:15'"))
		
		
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S804' and datetime_='2012-06-24 01:40:20'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,"
									WHERE nest='S804' and datetime_>'2012-06-22 20:24:55' and datetime_<'2012-06-24 01:40:20'"))
		

		}
		{#S1001
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = bout_ID-1
									WHERE nest='S1001' and datetime_>'2012-06-25 19:18:25' and datetime_<'2012-06-25 23:58:00'"))
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = bout_ID-2
									WHERE nest='S1001' and datetime_>'2012-06-25 23:57:55' and datetime_<'2012-06-26 02:07:50'"))
		
		
		}
		
		}	
	{# adjust bout_IDs 
		nests = wet(con, "SELECT distinct nest FROM MB_incubation_2012 where nest not in ('S809')")$nest 
		{# the last one at the nest where only one bird tagged
			for  (i in 1:length(nests)) {	
				b=wet(con, paste("SELECT nest, pk, capture, bird_ID, inc_t, incubation, bout_ID FROM
											_SANDBIN.MB_incubation_2012
														WHERE inc_t is not NULL and nest ='", nests[i],"'",sep="" )) 
														
				#b$datetime_= as.POSIXct(strptime(b$datetime_, format="%Y-%m-%d %H:%M:%S"))	
				b=b[order(b$pk),]
				max=max(b$pk[-which(is.na(b$bird_ID))])
				print(b[b$pk==max,])
				tst=unique(b$bird_ID[-which(is.na(b$bird_ID))])
				
				
				if (length(tst)==1 & nrow(b[b$pk>max & b$bout_ID==b$bout_ID[b$pk==max],])>0) 
								{ 	b$bout_ID=ifelse(b$bout_ID==b$bout_ID[b$pk==max] & b$pk>max, b$bout_ID+1, b$bout_ID)
									#b$sex_inc_filled=   ifelse(b$bout_ID==b$bout_ID[b$pk==max+1] & b$sex_inc_filled=='m','f',
									#ifelse(b$bout_ID==b$bout_ID[b$pk==max+1] & b$sex_inc_filled=='f', 'm', b$sex_inc_filled))
								
								
								t=b[b$bout_ID==b$bout_ID[b$pk==max+1],c("pk", "bout_ID")]
								dbWriteTable(con, "MB_temp_2012", t, row.names = FALSE)
				
								wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012  b,_SANDBIN.MB_temp_2012 t
										SET b.bout_ID = t.bout_ID
											WHERE b.pk= t.pk
												")) 
								wet(con, "DROP TABLE MB_temp_2012")
								print(b[b$pk==max+1,])
								} 
				
			}	
		}		
		{# adjust starts and ends of bout ids based on inc_t			
			for  (i in 1:length(nests)) {	
				b=wet(con, paste("SELECT nest, pk, capture, bird_ID, inc_t, incubation, bout_ID FROM _SANDBIN.MB_incubation_2012
														WHERE inc_t is not NULL and nest ='", nests[i],"'",sep="" )) 
														
				#b$datetime_= as.POSIXct(strptime(b$datetime_, format="%Y-%m-%d %H:%M:%S"))	
				b=b[order(b$pk),]
				b$dataset='b'
				bsplit = split(b, b$bout_ID)

							foo=lapply(bsplit,function(x) {
											#x=bsplit$"1"
									
											#x$inc_t_prior=c(x$inc_t[1],x$inc_t[-nrow(x)])
											x$inc_prior=c(x$incubation[1],x$incubation[-nrow(x)])
											x$bird_ID_prior=c(x$bird_ID[1],x$bird_ID[-nrow(x)])	# first time is repeated
											
											x$switch=ifelse(x$incubation!=x$inc_prior,1,0)
											x$inc_ID=cumsum(x$switch)
											#nrow(x[x$inc_ID==0,])
											x$bout_ID_x=x$bout_ID
											tst=unique(x$incubation[x$inc_ID==0])
											tst_=unique(x$bird_ID[x$inc_ID==0 & !is.na(x$bird_ID)])
											
											if(length(tst)>1) {print(x[1,]); print(tst)}
										
											# assigns beginning of bout (where a) no bird known, but somebody incubates based on temperature, b) is shorter than 180s) to previous bout
											if(length(tst)==1 &  length(tst_)==0) {
												if (tst==1) { 
													x$bout_ID_x= ifelse(x$inc_ID==0 & 
													nrow(x[x$inc_ID==0,])>0 & nrow(x[x$inc_ID==0,])<36 & x$incubation==1,  x$bout_ID_x-1, x$bout_ID_x) 
													}}
											
											# assings end of the bout where bird_ID NA: 
												
											tst2=unique(x$incubation[x$inc_ID==x$inc_ID[nrow(x)]])
											tst3=unique(x$bird_ID[x$inc_ID==x$inc_ID[nrow(x)] & !is.na(x$bird_ID)])
											tst4=nrow(x[x$inc_ID==x$inc_ID[nrow(x)],])
											
											if(length(tst2)==1 & length(tst3)==0) {
												# and no incubation to the next bout as a gap 											
												if (tst2==0){								
													x$bout_ID_x= ifelse(x$inc_ID==x$inc_ID[nrow(x)], x$bout_ID_x+1, x$bout_ID_x) 
													}
												
												# and bird incubates to the next bout as incubation 
												if (tst2==1 & tst4>0 & tst4<36) {
													x$bout_ID_x=ifelse(x$inc_ID==x$inc_ID[nrow(x)],x$bout_ID_x+1,x$bout_ID_x)
												}
												}
											#x$test=ifelse(x$pk<(x$pk[1]+35), "t", NA)
											#
											
											
											return(x)
												}
												)
												
							k=do.call(rbind, foo)
				
				# same as above for cases where last few lines of inc where attached to next bout, but the newly appearing gaps are still attached to previous bout
				k=k[order(k$pk),]
				k$dataset='k'
				
				ksplit = split(k, k$bout_ID_x)

							foo2=lapply(ksplit,function(x) {
											#x=ksplit$"21"
									
											#x$inc_t_prior=c(x$inc_t[1],x$inc_t[-nrow(x)])
											x$inc_prior=c(x$incubation[1],x$incubation[-nrow(x)])
											x$bird_ID_prior=c(x$bird_ID[1],x$bird_ID[-nrow(x)])	# first time is repeated
											
											x$switch=ifelse(x$incubation!=x$inc_prior,1,0)
											x$inc_ID=cumsum(x$switch)
											#x[x$inc_ID==0,]
											x$bout_ID_x_x=x$bout_ID_x
											
											# assings end of the bout where bird_ID NA and no incubation to next bout: 
												#x[x$inc_ID==x$inc_ID[nrow(x)],]
											tst2=unique(x$incubation[x$inc_ID==x$inc_ID[nrow(x)]])
											tst3=unique(x$bird_ID[x$inc_ID==x$inc_ID[nrow(x)] & !is.na(x$bird_ID)])
											tst4=nrow(x[x$inc_ID==x$inc_ID[nrow(x)],])
											
											if(length(tst2)==1 & length(tst3)==0) {
												# and no incubation to the next bout as a gap 											
												if (tst2==0){								
													x$bout_ID_x_x= ifelse(x$inc_ID==x$inc_ID[nrow(x)], x$bout_ID_x+1, x$bout_ID_x) 
													}
												
												# and bird incubates to the next bout as incubation 
												if (tst2==1 & tst4>0 & tst4<36) {
													x$bout_ID_x_x=ifelse(x$inc_ID==x$inc_ID[nrow(x)],x$bout_ID_x+1,x$bout_ID_x)
												}
												}
											
												return(x)
												}
												)
												
							kk=do.call(rbind, foo2)
				
				
					g=kk[,c("pk", "bout_ID_x_x")]					
				
				dbWriteTable(con, "MB_temp_2012", g, row.names = FALSE)
				wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
								SET b.bout_ID = t.bout_ID_x_x
									WHERE b.pk= t.pk" )) 
				wet(con, "DROP TABLE MB_temp_2012")
				
				
				#print(k[1,])
					}
	}
	}	
	}
					
{# ASSIGNING SEX to BOUTS sex_inc_filled) and CREATE EXCHANGE GAPS
	nests = wet(con, "SELECT distinct nest FROM MB_incubation_2012")$nest #where nest not in ('B301', 'B501')"
			
		
			for  (i in 1:length(nests)) {
					#	i = 49
					wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012  
											SET sex_inc_filled = NULL,
												exchange_gap = NULL
													where nest ='", nests[i],"'",sep="" ) )  
								
					b=wet(con, paste("SELECT pk, nest, datetime_, incubation, bout_ID, bird_ID, sex  
							FROM _SANDBIN.MB_incubation_2012 where nest ='", nests[i],"'",sep="" ) )
		
					b$datetime_= as.POSIXct(strptime(b$datetime_, format="%Y-%m-%d %H:%M:%S"))		
					b$incubation=as.numeric(b$incubation)
					b=b[order(b$datetime_),]
					
					b$sex_inc_filled=NA
					b$exchange_gap=NA	
				
					# fill in the values for each bout (also those where incubation = NA
					bsplit = split(b, b$bout_ID)
					
									foo=lapply(bsplit,function(x) {
											#x=bsplit$"2"
											tst = x$sex[!is.na(x$sex)]
											#print(table(tst))
											if( length(tst) > 0) {
													x$sex_inc_filled =unique(x$sex[!is.na(x$sex)])[1]
													} 
											#if(length(unique(x$sex[!is.na(x$sex)]))>1){
											#print(x[1,])
											#print(table(unique(x$sex[!is.na(x$sex)])))}
												
											return(x)	
												}
											)
											
					a=do.call(rbind, foo)	
					a=a[order(a$pk),]
					#table(a$sex_inc_filled,a$bout_ID)					
					a$sex_after=c(a$sex_inc_filled[-1],NA)	#this works, don't change it, still NA put instead of a$sex_inc_filled[nrow(a)]
					a$bout_ID_after=c(a$bout_ID[-1],NA)#this works, don't change it, still NA put instead of a$bout_ID[nrow(a)]
					
					a$sex_prior=c(a$sex_inc_filled[1],a$sex_inc_filled[-nrow(a)])
					a$bout_ID_prior=c(a$bout_ID[1],a$bout_ID[-nrow(a)])			
					a$dataset='a'
					
				#a[a$datetime_>as.POSIXct('2012-07-15 18:28:00') & a$datetime_<as.POSIXct('2012-07-15 18:30:00') ,]		
					
					# fill in values for bouts where sex is still NA (based on previous sex)
					asplit = split(a, a$bout_ID)
					
									foo2=lapply(asplit,function(x) {
											#x=asplit$"2"
											tst = x$sex_inc_filled[!is.na(x$sex_inc_filled)]
											
											if(length(tst)==0) {
												tst2 = c(unique(x$sex_prior[x$bout_ID_prior!=x$bout_ID]),unique(x$sex_after[x$bout_ID_after!=x$bout_ID]))
												
												x[x$bout_ID_prior!=x$bout_ID,]
												
												
												tst2 = unique(tst2[!is.na(tst2)])
												#print(table(tst))
												#print(x[1,])
												#print(table(tst2))									
												
												if (length(tst2)==0) {
													# this is corrected in the nsplit part
													#print(x[1,])
													} 
													else{
													
												if (tst2[1]=="m"){
													x$sex_inc_filled="f"
													} else {x$sex_inc_filled="m"
													  }
													  }
																	}
											return(x)	
																						
															}
															)
																							
						m=do.call(rbind, foo2)
							
		# adjust for long non-incubation spells (+ those non-inc spelles also attached to exchanging bird)- possibly exclude A301
		# when bout with gap only and a) prior and after bout have same sex, then sex of the current is oposite, b) prior and after have different sex than current has sex of next and bout id of that one
						
						m=m[order(m$pk),]
						
						m$incubation=as.numeric(m$incubation)
						
						m$bout_ID_prior=c(m$bout_ID[1],m$bout_ID[-nrow(m)])
						m$bout_ID_after=c(m$bout_ID[-1],m$bout_ID[nrow(m)])  
						m$sex_prior=c(m$sex_inc_filled[1],m$sex_inc_filled[-nrow(m)])
						m$sex_after=c(m$sex_inc_filled[-1],m$sex_inc_filled[nrow(m)])	
						max=max(m$bout_ID, na.rm=TRUE)	
						m$dataset='m'

				#b[b$datetime_>as.POSIXct('2012-07-12 08:00:50') & b$datetime_<as.POSIXct('2012-07-12 08:01:25') ,]						
						msplit = split(m, m$bout_ID)
									
								foo3=lapply(msplit,function(x) {
											#x=msplit$"27"
											tst_base=unique(x$sex[!is.na(x$sex)])
											if(length(tst_base)==0){
											
											tst=unique(x$sex_inc_filled[!is.na(x$sex_inc_filled)])
											
											if(length(tst)>1){
											print(x[1,])
											print(table(tst))
											}
											
										if (unique(x$bout_ID)==max){
													NA												
										}else{
																		
											if (length(x$sex_prior[x$bout_ID_prior!=x$bout_ID])==0){
													tst_p=tst } else {tst_p = x$sex_prior[x$bout_ID_prior!=x$bout_ID]}
											x[x$bout_ID_prior!=x$bout_ID,]
											if (length(x$sex_after[x$bout_ID_after!=x$bout_ID])==0){
													if (tst=="m"){tst_a="f"} 
													if (tst=="f"){tst_a="m"} 
													} else {tst_a = x$sex_after[x$bout_ID_after!=x$bout_ID]}
											
											if(length(tst_p)>1){
											print(x[1,])
											print(table(tst_p))
												}
											
											if(length(tst_a)>1){
											print(x[1,])
											print(table(tst_a))
												}
																								
																					
											if (is.na(tst_p)) {tst_p='n'}
											if (is.na(tst_a)) {tst_a='n'}
											#print(table(tst_p))
											#print(table(tst_a))
											
											tst2= x$incubation[!is.na(x$incubation) & x$incubation==1 & !is.na(x$bird_ID)]
																				
										# where no bird ID, sex prior, now and after are same, use different sex for now
											if (length(tst2)==0 & tst==tst_p & tst==tst_a ) {
													if (tst=="m") {x$sex_inc_filled="f"
													} else { 
														if (tst=="f") { x$sex_inc_filled="m"
															} else x$sex_inc_filled=x$sex_inc_filled
															}
													} 
											
										# where no bird ID, sex prior not same as now, but after is same, make current bout_ID same to the one that is next
											if (length(tst2)==0 & tst!=tst_p & tst==tst_a ) {
													x$bout_ID=x$bout_ID_after[x$bout_ID_after!=x$bout_ID]
													} 
										# where no bird ID, sex after not same as now, but prior is same, make current bout_ID and sex same to the one that is next											
											
										if(unique(x$bout_ID)!=-1 & unique(x$nest)!='S417'){ # prevents first big bout of f to be named as male
											if (length(tst2)==0 & tst==tst_p & tst!=tst_a ) {
													x$bout_ID=x$bout_ID_after[x$bout_ID_after!=x$bout_ID]
													if(tst_a!='n') {x$sex_inc_filled=tst_a}
													}
													}
												}								
													#print(x[1,])										
											}
											return(x)
											
											}
											
											)
												
					o=do.call(rbind, foo3)
					o$dataset='o'
					
			# creat exchange gaps			
					osplit = split(o, o$bout_ID)
							foo4=lapply(osplit,function(x) {
											#x=osplit$"37"
											if (length(x$datetime_[!is.na(x$incubation)& x$incubation==1])==0) {
												max=x$datetime_[1]-1
												min=x$datetime_[nrow(x)]+1
												} else {
												max=max(x$datetime_[x$incubation==1], na.rm=TRUE)
												min=min(x[x$incubation==1,]$datetime, na.rm=TRUE)
														}
														
											x$exchange_gap = ifelse(is.na(x$incubation), NA,
														ifelse(x$incubation==0 & x$datetime_>max & is.na(x$bird_ID) | 
														x$incubation==0 & x$datetime_<min & is.na(x$bird_ID), 0,NA))
												
											
																							
											return(x)
											}
											)
												
					n=do.call(rbind, foo4)
				
			# adjusting exchange gaps for last bouts
					max=max(n$bout_ID, na.rm=TRUE)
					max_pk=max(n$pk[is.na(n$exchange_gap)]) 
					n$sex_prior=c(n$sex_inc_filled[1],n$sex_inc_filled[-nrow(n)])
					n$bout_ID_prior=c(n$bout_ID[1],n$bout_ID[-nrow(n)])
					n$dataset='n'
					
					nsplit = split(n, n$bout_ID)		
							foo5=lapply(nsplit,function(x) {
											#x=nsplit$"16"
											tst_p=x$sex_prior[x$bout_ID_prior!=x$bout_ID]
											tst=unique(x$sex_inc_filled[!is.na(x$sex_inc_filled)])
											if(length(tst)==0) {tst='n'}
											
											# if last bout only exchange gap, give it a sex opposite of previous bout 									
											if (unique(x$bout_ID)==max & length(x$pk[is.na(x$exchange_gap)]) == 0)  {
												if (tst=='n' & tst_p=='f') {
															x$sex_inc_filled = ifelse(x$bout_ID==max & x$pk>max_pk, "m", x$sex_inc_filled)
															}
												
												if (tst=='n' & tst_p=='m'){
															x$sex_inc_filled = ifelse(x$bout_ID==max & x$pk>max_pk, "f", x$sex_inc_filled)
															}
											}
											
											# if last records are exchange gaps increase bout_ID, change sex, create exchange gap						
											if (unique(x$bout_ID)==max & length(x$pk[is.na(x$exchange_gap)]) > 0)  {
												
												if (tst=="m") { 
													x$sex_inc_filled = 	ifelse(x$pk>max_pk, "f", x$sex_inc_filled)
													x$bout_ID=ifelse(x$bout_ID==max & x$pk>max_pk, x$bout_ID+1, x$bout_ID)
														} 
												
												if (tst=="f"){
															x$sex_inc_filled = 	ifelse(x$bout_ID==max & x$pk>max_pk, "m", x$sex_inc_filled)
															x$bout_ID=ifelse(x$bout_ID==max & x$pk>max_pk, x$bout_ID+1, x$bout_ID)
															}
															
													}
											
											return(x)
											}
											)
					k=do.call(rbind, foo5)							
					
					g=k[,c("pk", "sex_inc_filled", "exchange_gap", "bout_ID","nest")]				
					g$dataset='g'
					print(g[1,])						
					 				
					dbWriteTable(con, "MB_temp_2012", g, row.names = FALSE)
					
					wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
							SET b.sex_inc_filled = t.sex_inc_filled,
								b.bout_ID = t.bout_ID,
								b.exchange_gap = t.exchange_gap								
									WHERE b.pk = t.pk
										and b.nest ='", nests[i],"'",sep="" ) ) 
			
					wet(con, "DROP TABLE MB_temp_2012")
					
					}# end of the loop - do not delete
					
}
{# adjust sex in exchange gap
			for  (i in 1:length(nests)) {
					#	i = 49
					b=wet(con, paste("SELECT pk, nest, datetime_, incubation, bout_ID, bird_ID, sex, sex_inc_filled, exchange_gap  
							FROM _SANDBIN.MB_incubation_2012 where nest ='", nests[i],"'",sep="" ) )
		
					b$datetime_= as.POSIXct(strptime(b$datetime_, format="%Y-%m-%d %H:%M:%S"))		
					b$incubation=as.numeric(b$incubation)
					b=b[order(b$pk),]
					bsplit = split(b, b$bout_ID)
												foo2=lapply(bsplit,function(x) {
											#x=bsplit$"1"
										#print(x[1,])
										tst=x$pk[x$incubation==1 & !is.na(x$incubation)]
										
										
										if(length(tst)!=0){ 
										tst2=unique(x$sex_inc_filled[!is.na(x$incubation)])	
										if(!is.na(tst2)) {
										tst0 = max(x$pk[x$incubation==1 & !is.na(x$incubation)])
										tst1 = max(x$pk)
										
										if (tst2=='f') {tst3='m'}
										if (tst2=='m') {tst3='f'}
										
										
										if (tst0<tst1){
										
										xx=x[x$pk>tst0,]
										tst4a=unique(xx$sex[!is.na(xx$sex)]) #5.9. changed from x to xx
										if (length(tst4a)!=0){
										tst4b=max(xx$pk[!is.na(xx$sex)])
										x$sex_inc_filled=ifelse(x$pk>tst4b,tst3,x$sex_inc_filled)
										x$bout_ID=ifelse(x$pk>tst4b,x$bout_ID+1,x$bout_ID)
										
										} else{
										x$sex_inc_filled=ifelse(x$pk>tst0,tst3,x$sex_inc_filled)
										x$bout_ID=ifelse(x$pk>tst0,x$bout_ID+1,x$bout_ID)
										}
										}
										}
										}
										return(x)	
																						
											}
											)
								
						k=do.call(rbind, foo2)
					g=k[,c("pk", "sex_inc_filled", "exchange_gap", "bout_ID","nest")]				
					print(g[1,])						
					 
					dbWriteTable(con, "MB_temp_", g, row.names = FALSE)
					
					wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_ t
							SET b.sex_inc_filled = t.sex_inc_filled,
								b.bout_ID = t.bout_ID
									WHERE b.pk = t.pk
										and b.nest ='", nests[i],"'",sep="" ) ) 
			
					wet(con, "DROP TABLE MB_temp_")
					
					}# end of the loop - do not delete

}
{# adjust for specific nests
	{#S405
	bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S405' and datetime_='2012-06-14 16:23:15'"))
		
			
	
	wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,",
								sex_inc_filled='f'
									WHERE nest='S405' and datetime_>'2012-06-14 16:19:40' and datetime_<'2012-06-14 16:19:55'"))
		
		}
	{#S511

		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S511' and datetime_='2012-07-01 11:04:05'"))
		
	
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,",
								sex_inc_filled='m'
									WHERE nest='S511' and datetime_='2012-07-01 11:01:00'"))
		
		
		
		}	
	{#S514
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S514' and datetime_='2012-06-24 10:15:30'"))
		
	
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,",
								sex_inc_filled='m'
									WHERE nest='S514' and datetime_>'2012-06-24 10:10:20' and datetime_<'2012-06-24 10:12:35'"))
		
			
		
		}
	{#S706
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S706' and datetime_='2012-06-14 10:22:50'"))
		
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,",
								sex_inc_filled='m',
								exchange_gap=NULL
									WHERE nest='S706' and datetime_>'2012-06-14 10:26:20'and datetime_<'2012-06-14 10:29:15'"))
	
		bout_ID=wet(con, paste("SELECT bout_ID from _SANDBIN.MB_incubation_2012 
											WHERE nest='S706' and datetime_='2012-06-14 10:30:25'"))
		bout_ID$bout_ID=bout_ID$bout_ID
		wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 
								SET bout_ID = ",bout_ID$bout_ID,",
								sex_inc_filled='f',
								exchange_gap=0
									WHERE nest='S706' and datetime_>'2012-06-14 10:29:10'and datetime_<'2012-06-14 10:30:25'"))
	}
		}
# test
w=wet(con, "SELECT*FROM _SANDBIN.MB_incubation_2012 where sex is not NULL and sex not in (sex_inc_filled)")
head(w)			
		
{# BIRD_ID_FILLED where sex_inc_filled known + TREATMENT, CONTROL(assignes known bird_Id for nest and sex)
		# set in, at the datetime_ where fake_egg_in	
		
		
		#nests = wet(con, "SELECT distinct nest FROM MB_incubation_2012 ")$nest	# where nest not in ('B301', 'B501')
			
		for  (i in 1:length(nests)) {
					wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012  
											SET treat = NULL,
												bird_ID_filled=NULL
													where nest ='", nests[i],"'",sep="" ) )  
					
					wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b, (SELECT nest, treatment, 
							mysql.RoundTime(datetime_,'S',5) as datetime_5s FROM EXTRA_AVESatBARROW.`2012_TREATMENT`) t
						SET b.treat = 'in'
								WHERE b.nest=t.nest and 
									b.datetime_ = t.datetime_5s and 
									t.treatment='fake_egg_in' and b.nest ='", nests[i],"'",sep="" )	)
					
					
					
					b =wet(con, paste("SELECT nest, pk, sex_inc_filled, bird_ID,bout_ID, treat, bird_ID_filled  
												FROM _SANDBIN.MB_incubation_2012 where nest ='", nests[i],"'",sep="" ) )
					
					tst_m = unique(b[b$sex_inc_filled=="m",]$bird_ID)
					tst_m=tst_m[!is.na(tst_m)]
					tst_f = unique(b[b$sex_inc_filled=="f",]$bird_ID)
					tst_f=tst_f[!is.na(tst_f)]
					
					bsplit = split(b, b$bout_ID)
					
							foo=lapply(bsplit,function(x) {
											#x=bsplit$"7"
											tst = unique(x$sex_inc_filled[!is.na(x$sex_inc_filled)])
											
											if(length(tst)>1) {
																print(x[1,])
																print(table(tst))}
											
											if( length(tst) > 0) {
											if(tst=='m' & length(tst_m)==1) {
													x$bird_ID_filled =tst_m} 
													
											if(tst=='f' & length(tst_f)==1) {
													x$bird_ID_filled =tst_f} 		
											
											} else {x$bird_ID_filled=NA}	
											#if(length(unique(x$sex[!is.na(x$sex)]))>1){
											#print(x[1,])
											#print(table(unique(x$sex[!is.na(x$sex)])))}
												
											return(x)	
												}
											)
											
					a=do.call(rbind, foo)	
						
					
					tst_tc=unique(a$treat)
					tst_tc=tst_tc[!is.na(tst_tc)]
						if(length(tst_tc)>1){print(length(tst_tc))}
						if (length(tst_tc)==0) {a$exper='c'} 
							
					dbWriteTable(con, "MB_temp_2012", a, row.names = FALSE)
					
					wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
							SET b.bird_ID_filled = t.bird_ID_filled,
								b.exper=t.exper
									WHERE b.pk= t.pk
										and b.nest ='", nests[i],"'",sep="" ) ) 
			
					wet(con, "DROP TABLE MB_temp_2012")
					
					print(a[1,])
					if(length(tst_m)>1){print(tst_m)}
					if(length(tst_f)>1){print(tst_f)}
						}
						
			}
			
{# GRAPH check all the new incubation, sex assignment data	PNG - CHECK IF ALL STARTS of BOUTS ARE REAL  

	#setwd("M:\\PROJECTS\\PHD\\PLOTS\\ACTOGRAMS")
	setwd("C:\\Users\\mbulla\\Documents\\2012")
	
	#nests = wet(con, "SELECT distinct nest FROM MB_incubation_2012 ")$nest #where nest not in ('B301', 'B501', 'S201')
	
	scales = list(x = list(at = 0:24, 
						labels = rep(format(seq.POSIXt(trunc(Sys.time(), 
						"day"), trunc(Sys.time(), "day") + 23 * 3600, 
						"hours"), "%H:%M"), len = 49), rot = 90, cex = 0.7, 
						limits = c(0,24)), y = list(limits=c(0,15), draw=FALSE))
						
	for  (i in 1:length(nests)) {
		bb=wet(con, paste("SELECT nest, datetime_, incubation, sex_inc_filled, exchange_gap, bout_ID, bout_ID_tr, treat 
							FROM _SANDBIN.MB_incubation_2012 where nest ='", nests[i],"'",sep="" ) )
	
		bb$sex_col=ifelse(!is.na(bb$incubation) & is.na(bb$exchange_gap) & bb$sex_inc_filled == "m" & bb$incubation==1, "deepskyblue",
		ifelse(!is.na(bb$incubation) & is.na(bb$exchange_gap) & bb$sex_inc_filled == "f" & bb$incubation==1, "red", NA))	
		 
		bb$act[!is.na(bb$incubation)]=10
		
			bb$exchange_gap=ifelse(!is.na(bb$exchange_gap) & bb$exchange_gap == 0, 12, NA)
			bb$male=ifelse(bb$sex_inc_filled == 'm', 10, NA)
			bb$female=ifelse(bb$sex_inc_filled == 'f', 10, NA)
			
			bb$datetime_= as.POSIXct(strptime(bb$datetime_, format="%Y-%m-%d %H:%M:%S"))	
			bb$day=as.Date(trunc(bb$datetime_, "day"))
			bb$time = as.numeric(difftime(bb$datetime_,trunc(bb$datetime_,"day"),   #DO NOT USE rfid_T$day here!! (the time will be wrong)
								units = "hours"))
			
			sl=unique(bb$day) 
					
				 
			strip.left = function(which.panel, ...) {
						LAB = format(sl[which.panel], "%b-%d")
						grid.rect(gp = gpar(fill = "grey"))
						ltext(0.5, 0.5, cex = 0.8, LAB)
									}  
				
			panel = function(...) {
				   panel.abline(v=c(1:23),col="light grey")
				   panel.xyplot(bb$time[bb$day==sl[panel.number()]],
								bb$act[bb$day==sl[panel.number()]],col=bb$sex_col[bb$day==sl[panel.number()]],type="h")
				   panel.xyplot(...)
					}
					
			maintitle=paste(nests[i])
			
			plot=
			xyplot(exchange_gap+male+female~time|day,data=bb, 
						col=c("orange", "deepskyblue", "red","yellow"), type=c("p"),
						cex=c(0.15), pch=19,
						strip.left=strip.left,
	                    as.table=TRUE,panel=panel, main=maintitle,
						layout = c(1, ifelse(length(sl)>30,30,length(sl))),
						strip=FALSE,distribute.type=TRUE, scales=scales,
						key=list(text= list(c("female","male" ,"exhchange gap")),
                        points=list(pch=19,col=c("red","deepskyblue","orange"))),
						lattice.options = list(layout.widths = list(strip.left = list(x = 3))))  
						
			png(paste(maintitle, "png", sep = "."), width = 8, height = 11.6929134, units = "in", res = 300)	
			
			print(plot)
			dev.off()
			print(nests[i])
			}
	}		

{# JOIN OTHER ENVI + GPS DATA to MAIN FRAME 	
	{# preparation

	wet(con,"ALTER TABLE _SANDBIN.MB_incubation_2012 
						ADD COLUMN (
							dist FLOAT NULL DEFAULT NULL COMMENT 'nearest distance (m) a gps is to the nest at a given time',
							t_station FLOAT NULL DEFAULT NULL COMMENT '2 meter ambient temperature in Celsius',
							wind_sp FLOAT NULL DEFAULT NULL COMMENT '10 meter wind speed in meters per second',
							wind_dir FLOAT NULL DEFAULT NULL COMMENT '10 meter wind direction in degrees',
							light_int FLOAT NULL DEFAULT NULL COMMENT 'watts/square meter',
							sun_elev FLOAT NULL DEFAULT NULL COMMENT 'e$sun_elevation = solarpos(cbind(-156.651758,  71.319186), e$datetime_ )[,2]',
							h FLOAT NULL DEFAULT NULL COMMENT 'humidity at each nest',
							h_avg FLOAT NULL DEFAULT NULL COMMENT 'averaged',
							time_on FLOAT NULL DEFAULT NULL COMMENT 'summed up time the bird sits within a given bout',
							visit INTEGER NULL DEFAULT NULL COMMENT '0=no visit, 1 = visit'
								)")	
								
	
	}							
	{# GPS data - distance of the nest to the nearest GPS
	
	
	
	GPS =  wet(con, "SELECT mysql.RoundTime(datetime_,'S', 5) datetime_, latit, longit, gpsID from LOGGERSatBARROW.GPS
							where year_= 2012
							group by datetime_")
						
	nests = wet(con, "SELECT distinct a.nest, b.latit, b.longit FROM _SANDBIN.MB_incubation_2012 a
								LEFT JOIN
									(SELECT nest, latit, longit FROM AVESatBARROW.NESTS where year_ = 2012) b
									on a.nest = b.nest
									group by a.nest") 
									
	for( i in 1:nrow(nests)) {
						
				nestsi = nests[i, ]
				x = cbind(GPS, dist = spDistsN1(as.matrix(GPS[, c("longit", "latit") ]), as.matrix(nestsi[, c("longit", "latit") ]), longlat = TRUE))
					#z = subset(data.frame(xtabs(~  datetime_+gpsID, x)), Freq > 0)
					#table(z$Freq)
				x = aggregate(dist ~ datetime_, FUN = min, data = x)
				x$dist = x$dist*1000
				x$dist= ifelse(x$dist == 0, 5, x$dist)
				
				f =  wet(con, paste("SELECT pk, datetime_ from _SANDBIN.MB_incubation_2012 WHERE nest = ", shQuote(nestsi$nest) ) )			
					# n=wet(con, paste("SELECT pk, datetime_ form _SANDBIN.MB_incubation_2012 where nest ='", nests[i],"'",sep="" ) )

				f = merge(f, x, all.x = TRUE)
				
				dbWriteTable(con, "MB_temp_2012", f ,row.names = FALSE)
				
				wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
								SET b.dist = t.dist
									WHERE b.pk= t.pk"))
				
				wet(con, "DROP TABLE MB_temp_2012") # or dbRemoveTable(con, "MB_temp_2012")	

				print(head(nestsi))
				}
				
		}	
	{# ENVI_ general
		# t, wind, light
			wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,LOGGERSatBARROW.ENVIRONMENTAL e
							SET 	b.t_station = e.temperature,
									b.wind_sp = e.wind_speed,
									b.wind_dir = e.wind_direction,
									b.light_int = e.light_intensity
											WHERE b.datetime_1m = e.datetime_")	
		
			wet(con, "UPDATE _SANDBIN.MB_incubation_2012 
								SET light_int = 0.001
											WHERE light_int<0")	
											
		# sun_elevation, resid_light_int, t_amp resid 
			b=wet(con,  paste("SELECT datetime_, light_int, t_ambient, t_station
										FROM _SANDBIN.MB_incubation_2012
											GROUP BY datetime_ " ))
				
				b$datetime_= as.POSIXct(b$datetime_, tz = "America/Anchorage")
				b$sun_elev = solarpos(cbind(-156.651758,  71.319186), b$datetime_ )[,2]
					
				#fm = lm(log(light_int) ~ poly(sun_elev, 2)  , b, na.action = na.exclude) # plot(allEffects(fm), ask = FALSE)
				#b$resid_light_int= resid(fm)
					
				#fm2 = lm(t_ambient ~ t_station , b, na.action = na.exclude) # plot(allEffects(fm), ask = FALSE)
				#b$t_amb_resid= resid(fm2)						
				
								
								
			dbWriteTable(con, "MB_temp_2012", b, row.names = FALSE)
			wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
											SET b.sun_elev = t.sun_elev
												 WHERE b.datetime_= t.datetime_"))
			wet(con, "DROP TABLE MB_temp_2012")
		
	
		# precipitation (remove the #, if run newly for 2012
		
			
			wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b, _SANDBIN.MB_precip_2012 e
									SET 	b.precip = e.precip
												WHERE b.datetime_ = e.datetime_ and e.precip is not NULL")
		# humidity and t_ambient/5s average

			wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_avgT_2012_5s h
						SET b.t_amb_avg = h.t_amb_avg,
						b.h_avg = h.h_avg
							WHERE b.datetime_ = h.datetime_")
				
			wet(con, "UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_MSR_2012_5s e
							SET 	b.h = e.h
										WHERE b.datetime_ = e.datetime_5s and b.nest=e.nest")	
}
	{# running time on
				#sd(d$bout_length) #157.1266
				#sd(d$bout_length[d$bout_length>500 & d$bout_length<756]) #SD withou upper and lower quantile 65.73942
			
			wet(con,"UPDATE _SANDBIN.MB_incubation_2012 
										SET time_on = NULL
											")
									
			nests = wet(con, "SELECT distinct nest FROM MB_incubation_2012")$nest # where nest not in ('B301', 'B501', 'S201')
	# time on the nest within bout
				for  (i in 1:length(nests)) {
					b=wet(con, paste("SELECT nest, pk, bout_ID, exchange_gap
							FROM _SANDBIN.MB_incubation_2012 where bout_ID is not null and exchange_gap is null and nest ='", nests[i],"'",sep="" ) )
				
				b=b[order(b$pk),]
				b$time_on_temp=5
				bsplit=split(b,b$bout_ID)
				bsplit=lapply(bsplit, function(x) cbind(x, time_on=cumsum(x$time_on_temp) ) )
				b=do.call(rbind,bsplit)
				
				dbWriteTable(con, "MB_temp_2012", b, row.names = FALSE)
				wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012 b,_SANDBIN.MB_temp_2012 t
												SET b.time_on = t.time_on
														WHERE b.pk= t.pk"))
				wet(con, "DROP TABLE MB_temp_2012")
				print(b[1,])
					}						
		}
	{# ADD INDEXes
			wet(con,"ALTER TABLE _SANDBIN.MB_incubation_2012 
									ADD INDEX `dist` (`dist`),
									ADD INDEX `sun_elev` (`sun_elev`)
									") 
			
		
	}
	{# JOIN VISITS 

	wet(con, paste("UPDATE _SANDBIN.MB_incubation_2012  
											SET visit = 0"))

											
	wet(con,"UPDATE _SANDBIN.MB_incubation_2012 i, _SANDBIN.MB_visit_2012 e 
										SET i.visit = 1
										WHERE i.datetime_= e.datetime_ and i.nest=e.nest
													")	
								
}
	}	
							
{# NOT USED NOW special incubation variable for nests where not T, but we know bird incubated - good for PROBABILITY MODEL
	wet(con,"ALTER TABLE _SANDBIN.MB_incubation_2012 
						ADD COLUMN (
							inc_s FLOAT NULL DEFAULT NULL COMMENT 'special incubation for specific nests... if sex assigned, but incubation NA, put 1, if sex assigned and incubation 0, put 0, otherwise same as incubation'
									)")	
	wet(con,"ALTER TABLE _SANDBIN.MB_incubation_2012 
									ADD INDEX `inc_s` (`inc_s`)
									") 							
	
	wet(con,"UPDATE _SANDBIN.MB_incubation_2012
										SET `inc_s` = `incubation` 
											")
	wet(con,"UPDATE _SANDBIN.MB_incubation_2012  
										SET inc_s = 1
										WHERE incubation is null and 
										sex_inc_filled in ('f','m') and 
										nest in ('S112', 'S513','S809')
												")
												
	}							
	


# weather_usual - exchange gap separate	

{# MAIN FRAME
	wet(con, "DROP TABLE if EXISTS MB_weather_2012_SESA")
	
	wet(con,	"CREATE TABLE _SANDBIN.MB_weather_2012_SESA (
							`pk` INT(11) NOT NULL AUTO_INCREMENT,
							`species` VARCHAR(4) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`nest` VARCHAR(5) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`exper` VARCHAR(1) NULL DEFAULT NULL COMMENT 't = treatment or c= control nest' COLLATE 'latin1_bin',
							`bird_tr` VARCHAR(1) NULL DEFAULT NULL COMMENT 'y = bird was treated or n= bird was not treated' COLLATE 'latin1_bin',
							`treat` VARCHAR(2) NULL DEFAULT NULL COMMENT 'status of the nest at the time, n=natural, in=bout where fake egg was put in, b=fake egg in, tt= fake egg on, tn= non treatement bird, a=fake egg off ' COLLATE 'latin1_bin',
							`bird_ID` INT(10) NULL DEFAULT NULL COMMENT 'unique bird_ID of the bout',
							`bird_ID_filled` INT(10) NULL DEFAULT NULL COMMENT 'filled bird_ID where sex_inc_filled is not NULL and bird_ID is known',
							`sex` VARCHAR(3) NULL DEFAULT NULL ,
							`bout_ID` FLOAT NULL DEFAULT NULL COMMENT 'ID of incubation bout, if exchange gap than bout_ID-0.5',
							`bout_type` VARCHAR(16) NULL DEFAULT NULL COMMENT 'incubation, exchange gap',
							`bout_ID_tr` INT(11) NULL DEFAULT NULL COMMENT 'ID of all incubation bouts after the treatment was started',
							`inc_start` DATETIME NULL DEFAULT NULL COMMENT 'datetime',
							`bout_start` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							`bout_end` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							`bout_length` FLOAT NULL DEFAULT NULL,
							`inc_eff` FLOAT NULL DEFAULT NULL COMMENT 'mean(b$inc_t, na.rm= T)', 
							`inc_eff_2` FLOAT NULL DEFAULT NULL COMMENT 'mean(b$incubation, na.rm= T)', 
							`disturb` FLOAT NULL DEFAULT NULL COMMENT 'sum(1/b[which(b$dist<210),]$dist, na.rm= T)', 
							`disturb_log` FLOAT NULL DEFAULT NULL COMMENT 'sum(1/log(b[which(b$dist<210),]$dist), na.rm= T)', 
							`dist_visit` FLOAT NULL DEFAULT NULL COMMENT 'same as disturb, but accounting for visits', 
							`dist_v_log` FLOAT NULL DEFAULT NULL COMMENT 'same as disturb, but accounting for visits', 
							`capture` INT(1) NULL DEFAULT NULL COMMENT 'number of captures within bout',
							`t_ambient_med` FLOAT NULL DEFAULT NULL COMMENT 'based on HOBO or MSR',
							`t_amb_avg` FLOAT NULL DEFAULT NULL COMMENT 'average t_ambient (from all hobos and MSRs running at that time',
							`t_station_med` FLOAT NULL DEFAULT NULL COMMENT '2 meter ambient temperature in Celsius',
							`wind_sp_med` FLOAT NULL DEFAULT NULL COMMENT '10 meter wind speed in meters per second',
							`wind_dir_med` FLOAT NULL DEFAULT NULL COMMENT '10 meter wind direction in degrees',
							`precip_sum` FLOAT NULL DEFAULT NULL COMMENT 'sum of precipitation for the bout in mm',
							`h_med` FLOAT NULL DEFAULT NULL COMMENT 'median humidity at the nest',
							`h_avg_med` FLOAT NULL DEFAULT NULL COMMENT 'median average humidity at the study site',
							`light_int_med` FLOAT NULL DEFAULT NULL COMMENT 'watts/square meter',
							`sun_elev_med` FLOAT NULL DEFAULT NULL COMMENT 'e$sun_elevation = solarpos(cbind(-156.651758,  71.319186), e$datetime_ )[,2]',
							`t_nest_med` FLOAT NULL DEFAULT NULL COMMENT 'based on TT or MSR',
							`t_type` VARCHAR(5) NULL DEFAULT NULL COMMENT 'MSR or TT',
							`t_amb_type` VARCHAR(5) NULL DEFAULT NULL COMMENT 'MSR, HOBO, AVG',
									PRIMARY KEY (`pk`),
									INDEX `species` (`species`),
									INDEX `nest` (`nest`),
									INDEX `bout_start` (`bout_start`),
									INDEX `bout_end` (`bout_end`),
									INDEX `bout_length` (`bout_length`),
									INDEX `inc_eff` (`inc_eff`),
									INDEX `disturb` (`disturb`),
									INDEX `bird_ID` (`bird_ID`),
									INDEX `bird_ID_filled` (`bird_ID_filled`),
									INDEX `sex` (`sex`)
										)"
										)	
		}
{# FILL IN		
		#wet(con, "DELETE from _SANDBIN.MB_weather_2012_SESA where nest in ('S804')")
		#nb=wet(con, "SELECT distinct CONCAT_WS('-', nest, bout_ID, exchange_gap) as nest_bout FROM _SANDBIN.MB_incubation_2012 where species = 'SESA' and bout_ID is not NULL and nest in ('S804')")
		
		nb=wet(con, "SELECT distinct CONCAT_WS('-', nest, bout_ID, exchange_gap) as nest_bout 
							FROM _SANDBIN.MB_incubation_2012 where species = 'SESA' and bout_ID is not NULL ")
		
		for( i in 1:nrow(nb)) {
							#i=18
							b=wet(con, paste("SELECT*FROM  _SANDBIN.MB_incubation_2012 
											where CONCAT_WS('-', nest, bout_ID, exchange_gap) ='", nb$nest_bout[i],"'",sep="" ) )
											
	
								b$incubation=as.numeric(b$incubation)
								b$datetime_= as.POSIXct(b$datetime_, tz = "America/Anchorage")
								b$dist_visit=ifelse(b$visit==1,5, b$dist)
								tst=unique(b$treat)
								tst=tst[!is.na(tst)]
								tst2=unique(b$bout_ID_tr)
								tst2=tst2[!is.na(tst2)]
								
								d=data.frame(nest = NA )	
								d$species=b$species[1]
								d$nest = b$nest[1]
								d$exper=b$exper[1]
								d$bird_tr=NA
								if (length(tst)==0) {d$treat=NA}else{d$treat=tst}							
								if(all(is.na(b$bird_ID))){d$bird_ID = NA}else 
									 {d$bird_ID = unique(b$bird_ID[!is.na(b$bird_ID)])}
								#d$bird_ID_filled=unique(b$bird_ID_filled[!is.na(b$bird_ID_filled)])
								if(all(is.na(b$bird_ID_filled))){d$bird_ID_filled = NA}else 
									{d$bird_ID_filled = unique(b$bird_ID_filled[!is.na(b$bird_ID_filled)])}
								
								d$sex=b$sex_inc_filled[1]
								
								d$bout_ID= ifelse(is.na(unique(b$exchange_gap)), b$bout_ID[1],b$bout_ID[1]-0.5 )
								d$bout_type = ifelse (is.na(unique(b$exchange_gap)), "inc", "gap")
								
								if (length(tst2)==0) {d$bout_ID_tr=NA}else{if (length(tst2)==2) {d$bout_ID_tr=tst2[1]}else{d$bout_ID_tr=tst2}}
								
								
								d$bout_start=b$datetime_[1]
								d$bout_end= b$datetime_[nrow(b)]+5
								d$bout_length = nrow(b)*1/12 # in minutes
																
								if(any(is.na(b$inc_t))) {d$inc_eff = NA } else {d$inc_eff= mean(b$inc_t)}
								if(any(is.na(b$inc_t))) {d$inc_eff_2 = NA } else {d$inc_eff_2= mean(b$incubation)}
															
								d$disturb=sum(1/b$dist[which(b$dist<210)], na.rm= T)	# should it be logged + does it exclude NAs
								d$disturb_log=sum(1/log(b$dist[which(b$dist<210)]), na.rm= T)
								d$dist_visit=sum(1/b$dist_visit[which(b$dist_visit<210)], na.rm= T)
								d$dist_v_log=sum(1/log(b$dist_visit[which(b$dist_visit<210)]), na.rm= T)
								
								d$capture= length(b$capture[!is.na(b$capture)]) # number of catchings
								
								d$t_ambient_med = median(b$t_ambient, na.rm= T)
								d$t_station_med = median(b$t_station, na.rm= T)
								d$t_amb_avg = median(b$t_amb_avg, na.rm= T)
														
								d$wind_sp_med = median(b$wind_sp, na.rm= T)
														d$wind_dir_med = median(b$wind_dir, na.rm= T)
								d$precip_sum = sum(b$precip, na.rm= T) # in mm
								d$h_med = median(b$h, na.rm= T)
								d$h_avg_med = median(b$h_avg, na.rm= T)
								
								d$light_int_med = median(b$light_int, na.rm= T)
								d$sun_elev_med = median(b$sun_elev, na.rm= T)
							
								d$t_type= b$t_nest_type[1]
								d$t_amb_type= b$t_ambient_type[1]
								d$t_nest_med = median(b$t_nest, na.rm= T)
								
								dbWriteTable(con, "MB_weather_2012_SESA", d, append = TRUE, row.names = FALSE)
							#print(d[1,])
							#print(unique(b$bird_ID))
							print(nb$nest_bout[i])
							}	
							
	}	

{# ADD VARIABLES into DB

	{# which bird is treated
		#load(file = "M:\\PROJECTS\\PHD\\STATS\\DATASETS\\TCmf_2012.Rdata")
		load(file = "C:\\Users\\mbulla\\Documents\\TCmf_2012.Rdata")
		
		
		b=wet(con, paste("SELECT pk, nest, bird_ID_filled, sex FROM _SANDBIN.MB_weather_2012_SESA where species = 'SESA' "))
	
		b=merge(b,d,by='nest', all.x=TRUE)
		b$treatment=ifelse(b$treatment=='c', 'cm', b$treatment)
		
		b$bird_tr=ifelse(b$treatment=='cf' & b$sex=='f','y', ifelse(b$treatment=='cm' & b$sex=='m','y','n'))
		
		con = dbConnect(dbDriver("MySQL"), username = 'mbulla', password=password, dbname = '_SANDBIN', host = 'scidb.orn.mpg.de')
		dbWriteTable(con, "MB_temp_2012", b, row.names = FALSE)
					
			wet(con, paste("UPDATE _SANDBIN.MB_weather_2012_SESA b,_SANDBIN.MB_temp_2012 t
							SET b.bird_tr = t.bird_tr
									WHERE b.pk= t.pk
										and b.nest = t.nest") ) 
			
					wet(con, "DROP TABLE MB_temp_2012")
				
	}
	{# start of incubation (sun elevation)
	
		b=wet(con, paste("SELECT nest, incubation_start as inc_start FROM AVESatBARROW.NESTS where year_=2012 and species = 'SESA'"))
		
		b=b[!is.na(b$inc_start),]
		
		b$inc_start= as.POSIXct(strptime(b$inc_start, format="%Y-%m-%d %H:%M:%S"), tz = "America/Anchorage")	
		#b$inc_start_se = solarpos(cbind(-156.651758,  71.319186), b$inc_start )[,2]
		
		
		dbWriteTable(con, "MB_temp_2012", b, row.names = FALSE)
		
		wet(con, paste("UPDATE _SANDBIN.MB_weather_2012_SESA b, MB_temp_2012 t
									SET b.inc_start = t.inc_start
											where b.nest=t.nest"))
							# b.inc_start_se = t.inc_start_se				
												
		wet(con, "DROP TABLE MB_temp_2012")	
	}
	{# end_stat, end_state_date, hatching_start
		wet(con,"ALTER TABLE _SANDBIN.MB_weather_2012_SESA 
							ADD COLUMN (
								nest_endstate VARCHAR(2)  NULL DEFAULT NULL COMMENT 'hd,p,d,ud',
								nest_endstate_date  DATETIME NULL DEFAULT NULL COMMENT 'datetime',
								hatch_start DATETIME  NULL DEFAULT NULL COMMENT 'datetime'
									)")	
			
		wet(con, paste("UPDATE  _SANDBIN.MB_weather_2012_SESA  b, AVESatBARROW.NESTS n
										SET b.nest_endstate = n.nest_endstate,
											b.nest_endstate_date = n.nest_endstate_date, 
											b.hatch_start = n.hatch_start
												WHERE b.nest = n.nest
													and n.year_=2012"))													
		

	}
	{# body measurements (#make sure that all bouts that should have bird_ID have it)
			wet(con,"ALTER TABLE _SANDBIN.MB_weather_2012_SESA 
							ADD COLUMN (
								tarsus FLOAT  NULL DEFAULT NULL COMMENT 'mm',
								culmen FLOAT NULL DEFAULT NULL COMMENT 'mm',
								tot_head FLOAT  NULL DEFAULT NULL COMMENT 'mm',
								wing FLOAT NULL DEFAULT NULL COMMENT 'mm',
								weight FLOAT NULL DEFAULT NULL COMMENT 'g',
								fat FLOAT NULL DEFAULT NULL COMMENT 'score',
								`caught` DATETIME NULL DEFAULT NULL,
								radioFr VARCHAR(2) NULL DEFAULT NULL COLLATE 'latin1_bin'
									)")	
			
			
			b=wet(con, "SELECT caught_date_time, nest, ID, recapture, carries_egg, culmen, totalHead, tarsus, wing, 
												weight, fat, ectoparasites, radioFr,
												author, comments  FROM AVESatBARROW.CAPTURES
													WHERE LEFT(comments,5) not in ('remea') and year_=2012 and culmen is not null 
														OR comments is null and year_=2012 and culmen is not null 
															ORDER BY ID")
				
			#which(duplicated(b$ID))	
			#b[which(b$ID==225118856),]
	
			#b=b[-which(b$ID==225118856 & b$author=='BK'),] 	
			b$weight[b$ID==225118856]= 27.0
			b$caught_date_time[b$ID==225118856]= "2012-06-07 11:55:23"
			#b[which(b$ID==229194738),]$weight= 25.9
			
			
			
			#b[which(b$carries_egg==1),]
			b$carries_egg=ifelse(is.na(b$carries_egg), 0, b$carries_egg)
			b$weight= ifelse(b$carries_egg==1, b$weight-7.3, b$weight) 
			
			b$radioFr=ifelse(is.na(b$radioFr), 'n', 'y')
			
			
			dbWriteTable(con, "MB_temp_2012_2", b, row.names = FALSE)
			wet(con, paste("UPDATE _SANDBIN.MB_weather_2012_SESA b, MB_temp_2012_2 t
									SET b.tarsus = t.tarsus,
										b.culmen = t.culmen,
										b.tot_head = t.totalHead,
										b.wing = t.wing,
										b.weight = t.weight,
										b.fat = t.fat,
										b.radioFr=t.radioFr,
										b.caught=t.caught_date_time
											where b.bird_ID_filled=t.ID")
												)
			
			wet(con, paste("UPDATE _SANDBIN.MB_weather_2012_SESA 
									SET radioFr='n'
											where nest='S510'")
												)
												
			wet(con, "DROP TABLE MB_temp_2012_2")
	}
	{# position

		#wet
		wet(con,"ALTER TABLE _SANDBIN.MB_weather_2012_SESA 
							ADD COLUMN (
								longit FLOAT  NULL DEFAULT NULL COMMENT 'degrees',
								latit FLOAT NULL DEFAULT NULL COMMENT 'degrees'
								
									)")	
		#wet
		wet(con, paste("UPDATE _SANDBIN.MB_weather_2012_SESA b, (Select nest, longit, latit from AVESatBARROW.NESTS where year_ = 2012) t 
									SET b.longit = t.longit,
										b.latit = t.latit
											where b.nest=t.nest"))	
			
		}		

}

{# CREAT RData FILE: ADD VAR + LIMIT THE DATASET TO GOOD DATA
		b=wet(con, paste("SELECT*FROM _SANDBIN.MB_weather_2012_SESA")) 
			
			b$inc_start= as.POSIXct(b$inc_start, tz = "America/Anchorage")
			b$bout_end= as.POSIXct(b$bout_end, tz = "America/Anchorage")
			b$bout_start= as.POSIXct(b$bout_start, tz = "America/Anchorage")
			b$hatch_start= as.POSIXct(b$hatch_start, tz = "America/Anchorage")
			b$nest_endstate_date= as.POSIXct(b$nest_endstate_date, tz = "America/Anchorage")
		
		# exclude last bout, if gap
		bsplit=split(b,b$nest)
			foo=lapply(bsplit,function(x) {
										#x=bsplit$"S322"
											
											if (x$bout_type[nrow(x)]=="gap") {
											x=x[!x$bout_ID==x$bout_ID[nrow(x)],]
											 } else x=x
										
										return(x)
											}
											)
												
			b=do.call(rbind, foo)
		
		# additional time var
				
			# start within incubation
				b$bout_start_j  = as.numeric(format(b$bout_start ,"%j")) - as.numeric(format(b$inc_start,"%j")) +1
			# start within season	
				b$b_st_season_j = as.numeric(format(b$bout_start,"%j")) - min(as.numeric(format(b$inc_start,"%j")))+1 
			# start of incubation within season
				b$inc_start_j = as.numeric(format(b$inc_start,"%j")) - min(as.numeric(format(b$inc_start,"%j")))+1
				
			# other
				b$time = as.numeric(difftime(b$bout_start,trunc(b$bout_start,"day"), units = "hours"))
				#b$sun_12=factor(ifelse(b$time>0 & b$time<=12, 1, 2))
			
			# time (in min) to radians
					#b$time = as.numeric(difftime(b$bout_start,trunc(b$bout_start,"day"), units = "mins"))
				b$rad= (2*pi*as.numeric(difftime(b$bout_start,trunc(b$bout_start,"day"), units = "mins"))) / (24*60)
				
				b$nest_bout=paste(b$nest, b$bout_ID, sep = "-", collapse = NULL)
				b$bird_ID_fake= ifelse(is.na(b$sex), NA, paste(b$nest, b$sex, sep = "-", collapse = NULL))
			
}
{# EXCLUDE			
		# exclude bouts where endstate occured		
		d=b
		d=d[-which(d$bout_end>=(d$nest_endstate_date-1800)), ] # reduce it by 1/2 hours to exclude more data prior to detected hatching start
	
		# exclude all bouts older than hatch_start-6 hours
			d=d[-which(d$bout_end>(d$hatch_start-21600)), ] # reduce it by 6 hours to exclude more data prior to detected hatching start
			
		nrow(d[d$bout_type=="inc",]) 	
		
		# exclude first 3 bouts of the data
			d=d[-which(d$bout_ID < 1),] 
			d=d[-which(d$bout_ID < 2 & !d$nest%in%c('S1003','S110','S201','S210','S313','S402','S405','S409','S413','S507','S511','S310',
			'S704','S707','S718','S809','S810','S811')),] 
						
		# exclude last bout, if gap
		dsplit=split(d,d$nest)
			foo=lapply(dsplit,function(x) {
										#x=bsplit$"S322"
											
											if (x$bout_type[nrow(x)]=="gap") {
											x=x[!x$bout_ID==x$bout_ID[nrow(x)],]
											 } else x=x
										
										return(x)
											}
											)
												
			d=do.call(rbind, foo)
		
			nrow(d[d$bout_type=="inc",]) 		
		    
		# exclusions based on visual check 
			# all bouts after one bird deserted
			# S514 - egg laying excluded
		
			
			d=d[-which(d$nest=='S1003' & d$bout_start>as.POSIXct("2012-07-06 02:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S110' & d$bout_start>as.POSIXct("2012-07-10 00:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S201' & d$bout_start>as.POSIXct("2012-06-21 00:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S310' & d$bout_start>as.POSIXct("2012-07-06 04:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S402' & d$bout_start>as.POSIXct("2012-06-22 00:00:00",tz = "America/Anchorage")),] 
		###### excluded due to catching issues, but might be used in the anal where consequitive data are not necessary if only 2 bout is excluded
			#d=d[-which(d$nest=='S405' & d$bout_start>as.POSIXct("2012-06-16 00:00:00",tz = "America/Anchorage") & d$bout_start<as.POSIXct("2012-06-16 17:00:00",tz = "America/Anchorage")),]	
			
			d=d[-which(d$nest=='S405' & d$bout_start<as.POSIXct("2012-06-16 10:00:00",tz = "America/Anchorage")),]	
			
			# check the results and maybe exclude the bouts in S507
			d=d[-which(d$nest=='S507' & d$bout_start<as.POSIXct("2012-06-17 06:00:00",tz = "America/Anchorage")),] 
		
			d=d[-which(d$nest=='S510' & d$bout_start<as.POSIXct("2012-06-19 12:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S514' & d$bout_start<as.POSIXct("2012-06-23 12:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S603' & d$bout_start<as.POSIXct("2012-06-11 22:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S603' & d$bout_start>as.POSIXct("2012-06-20 14:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S704' & d$bout_start>as.POSIXct("2012-07-01 00:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S706' & d$bout_start<as.POSIXct("2012-06-14 22:00:00",tz = "America/Anchorage")),]	
			d=d[-which(d$nest=='S706' & d$bout_start>as.POSIXct("2012-06-24 00:00:00",tz = "America/Anchorage")),]		
			d=d[-which(d$nest=='S717' & d$bout_start>as.POSIXct("2012-07-06 18:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S804' & d$bout_start<as.POSIXct("2012-06-13 22:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S804' & d$bout_start>as.POSIXct("2012-07-04 02:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S815') ,] 
			
		# possibly also exclude
			# S405 incubates only one egg from Jun 24 - so predation happend in these bout---
				#d=d[-which(d$nest=='S405' & d$bout_start>as.POSIXct("2012-06-23 12:00:00",tz = "America/Anchorage")),]
			# S310 possibly incubates only one egg from Jul 5	
				#d=d[-which(d$nest=='S310' & d$bout_start>as.POSIXct("2012-07-05 04:00:00",tz = "America/Anchorage")),] 
			# S804 d=d[-which(d$nest=='S804') ,] 
			nrow(d[d$bout_type=="inc",]) 

		# if caputure present, but identificators of the bouts (sex, bird_ID, inc_eff) to NA
			#d[d$capture>0,]$sex=NA
			#d[d$capture>0,]$bird_ID=NA
			#d[d$capture>0,]$bird_ID_filled=NA
			#d[d$capture>0,]$inc_eff=NA
		
		

}


#load(file = "M:\\PROJECTS\\PHD\\STATS\\DATASETS\\C_SESA.Rdata")	
#head(d[d$nest=='S804',])

{# test correctness of the exlusions
z=ddply(d,.(nest),summarise,min= min(bout_start, na.rm= T), max=max(bout_start, na.rm= T))
print(z)
}	
{# test correct sex assignment
				d=d[order(d$pk),]
				d$sex=as.character(d$sex)
				dd=ci
				dd=d[d$bout_type=='inc',]
				dsplit=split(dd,dd$nest)
				foo=lapply(dsplit,function(x) {
										#x=dsplit$"S704"
											
													
											
											#x$sex_check= c(x$sex[-1], x$sex[nrow(x)])  # last sex is repeated
											x$sex_check=c(x$sex[-1], NA) 		
											 										
											return(x)
											}
												)
												
			k=do.call(rbind, foo)
			print(k[which(k$sex==k$sex_check),])
			#dd[dd$nest=='S707',]
			#dd[dd$nest=='S717',]
	}
{# test correct ends of bouts assignment
				
				d=d[order(d$pk),]
				dsplit=split(d,d$nest)
				foo=lapply(dsplit,function(x) {
										#x=dsplit$"S704"
											
													
											
											#x$sex_check= c(x$sex[-1], x$sex[nrow(x)])  # last sex is repeated
											x$start_check=c(x$bout_start[-1], NA) 
											x$end_check=c(x$bout_end[-1], NA) 		
											 										
											return(x)
											}
												)
												
			k=do.call(rbind, foo)
			nrow(k[which(k$bout_start>=k$start_check),])
			print(k[which(k$bout_start>=k$start_check),])
			nrow(k[which(k$bout_end>=k$end_check),])
			print(k[which(k$bout_end>=k$end_check),])
			#k[k$nest=='S405',]
			
	}
{# test whether gaps are before actual bout
d=d[order(d$pk),]
				dsplit=split(d,d$nest)
				foo=lapply(dsplit,function(x) {
										#x=dsplit$"S803"
											
													
											
											#x$sex_check= c(x$sex[-1], x$sex[nrow(x)])  # last sex is repeated
											x$after_bout=c(x$bout_type[-1], NA) 
											x$after_sex=c(x$sex[-1], NA) 		
											x$before_sex=c(NA, x$sex[-nrow(x)]) 	
									
											return(x)
											}
												)
												
			k=do.call(rbind, foo)
			nrow(k[which(k$bout_type=='gap' & k$sex==k$before_sex),])
			print(k[which(k$bout_type=='gap' & k$sex==k$before_sex),])
			
}			
	
	
{# SAVE LOCALY b=full unlimited dataset, d=limited
save(b, d, file = "C:\\Users\\mbulla\\Documents\\C_SESA.Rdata")
save(b, d, file = "M:\\PROJECTS\\PHD\\STATS\\DATASETS\\C_SESA.Rdata")	
	}
{# hatching assigned to C
load(file = "M:\\PROJECTS\\PHD\\STATS\\DATASETS\\C_SESA.Rdata")	
load("M:\\PROJECTS\\PHD\\STATS\\DATASETS\\inc_start_estimation_2012.Rdata")	
	d=d[,names(d)!='inc_start']
	b=b[,names(b)!='inc_start']
	nrow(d)
	
	e$inc_start=e$inc_est
	ee=e[,c('nest','inc_start')]
	nrow(ee)
	
	d=merge(d,ee)
	b=merge(b,ee)
	d$inc_start= as.POSIXct(d$inc_start, tz = "America/Anchorage")
	b$inc_start= as.POSIXct(b$inc_start, tz = "America/Anchorage")
	# additional time var
				
			# start of bout within incubation
				d$bout_start_j = as.numeric(format(d$bout_start ,"%j")) - as.numeric(format(d$inc_start,"%j"))+1
				b$bout_start_j = as.numeric(format(b$bout_start ,"%j")) - as.numeric(format(b$inc_start,"%j"))+1
			# start of bout within season	
				d$b_st_season_j = as.numeric(format(d$bout_start,"%j")) - min(as.numeric(format(d$inc_start,"%j"))) +1
				b$b_st_season_j = as.numeric(format(b$bout_start,"%j")) - min(as.numeric(format(b$inc_start,"%j"))) +1
			# start of incubation within season
				d$inc_start_j = as.numeric(format(d$inc_start,"%j")) - min(as.numeric(format(d$inc_start,"%j")))+1
				b$inc_start_j = as.numeric(format(b$inc_start,"%j")) - min(as.numeric(format(b$inc_start,"%j")))+1

				d[d$inc_start_j<1,]
				
save(d,b, file = "M:\\PROJECTS\\PHD\\STATS\\DATASETS\\C_SESA_inc_start.Rdata")					
	}	 

	
{# test correct bout generation from limited
	{# make dataset
	wet(con, "DROP TABLE if EXISTS MB_test_lim_c")
	
	wet(con,	"CREATE TABLE _SANDBIN.MB_test_lim_c (
							`pk` INT(11) NOT NULL AUTO_INCREMENT,
							`nest` VARCHAR(5) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`datetime_` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							`bird_ID` INT(10) NULL DEFAULT NULL COMMENT 'ring number',
							`sex` VARCHAR(1) NULL DEFAULT NULL COMMENT 'm = male, f = female' COLLATE 'latin1_bin',
							`bout_ID` INT(11) NULL DEFAULT NULL COMMENT 'ID of all incubation and all nonincubation spells > 3h',
							`treat` VARCHAR(5) NULL DEFAULT NULL COMMENT '',
							`bout_type` VARCHAR(16) NULL DEFAULT NULL COMMENT 'inc or gap' COLLATE 'latin1_bin',
									PRIMARY KEY (`pk`),
									INDEX `nest` (`nest`),
									INDEX `datetime_` (`datetime_`),
									INDEX `bout_ID` (`bout_ID`),
									INDEX `bird_ID` (`bird_ID`),
									INDEX `sex` (`sex`)

						)"
			           )
	#load("C:\\Users\\mbulla\\Documents\\C_SESA.Rdata")
	load(file = "M:\\PROJECTS\\PHD\\STATS\\DATASETS\\C_SESA_inc_start.Rdata")	

		d=d[which(d$bout_type=='inc'),]
		d=d[order(d$pk),]
		
		#z=ddply(d,.(nest),summarise, inc=min(inc_start_j, na.rm= T), min= min(bout_start_j, na.rm= T), max=max(bout_start_j, na.rm= T))
		#z=z[order(z$inc),]
		#print(z)
		
		b=d
		
		bsplit=split(b,b$nest)
			foo=lapply(bsplit,function(x) {
										#x=bsplit$"S704"
										tst=unique(x$sex[x$bird_tr=='y'])
										x$n=1
										x$n=cumsum(x$n)
										
										if(unique(x$nest)=='S201') { # match with S504
													
													b=3
													s='m'
													
												x$treat=ifelse(x$bout_start<(x$inc_start+b*24*60*60) & x$sex==s, 'b',													
												 ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA)))
													
										
										}else {
												
										if(unique(x$nest)=='S402') { # match with S503
													
													n=5
													b=8
													
													x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex=='f' & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex=='f' & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex=='f' & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex=='f' & x$n>=(x$n[x$sex=='f' & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													
													NA))))										
										}else {
												
										if(unique(x$nest)=='S413') { # match with S312
													n=5
													b=10
													s='m'
												
												x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex==s & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))
														
													
									
													 
										}else {
												
										
										if(unique(x$nest)=='S405') { # match with S711
													n=10
													b=13
													
													
													x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex=='f' & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex=='f' & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex=='f' & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex=='f' & x$n>=(x$n[x$sex=='f' & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))
										
										
										}else {
												
										if(unique(x$nest)=='S704') { # match with S807
													n=12
													b=15
													s='m'
													
												x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex==s & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))
													
										}else {
										
										if(unique(x$nest)=='S810') { # match with S807
													n=9
													b=13
													s='m'
													
												x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex==s & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))
													
										}else {
										
										if(unique(x$nest)=='S1001') { # match with S807
													n=7
													b=12
													s='m'
													
												x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex==s & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))
													
										}else {
										
										if(unique(x$nest)=='S1003') { # match with S807
													n=7
													b=11
													s='m'
													
												x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex==s & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))			
										}else {
										
												
											if (tst=='f') {
											
													n=3
													b=7
													s='f'
													
												x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex==s & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))
														}
												

												
											if (tst=='m') {
												
													n=3
													b=7
													s='m'
													
												x$treat=ifelse(x$bout_start<(x$inc_start+n*24*60*60), 'n', # 3 days
													ifelse(x$bout_start>=(x$inc_start+n*24*60*60) & x$sex==s & x$bout_start<x$inc_start+b*24*60*60, 'b',													
													ifelse(x$sex==s & x$bout_start>=x$inc_start+b*24*60*60 & x$n<(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7)  , 'tt', 
													ifelse(x$sex==s & x$n>=(x$n[x$sex==s & x$bout_start>=(x$inc_start+b*24*60*60)][1] +7), 'a', 
													NA))))
													}
													
													
												}}}}}}}}
													
										return(x)
											}
											)
			
			b=do.call(rbind, foo)		
	
	b=b[order(b$pk),]
	bsplit=split(b,d$nest)
			foo=lapply(bsplit,function(x) {
										#x=dsplit$"S507"
											#x$t_a=c(x$treat[-1], NA) 	
											x$t_p=c(x$treat[1],x$treat[-nrow(x)])
											
											x$treat=ifelse(!is.na(x$treat), x$treat,
													ifelse(x$t_p=='n', 'n',
													ifelse(x$t_p=='b', 'b',
													ifelse(x$t_p=='tt', 'tn',
													ifelse(x$t_p=='a', 'a', x$treat)))))
													
											return(x)
											}
											)
												
		d=do.call(rbind, foo)

		d=d[, names(d) != "t_p"] 
	
	
	d=d[order(d$nest,d$bout_ID),]
	#n=c('S209', 'S503','S513') 
	#b=d[d$nest%in%n,]
	#main=c('Day-night','Running','Mixed')
	#d$nest_bout=paste(d$nest, d$bout_ID, sep = "-", collapse = NULL)
	nest_bout=unique(d$nest_bout)
	for( i in 1:length(nest_bout)) {
							
							xi = d[d$nest_bout==nest_bout[i], ]
								dd  = data.frame(pk=NA, nest = xi$nest, bird_ID=xi$bird_ID_filled, sex=xi$sex, bout_ID=xi$bout_ID, bout_type=xi$bout_type, treat=xi$treat, datetime_ = seq(from = as.POSIXct(xi$bout_start), 
									to = as.POSIXct(xi$bout_end), by = "5 secs")    )
							
							
							dbWriteTable(con, "MB_test_lim_c", dd ,append = TRUE, row.names = FALSE)
			
							print(nest_bout[i])
								}
					
		}
	{# plot	
	setwd("C:\\Users\\mbulla\\Documents\\C")
	nests = wet(con, "SELECT distinct nest FROM MB_test_lim_c ")$nest #where nest not in ('B301', 'B501', 'S201')
	
	scales = list(x = list(at = 0:24, 
						labels = rep(format(seq.POSIXt(trunc(Sys.time(), 
						"day"), trunc(Sys.time(), "day") + 23 * 3600, 
						"hours"), "%H:%M"), len = 49), rot = 90, cex = 0.7, 
						limits = c(0,24)), y = list(limits=c(0,15), draw=FALSE))
	
	for  (i in 1:length(nests)) {
		
		bb=wet(con, paste("SELECT*FROM _SANDBIN.MB_test_lim_c where nest ='", nests[i],"'",sep="" ) )
		
		bb=bb[order(bb$bout_ID),]
		bb$sex_col=ifelse(bb$bout_type=='inc'& bb$sex == "m", "deepskyblue",
					ifelse(bb$bout_type=='inc'& bb$sex == "f", "red", NA))	
		 
		bb$act[bb$bout_type=='inc']=10
			bb$exchange_gap=ifelse(bb$bout_type=='gap', 12, NA)
			bb$male=ifelse(bb$sex == 'm', 10, NA)
			bb$female=ifelse(bb$sex == 'f', 10, NA)
			
			bb$treat=ifelse(bb$treat=='b' , 1,ifelse(bb$treat=='tt', 3,ifelse(bb$treat=='tn',5,  ifelse(bb$treat=='a',7,NA))))
			
			
			bb$datetime_= as.POSIXct(strptime(bb$datetime_, format="%Y-%m-%d %H:%M:%S"))	
			bb$day=as.Date(trunc(bb$datetime_, "day"))
			bb$time = as.numeric(difftime(bb$datetime_,trunc(bb$datetime_,"day"),   #DO NOT USE rfid_T$day here!! (the time will be wrong)
								units = "hours"))
			
			sl=unique(bb$day) 
					
				 
			strip.left = function(which.panel, ...) {
						LAB = format(sl[which.panel], "%b-%d")
						grid.rect(gp = gpar(fill = "grey"))
						ltext(0.5, 0.5, cex = 0.8, LAB)
									}  
				
			panel = function(...) {
				   panel.abline(v=c(1:23),col="light grey")
				   panel.xyplot(bb$time[bb$day==sl[panel.number()]],
								bb$act[bb$day==sl[panel.number()]],col=bb$sex_col[bb$day==sl[panel.number()]],type="h")
				   panel.xyplot(...)
					}
					
			maintitle=paste(nests[i])
			
			plot=
			xyplot(exchange_gap+male+female+treat~time|day,data=bb, 
						col=c("orange", "deepskyblue", "red","yellow"), type=c("p"),
						cex=c(0.15), pch=19,
						xlab= list(label='Time [h]',cex=1.1),
						ylab= list(label='Date',cex=1.1),
						strip.left=strip.left,
	                    as.table=TRUE,panel=panel, main=maintitle,
						layout = c(1, ifelse(length(sl)>30,30,length(sl))),
						strip=FALSE,distribute.type=TRUE, scales=scales,
						key=list(text= list(c("female","male" ,"exhchange gap","treatment")),
                        points=list(pch=19,col=c("red","deepskyblue","orange", "yellow"))),
						lattice.options = list(layout.widths = list(strip.left = list(x = 3))))  
						
			
			
			png(paste(maintitle, "png", sep = "."), width = 8, height = 11.6929134, units = "in", res = 300)	
			
			print(plot)
			dev.off()
			print(nests[i])
			}
			}					
	}
	


# weather_full - exchange gap included in the bout length
{# MAIN FRAME
	wet(con, "DROP TABLE if EXISTS MB_weather_2012_T_SESA_full")
	
	wet(con,	"CREATE TABLE _SANDBIN.MB_weather_2012_T_SESA_full (
							`pk` INT(11) NOT NULL AUTO_INCREMENT,
							`species` VARCHAR(4) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`nest` VARCHAR(5) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`bird_ID` INT(10) NULL DEFAULT NULL COMMENT 'unique bird_ID of the bout',
							`bird_ID_filled` INT(10) NULL DEFAULT NULL COMMENT 'filled bird_ID where sex_inc_filled is not NULL and bird_ID is known',
							`sex` VARCHAR(3) NULL DEFAULT NULL ,
							
							`bout_ID` FLOAT NULL DEFAULT NULL COMMENT 'ID of incubation bout, if exchange gap than bout_ID-0.5',
							`bout_start_full` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							`bout_length_full` FLOAT NULL DEFAULT NULL COMMENT 'includs exchange gap',
							`exchange_gap` FLOAT NULL DEFAULT NULL COMMENT 'includs exchange gap',
														
							`bout_start` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							`bout_length` FLOAT NULL DEFAULT NULL COMMENT 'without exchange gap',
							`bout_end` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							
							`inc_eff` FLOAT NULL DEFAULT NULL COMMENT 'mean(b$incubation, na.rm= T)', 
							`disturb` FLOAT NULL DEFAULT NULL COMMENT 'sum(1/log(b[which(b$dist<210),]$dist), na.rm= T)', 
							`disturb_log` FLOAT NULL DEFAULT NULL COMMENT 'sum(1/log(b[which(b$dist<210),]$dist), na.rm= T)',
							`capture` INT(1) NULL DEFAULT NULL COMMENT 'number of captures within bout',

							`t_ambient_med` FLOAT NULL DEFAULT NULL COMMENT 'based on HOBO or MSR',
							`t_amb_avg` FLOAT NULL DEFAULT NULL COMMENT 'average t_ambient (from all hobos and MSRs running at that time',
							`t_station_med` FLOAT NULL DEFAULT NULL COMMENT '2 meter ambient temperature in Celsius',
							`t_SD_med` FLOAT NULL DEFAULT NULL COMMENT 'median of running SD of avg_t_ambient with 60min window = SD of bout lenght withou upper and lower quantile',
							`wind_sp_med` FLOAT NULL DEFAULT NULL COMMENT '10 meter wind speed in meters per second',
							`w_SD_med` FLOAT NULL DEFAULT NULL COMMENT 'median of running SD of sqrt(wind) with 60min window = SD of bout lenght withou upper and lower quantile',
							`wind_dir_med` FLOAT NULL DEFAULT NULL COMMENT '10 meter wind direction in degrees',
							`precip_sum` FLOAT NULL DEFAULT NULL COMMENT 'sum of precipitation for the bout in mm',
							`light_int_med` FLOAT NULL DEFAULT NULL COMMENT 'watts/square meter',
							`sun_elev_med` FLOAT NULL DEFAULT NULL COMMENT 'e$sun_elevation = solarpos(cbind(-156.651758,  71.319186), e$datetime_ )[,2]',
							`t_amb_resid_med` FLOAT NULL DEFAULT NULL,
							`resid_light_int_med` FLOAT NULL DEFAULT NULL COMMENT 'resid(fm = lm(log(light_int) ~ poly(sun_elev, 2)  , b, na.action = na.exclude))',
							`bout_start_se` FLOAT NULL DEFAULT NULL COMMENT 'e$sun_elevation = solarpos(cbind(-156.651758,  71.319186), e$datetime_ )[,2]',
							`t_nest_med` FLOAT NULL DEFAULT NULL COMMENT 'based on TT or MSR',
							`t_type` VARCHAR(5) NULL DEFAULT NULL COMMENT 'MSR or TT',
							`t_amb_type` VARCHAR(5) NULL DEFAULT NULL COMMENT 'MSR, HOBO, AVG',
									PRIMARY KEY (`pk`),
									INDEX `species` (`species`),
									INDEX `nest` (`nest`),
									INDEX `bout_start` (`bout_start`),
									INDEX `bout_end` (`bout_end`),
									INDEX `bout_length` (`bout_length`),
									INDEX `inc_eff` (`inc_eff`),
									INDEX `disturb` (`disturb`),
									INDEX `bird_ID` (`bird_ID`),
									INDEX `bird_ID_filled` (`bird_ID_filled`),
									INDEX `sex` (`sex`)
										)"
										)	
		}
{# FILL IN		
		nb=wet(con, "SELECT distinct CONCAT_WS('-', nest, bout_ID) as nest_bout 
							FROM _SANDBIN.MB_incubation_2012 where species = 'SESA' and bout_ID is not NULL ")
		
		for( i in 1:nrow(nb)) {
							#i=500
							b=wet(con, paste("SELECT*FROM  _SANDBIN.MB_incubation_2012 
											where CONCAT_WS('-', nest, bout_ID) ='", nb$nest_bout[i],"'",sep="" ) )
							#b=wet(con, paste("SELECT*FROM  _SANDBIN.MB_incubation_2012 
							#				where CONCAT_WS('-', nest, bout_ID) ='S320-3'") )			
	
								b$incubation=as.numeric(b$incubation)
								b$datetime_= as.POSIXct(b$datetime_, tz = "America/Anchorage")
								
								tst=length(b$exchange_gap[!is.na(b$exchange_gap)])
								tst2=length(b$exchange_gap[is.na(b$exchange_gap)])
								
								d=data.frame(nest = NA )	
								d$species=b$species[1]
								d$nest = b$nest[1]
								
								if(all(is.na(b$bird_ID))){d$bird_ID = NA} else 
									 {d$bird_ID = unique(b$bird_ID[!is.na(b$bird_ID)])}
								#d$bird_ID_filled=unique(b$bird_ID_filled[!is.na(b$bird_ID_filled)])
								if(all(is.na(b$bird_ID_filled))){d$bird_ID_filled = NA}else 
									{d$bird_ID_filled = unique(b$bird_ID_filled[!is.na(b$bird_ID_filled)])}
								
								d$sex=b$sex_inc_filled[1]
								
								d$bout_ID= b$bout_ID[1]
								
								d$bout_start_full=b$datetime_[1]
								d$bout_length_full = nrow(b)*1/12 # in minutes
								d$exchange_gap=tst*1/12
								
								if (tst==0) {d$bout_start=d$bout_start_full
												}else d$bout_start=b[is.na(b$exchange_gap),]$datetime_[1]
								
								d$bout_length = d$bout_length_full - d$exchange_gap # in minutes
								
								d$bout_end= b$datetime_[nrow(b)]+5
								
																
								if(any(is.na(b$inc_t))) {d$inc_eff = NA } else d$inc_eff= mean(b$incubation) # was if(any(is.na(b$incubation))) before
															
								d$disturb=sum(1/b$dist[which(b$dist<210)], na.rm= T)	# should it be logged + does it exclude NAs
								d$disturb_log=sum(1/log(b$dist[which(b$dist<210)]), na.rm= T)
								
								d$capture= length(b$capture[!is.na(b$capture)]) # number of catchings
								
								d$t_ambient_med = median(b$t_ambient, na.rm= T)
								d$t_station_med = median(b$t_station, na.rm= T)
								d$t_amb_avg = median(b$t_amb_avg, na.rm= T)
								d$t_SD_med=median(b$t_SD, na.rm=T)
								
								d$wind_sp_med = median(b$wind_sp, na.rm= T)
								d$w_SD_med=median(b$w_SD, na.rm=T)
								d$wind_dir_med = median(b$wind_dir, na.rm= T)
								d$precip_sum = sum(b$precip, na.rm= T) # in mm
								d$light_int_med = median(b$light_int, na.rm= T)
								d$sun_elev_med = median(b$sun_elev, na.rm= T)
								
								
								d$bout_start_se=b$sun_elev[1]
								d$t_type= b$t_nest_type[1]
								d$t_amb_type= b$t_ambient_type[1]
								
								d$t_amb_resid_med = median(b$t_amb_resid, na.rm= T)
								d$resid_light_int_med = median(b$resid_light_int, na.rm=T)
								
								d$t_nest_med = median(b$t_nest, na.rm= T)
							
								dbWriteTable(con, "MB_weather_2012_T_SESA_full", d, append = TRUE, row.names = FALSE)
							
							#print(d[1,])
							#print(unique(b$bird_ID))
							#print(nb$nest_bout[i])
							
							}	
							
	}	
{# ADD VARIABLES
	
		wet(con,"ALTER TABLE _SANDBIN.MB_weather_2012_T_SESA_full  
						ADD COLUMN (
							inc_start DATETIME NULL DEFAULT NULL COMMENT 'datetime'
										)")	
		
		b=wet(con, paste("SELECT nest, incubation_start as inc_start FROM AVESatBARROW.NESTS where year_=2012 and species = 'SESA'"))
		
		b=b[!is.na(b$inc_start),]
		
		b$inc_start= as.POSIXct(strptime(b$inc_start, format="%Y-%m-%d %H:%M:%S"), tz = "America/Anchorage")	
		#b$inc_start_se = solarpos(cbind(-156.651758,  71.319186), b$inc_start )[,2]
		
		
		dbWriteTable(con, "MB_temp_2012_2", b, row.names = FALSE)
		
		wet(con, paste("UPDATE _SANDBIN.MB_weather_2012_T_SESA_full b, MB_temp_2012_2 t
									SET b.inc_start = t.inc_start
											where b.nest=t.nest"))
							# b.inc_start_se = t.inc_start_se				
												
		wet(con, "DROP TABLE MB_temp_2012_2")	
	
	# end_stat, end_state_date, hatching_start
		wet(con,"ALTER TABLE _SANDBIN.MB_weather_2012_T_SESA_full  
							ADD COLUMN (
								nest_endstate VARCHAR(2)  NULL DEFAULT NULL COMMENT 'hd,p,d,ud',
								nest_endstate_date  DATETIME NULL DEFAULT NULL COMMENT 'datetime',
								hatch_start DATETIME  NULL DEFAULT NULL COMMENT 'datetime'
									)")	
			
		wet(con, paste("UPDATE  _SANDBIN.MB_weather_2012_T_SESA_full  b, AVESatBARROW.NESTS n
										SET b.nest_endstate = n.nest_endstate,
											b.nest_endstate_date = n.nest_endstate_date, 
											b.hatch_start = n.hatch_start
												WHERE b.nest = n.nest
													and n.year_=2012"))											
	# body measurements (#make sure that all bouts that should have bird_ID have it)
			wet(con,"ALTER TABLE _SANDBIN.MB_weather_2012_T_SESA_full  
							ADD COLUMN (
								tarsus FLOAT  NULL DEFAULT NULL COMMENT 'mm',
								culmen FLOAT NULL DEFAULT NULL COMMENT 'mm',
								tot_head FLOAT  NULL DEFAULT NULL COMMENT 'mm',
								wing FLOAT NULL DEFAULT NULL COMMENT 'mm',
								weight FLOAT NULL DEFAULT NULL COMMENT 'g',
								fat FLOAT NULL DEFAULT NULL COMMENT 'score',
								`caught` DATETIME NULL DEFAULT NULL,
								radioFr VARCHAR(2) NULL DEFAULT NULL COLLATE 'latin1_bin'
									)")	
			
			
			b=wet(con, "SELECT caught_date_time, nest, ID, recapture, carries_egg, culmen, totalHead, tarsus, wing, 
												weight, fat, ectoparasites, 
												author, radioFr, comments  FROM AVESatBARROW.CAPTURES
													WHERE LEFT(comments,5) not in ('remea') and year_=2012 and culmen is not null 
														OR comments is null and year_=2012 and culmen is not null 
															ORDER BY ID")
				
			#which(duplicated(b$ID))	
			#b[which(b$ID==225118856),]
	
			#b=b[-which(b$ID==225118856 & b$author=='BK'),] 	
			b[which(b$ID==225118856),]$weight= 27.0
			b[which(b$ID==225118856),]$caught_date_time= "2012-06-07 11:55:23"
			#b[which(b$ID==229194738),]$weight= 25.9
			
			#b[which(b$ID==255174303),]
			b=b[-which(b$ID==255174303 & b$author=='BK'),] 	
			#b[which(b$ID==255174306),]
			b=b[-which(b$ID==255174306 & b$author=='BK'),] 	
			
			#b[which(b$carries_egg==1),]
			b=b[-which(b$carries_egg==1),]
						
			b$radioFr=ifelse(is.na(b$radioFr), 'n', 'y')
			
			dbWriteTable(con, "MB_temp_2012_2", b, row.names = FALSE)
			wet(con, paste("UPDATE _SANDBIN.MB_weather_2012_T_SESA_full b, MB_temp_2012_2 t
									SET b.tarsus = t.tarsus,
										b.culmen = t.culmen,
										b.tot_head = t.totalHead,
										b.wing = t.wing,
										b.weight = t.weight,
										b.fat = t.fat,
										b.radioFr=t.radioFr,
										b.caught=t.caught_date_time
											where b.bird_ID_filled=t.ID")
												)
			
			wet(con, "DROP TABLE MB_temp_2012_2")
	

}
{# CREAT RData FILE: ADD VAR + LIMIT THE DATASET TO GOOD DATA
		b=wet(con, paste("SELECT*FROM _SANDBIN.MB_weather_2012_T_SESA_full")) 
		
			b$inc_start= as.POSIXct(b$inc_start, tz = "America/Anchorage")
			b$bout_end= as.POSIXct(b$bout_end, tz = "America/Anchorage")
			b$bout_start_full= as.POSIXct(b$bout_start_full, tz = "America/Anchorage")
			b$bout_start= as.POSIXct(b$bout_start, tz = "America/Anchorage")
			b$hatch_start= as.POSIXct(b$hatch_start, tz = "America/Anchorage")
			b$nest_endstate_date= as.POSIXct(b$nest_endstate_date, tz = "America/Anchorage")
		
		# exlude last bout if gap
		bsplit=split(b,b$nest)
			foo=lapply(bsplit,function(x) {
										#x=dsplit$"S320"
											
											if (x$bout_length_full[nrow(x)]==x$exchange_gap[nrow(x)]) {
											x=x[!x$bout_ID==x$bout_ID[nrow(x)],]
											 } else x=x
										
										return(x)
											}
											)
												
			b=do.call(rbind, foo)
		
		# additional time var
			# start of bout within incubation
				b$bout_start_j_full  = as.numeric(format(b$bout_start_full ,"%j")) - as.numeric(format(b$inc_start,"%j"))
				b$bout_start_j = as.numeric(format(b$bout_start ,"%j")) - as.numeric(format(b$inc_start,"%j"))
			
			
			# start of bout within season
				b$b_st_season_j = as.numeric(format(b$bout_start_full,"%j")) - min(as.numeric(format(b$inc_start,"%j")))
											
			# start of incubation within season
				b$inc_start_j = as.numeric(format(b$inc_start,"%j")) - min(as.numeric(format(b$inc_start,"%j")))
				
			# other
				#b$time = as.numeric(difftime(b$bout_start,trunc(b$bout_start,"day"), units = "hours"))
				#b$sun_12=factor(ifelse(b$time>0 & b$time<=12, 1, 2))
			
			# time to radians
					#b$time = as.numeric(difftime(b$bout_start,trunc(b$bout_start,"day"), units = "mins"))
					#b$rad= (2*pi*b$time) / (24*60)
				
				b$nest_bout=paste(b$nest, b$bout_ID, sep = "-", collapse = NULL)
				b$bird_ID_fake= ifelse(is.na(b$sex), NA, paste(b$nest, b$sex, sep = "-", collapse = NULL))
			
						
		# exclude last bouts (not relevant due to predation, hatching or our interaption)
				#bsplit = split(b, factor(b$nest))
				#		foo=lapply(bsplit,function(x) {
				#							#x=bsplit$"S302"
				#							d=x[1:nrow(x)-1,]
				#									})
				#d=do.call(rbind, foo)
		# exclude bouts where endstate occured		
		d=b
		d=d[-which(d$bout_end>=(d$nest_endstate_date-1800)), ] # reduce it by 1/2 hours to exclude more data prior to detected hatching start
	
		# exclude all bouts older than hatch_start-12hours
			d=d[-which((d$hatch_start-21600)<d$bout_end), ] # reduce it by 6 hours to exclude more data prior to detected hatching start
			
		nrow(d) 	
		
		# exclude first 3 bouts of the data
			d=d[-which(d$bout_ID < 1),] 
			d=d[-which(d$bout_ID < 2 & !d$nest==c('S703')),] 
			
				nrow(d)	
		# exlude last bout if gap
			dsplit=split(d,d$nest)
				foo=lapply(dsplit,function(x) {
										#x=dsplit$"S320"
											
											if (x$bout_length_full[nrow(x)]==x$exchange_gap[nrow(x)]) {
											x=x[!x$bout_ID==x$bout_ID[nrow(x)],]
											 } else x=x
										
										return(x)
											}
											)
												
			d=do.call(rbind, foo)
		  nrow(d)	
		# exclusions based on visual check
			d=d[-which(d$nest=='S101' & d$bout_start<as.POSIXct("2012-06-08 22:10:05",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S201'),]
			d=d[-which(d$nest=='S204' & d$bout_start<as.POSIXct("2012-06-08 15:31:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S209' & d$bout_start<as.POSIXct("2012-06-26 07:10:45",tz = "America/Anchorage")),]
			#d=d[-which(d$nest=='S302' & d$bout_start<as.POSIXct("2012-06-09 00:53:45",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S302' & d$bout_start<as.POSIXct("2012-06-09 00:3:45",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S303' & d$bout_start<as.POSIXct("2012-06-13 02:09:10",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S304' & d$bout_start<as.POSIXct("2012-06-11 07:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S306' & d$bout_start>as.POSIXct("2012-06-13 11:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S307' & d$bout_start<as.POSIXct("2012-06-13 12:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S308' & d$bout_start<as.POSIXct("2012-06-13 12:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S309' & d$bout_start>as.POSIXct("2012-06-26 10:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S312' & d$bout_start<as.POSIXct("2012-06-19 09:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S317'),]
			#d=d[-which(d$nest=='S320' & d$bout_start >as.POSIXct('2012-07-02 18:50:10',tz = "America/Anchorage")),] #& d$datetime_ <as.POSIXct('2012-07-05 18:38:05') & d$datetime_ >as.POSIXct('2012-07-07 11:38:00')),]
			#d=d[-which(d$nest=='S322' & d$bout_start>as.POSIXct("2012-06-27 04:00:00",tz = "America/Anchorage")),]
			#d=d[-which(d$nest=='S323' & d$bout_start>as.POSIXct("2012-06-28 12:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S401' & d$bout_start<as.POSIXct("2012-06-26 03:28:30",tz = "America/Anchorage")),] 
			#d=d[-which(d$nest=='S401' & d$bout_start>as.POSIXct("2012-06-29 03:00:00",tz = "America/Anchorage")),] 
			#d=d[-which(d$nest=='S405' & d$bout_ID<=0),]
			d=d[-which(d$nest =='S501'),]
			d=d[-which(d$nest=='S502'  & d$bout_start <as.POSIXct("2012-06-08 01:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S503'  & d$bout_start <as.POSIXct("2012-06-07 13:16:20",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S504'  & d$bout_start <as.POSIXct("2012-06-08 00:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S505'  & d$bout_start <as.POSIXct("2012-06-09 04:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S505'  & d$bout_start >as.POSIXct("2012-06-21 00:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S507'  & d$bout_start <as.POSIXct("2012-06-14 05:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S510'  & d$bout_start <as.POSIXct("2012-06-17 05:00:00",tz = "America/Anchorage")),]
			#d=d[-which(d$nest=='S510'  & d$bout_start >as.POSIXct("2012-06-23 12:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S513' & d$bout_start <as.POSIXct("2012-06-22 02:00:00",tz = "America/Anchorage")),]
			#d=d[-which(d$nest=='S513' & d$bout_start >as.POSIXct("2012-07-10 02:00:00",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S516' & d$bout_start <as.POSIXct("2012-06-27 10:33:45",tz = "America/Anchorage")),]
			d=d[-which(d$nest=='S518' & d$bout_start<as.POSIXct("2012-06-27 01:00:00",tz = "America/Anchorage")),] 
			#d=d[-which(d$nest =='S521'),]
			d=d[-which(d$nest =='S602'),]
			d=d[-which(d$nest=='S701' & d$bout_start<as.POSIXct("2012-06-10 17:00:00",tz = "America/Anchorage")),] 
			#d=d[-which(d$nest=='S701' & d$bout_start>as.POSIXct("2012-06-17 04:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S704' & d$bout_start<as.POSIXct("2012-06-12 09:00:00",tz = "America/Anchorage")),] 
			#d=d[-which(d$nest=='S706' & d$bout_start>as.POSIXct("2012-06-22 12:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S707' & d$bout_start<as.POSIXct("2012-06-14 05:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S708' & d$bout_start<as.POSIXct("2012-06-14 23:00:00",tz = "America/Anchorage")),] 
			d=d[-which(d$nest=='S709' & d$bout_start<as.POSIXct("2012-06-15 20:00:00",tz = "America/Anchorage")),] 	
			d=d[-which(d$nest=='S802' & d$bout_start<as.POSIXct("2012-06-21 04:00:00",tz = "America/Anchorage")),] 	
			#d=d[-which(d$nest =='S701'),]
			#d=d[-which(d$nest =='S707' & d$bout_ID_2 < 7),]
			#d=d[-which(d$nest =='S708' & d$bout_ID_2 < 5),]
			#d=d[-which(d$nest =='S709' & d$bout_ID_2 < 4),]
			
			nrow(d) 

		# if caputure present, but identificators of the bouts (sex, bird_ID, inc_eff) to NA
			#d[d$capture>0,]$sex=NA
			#d[d$capture>0,]$bird_ID=NA
			#d[d$capture>0,]$bird_ID_filled=NA
			#d[d$capture>0,]$inc_eff=NA
		
		

}
{# test correctness of the exlusions
z=ddply(d,.(nest),summarise,min= min(bout_start, na.rm= T), max=max(bout_start, na.rm= T))
print(z)
m=d[which(d$t_type =='MSR'),]
#e=m[-which(is.na(m$inc_eff)),]
nrow(m)
w=ddply(e,.(nest),summarise,min= min(bout_start, na.rm= T), max=max(bout_start, na.rm= T))
print(w)
}	
{# test correct sex assignment
				d$sex=as.character(d$sex)
				dd=d
				dsplit=split(dd,dd$nest)
				foo=lapply(dsplit,function(x) {
										#x=dsplit$"S704"
											
													
											
											#x$sex_check= c(x$sex[-1], x$sex[nrow(x)])  # last sex is repeated
											x$sex_check=c(x$sex[-1], NA) 		
											 										
											return(x)
											}
												)
												
			k=do.call(rbind, foo)
			print(k[which(k$sex==k$sex_check),])
			k=k[!is.na(k$sex_check),]
	}
{# SAVE LOCALY b=full unlimited dataset, d=limited
setwd("M:\\PROJECTS\\PHD\\STATS\\DATASETS")
#setwd("C:\\users\\annerutten\\My Documents\\DATASETS")
save(b, d, file = "weather_SESA_full.Rdata")	
	}

	
{# test correct bout generation by PLOT
	{# make dataset
	wet(con, "DROP TABLE if EXISTS MB_test_T")
	
	wet(con,	"CREATE TABLE _SANDBIN.MB_test_T (
							`pk` INT(11) NOT NULL AUTO_INCREMENT,
							`nest` VARCHAR(5) NULL DEFAULT NULL COLLATE 'latin1_bin',
							`datetime_` DATETIME NULL DEFAULT NULL COMMENT 'rounded to 5s',
							`bird_ID` INT(10) NULL DEFAULT NULL COMMENT 'ring number',
							`sex` VARCHAR(1) NULL DEFAULT NULL COMMENT 'm = male, f = female' COLLATE 'latin1_bin',
							`bout_ID` INT(11) NULL DEFAULT NULL COMMENT 'ID of all incubation and all nonincubation spells > 3h',
							`bout_type` VARCHAR(16) NULL DEFAULT NULL COMMENT 'inc or gap' COLLATE 'latin1_bin',
									PRIMARY KEY (`pk`),
									INDEX `nest` (`nest`),
									INDEX `datetime_` (`datetime_`),
									INDEX `bout_ID` (`bout_ID`),
									INDEX `bird_ID` (`bird_ID`),
									INDEX `sex` (`sex`)

						)"
						)
	
	nest_bout=unique(b$nest_bout)
	for( i in 1:length(nest_bout)) {
	
							xi = b[b$nest_bout==nest_bout[i], ]
								dd  = data.frame(pk=NA, nest = xi$nest, bird_ID=xi$bird_ID_filled, sex=xi$sex, bout_ID=xi$bout_ID, bout_type=xi$bout_type, datetime_ = seq(from = as.POSIXct(xi$bout_start), 
									to = as.POSIXct(xi$bout_end), by = "5 secs")    )
							
							
							dbWriteTable(con, "MB_test_T", dd ,append = TRUE, row.names = FALSE)
			
							#print(nest_bout[i])
								}
					
		}
	{# plot	
	setwd("C:\\Users\\mbulla\\Documents\\C")
	nests = wet(con, "SELECT distinct nest FROM MB_test_T ")$nest #where nest not in ('B301', 'B501', 'S201')
	
	scales = list(x = list(at = 0:24, 
						labels = rep(format(seq.POSIXt(trunc(Sys.time(), 
						"day"), trunc(Sys.time(), "day") + 23 * 3600, 
						"hours"), "%H:%M"), len = 49), rot = 90, cex = 0.7, 
						limits = c(0,24)), y = list(limits=c(0,15), draw=FALSE))
	
	for  (i in 1:length(nests)) {
		bb=wet(con, paste("SELECT*FROM _SANDBIN.MB_test_T where nest ='", nests[i],"'",sep="" ) )
		
		bb$sex_col=ifelse(bb$bout_type=='inc'& bb$sex == "m", "deepskyblue",
					ifelse(bb$bout_type=='inc'& bb$sex == "f", "red", NA))	
		 
		bb$act[bb$bout_type=='inc']=10
		
			bb$exchange_gap=ifelse(bb$bout_type=='gap', 12, NA)
			bb$male=ifelse(bb$sex == 'm', 10, NA)
			bb$female=ifelse(bb$sex == 'f', 10, NA)
			
			bb$datetime_= as.POSIXct(strptime(bb$datetime_, format="%Y-%m-%d %H:%M:%S"))	
			bb$day=as.Date(trunc(bb$datetime_, "day"))
			bb$time = as.numeric(difftime(bb$datetime_,trunc(bb$datetime_,"day"),   #DO NOT USE rfid_T$day here!! (the time will be wrong)
								units = "hours"))
			
			sl=unique(bb$day) 
					
				 
			strip.left = function(which.panel, ...) {
						LAB = format(sl[which.panel], "%b-%d")
						grid.rect(gp = gpar(fill = "grey"))
						ltext(0.5, 0.5, cex = 0.8, LAB)
									}  
				
			panel = function(...) {
				   panel.abline(v=c(1:23),col="light grey")
				   panel.xyplot(bb$time[bb$day==sl[panel.number()]],
								bb$act[bb$day==sl[panel.number()]],col=bb$sex_col[bb$day==sl[panel.number()]],type="h")
				   panel.xyplot(...)
					}
					
			maintitle=paste(nests[i])
			
			plot=
			xyplot(exchange_gap+male+female~time|day,data=bb, 
						col=c("orange", "deepskyblue", "red"), type=c("p"),
						cex=c(0.15), pch=19,
						strip.left=strip.left,
	                    as.table=TRUE,panel=panel, main=maintitle,
						layout = c(1, ifelse(length(sl)>30,30,length(sl))),
						strip=FALSE,distribute.type=TRUE, scales=scales,
						key=list(text= list(c("female","male" ,"exhchange gap")),
                        points=list(pch=19,col=c("red","deepskyblue","orange"))),
						lattice.options = list(layout.widths = list(strip.left = list(x = 3))))  
						
			png(paste(maintitle, "png", sep = "."), width = 8, height = 11.6929134, units = "in", res = 300)	
			
			print(plot)
			dev.off()
			print(nests[i])
			}
			}					
	}	
	

	
