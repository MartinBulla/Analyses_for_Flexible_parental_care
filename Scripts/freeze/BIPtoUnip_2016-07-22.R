{# INFO
 # data are in local time
 # data used in the analyses are from 2 hours after the equipment was placed on the nest, and 6 hours before the hatching start or 12 hours before the nest hatched
 # NESTS
	#BARRROW
		#nests=c('a301','a302','b301','b501','s602') # deserted 2011
		#nests=c('a303','a305','a403','a802','b303','b304','b402','b403','b404','b405','b601','d702','l301','l302','l401','s1003','s111','s201','s304','s305','s310','s313','s318','s325','s419','s420','s422','s502','s506','s509','s511','s609','s610','s704','s705','s706','s708','s711','s712','s716','s717','s720','s777','s804','s807','s812','s815','s901','s902') # deserted 2012 (s712 right during hatching, it may even be no desertion)
		#nests=c('l501','l502','l801','l805','s409','s510','s516','s520','s623','s627','w504','w505','w507','w508','w509','w511','w701','w702','w801','w802','w804') # deserted 2013 ('s707' not used as one parent keeps showing up
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
}
{# TOOLS
	# define working and output directory
		wd="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Data/"	
		
	require(XLConnect)
	require(RMySQL)
	con=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
	conE=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='extract_2013')
	wet=dbGetQuery

  # define time and load packages
	Sys.setenv(TZ="UTC")	

}
 # create datafile with on_nest and off_nest
		# load metadata
		m=readWorksheetFromFile(paste(wd,'nests_2016-07-22__.xls', sep=""), sheet='Sheet1',colTypes = c(XLC$DATA_TYPE.NUMERIC,
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
		# 2011
		x =  wet(con, "SELECT lower(a.nest) nest, a.year_, min(datetime_onNest) start, b.species 
							FROM LOGGERSatBARROW.Device_filestatus a
						LEFT JOIN
							(SELECT nest, lower(species) species FROM AVESatBARROW.NESTS where year_ = 2011) b
								on a.nest = b.nest							
								WHERE a.year_= 2011 and a.nest in 
									('a301','a302','b301','b501','s602')
									GROUP BY a.nest ")
		x$stop=m$end[match(paste(x$year_, x$nest,x$species), paste(m$year,m$nest, m$sp))]							
		
		# 2012
			y =  wet(con, "SELECT lower(a.nest) nest, a.year_, min(datetime_onNest) start, b.stop, b.species 
							FROM LOGGERSatBARROW.Device_filestatus a
						LEFT JOIN
							(SELECT nest, nest_endstate_date stop, species FROM AVESatBARROW.NESTS where year_ = 2012) b
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
			
			
			
			x[x$nest=='S602',]$start= '2012-06-10 07:00:00'
	x[x$nest=='S602',]$stop= '2012-06-27 21:00:00'
	x[x$nest=='S507',]$stop= '2012-07-01 21:00:00'							