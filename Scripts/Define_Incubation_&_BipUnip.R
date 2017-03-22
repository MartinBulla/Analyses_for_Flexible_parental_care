{# TOOLS
	{# define time 
	  Sys.setenv(TZ="UTC")	
	}
	
	{# load packages
	   sapply(c('plyr','magrittr','maptools','raster', 'rgeos', 'rgdal', 'RSQLite','zoo'),
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
	}

	{# establish database connections
	   dbq=dbGetQuery
	  
	  # name of sqlite database
	    db=paste(wd2,"bip_to_unip.sqlite",sep="")
	    db2=paste(wd2,"bip_to_unip_metadata.sqlite",sep="")
	}
	
	{# constants
		# cut off value for incubation non/incubation from Lesku et al 2012
			tr_=0.01085508#+0.009 
		
		# save the generated data to database 
			db_save=TRUE # or false		
		
		# save the generated data to txt file 
			txt_save=TRUE # or false
	}
}

{# LOAD metadata
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
  
{# DEFINE for each nest incubation/non_incubation and biparental/uniparental periods used in the analyes  
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
				
				# load data from database
					conLite = dbConnect(dbDriver("SQLite"),dbname = db)
					a=dbq(conLite,paste("SELECT*FROM", act_ID))
					dbDisconnect(conLite)
				
				# different incubation extraction for temperature and signal based data
					if(nests_$sp[i]=='pesa'){ 
						a$inc=ifelse(a$cv>tr_,0,1) 
					}else{
						{# extratction of incubation/non-incubation based on nest and surface temperature
							a$pk=1:nrow(a)
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
						}
				
				
				# define uniparental and biparental periods, no = not used
				  if(nests_$sp[i]=='rnph'){ 
				  # for phalarope
					tst=ifelse(nests_$state[nests_$nest==a$nest[1]]=='s',6, ifelse(nests_$state[nests_$nest==a$nest[1]]%in%c('h','l'),24,0)) # if start of hatching then end -6h, if hatching or left nest that -24h, else 0
					a$type=ifelse(a$datetime_>=a$datetime_[1]+60*60*2 & a$datetime_<nests_$end[nests_$nest==a$nest[1]]-tst*60*60 ,'uni','no') # start of data defined 2h after equipment was installed on the nest   
				  }else if(nests_$sp[i]=='pesa'){
				   # for pectoral sandpiper
				   a$type=ifelse(a$datetime_>=start_ & a$datetime_<end_,'uni', 'no') 
				  }else{
				  # for biparental species
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
					a$type=ifelse(a$datetime_<inc_start,'no',a$type) # exclude laying
					
					# parts or whole data for given nest not used because of equipment issues or incubation of only one egg
					if(nest%in%c('s815','s704')){a$type='no'}
					if(nest=='s804'){a$type=ifelse(a$datetime_<as.POSIXct('2012-06-22 20:25:00',tz='UTC') | a$datetime_>as.POSIXct('2012-07-03 09:25:05',tz='UTC'), a$type, 'no')}
					if(nest=='w504'){a$type=ifelse(a$datetime_>as.POSIXct('2013-07-09 10:08:05',tz='UTC'), a$type, 'no')}
				}
				
				# save
					aa=a[,c('act_ID','sys','sp','site','year','t_method','nest','bird_ID','tag','who','datetime_',"disturb",'t_surface','t_nest',"cv",'inc','type' )]
					
					# to database				
						if(db_save==TRUE){
							conLite = dbConnect(dbDriver("SQLite"),dbname = db)
							dbq(conLite,paste( "DROP TABLE IF EXISTS",act_ID))
							dbWriteTable(conn=conLite, name = act_ID, value = aa,row.names = FALSE)
							dbDisconnect(conLite)
							}
					# to txt
						if(txt_save==TRUE){ write.table(aa, paste(wd3,nest,".txt",sep=""), sep=",", row.names=FALSE)}
				
				print(paste(nest,act_ID, i,sep=" "))
				}
				
}

{# sessionInfo()
#R version 3.3.0 (2016-05-03)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 7 x64 (build 7601) Service Pack 1

#locale:
#[1] LC_COLLATE=English_United States.1252 
#[2] LC_CTYPE=English_United States.1252   
#[3] LC_MONETARY=English_United States.1252
#[4] LC_NUMERIC=C                          
#[5] LC_TIME=English_United States.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
# [1] zoo_1.7-13      RSQLite_1.0.0   DBI_0.4-1       rgdal_1.1-10   
# [5] rgeos_0.3-19    raster_2.5-8    maptools_0.8-39 sp_1.2-3       
# [9] magrittr_1.5    plyr_1.8.3     

#loaded via a namespace (and not attached):
#[1] foreign_0.8-66  Rcpp_0.12.5     grid_3.3.0      lattice_0.20-33

}				
				