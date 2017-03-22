{# INFO
# R Script used to generate the dataset used to recreate the statistical outputs from
# Bulla, M. et al. (2017).  Flexible parental care: Uniparental incubation in biparentally incubating shorebirds'. bioRxive. 
# --------------------------------------------------------------------------------------------------------
# Questions can be directed to: Martin Bulla (bulla.mar@gmail.com)
# --------------------------------------------------------------------------------------------------------
# The script is optimezed for use with Notepad++ editor.
# For better readability the subsections of the script can be collapsed  (Alt+0)
# The script runs with 
#	- R version 3.3.0; for details and package versions see the sessionInfo() at the end of the script
#   - Functions_&_constants.R script available from https://osf.io/ug3h5/
#	- bip_to_unip.sqlite databased available from https://osf.io/n52xw/
#   - csv metadata files available from  https://osf.io/t9e2x/
#--------------------------------------------------------------------------------------------------------

#WHEN USING any of the Supporting information, PLEASE CITE both the original paper and the Supporting information
#	Bulla, M. at al. (2017).  Flexible parental care: Uniparental incubation in biparentally incubating shorebirds'. bioRxive. 
#	Bulla, M. (2017).  Supporting information for 'Flexible parental care: Uniparental incubation in biparentally incubating shorebirds'. Open Science Framework,   	
#	http://doi.org/10.17605/OSF.IO/3RSNY. ADD DATETIME.

#For any publication making substantial use of the data, please contact Martin Bulla (bulla.mar@gmail.com), as the authors welcome the opportunity for collaboration and wish to #comment prior to publication.
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

{# sessionInfo
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
#[1] RSQLite_1.0.0   DBI_0.4-1       plyr_1.8.3      lattice_0.20-33

#loaded via a namespace (and not attached):
#[1] Rcpp_0.12.5 grid_3.3.0 
}