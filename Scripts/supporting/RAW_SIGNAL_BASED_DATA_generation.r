{# RAW SIGNAL BASED DATA - check why zeros in the data
{# light logger & UvA pairs - DONE - RUN AGAIN WITH MORE VARIABLES
	{# lig 
	    f=list.files(path="C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/pairs/lig/", 
						pattern='.Rdata',recursive=TRUE,full.names=TRUE)
	    
	   d_=d[d$method=="bas",]
	   length(unique(d_$pk))
	   dd=d_[!is.na(d_$logger),]
	   length(unique(dd$pk))
	   
	   z=ddply(d_,.(sp,breeding_site,year, nest, nn), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 z$pk=paste(z$sp,z$breeding_site,z$year,z$nest,z$nn)
		 
			zz=z[!is.na(z$f) & !is.na(z$m),]
		z1=d_[!(duplicated(paste(d_$tag,d_$bird_ID,d_$sex))),c('tag','bird_ID','sex')]
		z1=z1[which(!is.na(z1$tag)),]
	   
	   # extract renests in one go, as they are all within 10-12 days and hence data would be included anyway
			zzz=zz[paste(zz$nest,zz$year)%in%paste(zz$nest,zz$year)[duplicated(paste(zz$nest,zz$year))],]
			# extract min/max withou renest
			z=ddply(d_,.(sp,breeding_site,year, nest), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 z$pk=paste(z$sp,z$breeding_site,z$year,z$nest)
		 
			zz=z[!is.na(z$f) & !is.na(z$m),]	 
					
	    for(i in 1:nrow(zz)) {
				load(f[grep(paste(zz$year[i],zz$nest[i], sep="_"),toupper(f))])
				#a$datetime_[1]
				aa=a[a$datetime_>as.POSIXct(substring(zz$min_[i]-keeps,1,10)) & a$datetime_<as.POSIXct(substring(zz$max_[i]+keepe,1,10)),]
				
				aa$bird_ID=toupper(z1$bird_ID[match(tolower(aa$logger), tolower(z1$tag))])
				aa$nest=zz$nest[i]
				aa$breeding_site=zz$breeding_site[i]
				aa$who=tolower(z1$sex[match(tolower(aa$logger), tolower(z1$tag))])
				aa$sp=zz$sp[i]
				aa$method=tolower(d_$method[1])
				aa$signal=aa$light
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa=aa[which(!is.na(aa$datetime_)),]
				aa$act_ID=paste('lightBas',ifelse(nchar(i)==1,paste("00",i,sep=""),ifelse(nchar(i)==2,paste("0",i,sep=""),i)),sep="_")
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
				# state not used
				if(i==1){write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), row.names=FALSE,append =FALSE, sep=",")
				}else{write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")}
				print(i)
				print(paste(zz$year[i],zz$nest[i], sep="_"))
				print(paste(aaa$year[1],aaa$nest[1], sep="_"))
				}
		}
	{# lux
	    f=list.files(path="C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/pairs/lux/", 
						pattern='.Rdata',recursive=TRUE,full.names=TRUE)
	   
	   d_=d[d$method=="intigeo",]
	   length(unique(d_$pk))
	   dd=d_[!is.na(d_$logger),]
	   length(unique(dd$pk))
	   
	   
	   z=ddply(d_[d_$nest!="DUNLMLM017",],.(sp,breeding_site,year, nest, nn), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 z$pk=paste(z$sp,z$breeding_site,z$year,z$nest,z$nn)
		 
			zz=z[!is.na(z$f) & !is.na(z$m),]
		
		z1=d_[!(duplicated(paste(d_$tag,d_$bird_ID,d_$sex))),c('tag','bird_ID','sex')]
		z1=z1[which(!is.na(z1$tag)),]
	   
	   # extract renests in one go, as they are all within 10-12 days and hence data would be included anyway
			zzz=zz[paste(zz$nest,zz$year)%in%paste(zz$nest,zz$year)[duplicated(paste(zz$nest,zz$year))],]
			# extract min/max withou renest
			z=ddply(d_[d_$nest!="DUNLMLM017",],.(sp,breeding_site,year, nest), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 z$pk=paste(z$sp,z$breeding_site,z$year,z$nest)
		 
			zz=z[!is.na(z$f) & !is.na(z$m),]		
			for(i in 1:nrow(zz)) {
				load(f[grep(paste(zz$year[i],zz$nest[i], sep="_"),toupper(f))])
				
				aa=a[a$datetime_>as.POSIXct(substring(zz$min_[i]-keeps,1,10)) & a$datetime_<as.POSIXct(substring(zz$max_[i]+keepe,1,10)),]
				
				aa$bird_ID=toupper(z1$bird_ID[match(tolower(aa$logger), tolower(z1$tag))])
				aa$nest=zz$nest[i]
				aa$breeding_site=zz$breeding_site[i]
				aa$who=tolower(z1$sex[match(tolower(aa$logger), tolower(z1$tag))])
				aa$sp=zz$sp[i]
				aa$method=tolower(d_$method[1])
				aa$signal=aa$light
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa=aa[which(!is.na(aa$datetime_)),]
				aa$act_ID=paste('lightIntigeo',ifelse(nchar(i)==1,paste("00",i,sep=""),ifelse(nchar(i)==2,paste("0",i,sep=""),i)),sep="_")
				
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
				
				write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")
					print(i)
				print(paste(zz$year[i],zz$nest[i], sep="_"))
				print(paste(aaa$year[1],aaa$nest[1], sep="_"))
				}
		{# just for nest "DUNLMLM017" - has only one bout
			d_=d[d$nest=="DUNLMLM017",]
			
			load(f[grep(paste("2013","DUNLMLM017", sep="_"),toupper(f))])
				#a$datetime_[1]
				aa=a[a$datetime_>as.POSIXct(substring(d_$datetime_on[d_$nest=="DUNLMLM017"]-keeps,1,10)) & a$datetime_<as.POSIXct(substring(d_$datetime_off[d_$nest=="DUNLMLM017"]+keepe,1,10)),]
							
				aa$bird_ID=toupper(bb$bird_ID[match(tolower(aa$logger), tolower(bb$logger))])
				aa$nest=toupper(bb$nest[match(tolower(aa$logger), tolower(bb$logger))])
				aa$breeding_site=tolower(bb$study_site[match(tolower(aa$logger), tolower(bb$logger))])[1]
				aa$who=tolower(bb$sex[match(tolower(aa$logger), tolower(bb$logger))])
				aa$sp=toupper(bb$sp[match(tolower(aa$logger), tolower(bb$logger))])
				aa$method=tolower(d_$method[1])
				aa$signal=aa$light
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa$act_ID=paste('lightIntigeo',ifelse(nchar(nrow(zz)+1)==1,paste("00",nrow(zz)+1,sep=""),ifelse(nchar(nrow(zz)+1)==2,paste("0",nrow(zz)+1,sep=""),nrow(zz)+1)),sep="_")
				
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
				
				write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")
			
		}	
		}
	{# UvA
	    f=list.files(path="C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/pairs/UvA/", 
						pattern='.Rdata',recursive=TRUE,full.names=TRUE)
	      
	   d_=d[d$method=="uva",]
	   length(unique(d_$pk))
	    
	   z=ddply(d_,.(sp,breeding_site,year, nest, nn), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 z$pk=paste(z$sp,z$breeding_site,z$year,z$nest,z$nn)
		 
			zz=z[!is.na(z$f) & !is.na(z$m),]
		z1=d_[!(duplicated(paste(d_$tag,d_$bird_ID,d_$sex))),c('tag','bird_ID','sex')]
		z1=z1[which(!is.na(z1$tag)),]
	   
	    for(i in 1:nrow(zz)) {
				load(f[grep(paste(zz$year[i],zz$nest[i], sep="_"),toupper(f))])
				a=b
				#a$datetime_[1]
				aa=a[a$datetime_>as.POSIXct(substring(zz$min_[i]-keeps,1,10)) & a$datetime_<as.POSIXct(substring(zz$max_[i]+keepe,1,10)),]
				
				aa$bird_ID=toupper(z1$bird_ID[match(tolower(aa$logger), tolower(z1$tag))])
				aa$nest=zz$nest[i]
				aa$breeding_site=zz$breeding_site[i]
				aa$who=tolower(z1$sex[match(tolower(aa$logger), tolower(z1$tag))])
				aa$sp=zz$sp[i]
				aa$method=tolower(d_$method[1])
				aa$signal=aa$distancetonest
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa=aa[which(!is.na(aa$datetime_)),]
				aa$act_ID=paste("gps",ifelse(nchar(i)==1,paste("00",i,sep=""),ifelse(nchar(i)==2,paste("0",i,sep=""),i)),sep="_")
				
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
						
				write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")
					print(i)
				print(paste(zz$year[i],zz$nest[i], sep="_"))
				print(paste(aaa$year[1],aaa$nest[1], sep="_"))
				}
		}	
}
{# light logger single individuals
	{# lig 
	   f=list.files(path="C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/lig/", 
						pattern='.Rdata',recursive=TRUE,full.names=TRUE)
		   
	   d_=d[d$method=="bas",]
	  
	   z=ddply(d_[!d_$nest%in%c("DUNLMLM017","RUM03"),],.(sp,breeding_site,year, nest, nn), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 z$pk=paste(z$sp,z$breeding_site,z$year,z$nest,z$nn)
			 zz=z[which(is.na(z$f) | is.na(z$m)),]
	   # extract renests in one go, as they are all within 10-12 days and hence data would be included anyway
			zzz=zz[paste(zz$nest,zz$year)%in%paste(zz$nest,zz$year)[duplicated(paste(zz$nest,zz$year))],]
			# extract min/max withou renest
			z=ddply(d_[!d_$nest%in%c("DUNLMLM017","RUM03"),],.(sp,breeding_site,year, nest), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 z$pk=paste(z$sp,z$breeding_site,z$year,z$nest)
		 
			zz=z[which(is.na(z$f) | is.na(z$m)),]	
			#zz[zz$nest%in%zz$nest[duplicated(zz$nest)],]
			#zz$nest=sub("E-", "NA-",zz$nest)
			#zz$nest=sub("NA_", "NA-",zz$nest)
		z1=d_[!(duplicated(paste(d_$tag,d_$bird_ID,d_$sex))),c('tag','bird_ID','sex')]
		z1=z1[which(!is.na(z1$tag)),]
		
	    for(i in 1:nrow(zz)) {
				#zz[i,]
				#head(d_[d_$nest==zz$nest[i],])
				x=zz$nest[i]
				x2=c(zz$f[i],zz$m[i])
				x2=x2[!is.na(x2)]
				if(substr(x,1,2)=="E-"){x=substr(x,3, nchar(x))}else{if(substr(x,1,3)%in%c("NA-","NA_")){x=substr(x,4, nchar(x))}else{x=x}} 
				
				fzz=f[grep(toupper(x),toupper(f))]
				ffzz=fzz[grep(zz$year[i],toupper(fzz))]
				if(length(ffzz)==1){load(ffzz)}else{
					fz2=ffzz[grep(toupper(zz$breeding_site[i]),toupper(ffzz))]
					if(length(fz2)==1){load(fz2)}else{load(fz2[grep(toupper(x2),toupper(fz2))])}
					}
			
				a=b
				if(toupper(zz$nest[i])=="BBK02"){a$logger=202}
				#a$datetime_[1]
				aa=a[a$datetime_>as.POSIXct(substring(zz$min_[i]-keeps,1,10)) & a$datetime_<as.POSIXct(substring(zz$max_[i]+keepe,1,10)),]
				
				
				aa$bird_ID=toupper(z1$bird_ID[match(tolower(aa$logger), tolower(z1$tag))])
				aa$nest=zz$nest[i]
				aa$breeding_site=zz$breeding_site[i]
				aa$who=tolower(z1$sex[match(tolower(aa$logger), tolower(z1$tag))])
				aa$sp=zz$sp[i]
				aa$method=tolower(d_$method[1])
				aa$signal=aa$light
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa=aa[which(!is.na(aa$datetime_)),]
				aa$act_ID=paste('lightBas',ifelse(nchar(50+i)==1,paste("00",50+i,sep=""),ifelse(nchar(50+i)==2,paste("0",50+i,sep=""),50+i)),sep="_")
				
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
								
				write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")
				print(paste(zz$year[i],zz$nest[i], sep="_"))
				}
		}
	{# lux (without DUNLMLM017 and RUM03 - done separately, 
	    d_=d[d$method=="intigeo",]
	   
	   # check whether all nests have one logger
		#z=ddply(d_,.(sp,breeding_site,year, nest, nn), summarise, f=length(unique(logger[sex%in%c("f","u")])))
		#z[z$f!=1,]
	   
	   
	   z=ddply(d_[!d_$nest%in%c("DUNLMLM017","RUM03"),],.(sp,breeding_site,year, nest, nn), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		zz=z[which(is.na(z$f) | is.na(z$m)),]	
	   # extract renests in one go, as they are all within 10-12 days and hence data would be included anyway
			zzz=zz[paste(zz$nest,zz$year)%in%paste(zz$nest,zz$year)[duplicated(paste(zz$nest,zz$year))],]
			# extract min/max withou renest
			z=ddply(d_[!d_$nest%in%c("DUNLMLM017","RUM03"),],.(sp,breeding_site,year, nest), summarise, f=unique(logger[sex%in%c("f","u")]), m=unique(logger[sex%in%c("m","w")]),min_=min(datetime_on), max_=max(datetime_off))
	   		 
		 
			zz=z[which(is.na(z$f) | is.na(z$m)),]	
			#zz[zz$nest%in%zz$nest[duplicated(zz$nest)],]
			#zz$nest=sub("E-", "NA-",zz$nest)
			#zz$nest=sub("NA_", "NA-",zz$nest)
		z1=d_[!(duplicated(paste(d_$tag,d_$bird_ID,d_$sex))),c('tag','bird_ID','sex')]
		z1=z1[which(!is.na(z1$tag)),]
		
		f=list.files(path="C:/Users/mbulla/Documents/Dropbox/Incubation_cooperation/datafiles/extracted/lux/", 
						pattern='.Rdata',recursive=TRUE,full.names=TRUE)
			
	    for(i in 1:nrow(zz)) {
				#zz[i,]
				#head(d_[d_$nest==zz$nest[i],])
				x=zz$nest[i]
				if(x=="AMGP+CAW12"){x="AMGP_CAW12"}
				if(x=="AMGP+CAW13"){x="AMGP_CAW13"}
				x2=c(zz$f[i],zz$m[i])
				x2=x2[!is.na(x2)]
				if(substr(x,1,2)=="E-"){x=substr(x,3, nchar(x))}else{if(substr(x,1,3)%in%c("NA-","NA_")){x=substr(x,4, nchar(x))}else{x=x}} 
				
				fzz=f[grep(toupper(x),toupper(f))]
				ffzz=fzz[grep(zz$year[i],toupper(fzz))]
				if(length(ffzz)==1){load(ffzz)}else{
					fz2=ffzz[grep(toupper(zz$breeding_site[i]),toupper(ffzz))]
					if(length(fz2)==1){load(fz2)}else{load(fz2[grep(toupper(x2),toupper(fz2))])}
					}
			
				a=b
				#a$datetime_[1]
				aa=a[a$datetime_>as.POSIXct(substring(zz$min_[i]-keeps,1,10)) & a$datetime_<as.POSIXct(substring(zz$max_[i]+keepe,1,10)),]
				
				aa$bird_ID=toupper(z1$bird_ID[match(tolower(aa$logger), tolower(z1$tag))])
				aa$nest=zz$nest[i]
				aa$breeding_site=zz$breeding_site[i]
				aa$who=tolower(z1$sex[match(tolower(aa$logger), tolower(z1$tag))])
				aa$sp=zz$sp[i]
				aa$method=tolower(d_$method[1])
				aa$signal=aa$light
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa=aa[which(!is.na(aa$datetime_)),]
				aa$act_ID=paste('lightIntigeo',ifelse(nchar(11+i)==1,paste("00",11+i,sep=""),ifelse(nchar(11+i)==2,paste("0",11+i,sep=""),11+i)),sep="_")
			
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
								
				write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")
				print(paste(zz$year[i],zz$nest[i], sep="_"))
				}
		

	  
		
		}
	{# add nest with lig and lux
		load("C:\\Users\\mbulla\\Documents\\Dropbox\\Incubation_cooperation\\datafiles\\extracted\\_RUTU_sois_RUM03_2012_m_1457_B472_.Rdata")
				d_=d[d$nest=="RUM03",]
				z1=d_[!(duplicated(paste(d_$tag,d_$bird_ID,d_$sex))),c('tag','bird_ID','sex')]
				z1=z1[which(!is.na(z1$tag)),]
				a=b
				#a$datetime_[1]
				aa=a[a$datetime_>as.POSIXct(substring(min(d_$datetime_on)-keeps,1,10)) & a$datetime_<as.POSIXct(substring(max(d_$datetime_off)+keepe,1,10)),]
				
				aa$method=tolower(ifelse(aa$logger=="1457", "bas", "intigeo"))
				aa$signal=ifelse(aa$method=="intigeo", exp(aa$light/10), aa$light)
				aa$bird_ID="155301457"
				aa$nest="RUM03"
				aa$breeding_site="sois"
				aa$who="m"
				aa$sp=d_$sp[1]
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa$act_ID="lightBasIntigeo_001"
				aa=aa[which(!is.na(aa$datetime_)),]
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
								
				write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")
	}
	{# add nest with lux and UvA
			bi=d[d$nest=="1415901803",]
			z1=bi[!(duplicated(paste(bi$tag,bi$bird_ID,bi$sex))),c('tag','bird_ID','sex')]
			z1=z1[which(!is.na(z1$tag)),]
			{# Combine
				load(file ="C:\\Users\\mbulla\\Documents\\Dropbox\\Martinuv_cernoch\\Comparative\\figs\\extended_data\\Extended_Data_Fig_6\\_BLGO_frie_1415901803_2014_f_E391_2016-05-03.Rdata")
					b$signal=b$light
					b$method="intigeo"
					aa=b[,c('year','datetime_','logger','signal', 'method')] 
				
				#load(file ="M:\\PROJECTS\\PHD\\STATS\\DATASETS\\comparative\\_Limosa limosa_E391_Female_2014_5_int_.Rdata")
				load(file ="C:\\Users\\mbulla\\Documents\\Dropbox\\Martinuv_cernoch\\Comparative\\figs\\extended_data\\Extended_Data_Fig_6\\_Limosa limosa_B3RLLL_Female_2014_30_UiA_.Rdata")
					a$light=NA
					a$signal=a$distm
					a$method="uva"
					aaa=a[,c('year','datetime_','logger','signal', 'method')] 
					
					aa=rbind(aaa,aa)
				
				aa$bird_ID=toupper(z1$bird_ID[match(tolower(aa$logger), tolower(z1$tag))])
				
				aa$nest="1415901803"
				aa$breeding_site=bi$breeding_site[1]
				aa$who="f"
				aa$sp=bi$sp[1]
				aa$tag=tolower(aa$logger)
				aa$datetime_=as.character(aa$datetime_)
				aa$act_ID="lightIntigeoGps_001"
				aa=aa[which(!is.na(aa$datetime_)),]
				aaa=aa[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
				write.table(aaa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")
	}
		
	
	
	}
}
{# ARS - gets MYSQL warnings when getting datetime and tag columns, but that is fine
		  {# define constants
			# define database
				#con = dbcon(user='mbulla',host='scidb.orn.mpg.de', password='Kvetak25',database='LOGGERSatBARROW')
				conMy = dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
								
			# define nests for actos
				nests=d[!duplicated(paste(d$year,d$sp,d$nest)) & d$method%in%c("it"),c('year','sp','nest','nn','breeding_site','lon')]
				nests$nest=tolower(nests$nest)
				# set minimum and maximum time for which to use the RFID data (first egg -1 and end_date+1 day)
				nn=n[n$breeding_site=="barr" & n$year<2010,]
				nests$start_=nn$first_egg_laid[match(tolower(paste(nests$year,nests$sp,nests$nest)),tolower(paste(nn$year,nn$sp,nn$nest)))]-1*24*60*60
				nests$end_=nn$end_datetime[match(tolower(paste(nests$year,nests$sp,nests$nest)),tolower(paste(nn$year,nn$sp,nn$nest)))]+1*24*60*60
				
				nests[is.na(nests$end_),]
				nests[is.na(nests$start_),]
				
				bird=d[!duplicated(paste(d$year,d$sp,d$nest, d$logger)) & d$method%in%c("it"),c('year','sp','nest','nn','breeding_site','lon',"logger")]
				
		  }			
		 
		  {# data extraction function
			meta_nest.data=function(con, nest, yr){
			if (missing(nest)) 
			nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from loggersatbarrow.device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
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
				m$logger="IT"						
		return(m)
		
	}
			meta_birds.data=function(con, nest, yr){
			 if (missing(nest)) 
				   nest = pickOne(dbq(con, paste("select distinct lower(nest) nest from loggersatbarrow.device_filestatus where device='RFID' and year_ =",yr," order by nest"))$nest)
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
			ARS.actogram_data = function (con = con, nest, yr,start_,end_) {
			#on.exit(mysqlCloseConnection(con))
			 	nest_l=tolower(nest)
				nest_ =shQuote(tolower(nest))
				tag_m=s_b$radioFr[s_b$sex==1]
				tag_f=s_b$radioFr[s_b$sex==2]
				it_m =  dbq(conMy, paste("select datetime_, tag_ID tag, ss_base signal_ from LOGGERSatBARROW.RAW_INTERACTION_TAGS_09 where ss_base>-150 and tag_ID= ",tag_m," order by datetime_")) 
				it_m$sex="m"	
				it_m$signal_=it_m$signal_*-1
				it_f =  dbq(conMy, paste("select datetime_, tag_ID tag, ss_base signal_ from LOGGERSatBARROW.RAW_INTERACTION_TAGS_09 where ss_base>-150 and tag_ID= ",tag_f," order by datetime_")) 
				it_f$sex="f"
				it_f$signal_=it_f$signal_*-1
				it=rbind(it_m,it_f)
				it$who=it$sex
				it$nest=tolower(nest)
				
				it2 = dbq(conMy, paste0("SELECT lower(nest) nest, FUNCTIONS.RoundTime(hatch_start,'M',1) datetime_,
												   NULL tag, NULL signal_, 'hatching start' who
													from AVESatBARROW.NESTS where lower(nest) = ",nest_,"and year_ =",yr
												))
				it2$sex=NA
				it=rbind(it,it2)
				
			  if (nrow(it) > 0) { 
				it$datetime_=as.POSIXct(it$datetime_)
				it=it[it$datetime_>=start_ & it$datetime_<=end_,]
				#it$day = as.Date(trunc(it$datetime_, "day"))
				#it$time = as.numeric(difftime(it$datetime_, trunc(it$datetime_, "day"), units = "hours"))
					 }    
					return(it) 
					}
}

		  {# extract data
			tt="txt"
			for (i in (1:nrow(nests))) {
				m=meta_nest.data(con = conMy, nest = nests$nest[i],yr = nests$year[i])
				s_b=meta_birds.data(con = conMy, nest = nests$nest[i],yr = nests$year[i])
				a = ARS.actogram_data(con = conMy, nest = nests$nest[i],yr = nests$year[i], start_ = nests$start_[i], end_ = nests$end_[i])
				#gps=GPS.temperature_actogram_data (con = con, nest = nests$nest[i],yr = nests$yr[i])
				#RFID.temperature_actogram(dfr,gps,yr=yr,type=type, utc=TRUE)
				a$nest=toupper(nests$nest[i])
				a$nn=toupper(nests$nn[i])
				a$sp=toupper(nests$sp[i])
				a$year=tolower(nests$year[i])
				a$breeding_site="barr"
				
				a$datetime_utc=a$datetime_+m$utc_plus*60*60
				
				a$datetime_= a$datetime_utc + (nests$lon[i]/15)*60*60
				a$method="it"
				z=d[tolower(d$breeding_site)=="barr",]
				z1=z[!(duplicated(paste(z$nest,z$year,z$bird_ID, z$sex))),]
				a$signal=a$signal_
				a$bird_ID=tolower(z1$bird_ID[match(tolower(paste(a$year, a$nest, a$who)), tolower(paste(z1$year, z1$nest,z1$sex)))])
				
				a$datetime_=as.character(a$datetime_)
				a=a[which(!is.na(a$datetime_)),]
				a$act_ID=paste("radio_00",i,sep="")
				aa=a[,c('sp','breeding_site','year','method','nest','bird_ID','tag','who','datetime_','signal','act_ID')]
				if(tt=="txt"){write.table(aa,paste(wd2,'raw_signal_based_data.txt', sep=""), col.names=FALSE,row.names=FALSE,append =TRUE, sep=",")}
				if(tt=="sqlite"){
				conLite = dbConnect(dbDriver("SQLite"),dbname = db)
				#dbRemoveTable(conLite,name = "rfid_try")
				dbWriteTable(conLite, name = "raw_signal_based_data", value = aa, row.names = FALSE, header=TRUE, append=TRUE)
				dbDisconnect(conLite)
				}
				print(paste(nests$nest[i],nests$year[i]))
			 } 
     		}
	
}	

{# create SQL database  	
	wet=dbGetQuery
	db=paste(wd2,"biparental_incubation.sqlite",sep="")
	conLite = dbConnect(dbDriver("SQLite"),dbname = db)
	
	
	h=read.table(paste(wd2,'raw_signal_based_data.txt', sep=""), sep=",", header=TRUE, stringsAsFactors=FALSE) 
	
	#names(h)[4]="tag"
	wet(conLite,"DROP TABLE IF EXISTS raw_signal_based_data ")
	dbWriteTable(conLite, name = "raw_signal_based_data", value = h, row.names = FALSE, header=TRUE)
	wet(conLite,"CREATE INDEX act_ID_ ON raw_signal_based_data (act_ID)")
	dbDisconnect(conLite)
}
}	
