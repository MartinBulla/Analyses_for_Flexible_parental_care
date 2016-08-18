# FUNCTIONS & CONSTANTS	
	{# load packages
	   sapply(c('ggplot2', 'ggthemes','raster'),
			function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))
	}
	
	{# define workingdirectory
	     wd="C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Bip_to_uni/Analyses/Data/"	
	}
	
	{# define time 
	  Sys.setenv(TZ="UTC")	
	}
	
	{# define functions
	  transpcol = function (col = "red", newalpha = 100, mcv = 255) 
				{
					mycol = col2rgb(col)
					rgb(mycol[1, ], mycol[2, ], mycol[3, ], alpha = newalpha, 
						maxColorValue = mcv)
				}
	  RFID.temperature_actogram = function(dfr,figCap = figCap_, latlon = latlon_, inp = ip_, ins = inc_start, type = "PNG", min_=-3, max_=49, UTC=FALSE, UTC_met=FALSE, signal=FALSE, day=TRUE) {
				# dfr - data frame
				# figCap - figure captions
				# latlon - latitude longitude
				# day - panel labels as day or as day of incubation period and inc constancy
				# ins = start of incubation period
				# inp = lenght of incubation period
				# type = PNG -created, PDF - created, SAVE - also Rdata file saved
				# min_/max_ - limits of y-axis in the panel
				# UTC/UTC_met -  shall data and metadata be transformed to longitudinal time (yes = TRUE, no=FALSE) 
				# signal - are data based on automated tracking?
				
		 #  if (type =='PDF') {
		 #      tf = paste0(outdir,'/',dfr$nest[1], ".pdf")
		  #     pdf(tf, paper = "a4", width = 8, height = 11.6929134)
			#}
			if(UTC==TRUE){dfr$datetime_=as.POSIXct(dfr$datetime_, tz="UTC")+(nests_$local_plus[i]*60*60))}# adjusts to longitudinal time if aksed for
			 dfr$datetime_=as.POSIXct(dfr$datetime_, tz="UTC")
			 dfr$day = as.Date(trunc(dfr$datetime_, "day"))
			 dfr$time = as.numeric(difftime(dfr$datetime_, trunc(dfr$datetime_,"day"), units = "hours"))
			
			if(UTC_met==TRUE){ins=ins+nests_$local_plus[nests_$nest==nest]*60*60} # adjusts to longitudinal time if aksed for
			
			 sl1 = unique(dfr$day)
			 sl1=sl1[order(sl1)]
			 
			 # prepare panel labels
    		 sl2=ddply(dfr,.(day),summarise, const=mean(inc,na.rm=TRUE))
			 sl2$day_j=as.numeric(format(sl2$day ,"%j")) - as.numeric(format(as.Date(trunc(ins, "day")),"%j"))+1
			 sl2$day_j=ifelse(nchar(sl2$day_j)==1, paste(0,sl2$day_j,sep=""),sl2$day_j)
			 sl2$day_inc_per=as.character(paste(sl2$day_j,"/",round(as.numeric(inp),0),"; ",round(sl2$const*100,0),'%',sep=""))
			{# prepare polygons for night and twilight
					nt=data.frame(day=sl1, day_pos=as.POSIXct(sl1,"UTC"),stringsAsFactors =FALSE)
						nt$lat_=latlon$lat
					 nt$sunrise=sunriset(matrix(c(0,nt$lat_[1]),nrow=1),nt$day_pos, direction=c("sunrise"),POSIXct.out=TRUE)$time
						nt$sunrise=as.numeric(difftime(nt$sunrise, trunc(nt$sunrise,"day"), units = "hours"))
					 nt$sunset=sunriset(matrix(c(0,nt$lat_[1]),nrow=1),nt$day_pos, direction=c("sunset"),POSIXct.out=TRUE)$time
							nt$sunset=as.numeric(difftime(nt$sunset, trunc(nt$sunset,"day"), units = "hours"))
					 nt$c_dawn=crepuscule(matrix(c(0,nt$lat_[1]),nrow=1),nt$day_pos, solarDep=6,direction=c("dawn"),POSIXct.out=TRUE)$time
						nt$c_dawn=as.numeric(difftime(nt$c_dawn, trunc(nt$c_dawn,"day"), units = "hours"))
					 nt$c_dusk=crepuscule(matrix(c(0,nt$lat_[1]),nrow=1),nt$day_pos, solarDep=6,direction=c("dusk"),POSIXct.out=TRUE)$time
						nt$c_dusk=as.numeric(difftime(nt$c_dusk, trunc(nt$c_dusk,"day"), units = "hours"))
			  }
			{# prepare polygons for bip and unip
				if(length(dfr$type[which(dfr$type=='bip')])>0){ bi=ddply(dfr[which(dfr$type=='bip'),],.(day),summarise, start_=min(time),end_=max(time), col_=bip_col)}
				u=ddply(dfr[which(dfr$type=='uni'),],.(day),summarise, start_=min(time),end_=max(time))
				if(nrow(u)==0){u=data.frame(start_=NA, end_=NA) # nests with too short uniparental incubation where not used in the unip bip comparison and hence have no background
								bi=data.frame(start_=NA, end_=NA)}
					
					tst=nests_$sex[i]
					u$col_=ifelse(tst=='f',f_col, m_col)
				
			}
			{# disturbance
				cap = dfr[which(dfr$disturb=='capture'),] 
					if(nrow(cap)>0){cap$day = as.Date(trunc(cap$datetime_, "day"))
									cap$time = as.numeric(difftime(cap$datetime_, trunc(cap$datetime_,"day"), units = "hours"))
									cap$act=max_} else{cap=data.frame(day=NA, time=NA)}
				vis= dfr[which(dfr$disturb=='nest visit' & is.na(dfr$tag)),] 
					if(nrow(vis)>0){vis$day = as.Date(trunc(vis$datetime_, "day"))
									vis$time = as.numeric(difftime(vis$datetime_, trunc(vis$datetime_,"day"), units = "hours"))
									vis$act=vc}else{vis=data.frame(day=NA, time=NA)}		
							
			 }
			{# end state and hatching start
				nb=n[tolower(n$nest)==nest & n$year==yr,]
				nbi=nb[nb$state=='hs' | nb$datetime_==nb$datetime_[nrow(nb)],]
				
				if(UTC_met==TRUE){nbi$datetime_=nbi$datetime_+nests_$local_plus[nests_$nest==nest]*60*60} # adjusts to longitudinal time if aksed for
				
				is_=nbi[nbi$state%in%c('hs'),]
					if(nrow(is_)>0){is_$day = as.Date(trunc(is_$datetime_, "day"))
									is_$time = as.numeric(difftime(is_$datetime_, trunc(is_$datetime_,"day"), units = "hours"))
									is_$act=max_}else{is_=data.frame(day=NA, time=NA)}
									
				ie_=nbi[nbi$state!='hs' & !is.na(nbi$state),]
					if(nrow(ie_)>0){ie_$day = as.Date(trunc(ie_$datetime_, "day"))
								   ie_$time = as.numeric(difftime(ie_$datetime_, trunc(ie_$datetime_,"day"), units = "hours"))
								   ie_$act=max_
								   ie_$who=ifelse(ie_$state=="w","eggs warm", ifelse(ie_$state=="d", "deserted", ifelse(ie_$state=="u", "unknown", ifelse(ie_$state=="p", "depredated", ifelse(ie_$state=="hg", "hatching",ifelse(ie_$state=="ln", "chicks out", ifelse(ie_$state=="hd","hatched",ifelse(ie_$state=="tag_loss","tag loss", NA))))))))
				   				   }
			 }
					 
			{# add colors for ploting	
			  dfr$who=ifelse(!is.na(dfr$who) & dfr$who%in%c("nest visit","visit","fieldteam","capture", "unknown", 'stranger','misread'), 'disturb',dfr$who)
			 
			 # general cols and sizes for actogram
			  clr = act_c
						# check whether sex is unique and not multiple
							# mf = ddply(dfr,.(tag, who),summarize, ntag = length(tag))
							  #mf = mf[order(mf$who, mf$ntag),]
							  #mm=mf$tag[mf$who=='m'][1]
							  #ff=mf$tag[mf$who=='f'][1]
							 # mm=dfr$tag[dfr$who=='male' & nchar(dfr$tag)>12][1]
							  #ff=dfr$tag[dfr$who=='female' & nchar(dfr$tag)>12][1]
			 
			  dfr     = merge(dfr,clr,all.x=TRUE)	
			 
			 # taglist = unique(dfr$tag[which(!is.na(dfr$tag))])
			 # trCol      = data.frame(tag = taglist, col2 = rainbow(length(taglist)),stringsAsFactors = FALSE)
			 # dfr        = merge(dfr,trCol,all.x=TRUE)
			 # dfr$cols[which(is.na(dfr$cols))] = dfr$col2[which(is.na(dfr$cols))]
			 #  unknowns = trCol[which(trCol$tag %in% dfr$tag[which(dfr$who=='unknown')]),]
			  
			  dfr = dfr[order(dfr$day,dfr$time), ]
			  # color for temperature
			 dfr$col_t_nest=ifelse(is.na(dfr$inc), NA, ifelse(dfr$inc==1,act_c$cols[act_c$who=='nest temperature incubation'],act_c$cols[act_c$who=='nest temperature no incubation']))
			 dfr$col_cv_nest=ifelse(is.na(dfr$inc), NA, ifelse(dfr$inc==1,signal_inc,signal_no_inc))
				#dfr$col_type=ifelse(dfr$type=='no',none_col, ifelse(dfr$type=='bip',bip_col, ifelse(dfr$type=='uni',uni_col, NA)))
			}
			
			{# scales, 
				 if(day==TRUE){ 
								strip.left1 = function(which.panel, ...) {
										LAB = format(sl1[which.panel], "%b-%d")
										grid.rect(gp = gpar(fill = "grey95", col=ln_col))
										ltext(0.5, 0.5, cex = 0.6, LAB, col=wr_col)
										}
								ylab_=list('Date',cex=0.7, col=wr_col, vjust=1, hjust=0.5)		
										
								}else{
									strip.left1 = function(which.panel, ...) {
										LAB = sl2$day_inc_per[which.panel]
										grid.rect(gp = gpar(fill = "grey95", col=ln_col))
										ltext(0.5, 0.5, cex = 0.6, LAB, col=wr_col)
													} 									
									ylab_=list('Day of incubation / incubation period; incubation constancy %',cex=0.7, col=wr_col, vjust=1, hjust=0.5)		
									}
				   if(length(c(dfr$t_nest[!is.na(dfr$t_nest)],dfr$t_surface[!is.na(dfr$t_surface)]))>0){
							scales1 = list(x = list(at=c(0,6,12,18,24),labels=c('00:00','06:00','12:00','18:00','24:00') , cex = 0.6, tck=0.4,
										limits = c(0,24),col=wr_col,col.line = ln_col, alternating=3), y = list(limits = c(min_, max_),at =c(max_*10/max_,max_*30/max_),draw=TRUE), col=wr_col,cex=0.5, tck=0.4,alternating=2, col.line=ln_col)
							ylab_right=list('Temperature [°C]',cex=0.7, col=wr_col,vjust=-0.3)
												
							}else{
							 scales1 = list(x = list(at=c(0,6,12,18,24),labels=c('00:00','06:00','12:00','18:00','24:00') , cex = 0.6, tck=0.4,
										limits = c(0,24),col=wr_col,col.line = ln_col, alternating=3), y = list(limits = c(min_, max_),at =c(max_*10/max_,max_*30/max_),draw=TRUE), col=tra,cex=0.5, tck=0,alternating=2, col.line=ln_col)
										
							ylab_right=list('CV of signal strength',cex=0.7, col=wr_col,vjust=-0.3)		#hjust=0
							}
				   #scales = list(x = list(at=c(0,6,12,18,24),labels=c('00:00','06:00','12:00','18:00','24:00') , #cex = 0.7, 
					#			limits = c(0,24),alternating=3), y = list(limits = c(0, 50),at =c(10,30), alternating=2, cex=0.6, tck=0.4))	
			}	
			{# legend 
					{# caption
					if(nest%in%c('s409','s510','s516')){
						clr_0=list(text=list(c(figCap$scinam, figCap$species,figCap$ID,"Bird removal experiment"),cex=0.6, col=c(wr_col,wr_col,wr_col,wr_col), font=c(3,2,1,1)),
								points=list(pch=c(15),cex= c(0.8), col=c(tra)))
						
						}else if (nest%in%c('s704')){
						clr_0=list(text=list(c(figCap$scinam, figCap$species,figCap$ID,"Dislocated T probe"),cex=0.6, col=c(wr_col,wr_col,wr_col,wr_col), font=c(3,2,1,1)),
								points=list(pch=c(15),cex= c(0.8), col=c(tra)))
						}else if (nest%in%c('s310','s815')){
						clr_0=list(text=list(c(figCap$scinam, figCap$species,figCap$ID,"Incubates one egg"),cex=0.6, col=c(wr_col,wr_col,wr_col,wr_col), font=c(3,2,1,1)),
								points=list(pch=c(15),cex= c(0.8), col=c(tra)))
						}else if(nest%in%c('b402','l301','l401','l501','s304','s902','w504')){
						clr_0=list(text=list(c(figCap$scinam, figCap$species,figCap$ID,"Considered"),cex=0.6, col=c(wr_col,wr_col,wr_col,wr_col), font=c(3,2,1,1)),
								points=list(pch=c(15),cex= c(0.8), col=c(tra)))
						}else{		
						clr_0=list(text=list(c(figCap$scinam, figCap$species,figCap$ID,"Limnodromus scolopaceus"),cex=0.6, col=c(wr_col,wr_col,wr_col,tra), font=c(3,2,1,1)),
									points=list(pch=c(15),cex= c(0.8), col=c(tra)))
						}
					clr_e=list(text=list(c(figCap$species,figCap$scinam, figCap$ID,"Limnodromus scolopaceus"),cex=0.6, col=c(tra)))	
					}
					{# used	for analyses		
						
						r=length(dfr$type[dfr$type=="bip"])
						rr=length(dfr$type[dfr$type=="uni"])
						tst=nests_$sex[i]
						unip=ifelse(tst=='f','uniparental  \u2640 ','uniparental \u2642 ')
						uni_col=ifelse(tst=='f',f_col, m_col)
						
						if(rr==0){clr_1=list(	text= list(c('used for:', paste("biparental   "), "uniparental   "),cex=0.6, col=c(wr_col_out,wr_col_out,wr_col_out),font=c(1,1,1)),
											points=list(pch=15,cex= c(0.8), col=c(tra, tra, tra)))
						
							}else if(r>0){clr_1=list(	text= list(c('used for:', paste("biparental"), unip),cex=0.6, col=c(wr_col,wr_col,wr_col),font=c(1,1,1)),
											points=list(pch=15,cex= c(0.8), col=c(tra, bip_col, uni_col)))
						
							}else{clr_1=list(   text= list(c('used for', paste("biparental"),unip),cex=0.6, col=c(wr_col,wr_col_out,wr_col),font=c(1,1,1)),
												points=list(pch=15,cex= c(0.8), col=c(tra, tra, uni_col)))
											}
					
						#text = list(c("-","-","|","|"), cex=c(1,1,0.6,0.6), font=c("plain","plain","bold","bold"), col = col_r_p
					}
					{# create legend column for temperatures or cv or signal if present
						if(signal==FALSE){clr_3=list(text = list(c("T [°C] nest ","T [°C] nest: incubation","T [°C] surface"),cex=0.6, col=wr_col),
														#lines = list(col=act_c$cols[act_c$who%in%c("nest temperature","surface temperature")],lwd=2,size=1))}								
														points = list(col=c(act_c$cols[act_c$who%in%c("nest temperature no incubation")],act_c$cols[act_c$who%in%c("nest temperature incubation","surface temperature")]),pch=20,cex=0.5))	
										 }else{clr_3=list(text = list(c("CV of signal strength:","no incubation","incubation"),cex=0.6, col=c(wr_col,wr_col,wr_col)),
														#lines = list(col=act_c$cols[act_c$who%in%c("nest temperature","surface temperature")],lwd=2,size=1))}								
														points = list(col=c(tra, signal_no_inc, signal_inc),pch=20,cex=0.5))	
												}
					}				
					{# create legend column for f,m, disturb if present
						r1=length(dfr$who[!is.na(dfr$who) & dfr$who=="f"])
							if(length(r1)==0){r1=0}
						r2=length(dfr$who[!is.na(dfr$who) & dfr$who=="m"])
							if(length(r2)==0){r2=0}
						r3=length(c(dfr$who[!is.na(dfr$who) & dfr$who=="disturb"],dfr$disturb[!is.na(dfr$disturb)]))
							if(length(r3)==0){r3=0}	
						
						if(r2==0 & r3==0){	col_r_t= c(wr_col,wr_col_out,wr_col_out,tra)
											col_r_p=c(act_c$cols[act_c$who%in%c("f")],tra,tra,tra)
											}
						if(r1==0 & r3==0){	col_r_t= c(wr_col_out,wr_col,wr_col_out,tra)
											col_r_p=c(tra,act_c$cols[act_c$who%in%c("m")],tra,tra)
											}
						if(r1==0 & r2==0 & r3>0){	col_r_t= c(wr_col_out,wr_col_out,wr_col,tra)
											col_r_p=c(tra,tra,act_c$cols[act_c$who%in%c("disturb")],tra )
											}
						if(r1>0 & r2==0 & r3>0){	col_r_t= c(wr_col,wr_col_out,wr_col,tra)
											col_r_p=c(act_c$cols[act_c$who%in%c("f")],tra,act_c$cols[act_c$who%in%c("disturb")],tra )
											}
						if(r1==0 & r2>0 & r3>0){	col_r_t= c(wr_col_out,wr_col,wr_col,tra)
											col_r_p=c(tra,act_c$cols[act_c$who%in%c("m")],act_c$cols[act_c$who%in%c("disturb")],tra )
												}					
						if(r1>0 & r2>0 & r3==0){	col_r_t= c(wr_col,wr_col,wr_col_out,tra)
											col_r_p=c(act_c$cols[act_c$who%in%c("f")],act_c$cols[act_c$who%in%c("m")],tra,tra)
											}
						if(r1>0 & r2>0 & r3 >0){	col_r_t= c(wr_col,wr_col,wr_col,tra)
													col_r_p=c(act_c$cols[act_c$who%in%c("f")],act_c$cols[act_c$who%in%c("m")],act_c$cols[act_c$who%in%c("disturb")],tra)
											}
						
						if(nests_$sp[i]%in%c("rnph","pesa") & r3==0){	col_r_t= c(wr_col_out,wr_col_out,wr_col_out,tra)
																		col_r_p=tra 
																	}
						if(nests_$sp[i]%in%c("rnph","pesa") & r3>0){	col_r_t= c(wr_col_out,wr_col_out,wr_col,tra)
																		col_r_p=c(tra,tra,act_c$cols[act_c$who%in%c("disturb")],tra)
																	}
						
						clr_2=list( text = list(c('RFID reading \u2640','RFID reading \u2642','disturbance'),col=c(col_r_t),cex=0.6),
												text = list(c("|","|","|"), cex=c(0.6,0.6,0.6), font="bold", col = c(col_r_p)))
						}									
					{# sun elevation
						nt1=nrow(nt[!is.na(nt$sunrise),])
							if(length(nt1)==0){nt1=0}
						nt2=nrow(nt[!is.na(nt$c_dawn),])
									if(length(nt2)==0){nt2=0}
						
						if(nt1==0 & nt2 ==0){	col_s_t=c(wr_col_out,wr_col_out)
											    col_s_p=rgb(1,1,1,1,maxColorValue = 1)}
						if(nt1>0 & nt2 ==0){	col_s_t=c(wr_col[1],wr_col_out)
												col_s_p=c(act_c$cols[act_c$who%in%c("sun < 0°")],rgb(1,1,1,1,maxColorValue = 1))}			
						if(nt1==0 & nt2 >0){	col_s_t=c(wr_col_out,wr_col)
												col_s_p=c(rgb(1,1,1,1,maxColorValue = 1),act_c$cols[act_c$who%in%c("sun < -6°")])}	
						if(nt1>0 & nt2 >0){	col_s_t=c(wr_col,wr_col)
											col_s_p=act_c$cols[act_c$who%in%c("sun < 0°","sun < -6°")]}
							
						clr_4=list(text = list(c("twilight","night"),col=c(col_s_t), cex=0.6),
														   points = list(col=c(col_s_p),pch=c(15),cex= c(0.6)))							
													   
					}
					{# hatch start and end state
						in1=length(unique(is_$state))
							if(length(in1)==0){in1=0}
						in2=length(unique(ie_$state))
									if(length(in2)==0){in2=0}
						
						if(in1==0 & in2==0){	col_i_t=c(wr_col_out,wr_col_out)
												col_i_p=c(tra,tra)}
						if(in1==0 & in2 >0){	col_i_t=c(wr_col_out,wr_col[1])
												col_i_p=c(tra,act_c$cols[act_c$who%in%c("incubation end")])}	
						if(in1>0 & in2 >0){	col_i_t=c(wr_col,wr_col)
											col_i_p=act_c$cols[act_c$who%in%c("hatching start","incubation end")]}	
						
						clr_5=list( text = list(c("hatching start",paste("end:",ie_$who, sep=" ")),cex=0.6, col=c(col_i_t)),
												text = list(c("|","|"),col=c(col_i_p),cex= c(0.6),font="bold"))
												
						}							   
					{# adds buffer around legend columns by creating fake legend columns
						clr_n=list(text = list(c("")))				
					}	
					{# combine
					if(figCap$site=="barr" ){ 
						key1 = c(  # captions
									clr_0,
									clr_n,
								# used
									clr_1,
									clr_n,
								# temperature
									clr_3,
									clr_n,
								# rfid
									clr_2,
									clr_n,
								# sun
									clr_4,
									clr_n,	
								# incubation
									clr_5,
									clr_n,
								# ending to compensate for captions
									clr_e,
								rep=FALSE, between=0.9 #just=-0.3#, 	padding.text=1.1,#, border=ln_col #columns
								)	
					}else{if(figCap$species%in%c("Eurasian golden plover","Eurasian oystercatcher","Kentish plover")){
					key1 = c(  # captions
									clr_0,
								# extracted
									clr_1,
									clr_n,
								# rfid
									clr_2,
									clr_n,
								# temperature
									clr_3,
									clr_n,
								# sun
									clr_4,
									clr_n,
								# incubation
									clr_5,
									clr_n,
									clr_n,
								# ending to compensate for captions
									clr_e,
								
								rep=FALSE, between=0.9 #just=-0.3#, 	padding.text=1.1,#, border=ln_col #columns
								)		
							}else{
							key1 = c(# captions
									clr_0,
									clr_n,
								# extracted
									clr_1,
									clr_n,
								# rfid
									clr_2,
									clr_n,
								# temperature
									clr_3,
									clr_n,
								# sun
									clr_4,
									clr_n,
								# incubation
									clr_5,
									clr_n,
									clr_n,
									clr_n,
									clr_n,
								# ending to compensate for captions
									clr_e,
								
								
								rep=FALSE, between=0.9 #just=-0.3#, 	padding.text=1.1,#, border=ln_col #columns
								)		
							}}
						}	
				}
			   panel1 = function(...) {
							   #panel.abline(v=c(1:5,7:11,13:17,19:23),col="light grey")
							  # panel.abline(v=c(6,12,18,24),col = "grey70")
							   #panel.abline(v=a$time_[a$day==sl1[panel.number()] & a$twilight==1],col="grey66")
							   #panel.abline(v=a$time_[a$day==sl1[panel.number()] & a$night==1],col="grey35")	
							   	dfri= dfr[which(dfr$day == sl1[panel.number()]),]	
							   {# indicate unip and bip
									ui = u[which(u$day == sl1[panel.number()]),]
									if(nrow(ui)>0){panel.rect(xleft=ui$start_, ybottom=min_, xright=ui$end_, ytop=max_, col=ui$col_, border=0)
											}
									if(length(dfr$type[which(dfr$type=='bip')])>0){
														bii = bi[which(bi$day == sl1[panel.number()]),]
														if(nrow(bii)>0){panel.rect(xleft=bii$start_, ybottom=min_, xright=bii$end_, ytop=max_, col=bii$col_, border=0)
																		}				
																		}
									#panel.xyplot(dfri$time, 1, col = dfri$col_type, type='l',lwd=2)
								}
							   {# disturbance
									visi = vis[which(vis$day == sl1[panel.number()]),] 
													if (nrow(visi)>0){panel.xyplot(visi$time[visi$day==sl1[panel.number()]],visi$act[visi$day==sl1[panel.number()]],col=c(act_c$cols[act_c$who%in%c("disturb")]), type="h", origin=min_)
																		}
									capi = cap[which(cap$day == sl1[panel.number()]),] 
													if (nrow(capi)>0){panel.xyplot(capi$time[visi$day==sl1[panel.number()]],capi$act[visi$day==sl1[panel.number()]],col=c(act_c$cols[act_c$who%in%c("capture")]), type="h", lwd=2,origin=min_)
																		}
													
							   }

							   {# rfid and metadata
								if(signal==FALSE){	
									if(!dfr$sp[1]%in%c('rnph','pesa')){							
										panel.xyplot(dfri$time, dfri$act, col = dfri$cols, type = "h", origin=min_)
										}
									panel.xyplot(dfri$time, dfri$t_nest, col = dfri$col_t_nest, cex = 0.1)
									}else{	panel.xyplot(dfri$time, dfri$cv, col = dfri$col_cv_nest, cex = 0.1)}
								}
							   
							   {# twilight and night upper - CHANGE TO BACKROUND
								nti = nt[which(nt$day == sl1[panel.number()]),] 
								
									# twilight and nigth present
									if(nrow(nti)>0 & !is.na(nti$sunrise) & !is.na(nti$c_dawn)){	panel.rect(xleft=0, ybottom=max_, xright=nti$c_dawn, ytop=max_*0.85, col=act_c$cols[act_c$who%in%c("sun < -6°")], border=0)
																					panel.rect(xleft=nti$c_dawn, ybottom=max_, xright=nti$sunrise, ytop=max_*0.85, col=act_c$cols[act_c$who%in%c("sun < 0°")], border=0)
																				}
									if(nrow(nti)>0 & !is.na(nti$sunset) & !is.na(nti$c_dusk)) {	panel.rect(xleft=nti$sunset, ybottom=max_, xright=nti$c_dusk, ytop=max_*0.85, col=act_c$cols[act_c$who%in%c("sun < 0°")], border=0)
																							panel.rect(xleft=nti$c_dusk, ybottom=max_, xright=23.9999, ytop=max_*0.85, col=act_c$cols[act_c$who%in%c("sun < -6°")], border=0)
																								}
									# only twilight present												
									if(nrow(nti)>0 & !is.na(nti$sunrise) & is.na(nti$c_dawn)){	panel.rect(xleft=0, ybottom=max_, xright=nti$sunrise, ytop=max_*0.85, col=act_c$cols[act_c$who%in%c("sun < 0°")], border=0)
																				}
									if(nrow(nti)>0 & !is.na(nti$sunset) & is.na(nti$c_dusk)){	panel.rect(xleft=nti$sunset, ybottom=max_, xright=23.9999, ytop=max_*0.85, col=act_c$cols[act_c$who%in%c("sun < 0°")], border=0)}
								}							
							   {# hatching and end state
									isi = is_[which(is_$day == sl1[panel.number()]),] 
													if (nrow(isi)>0){panel.xyplot(isi$time[isi$day==sl1[panel.number()]],isi$act[isi$day==sl1[panel.number()]],col=c(act_c$cols[act_c$who%in%c("hatching start")]), type="h",origin=min_, lwd=2)#,bg=c(act_c$cols[act_c$who%in%c("incubation start")]))#pch=25, cex= c(0.6),
																	}
													
									iei = ie_[which(ie_$day == sl1[panel.number()]),] # iei = ie_[which(ie_$day == "2013-07-03"),] 
									if (nrow(iei)>0){ iei$day = as.Date(trunc(iei$datetime_, "day"))
												   iei$time = as.numeric(difftime(iei$datetime_, trunc(iei$datetime_,"day"), units = "hours"))
												   iei$act=max_
												   panel.xyplot(iei$time[iei$day==sl1[panel.number()]],iei$act[iei$day==sl1[panel.number()]],col=c(act_c$cols[act_c$who%in%c("incubation end")]),origin=min_, type="h",lwd=2)#pch=24, cex= c(0.6)	,col=c(act_c$cols[act_c$who%in%c("incubation end")]),
												   }			   
														
								}	
								
								# surface temperature
									panel.xyplot(...)
							}
   
			{# plot				
			#dev.new(width=8.26771654, height=11.6929134)
						
			rfidsplot = xyplot(t_surface ~ time | day, 
									data = dfr, 
									col = c(act_c$cols[act_c$who=="surface temperature"]),
									cex = 0.1, cex.title=0.5, main = NULL,
									layout = c(1,length(sl1)),# ifelse(length(sl1) > 30, 30, length(sl1))), 
									strip.left = strip.left1, 
									scales = scales1,
									panel=panel1, 
									key=key1,
									ylab=ylab_,
									ylab.right=ylab_right,
									xlab.top=list('Time [h]',cex=0.7,col=wr_col, vjust=1),
									xlab=NULL,
									par.settings=list(axis.components=list(left=list(tck=0)), layout.widths=list(right.padding=2),axis.line = list(col = ln_col)), #box.3d=list(col = wr_col)), #top=list(tck=0.4),
									as.table=TRUE,
									#aspect = "fill", 
									strip = FALSE, distribute.type = TRUE,    
									lattice.options = list(layout.widths = list(strip.left = list(x = 3)))
									)
									
			
				#grid.rect(gp=gpar(lty="dashed", col="red"))
			# prepare the location for map	
				coordinates(latlon) = ~ lon+lat
				proj4string(latlon) = p4s_latlon
				loc = spTransform(latlon, CRS(p4s))
				gg=g+geom_point(data=data.frame(loc@coords), aes(x=lon, y=lat),color='red', size=0.2)
				#vp <- viewport(x=0.89,y=0.94,width=0.11*1.5, height=0.11) 						
				#pushViewport(vp)
				#print(gg, newpage=FALSE)
				#plot(1:10,1:10)
			}	
			if(type %in% c('SAVE')) {
					save(rfidsplot,dfr, sl1, strip.left1,scales1,panel1,key1,nt, is_,ie_, act_c, ex,latlon, figCap, file=paste("C:/Users/mbulla/Documents/ownCloud/ACTOSforHTML/",paste("rfid",i,sep="_"),".Rdata",sep=""))
					tf = paste0(outdir,pkk$act_ID[i], paste("_",pkk$pkk[i],sep=""), "_%03d.png",sep="") 
														png(tf,width = 210, height = 57+8*length(sl1), units = "mm", res = 600)	
									par(pin = c(8.26771654, (57+8*length(sl1))/25.4)) #c(8.26771654, 11.6929134)
									print(rfidsplot)
									vp <- viewport(x=0.89,y=((57+8*length(sl1))-12.6225)/(57+8*length(sl1)),width=0.11*1.5, height=32.67/(57+8*length(sl1))) # creates area for map	 	##y=0.9575 for 297mm height							
									pushViewport(vp)
									print(gg, newpage=FALSE) # prints the map
									dev.off()
				}else{if(type %in% c('PNG')) {
									tf = paste0(outdir, paste(figCap$ID,"_",figCap$sp,"_",figCap$site,"_",figCap$year,"_",figCap$nest,sep=""), "_%03d.png",sep="") 
									png(tf,width = 210, height = 57+8*length(sl1), units = "mm", res = 600)	#png(tf,width = 210, height = 297, units = "mm", res = 600)	
									par(pin = c(8.26771654, (57+8*length(sl1))/25.4)) #c(8.26771654, 11.6929134)#par(pin = c(8.26771654, 11.6929134)) 
									print(rfidsplot)
									vp <- viewport(x=0.89,y=((57+8*length(sl1))-12.6225)/(57+8*length(sl1)),width=0.11*1.5, height=32.67/(57+8*length(sl1)))#vp <- viewport(x=0.89,y=0.9575,width=0.11*1.5, height=0.11) # creates area for map	 					
									pushViewport(vp)
									print(gg, newpage=FALSE) # prints the map
									dev.off()
									}else{
									#dev.new(widht=8.26771654,height=11.6929134)
									par(pin = c(8.26771654, 11.6929134)) 
									print(rfidsplot)
									vp <- viewport(x=0.89,y=0.9575,width=0.11*1.5, height=0.11) # creates area for map	 	vp <- viewport(x=0.89,y=0.94,width=0.11*1.5, height=0.11) # creates area for map	 						
									pushViewport(vp)
									print(gg, newpage=FALSE) # prints the map
									}}
				}	
	}
	
	{# define constants
			# raw female,male rfid readings and capture line height
				fm=15
				vc=10
			# actogram colors
			
				act_c=data.frame(who  = c('bip','unip','none',
									'f','m','f?','m?','disturb','capture','error',#"invisible",
									"nest temperature incubation",
									"nest temperature no incubation","surface temperature", 
									"sun < 0°", "sun < -6°",
									"hatching start", "incubation end"
									),
								cols = c("white","black","grey80",
									"#FCB42C","#535F7C","#FCB42C","#535F7C","#5eab2b","#5eab2b","#5eab2b",#"#9e6eb2","#9e6eb2","#9e6eb2",##"#753192","#753192","#753192""#9692b2","#9692b2","#9692b2",##"honeydew2",
									"#99c978",#"#438585",#"#8ec46a",#"deepskyblue3",#"deepskyblue4",#"red3",#"#1E90FF96",#"lightseagreen",
									"#f0b2b2","#ADD8E6",#"lightblue","lightsteelblue3", ### no incubation"indianred3","#e16666"
									"grey66","grey35",
									"#ceb6d8","#9e6eb2"#"#DFC27D","#BF812D" #"#438585","#7ba9a9"#"violet","violetred3"
									),
							act=	c(NA, NA,NA,
									  fm,fm,fm,fm,vc,vc,vc,#fm,
									  NA,NA,NA,
									  NA,NA,
									  NA,NA),
						   stringsAsFactors = FALSE)
			# raw data colors
				male_col='#535F7C'#'dodgerblue'	#
				female_col= '#FCB42C'#'orange'	#
				signal_no_inc='#e88787'
				signal_inc='#5eab2b'
			# extracted data colors
				bip_col="#f4f9f1"#"#f6faf3"#
				m_col="#eef0f2"#"#f2f3f5"#"#edeff1"
				f_col="#fef7e9"
				
			# writing color
				wr_col="grey50"
				ln_col="grey80"
			# writing color for legend variable that are not present in the graph
				wr_col_out="grey70"
			# transparent	
				tra=rgb(1,1,1,1,maxColorValue = 1)
			# color for ??
				uncer="grey50"
				uncerm="grey70"
				
		# map projection
			
				p4s_latlon = CRS("+proj=longlat +datum=WGS84")
				p4s = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=-72 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
				
			# create OR load map and set map projection and prepare ggplot with map
				# create map a new
					#tmin = getData('worldclim', var = 'tmin', res = 10) 
					 # z1 = tmin[[ c('tmin6')]]
					 # z1 = aggregate(z1, 4)
					 # z = calc( z1, function(x) x/10)
					 # zp = projectRaster(z, crs = p4s)
					 # zp = rasterToPoints(zp) %>% data.frame %>% data.table
					 # latlon = spTransform(SpatialPoints( zp[, .(x,y)] , proj4string = CRS(p4s)), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% coordinates
					 # zp = cbind(zp, latlon)
					 # v
				# load map
			 		load(file=paste(wd,"map_for_supplement_10res.Rdata", sep=""))#10 res
				
				# prepare map-plot for adding the breeding site
				 g=	 ggplot() +
					coord_equal() +
					geom_raster(data = zp, aes(x = x, y = y, fill=temp))   +
					scale_fill_gradientn(colours = c("grey90","grey90"), guide="none")+
					#geom_path(data = fortify(grt) ,aes(x = long, y = lat, group = group), colour = "white", size   = .3)+
					theme_map() + theme(plot.margin = grid::unit(c(0.5,0.5,0.5,0.5), "mm"))		
			
	}