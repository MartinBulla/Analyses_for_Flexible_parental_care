# R 3.3.0 RSQLite bug/conflict with rworldmap
	
	require(RSQLite) # load package
	require(rworldmap)
	 
	# created dataframe
		my.df <- data.frame(col1 = 1:500000,
					col2 = as.character("blah"),
					col3 = as.character("blahblahblah"),
					col4 = as.character("2016-08-10 12:47:07"), 
					col5 = as.character("2016-08-10 12:47:07"), 
					col6 = as.character("2016-08-10 12:47:07"), 
					col7 = as.character("2016-08-10 12:47:07"), 
					col8 = as.character("2016-08-10 12:47:07"), 
					col9 = as.character("2016-08-10 12:47:07"), 
					col10 = as.character("2016-08-10 12:47:07"), 
					col11 = as.character("2016-08-10 12:47:07"), 
					col12 = 1,col13 = 1,col14 = 1,col15 = 1,col16 = 1,col17 = 1,col18 = 1
					)
			
	# connect and upload to SQLite database	
			db=paste("test.sqlite",sep="")
			
			conLite = dbConnect(dbDriver("SQLite"),dbname = db)
			dbGetQuery(conLite,paste( "DROP TABLE IF EXISTS test"))
			RSQLite::dbWriteTable(conn=conLite, name = 'test', value = my.df, row.names = FALSE)
			dbDisconnect(conLite)
			
# sessionInfo()	for R version 3.3.0	
R version 3.3.0 (2016-05-03)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 7 x64 (build 7601) Service Pack 1

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] rworldmap_1.3-6 sp_1.2-3        RSQLite_1.0.0   DBI_0.4-1      

loaded via a namespace (and not attached):
[1] foreign_0.8-66  fields_8.4-1    maps_3.1.0      maptools_0.8-39
[5] grid_3.3.0      spam_1.3-0      lattice_0.20-33
			
# sessionInfo()	for R version 3.1.1	
R version 3.1.1 (2014-07-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] rworldmap_1.3-6 sp_1.0-15       RSQLite_0.11.4  DBI_0.3.1      

loaded via a namespace (and not attached):
[1] fields_8.3-6    foreign_0.8-61  grid_3.1.1      lattice_0.20-29
[5] maps_2.3-9      maptools_0.8-30 spam_1.3-0

