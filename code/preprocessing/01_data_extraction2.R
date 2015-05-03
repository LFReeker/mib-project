############################################################################################
#							                                   #                                      #
#                                   M O N G O   C O D E			           	   #          
#          E X T R A C T   T H E    D A T A    F R O M    M O N G O   S E R V E R	   #     
#                      P U T   T H E   F I N A L   D A T A   T O  .csv  F I L E		   #    
#							                                   #                                          #
############################################################################################   


## Take the dump from remote server
mongodump --host heinz-tjle.heinz.cmu.edu:27017 -u student -p helloWorld --db admin --out mobile_dump --collection cellular

## Restore the dump in local Mongo Server
mongorestore mobile_dump


## Create Separate Collections for each day
db.createCollection("march1")
db.createCollection("march2")
db.createCollection("march3")
db.createCollection("march4")
db.createCollection("march5")
db.createCollection("march6")
db.createCollection("march7")



## Subset the data for each day and put it into respective collection
## This will take a while to execute

db.cellular.aggregate([ 
                     { $match: { date: "20080301" } }, { $out: "march1" }
                    ]
                   ) 

db.cellular.aggregate([ 
                     { $match: { date: "20080302" } }, { $out: "march2" }
                    ]
                   )   

db.cellular.aggregate([ 
                     { $match: { date: "20080303" } }, { $out: "march3" }
                    ]
                   )
		     
db.cellular.aggregate([ 
                     { $match: { date: "20080304" } }, { $out: "march4" }
                    ]
                   )

db.cellular.aggregate([ 
                     { $match: { date: "20080305" } }, { $out: "march5" }
                    ]
                   )  

db.cellular.aggregate([ 
                     { $match: { date: "20080306" } }, { $out: "march6" }
                    ]
                   )  

db.cellular.aggregate([ 
                     { $match: { date: "20080307" } }, { $out: "march7" }
                    ]
                   )  


## Export each collection into csv

mongoexport --host localhost --db admin --collection march1 --type=csv --out march1.csv --fields _id,caller_id,callee_id,date,time,call_duration,cell_id,imei

mongoexport --host localhost --db admin --collection march2 --type=csv --out march2.csv --fields _id,caller_id,callee_id,date,time,call_duration,cell_id,imei

mongoexport --host localhost --db admin --collection march3 --type=csv --out march3.csv --fields _id,caller_id,callee_id,date,time,call_duration,cell_id,imei

mongoexport --host localhost --db admin --collection march4 --type=csv --out march4.csv --fields _id,caller_id,callee_id,date,time,call_duration,cell_id,imei

mongoexport --host localhost --db admin --collection march5 --type=csv --out march5.csv --fields _id,caller_id,callee_id,date,time,call_duration,cell_id,imei

mongoexport --host localhost --db admin --collection march6 --type=csv --out march6.csv --fields _id,caller_id,callee_id,date,time,call_duration,cell_id,imei

mongoexport --host localhost --db admin --collection march7 --type=csv --out march7.csv --fields _id,caller_id,callee_id,date,time,call_duration,cell_id,imei

######################################################################################
#							                                                                       #
#                            E N D   O F    P R O G R A M			                       #
#							                                                                       #
######################################################################################
