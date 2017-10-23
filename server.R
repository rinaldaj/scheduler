#created by Anthony Rinaldo
library(shiny)
library(dplyr)
library(gdata)
library(DT)


isWorthy <- function(sub,num,db){ #the sub is the subject code , num is the course number, and db is the list of courses
  #This function returns a boolean array telling if a course is in db
  a <- NULL
  debug <- NULL
  for (i in 1:length(sub)){
    tmp = FALSE
    for(h in 1:length(db$subjCode)){	 
      if(sub[i] == db$subjCode[h] && num[i] == db$courseNum[h]){
        tmp = TRUE 
        break
      }
    }
    a[i] = tmp
  }
  return(a)
}

convertTime <- function(sheet){#converts time from AM PM to hours from midnight
  #returns the original sheet with columns of the converted time
  Stime = NULL
  for(i in sheet$Class.Start.Time){
    a = strsplit(i,split = '[[:upper:]]')
    a = strsplit(a[[1]][1],split = ':')
    hour = strtoi(a[[1]][1])
    minute = strtoi(a[[1]][2])
    a = strsplit(i,split = NULL)
    if('P' %in% a[[1]] && hour < 12)
      hour = hour + 12
    timeTotal = hour + minute/60
    Stime = rbind(Stime,timeTotal)
    
  }
  s = cbind(sheet,Stime)
  Etime = NULL
  for(i in sheet$Class.End.Time){
    a = strsplit(i,split = '[[:upper:]]')
    a = strsplit(a[[1]][1],split = ':')
    hour = strtoi(a[[1]][1])         
    minute = strtoi(a[[1]][2])
    a = strsplit(i,split = NULL)
    if('P' %in% a[[1]] && hour < 12) 
      hour = hour + 12
    timeTotal = hour + minute/60
    Etime = rbind(Etime,timeTotal)
    
  }
  s=cbind(s,Etime)
  
}

conflicts <- function(cla,sch){
  #this program tests to see if the class cla is in conflict with the schedule in sch
  ret = FALSE
  if(is.null(sch))
	return(FALSE)
  for(i in 1:length(sch$Stime)){
    #tmp = mapply(function(x,y){x == y}, x = strsplit(as.character(cla$Meeting.Days[1]),NULL), y = strsplit(as.character(sch$Meeting.Days[i]),NULL))
    #sameDay = FALSE
    #tmp = 0
    #for (j in strsplit(as.character(sch$Meeting.Days[i]),NULL) ){
     # print(j)
      #tmp = tmp + 1
    #  print(tmp)
    #}
    if(TRUE %in% (strsplit(as.character(cla$Meeting.Days[1]),NULL)[[1]] %in% strsplit(as.character(sch$Meeting.Days[i]),NULL)))
    {
      if((cla$Etime[1] >= sch$Stime[i] && cla$Stime[1] <= sch$Stime[i])
	||(cla$Stime[1] <= sch$Etime[i] && cla$Stime[1] >= sch$Stime[i])
	){
        ret = TRUE
        break
      }	
    }
  }
  ret
}

#I think this function is deprecated
generateSchedule <- function(incRe,classes,db){	
  masterlectures = filter(classes,(nchar(as.character(Meeting.Days)) > 1))
  masterLabs = filter(classes,(nchar(as.character(Meeting.Days)) == 1))
  schedule = NULL
  
  for(j in 1:length(db$subjCode)){
    lectures = filter(masterlectures,isWorthy(Subject,Catalog,db[j,]))
    if(is.null(schedule)){
      schedule = lectures[incRe,]
    }else{
      for(i in 1:length(lectures$Stime)){
        if(!conflicts(lectures[i,],schedule)){
          schedule = rbind(schedule,lectures[i,])
          break
        }
        
      }
    }
    lab = filter(masterLabs,isWorthy(Subject,Catalog,db[j,] ))
    discussion = filter(lab,Etime - Stime < 1.5)
    lab  = filter(lab,Etime - Stime > 1.5)
    if(length(lab$Stime) > 0 ){
      for(i in 1:length(lab$Stime)){
        if(!conflicts(lab[i,],schedule)){
          schedule = rbind(schedule,lab[i,])
          break
        }
      }
    }
  

  if(length(discussion$Stime) > 0 ){
      for(i in 1:length(discussion$Stime)){
        if(!conflicts(discussion[i,],schedule)){
          schedule = rbind(schedule,discussion[i,])
          break
        }
      }
    }
  }

  
  schedule	
  
}

swapClass <- function(claIndex,sch,classes){
	#takes claIndex as the class to be swapped
	#finds next class in classes to add to sch and returns a schedule
	#if no next class returns the first non-conflicting class
	tmp = sch[-claIndex,]
	inClasses = classes[classes$Catalog == sch$Catalog[claIndex] & classes$Subject == sch$Subject[claIndex] & nchar(as.character(classes$Meeting.Days)) == nchar(as.character(sch$Meeting.Days[claIndex])),]
	inClasses = inClasses[(inClasses$Etime - inClasses$Stime) == (sch$Etime[claIndex] - sch$Stime[claIndex]),]
	sinDex = which(inClasses$Section == sch$Section[claIndex] )
	sinDex = sinDex + 1
	for(i in sinDex[1]:length(inClasses$Catalog)){
		if(!conflicts(inClasses[i,],tmp) & !is.null(inClasses[i,])){
			#tmp = rbind(tmp,inClasses[i,])
			sch[claIndex,] = inClasses[i,]
			break
		}
	}
	if(is.na(sch$Catalog[claIndex])){
		for(i in 1:length(inClasses$Catalog)){
			if(!conflicts(inClasses[i,],tmp)){
				sch[claIndex,] = inClasses[i,]
				break
			}
		}
	}

sch
}

sortdb <- function(classes,db){
	#sorts the database so that it is in order by the classes with the least number of sections
	clasLeng = integer()
	for(i in 1:length(db$subjCode)){
		subNums = db[i,]
		if(is.null(db$tpe)){
		  tmp = filter(classes,isWorthy(Subject,Catalog,subNums))
		  tmp = filter(tmp,(nchar(as.character(Meeting.Days)) > 1))
		} else {
		if(db$tpe[i] == 2)
		  tmp <- filter(classes,isWorthy(Subject,Catalog,db[i,]),(nchar(as.character(Meeting.Days)) == 1),Etime -Stime < 1.5)
		else if(db$tpe[i] == 1)
		  tmp <- filter(classes,isWorthy(Subject,Catalog,db[i,]),(nchar(as.character(Meeting.Days)) == 1),Etime -Stime > 1.5)
		else
		  tmp <- filter(classes,isWorthy(Subject,Catalog,db[i,]),(nchar(as.character(Meeting.Days)) > 1))
		}
		clasLeng[i] = length(tmp$Meeting.Days)
	}

	#sorts into correct order
	dv = cbind(clasLeng,db)
	dv = dv[order(dv$clasLeng),]
	
	dv$clasLeng <- NULL
	row.names(dv)<-NULL
	dv
}

#this function is for makeing the actual schedule
makeSch <- function(class,db,sched = NULL){
	#add column to db to make each lecture/lab/discussion seperate classes

	#makes the schedule
	#make list of classes that don't conflict with sched
	
  
  db2 = NULL	
  for(i in 1:length(db$subjCode))
  {
  
    lecture = filter(class,isWorthy(Subject,Catalog,db[i,]),(nchar(as.character(Meeting.Days)) > 1))
    lab = filter(class,isWorthy(Subject,Catalog,db[i,]),(nchar(as.character(Meeting.Days)) == 1))
    discussion = filter(lab, Etime -Stime < 1.5)
    lab = filter(lab,Etime - Stime > 1.5)
    
    if(length(lecture$Stime) > 0){
      db2 = rbind(db2,data.frame(subjCode = db$subjCode[i],courseNum = db$courseNum[i],tpe = 0,stringsAsFactors = FALSE ))
    }
    if(length(lab$Stime) > 0)
      db2 = rbind(db2,data.frame(subjCode = db$subjCode[i],courseNum = db$courseNum[i],tpe = 1,stringsAsFactors = FALSE ))
    if(length(discussion$Stime) > 0 )
      db2 = rbind(db2,data.frame(subjCode = db$subjCode[i],courseNum = db$courseNum[i],tpe = 2,stringsAsFactors = FALSE ))
  }

	while(length(db2$subjCode) > 0){
	
	classes = NULL
	if(!is.null(sched)){
		for(i in 1:length(class$Catalog)){
			if(!conflicts(class[i,],sched))
				classes = rbind(classes,class[i,])
		}
	} else
	   classes=class

	
	#reorder db based on above classes
	if(!is.null(classes))
		db2  = sortdb(classes,db2)
	else{
		db2 = db2[-1,]
		next
	}


######
  #checks what type of thing db2 is refering to and inserts it
	if(db2$tpe[1] == 0)
		schedule <- filter(classes,isWorthy(Subject,Catalog,db2[1,]),(nchar(as.character(Meeting.Days)) > 1))
	else if(db2$tpe[1] == 1)
		schedule <- filter(classes,isWorthy(Subject,Catalog,db2[1,]),(nchar(as.character(Meeting.Days)) == 1),Etime -Stime > 1.5)
	else
		schedule <- filter(classes,isWorthy(Subject,Catalog,db2[1,]),(nchar(as.character(Meeting.Days)) == 1),Etime -Stime < 1.5)
	




#####

	sched <- rbind(sched,schedule[1,])
	sched <- na.omit(sched)
	db2 <- db2[-1,]
  }

sched
}

getMissing<-function(sched,db,classes){
	#Returns a dataframe of the classes that where not included
	ret <- NULL
	for(i in 1:length(db$subjCode)){
		chunks = makeSch(classes,db[i,])
		chunkNum = length(chunks$Catalog)
		classNum = filter(sched,isWorthy(Subject,Catalog,db[i,] ))
		classNum = length(classNum$Catalog )
		if(chunkNum != classNum){
			ret <- rbind(ret,db[i,])
		}
	}
	ret
}

parseInput <- function(inPut){
	#parses the text input
	inPut = trim(inPut)
	inPut = toupper(inPut)
	inChar = strsplit(as.character(inPut),NULL)[[1]]
	index = -1
	for (i in 1:length(inChar)){
		if(!is.na(as.integer(inChar[i] ) ) ){
			index = i
			break
		}
	}
	data.frame(subjCode = substr(inPut,1,index-1 ), courseNum =as.integer (substr(inPut,index,length(inChar ) )),stringsAsFactors = FALSE )
}



#ds = data.frame(read.csv('ps.csv'),stringsAsFactors = FALSE)
#ds = trim(ds)
#ds = filter(ds,Available.Seats > 0,Meeting.Days != '')



shinyServer(function(input, output,session) {
  ds = data.frame(read.csv('ps.csv'),stringsAsFactors = FALSE)
  ds = trim(ds)
  #ds = filter(ds,Available.Seats > 0,Meeting.Days != '')
   ds = filter(ds,Meeting.Days != '',!is.na(Class.End.Time),!is.na(Class.Start.Time))

  classList <- reactiveValues()
  clas <- reactiveValues()
  observeEvent(input$addClass,
			{
			   dq = parseInput(input$subjCode)	
			   if(TRUE %in% (ds$Subject == trim(toupper(dq$subjCode)) & ds$Catalog == trim(dq$courseNum) )){
                              classList$db <- rbind(classList$db,dq)
                              clas$classes <- filter(ds,isWorthy(Subject,Catalog,classList$db))
                              clas$classes <- convertTime(clas$classes)
			      classList$db <- sortdb(clas$classes,classList$db)
			      classList$db <- unique(classList$db)
			   }
			   else{
				 session$sendCustomMessage(type = 'generalErrorMessage', message = list('That class is not offered this semester'))
				}
			}
                            )
  output$classDB <- DT::renderDataTable({
     classList$db
  },selection = 'single')


  genSch <- reactive({
   if(input$full){
	clas$classes =  filter(ds,isWorthy(Subject,Catalog,classList$db))
   } else{
	clas$classes =  filter(ds,Available.Seats > 0,isWorthy(Subject,Catalog,classList$db))

   }
   clas$classes = convertTime(clas$classes)
   if(!is.null(clas$classes)&&!is.null(classList$db)){ 
    #generateSchedule(1,clas$classes,classList$db)
	a = makeSch(clas$classes,classList$db)
	missing = getMissing(a,classList$db,clas$classes)
	if(!is.null(missing)){
		 session$sendCustomMessage(type = 'generalErrorMessage',message = list(c('classes are missing',paste(missing$subjCode,missing$courseNum ))))
	}
	return(a)
   }
   else
	NULL
  })
  
  observeEvent(input$generSch,{
	if(!is.null(classList$db) && !length(classList$db$subjCode)<1){
	clas$sch <- genSch()	
	}
	else
		session$sendCustomMessage(type = 'generalErrorMessage',message = list('please add classes'))
	})
 observeEvent(input$swapClass,{
	if(!is.null(clas$sch) ){
		if(!is.null(input$schedule_rows_selected) && !is.null(input$otherClas_rows_selected)) {
			clas$sch[input$schedule_rows_selected,] <- clas$ot[input$otherClas_rows_selected,]
		}
		else
			session$sendCustomMessage(type = 'generalErrorMessage', message = list('you need to select a class'))
	}
	else
	{
		session$sendCustomMessage(type = 'generalErrorMessage', message = list('you need to generate  schedule'))
	}
	
 })

 #show alternate class options
output$otherClas <- renderDataTable({
  tmp = NULL
  if(!is.null(input$schedule_rows_selected)) { 
  sch = isolate(clas$sch)
  classes = isolate(clas$classes)
  claIndex = isolate(input$schedule_rows_selected)
  inClasses = classes[classes$Catalog == sch$Catalog[claIndex] & classes$Subject == sch$Subject[claIndex] & nchar(as.character(classes$Meeting.Days)) == nchar(as.character(sch$Meeting.Days[claIndex])),]
  inClasses = inClasses[(inClasses$Etime - inClasses$Stime) == (sch$Etime[claIndex] - sch$Stime[claIndex]),]
  inClasses = filter(inClasses,Section != sch$Section[claIndex])
  for( i in 1:length(inClasses$Subject)) {
  
    if(!conflicts(inClasses[i,],sch))
      tmp = rbind(tmp,inClasses[i,])
  }
  }
  clas$ot <- tmp
  tmp[,c("Class.Nbr","Subject","Catalog","Section","Description","Available.Seats","Class.Start.Time","Class.End.Time","Meeting.Days")] 
},selection = 'single')	


 output$schedule <- renderDataTable({
   clas$sch[,c("Class.Nbr","Subject","Catalog","Section","Description","Available.Seats","Class.Start.Time","Class.End.Time","Meeting.Days")] 
   },selection = 'single')
 observeEvent(input$rmClass,{
	if(!is.null(input$classDB_rows_selected)){
	classList$db <- classList$db[-input$classDB_rows_selected,]}
	 else{
                session$sendCustomMessage(type = 'generalErrorMessage', message = list('you need to select a row'))
        }

})
 
 output$calendar <-renderDataTable({
   tmp = NULL
   
   if(is.null(input$interval) || is.na(input$interval))
      return(NULL)
   if(!is.null(clas$sch) && input$interval > 0)
   {
     sch = isolate(clas$sch)
     interval = isolate(input$interval)
     for(i in 0:((18 - 8)*4)){
       tmp = rbind(tmp,data.frame(Time = (i * interval) / 60 + 8,Mon = "",Tues = "",Weds = "",Thurs = "",Fri ="",stringsAsFactors = FALSE ))
     }
     for(i in 1:length(sch$Subject)){
       print( strsplit(as.character(sch$Meeting.Days[i])[[1]],NULL))
       days = strsplit(as.character(sch$Meeting.Days[i]),NULL)[[1]]
       if( 'M' %in% days)
       tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Mon <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( "T" %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Tues <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( 'W' %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Weds <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( 'R' %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Thurs <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( 'F' %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Fri <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
     }
     
   }
   tmp
 },selection = 'single')
 
 output$schedDown <- downloadHandler(function(){"schedule.csv"},function(file){
   tmp = NULL
   
   if(is.null(input$interval) || is.na(input$interval))
     return(NULL)
   if(!is.null(clas$sch) && input$interval > 0)
   {
     sch = isolate(clas$sch)
     interval = isolate(input$interval)
     for(i in 0:((18 - 8)*4)){
       tmp = rbind(tmp,data.frame(Time = (i * interval) / 60 + 8,Mon = "",Tues = "",Weds = "",Thurs = "",Fri ="",stringsAsFactors = FALSE ))
     }
     for(i in 1:length(sch$Subject)){
       print( strsplit(as.character(sch$Meeting.Days[i])[[1]],NULL))
       days = strsplit(as.character(sch$Meeting.Days[i]),NULL)[[1]]
       if( 'M' %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Mon <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( "T" %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Tues <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( 'W' %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Weds <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( 'R' %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Thurs <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
       
       if( 'F' %in% days)
         tmp[sch$Etime[i] >= tmp$Time & sch$Stime[i] <= tmp$Time,]$Fri <- paste(sch$Subject[i],sch$Catalog[i],sch$Section[i])
     }
     
   }
   tmp
   write.csv(file = file,tmp)
 })
  
})
