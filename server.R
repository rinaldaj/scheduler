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
    if(TRUE %in% (strsplit(as.character(cla$Meeting.Days[1]),NULL)[[1]] %in% strsplit(as.character(sch$Meeting.Days[i]),NULL)[[1]]))
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
		tmp = filter(classes,isWorthy(Subject,Catalog,subNums))
		tmp = filter(tmp,(nchar(as.character(Meeting.Days)) > 1)) 
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
	print("starting big loop")
	while(length(db$subjCode) > 0){
	classes = NULL
	if(!is.null(sched)){
		for(i in 1:length(class$Catalog)){
			if(!conflicts(class[i,],sched))
				classes = rbind(classes,class[i,])
		}
	}else
		classes = class
	print("starting generation of db2")
	db2 = NULL	
	for(i in 1:length(db$subjName))
	{
		lecture = filter(classes,isWorthy(Subject,Catalog,db[i,]),(nchar(as.character(Meeting.Days)) > 1))
		lab = filter(classes,isWorthy(Subject,Catalog,db[i,]),(nchar(as.character(Meeting.Days)) == 1))
		discussion = filter(lab, Etime -Stime > 1.5)
		lab = filter(lab,Etime - Stime < 1.5)
		print(db[1,])
		if(length(lecture$Stime) > 0)
			db2 = rbind(db2,data.frame(subjName = db$subjName[i],courseNum = db$courseNum[i],tpe = 0 ))
		if(length(lab$Stime) > 0)
			db2 = rbind(db2,data.frame(subjName = db$subjName[i],courseNum = db$courseNum[i],tpe = 1 ))
		if(length(discussion$Stime) > 0 )
			db2 = rbind(db2,data.frame(subjName = db$subjName[i],courseNum = db$courseNum[i],tpe = 2 ))
	}
	print("db2 generated")
	#reorder db based on above classes
	if(!is.null(classes))
		d  = sortdb(classes,db2)
	else{
		db = db[-1,]
		next
	}

#	lecture = filter(classes,isWorthy(Subject,Catalog,d[1,]),(nchar(as.character(Meeting.Days)) > 1))
#	lab = filter(classes,isWorthy(Subject,Catalog,d[1,]),(nchar(as.character(Meeting.Days)) == 1))
#	discussion = filter(lab, Etime -Stime > 1.5)
#	lab = filter(lab,Etime - Stime < 1.5)
#	schedule = lecture[1,]
######
	#inserts labs and discussions into schedule
	if(d$tpe[1] == 0)
		schedule <- filter(classes,isWorthy(Subject,Catalog,d[1,]),(nchar(as.character(Meeting.Days)) > 1))
	else if(d$tpe[1] == 1)
		schedule <- filter(classes,isWorthy(Subject,Catalog,d[1,]),(nchar(as.character(Meeting.Days)) == 1),Etime -Stime > 1.5)
	else
		schedule <- filter(classes,isWorthy(Subject,Catalog,d[1,]),(nchar(as.character(Meeting.Days)) == 1),Etime -Stime < 1.5)
		  


#####

	sched <- rbind(sched,schedule[1,])
	sched <- na.omit(sched)
	db <- db[-1,]
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
   ds = filter(ds,Meeting.Days != '')

  classList <- reactiveValues()
  clas <- reactiveValues()
  observeEvent(input$addClass,
			{
			   dq = parseInput(input$subjCode)	
			   if(TRUE %in% (ds$Subject == trim(toupper(dq$subjCode)) & ds$Catalog == trim(dq$courseNum) )){
                              classList$db <- rbind(classList$db,dq)
			      print(classList$db )
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
		if(!is.null(input$schedule_rows_selected)) {
			clas$sch <- swapClass(input$schedule_rows_selected,isolate(clas$sch),clas$classes)
		}
		else
			session$sendCustomMessage(type = 'generalErrorMessage', message = list('you need to select a class'))
	}
	else
	{
		session$sendCustomMessage(type = 'generalErrorMessage', message = list('you need to generate  schedule'))
	}
	
 })

 output$show <- reactive({
	(!is.null(input$schedule_rows_selected)&&!is.null(clas$sch ) )
})
 observeEvent(input$schedule_rows_selected,{
	
}
)

 output$schedule <- renderDataTable({clas$sch },selection = 'single')
 observeEvent(input$rmClass,{
	if(!is.null(input$classDB_rows_selected)){
	classList$db <- classList$db[-input$classDB_rows_selected,]}
	 else{
                session$sendCustomMessage(type = 'generalErrorMessage', message = list('you need to select a row'))
        }

})
  
})
