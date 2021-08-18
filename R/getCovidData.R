# want these bits of info:

# age
# sex
# handedness
# covid diagnosis
# no neurological conditions

# tasks are only in part 1

# age, sex and handedness, opiate use and neurological conditions are in demographics.csv

getCovidData <- function() {
  
  demographics <- read.csv("/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/demographics1.csv", stringsAsFactors=F)
  columns <- c("What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",           
               "Sex....Selected.Choice",                                                                                
               "Sex....Other...Text",                                                                                   
               "What.is.your.handedness.",                                                                              
               "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",       
               "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",                          
               "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
               "id")
  demographics <- demographics[columns]
  names(demographics) <- c('age','sex','sex_descript','handedness','recreational_opiate_use','neurological_condition','neurological_condition_descript','id')
  
  # covid19:
  
  covid19 <- read.csv("/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/covid19Part1.csv", stringsAsFactors = F)
  
  columns <- c("Have.you.suffered.from.COVID.19....Selected.Choice",
               "Have.you.suffered.from.COVID.19....Yes..my.diagnosis.was.confirmed.with.a.positive.test.result..Please.specify.when..mm.dd.yyyy.....Text", 
               "Have.you.suffered.from.COVID.19....Yes..I.had.all.the.symptoms.but.I.did.not.receive.a.test..Please.specify.when..mm.dd.yyyy.....Text", 
               "What.prevented.you.from.getting.a.test.to.confirm.your.diagnosis.",                                                                                                                                                                                                                                                  
                "Were.you.hospitalized.for.COVID.19.",                                                                                                                                                                                                                                                                                
                "Did.you.have.any.serious.symptoms.",                                                                                                                                                                                                                                                                                 
                "When.did.your.serious.symptoms.emerge...Please.enter.a.date.in.this.format..mm.dd.yyyy.",                                                                                                                                                                                                                            
                "How.long.did.your.serious.symptoms.last...in.days.",                                                                                                                                                                                                                                                                 
                "If.you.are.no.longer.positive.for.COVID.19..do.you.feel.that.you.still.suffer.from.COVID.19.symptoms.",                                                                                                                                                                                                              
                "While.suffering.from.COVID.19.were.you.able.to.live.alone.without.assistance.from.another.person...e.g..independently.being.able.to.eat..walk..use.the.toilet.and.manage.routine.daily.hygiene.",                                                                                                                    
                "While.suffering.from.COVID.10.were.there.duties.activities.at.home.or.at.work.which.you.were.no.longer.able.to.perform.yourself.",                                                                                                                                                                                   
                "While.suffering.from.COVID.19.did.you.suffer.from.symptoms..pain..depression.or.anxiety.",                                                                                                                                                                                                                           
                "While.suffering.from.COVID.19.did.you.need.to.avoid.or.reduce.duties.activities.or.spread.these.over.time.",                                                                                                                                                                                                         
                "Are.you.still.in.the.recovery.period.for.COVID.19.",                                                                                                                                                                                                                                                                 
                "Can.you.live.alone.without.assistance.from.another.person...e.g..independently.being.able.to.eat..walk..use.the.toilet.and.manage.routine.daily.hygiene.",                                                                                                                                                           
                "Are.there.duties.activities.at.home.or.at.work.which.you.are.no.longer.able.to.perform.yourself.",                                                                                                                                                                                                                   
                "Do.you.suffer.from.symptoms..pain..depression.or.anxiety.",                                                                                                                                                                                                                                                          
                "Do.you.need.to.avoid.or.reduce.duties.activities.or.spread.these.over.time.",                                                                                                                                                                                                                                        
                "How.much.are.you.currently.affected.in.your.everyday.life.by.COVID.19...Please.indicate.which.one.of.the.following.statements.applies.to.you.most.",
                "id")
  
  covid19 <- covid19[columns]
  names(covid19) <- c('suffered_from_covid', 'confirmed_diagnosis_text', 'unconfirmed_diagnosis_text','reason_for_not_getting_tested',                                                                                                                                                                                                                                                  
                      'hospitalized',                                                                                                                                                                                                                                                                                
                      'serious_symptoms',                                                                                                                                                                                                                                                                                 
                      'date_serious_symptoms_emerged',                                                                                                                                                                                                                            
                      'length_of_serious_symptoms_days',                                                                                                                                                                                                                                                                 
                      'If.you.are.no.longer.positive.for.COVID.19..do.you.feel.that.you.still.suffer.from.COVID.19.symptoms.',                                                                                                                                                                                                              
                      'While.suffering.from.COVID.19.were.you.able.to.live.alone.without.assistance.from.another.person...e.g..independently.being.able.to.eat..walk..use.the.toilet.and.manage.routine.daily.hygiene.',                                                                                                                    
                      'While.suffering.from.COVID.10.were.there.duties.activities.at.home.or.at.work.which.you.were.no.longer.able.to.perform.yourself.',                                                                                                                                                                                   
                      'While.suffering.from.COVID.19.did.you.suffer.from.symptoms..pain..depression.or.anxiety.',                                                                                                                                                                                                                           
                      'While.suffering.from.COVID.19.did.you.need.to.avoid.or.reduce.duties.activities.or.spread.these.over.time.',                                                                                                                                                                                                         
                      'Are.you.still.in.the.recovery.period.for.COVID.19.',                                                                                                                                                                                                                                                                 
                      'Can.you.live.alone.without.assistance.from.another.person...e.g..independently.being.able.to.eat..walk..use.the.toilet.and.manage.routine.daily.hygiene.',                                                                                                                                                           
                      'Are.there.duties.activities.at.home.or.at.work.which.you.are.no.longer.able.to.perform.yourself.',                                                                                                                                                                                                                   
                      'Do.you.suffer.from.symptoms..pain..depression.or.anxiety.',                                                                                                                                                                                                                                                          
                      'Do.you.need.to.avoid.or.reduce.duties.activities.or.spread.these.over.time.',                                                                                                                                                                                                                                        
                      'How.much.are.you.currently.affected.in.your.everyday.life.by.COVID.19...Please.indicate.which.one.of.the.following.statements.applies.to.you.most.','id')
  
  
  
  # are there some exclusion criteria in the generic session info?
  
  session <- read.csv("/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/session1.csv", stringsAsFactors = F)
  columns <- c("Start.Date",                                                                                                                                                                                                                                     
               "Progress",                                                                                                                                                                                                                                       
               "Finished",                                                                                                                                                                                                                                       
               "Informed.Consent.Form",                                                                                                                                                                                                                         
               "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",                                                                                                                                                   
               "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",                                                                                                                                        
               "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.",
               "id" )
  session <- session[columns]
  
  names(session) <- c('start_date','progress','finished','informed_consent','need_corrective_device','wear_corrective_device','session_problems','id')
  
  
  
  
  # combine all the info:
  info <- cbind(demographics, covid19, session)
  info <- info[!duplicated(names(info))]
  
  # select participants:
  info <- info[which(info$informed_consent == 'I agree to participate in this study'),]
  info <- info[which(info$wear_corrective_device != 'No'),]
  # 55 duplicate participants left... which ones have the right info?
  # let's remove unfinished ones (as there is missing info):
  # or do we remove all participants that have any duplicated info?
  info <- info[which(info$finished == 'TRUE'),]
  # now there are 6 participants with duplicate info left... they finished it multiple times?
  
  # remove later instances or remove completely?
  
  # remove completely: (what are they DOING?!)
  double_info <- unique(info$id[which(duplicated(info$id))])
  info <- info[which(!(info$id %in% double_info)),]
  
  # remove people with neurological conditions, except migraine
  idx <- which(info$neurological_condition == 'No' | info$neurological_condition_descript == "headache and migraine ")
  info <- info[idx,]
  
  # remove those using opiates
  #info <- info[which(info$recreational_opiate_use == "No"),]
  
  # assign two cannabis use groups:
  
  info$covid_diagnosis <- NA
  info$covid_diagnosis[which(info$suffered_from_covid == 'No')] <- 'No'
  info$covid_diagnosis[which(info$suffered_from_covid == 'Yes, my diagnosis was confirmed with a positive test result. Please specify when (mm/dd/yyyy).')]<- 'ConfirmedDiagnosis'
  info$covid_diagnosis[which(info$suffered_from_covid == 'Yes, I had all the symptoms but I did not receive a test. Please specify when (mm/dd/yyyy).')]<- 'UnconfirmedDiagnosis'
  
  
  # remove all those without an answer or prefer not to say:
  #info <- info[which(info$covid_diagnosis %in% c('No', 'ConfirmedDiagnosis','UnconfirmedDiagnosis')),]
  
  cat('GENERIC\n')
  print(table(info$covid_diagnosis))
  
  # now merge with task data...
  vs <- getGroupPerformance('2020', 'fall', 'visualsearch')
  vs <- vs[which(vs$passedscreening == TRUE),]
  gng <- getGroupPerformance('2020', 'fall', 'gonogo')
  gng <- gng[which(gng$passedscreening == TRUE),]
  nBack <- getGroupPerformance('2020', 'fall', 'nBack')
  nBack <- nBack[which(nBack$passedscreening == TRUE),]
  tswitch<- getGroupPerformance('2020', 'fall', 'taskswitching')
  print(tswitch)
  tswitch <- tswitch[which(tswitch$passedscreening == TRUE),]
  
  vs    <- merge(info, vs,    by.x='id', by.y='participant', all=FALSE)
  gng   <- merge(info, gng,   by.x='id', by.y='participant', all=FALSE)
  nBack <- merge(info, nBack, by.x='id', by.y='participant', all=FALSE)
  tswitch <- merge(info, tswitch, by.x='id', by.y='participant', all=FALSE)
  

  
  write.csv(vs, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/visualsearchCOVID.csv", row.names=FALSE)
  write.csv(gng, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/gonogoCOVID.csv", row.names=FALSE)
  write.csv(nBack, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/nBackCOVID.csv", row.names=FALSE)
  write.csv(tswitch, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/taskswitchingCOVID.csv", row.names=FALSE)
  
  
  cat('VISUAL SEARCH\n')
  print(table(vs$covid_diagnosis))
  cat('GO/NOGO\n')
  print(table(gng$covid_diagnosis))
  cat('N-BACK\n')
  print(table(nback$covid_diagnosis))
  cat('TASK SWITCHING\n')
  print(table(tswitch$covid_diagnosis))
  
  
  return(list('visualsearch'=vs,
              'gonogo'=gng,
              'nBack'=nBack, 
              'taskswithcing' = tswitch))
  

}