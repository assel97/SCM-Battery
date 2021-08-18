# want these bits of info:

# age
# sex
# handedness
# cannabis use frequency
# use opiates
# no neurological conditions

# tasks are only in part 1

# age, sex and handedness, opiate use and neurological conditions are in demographics.csv

getCannabisData <- function() {
  
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
  
  # cannabis use:
  
  cannabis <- read.csv("/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/cannabis1.csv", stringsAsFactors = F)
  columns <- c("Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",                                                                       
              "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",                                                                 
              "Primary.methods.of.intake..select.all.that.apply.....Selected.Choice",                                                                  
               "Primary.methods.of.intake..select.all.that.apply.....Other...Text",                                                                     
               "Why.do.you.usually.use.marijuana..select.all.that.apply.....Selected.Choice",                                                          
               "Why.do.you.usually.use.marijuana..select.all.that.apply.....Other...Text",                                                             
               "If.possible..describe.the.dose..THC.mg..or.number.of.hits.",                                                                         
               "How.often.do.you.combine.marijuana.with.alcohol...1.represents.never.and.5.represents.every.time.",                                  
               "When.was.the.last.time.you.used.marijuana.",                                                                                    
               "Do.you.foresee.yourself.using.marijuana.anytime.soon...select.the.best.response....Selected.Choice",                                   
                "Do.you.foresee.yourself.using.marijuana.anytime.soon...select.the.best.response....Other...Text",                                    
                "How.old.were.you.when.you.first.used.marijuana.or.cannabis......Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
                "Do.you.have.your.driver.s.license....Selected.Choice",                                                                                  
                "Do.you.have.your.driver.s.license....Other...Text",                                                                                     
                "id"  )
  cannabis <- cannabis[columns]
  
  names(cannabis) <- c('use_cannabis','cannabis_use_frequency','primary_intake_method_choice',
                       'primary_intake_method_text','reason_for_use_choice','reason_for_use_text',
                       'usual_dose','combine_alcohol_and_cannabis','last_used','forsee_using_again_choice',
                       'forsee_using_again_text','age_of_use_onset','drivers_license_choice','drivers_license_text','id')
  
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
  info <- cbind(demographics, cannabis, session)
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
  info <- info[which(info$recreational_opiate_use == "No"),]
  
  # assign two cannabis use groups:
  
  info$cannabis_group <- NA
  info$cannabis_group[which(info$use_cannabis == 'No')] <- 'non-user'
  info$cannabis_group[which(info$cannabis_use_frequency %in% c('Daily','Once a week', 'A few times a week'))] <- 'frequent-user'
  info$cannabis_group[which(info$cannabis_use_frequency %in% c('Once or twice within these past 3 months', 'A few times a month','Around once a month'))] <- 'nonfrequent-user'
  
  
  # remove all the intermediate users:
  info <- info[which(info$cannabis_group %in% c('non-user', 'frequent-user', 'nonfrequent-user')),]
  
  cat('GENERIC\n')
  print(table(info$cannabis_group))
  
  # now merge with task data...
  vs <- getGroupPerformance('2020', 'fall', 'visualsearch')
  vs <- vs[which(vs$passedscreening == TRUE),]
  gng <- getGroupPerformance('2020', 'fall', 'gonogo')
 gng <- gng[which(gng$passedscreening == TRUE),]
  nBack <- getGroupPerformance('2020', 'fall', 'nBack')
 nBack <- nBack[which(nBack$passedscreening == TRUE),]
 tswitch<- getGroupPerformance('2020', 'fall', 'taskswitching')
 tswitch <- nBack[which(tswitch$passedscreening == TRUE),]
  
  vs    <- merge(info, vs,    by.x='id', by.y='participant', all=FALSE)
  gng   <- merge(info, gng,   by.x='id', by.y='participant', all=FALSE)
  nBack <- merge(info, nBack, by.x='id', by.y='participant', all=FALSE)
  tswitch <- merge(info, tswitch, by.x='id', by.y='participant', all=FALSE)
  
  
  write.csv(vs, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/visualsearchCannabis.csv", row.names=FALSE)
  write.csv(gng, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/gonogoCannabis.csv", row.names=FALSE)
  write.csv(nBack, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/nBackCannabis.csv", row.names=FALSE)
  write.csv(tswitch, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/taskswitchingCannabis.csv", row.names=FALSE)
  
  
  cat('VISUAL SEARCH\n')
  print(table(vs$cannabis_group))
  cat('GO/NOGO\n')
  print(table(gng$cannabis_group))
  cat('N-BACK\n')
  print(table(nback$cannabis_group))
  cat('TASK SWITCHING\n')
  print(table(tswitch$cannabis_group))
  
  
  return(list('visualsearch'=vs,
              'gonogo'=gng,
              'nBack'=nBack, 
              'taskswithcing' = tswitch))
  
}