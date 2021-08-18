qualtrics1 <- data.frame(read.csv("qualtricspart1.csv"))

#cannabis
cannabis1 <- subset(qualtrics1, select = c(Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..:
    Do.you.have.your.driver.s.license....Other...Text
  , id))

write.csv(cannabis1, 
          "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/cannabis1.csv")

#COVID-19
covid19Part1 <- subset(qualtrics1, select = c(Have.you.suffered.from.COVID.19....Selected.Choice:
                                           If.your.family.friends.are.interested.in.participating.in.this.study..please.ask.them.to.email.us.at...canbyork.yorku.ca..Otherwise..you.can.provide.us.with.your.email.address..or.your.URPP.ID..if.you.would.like.to.remain.anonymous..and.we.will.contact.you.to.arrange.for.your.family.friends.to.participate., id ))
write.csv(covid19Part1, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/covid19Part1.csv")                 

#Session1
session1 <- subset(qualtrics1, select = c(Start.Date,
                                              Progress,
                                              Finished,
                                              Informed.Consent.Form,
                                              Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.,
                                              Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.,
                                              Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.,
                                              id))
write.csv(session1, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/session1.csv")                 


#Demographics 
demographics1 <- subset(qualtrics1, select = c(What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.,
               Sex....Selected.Choice,
               Sex....Other...Text,
               What.is.your.handedness.,
               Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.,
               Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice,
               Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text,
               id))
write.csv(demographics1, "/Users/asselal-bayati/Documents/R/SCM-Battery-main/data/2020/demographics1.csv")                 

