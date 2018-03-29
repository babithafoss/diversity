#libraries
library(dplyr)
library(ggplot2)

# Read csv files
negative_incidents <- read.csv("E:/git/data_for_public_release/data_for_public_release/negative_incidents.csv", stringsAsFactors=FALSE)


#Segreggate data  negative witness, experience, response, consequence
negative.witness <- negative_incidents$NEGATIVE.WITNESS.RUDENESS | negative_incidents$NEGATIVE.WITNESS.NAME.CALLING |negative_incidents$NEGATIVE.WITNESS.THREATS |negative_incidents$NEGATIVE.WITNESS.IMPERSONATION |negative_incidents$NEGATIVE.WITNESS.SUSTAINED.HARASSMENT | negative_incidents$NEGATIVE.WITNESS.CROSS.PLATFORM.HARASSMENT | negative_incidents$NEGATIVE.WITNESS.STALKING | negative_incidents$NEGATIVE.WITNESS.SEXUAL.ADVANCES| negative_incidents$NEGATIVE.WITNESS.STEREOTYPING | negative_incidents$NEGATIVE.WITNESS.DOXXING | negative_incidents$NEGATIVE.WITNESS.OTHER 
negative.experience <- negative_incidents$NEGATIVE.EXPERIENCE.RUDENESS | negative_incidents$NEGATIVE.EXPERIENCE.NAME.CALLING|negative_incidents$NEGATIVE.EXPERIENCE.THREATS|negative_incidents$NEGATIVE.EXPERIENCE.IMPERSONATION | negative_incidents$NEGATIVE.EXPERIENCE.SUSTAINED.HARASSMENT | negative_incidents$NEGATIVE.EXPERIENCE.CROSS.PLATFORM.HARASSMENT | negative_incidents$NEGATIVE.WITNESS.STALKING | negative_incidents$NEGATIVE.EXPERIENCE.SEXUAL.ADVANCES|negative_incidents$NEGATIVE.EXPERIENCE.STEREOTYPING|negative_incidents$NEGATIVE.EXPERIENCE.DOXXING|negative_incidents$NEGATIVE.EXPERIENCE.OTHER
negative.response <- negative_incidents$NEGATIVE.RESPONSE.ASKED.USER.TO.STOP |negative_incidents$NEGATIVE.RESPONSE.BLOCKED.USER|negative_incidents$NEGATIVE.RESPONSE.SOLICITED.COMMUNITY.SUPPORT|negative_incidents$NEGATIVE.RESPONSE.REPORTED.TO.MAINTAINERS|negative_incidents$NEGATIVE.RESPONSE.REPORTED.TO.HOST.OR.ISP|negative_incidents$NEGATIVE.RESPONSE.CONSULTED.LEGAL.COUNSEL|negative_incidents$NEGATIVE.RESPONSE.CONTACTED.LAW.ENFORCEMENT|negative_incidents$NEGATIVE.RESPONSE.OTHER
negative.consequence <- negative_incidents$NEGATIVE.CONSEQUENCES.STOPPED.CONTRIBUTING| negative_incidents$NEGATIVE.CONSEQUENCES.PSEUDONYM|negative_incidents$NEGATIVE.CONSEQUENCES.WORK.IN.PRIVATE|negative_incidents$NEGATIVE.CONSEQUENCES.CHANGE.USERNAME|negative_incidents$NEGATIVE.CONSEQUENCES.CHANGE.ONLINE.PRESENCE|negative_incidents$NEGATIVE.CONSEQUENCES.SUGGEST.COC|negative_incidents$NEGATIVE.CONSEQUENCES.PRIVATE.COMMUNITY.DISCUSSION|negative_incidents$NEGATIVE.CONSEQUENCES.PUBLIC.COMMUNITY.DISCUSSION|negative_incidents$NEGATIVE.CONSEQUENCES.OFFLINE.CHANGES|negative_incidents$NEGATIVE.CONSEQUENCES.OTHER

#Prescence of any kind of negetive witness or experience
negative.witnexp  <- negative.witness | negative.experience

#Create dataframes

github.df <- negative_incidents %>% mutate(negative.witnexp ,negative.consequence) %>% filter(negative_incidents$POPULATION %in% c("github") ) %>%  select(negative.witnexp,negative.consequence, POPULATION)

offsite.df <- negative_incidents %>% mutate(negative.witnexp ,negative.consequence) %>% filter(negative_incidents$POPULATION %in% c("off site community") ) %>%  select(negative.witnexp,negative.consequence, POPULATION)

ggplot(github.df, aes(x=negative.witnexp , y=negative.consequence))+geom_count()

ggplot(github.df, aes(x=negative.witnexp  , y=negative.consequence))+geom_count()+xlab("Negative Witness and Experience")+ylab("Negative Consequence")+ggtitle("Density of Addressed incidents in Github")
ggplot(offsite.df, aes(x=negative.witnexp  , y=negative.consequence))+geom_count()+xlab("Negative Witness and Experience")+ylab("Negative Consequence")+ggtitle("Density of Addressed incidents in off site community")
