#techincalities ----

library(dplyr)
library(car)
library(psych)
library(readr)

#merge raw data ----
survey_321192_R_data_file1 <- read_csv("data/survey_321192_R_data_file_run1.csv")
survey_795331_R_data_file <- read_delim("data/survey_795331_R_data_file.csv",
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
survey_321192_R_data_file2 <- read_delim("data/survey_321192_R_data_file.csv",
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
data.raw.1 <- subset(survey_321192_R_data_file1, select = -c(8,17,134:194))
data.raw.2 <- subset(survey_795331_R_data_file, select = -c(16,133:193))
data.raw.3 <- subset(survey_321192_R_data_file2, select = -c(8:9,18,135:196))
data.all <- merge.data.frame(data.raw.1, data.raw.3, all.x = TRUE, all.y = TRUE)
data.all <-subset(data.all, select =
                -c(`submitdate. Date submitted`,
                   `lastpage. Last page`,
                   `seed. Seed`,
                   `G8Q00001.`,
                   `startdate. Date started`,
                   `datestamp. Date last action`,
                   `G7Q00001.  We would like to take this opportunity to say Thank You for taking the time to answer this questionnaire.      Please be assured, all data collected will be treated in the strictest confidence. You are free to withdraw your data from the research at any time by contacting MRes student Sever Sava ssava001@gold.ac.uk or the supervisor Dr Daniel Müllensiefen D.Mullensiefen@gold.ac.uk .     The completed research will help to gain an understanding of the factors that influence musical abilities in children. Moreover, we will gain an insight into how factors evolve from one age group to the other. You were chosen to take part in the study because of one of the relations cited during the questionnaire.      If you have any questions about the study or were unduly or unexpectedly affected by taking part, please feel free to feed it back to the researcher ssava001@gold.ac.uk or his supervisor Dr Daniel Müllensiefen D.Mullensiefen@gold.ac.uk . If you feel unable for whatever reason to talk with the researcher team, then please contact one of the Heads of Psychology at Goldsmiths, Jan de Fockert j.de-fockert@gold.ac.uk . You are also able to contact any of these people if you would like to withdraw your data any time. `,
                   `G6Q00001. Please indicate the most appropriate categeory.`,
                   `G5Q00005. Please indicate the most appropriate category:`,
                   `G2Q00004. In the following questionnaire, we would like to get to know something about this child's relationship with music.`))
data.raw.2 <-subset(data.raw.2, select =
                    -c(`submitdate. Date submitted`,
                       `lastpage. Last page`,
                       `seed. Seed`,
                       `G8Q00001.`,
                       `startdate. Date started`,
                       `datestamp. Date last action`,
                       `G7Q00001.  We would like to take this opportunity to say Thank You for taking the time to answer this questionnaire.      Please be assured, all data collected will be treated in the strictest confidence. You are free to withdraw your data from the research at any time by contacting MRes student Sever Sava ssava001@gold.ac.uk or the supervisor Dr Daniel Müllensiefen D.Mullensiefen@gold.ac.uk .     The completed research will help to gain an understanding of the factors that influence musical abilities in children. Moreover, we will gain an insight into how factors evolve from one age group to the other. You were chosen to take part in the study because of one of the relations cited during the questionnaire.      If you have any questions about the study or were unduly or unexpectedly affected by taking part, please feel free to feed it back to the researcher ssava001@gold.ac.uk or his supervisor Dr Daniel Müllensiefen D.Mullensiefen@gold.ac.uk . If you feel unable for whatever reason to talk with the researcher team, then please contact one of the Heads of Psychology at Goldsmiths, Jan de Fockert j.de-fockert@gold.ac.uk . You are also able to contact any of these people if you would like to withdraw your data any time. `,
                       `G6Q00001. Please indicate the most appropriate categeory.`,
                       `G5Q00005. Please indicate the most appropriate category:`,
                       `G2Q00004. In the following questionnaire, we would like to get to know something about this child's relationship with music.`))
data <- merge.data.frame(data.all, data.raw.2, all.x = TRUE, all.y = TRUE)

rm(survey_321192_R_data_file1, survey_321192_R_data_file2, survey_795331_R_data_file, data.raw.1, data.raw.2, data.raw.3, data.all)
print(table(data$`startlanguage. Start language`))

# describe raw data ----

print(table(data$`G6Q00016. Please indicate your gender.`))
mean(data$`G6Q00015. How old are you?`, na.rm=TRUE)
SD(data$`G6Q00015. How old are you?`, na.rm=TRUE)

#clean data ----

data$nas <- rowSums(!is.na(data))
print(table(data$nas,data$G1Q00005.))
data <- subset.data.frame(data, nas>55)
data$interviewtimez <- scale(data$`interviewtime. Total time`)
#print(table(data$interviewtimez,data$G1Q00005.))
data <- subset.data.frame(data, interviewtimez>-3.5)

print(table(data$`G5Q00001. How old is he/she?`,data$G1Q00005.))
data <- subset.data.frame(data, `G5Q00001. How old is he/she?`<11)
data <- subset.data.frame(data, `G5Q00001. How old is he/she?`>2)
print(table(data$`G2Q00002. How many children in that age group do you regularly spend time with?  [Please indicate a single number, different from 0.]`,data$G1Q00005.))
data <- subset.data.frame(data, `G2Q00002. How many children in that age group do you regularly spend time with?  [Please indicate a single number, different from 0.]`<500)
print(table(data$`G2Q00006. How many hours do you spend with this child on average every week?`,data$G1Q00005.))
data$`G2Q00006. How many hours do you spend with this child on average every week?` <- car::recode(data$`G2Q00006. How many hours do you spend with this child on average every week?`, "'200' ->  NA/ '170' ->  NA", as.factor=FALSE, as.numeric=FALSE, to.value="->",separator="/")

#describe final dataset ----
n.all <- mean(data$`G1Q00001. Have you read and understood the Research Participant Information Sheet?` == "A1", na.rm=TRUE)
print(table(data$`startlanguage. Start language`))
print(table(data$`G6Q00016. Please indicate your gender.`))
mean(data$`G6Q00015. How old are you?`, na.rm=TRUE)
SD(data$`G6Q00015. How old are you?`, na.rm=TRUE)
mean(data$`G5Q00001. How old is he/she?`, na.rm=TRUE)
SD(data$`G5Q00001. How old is he/she?`, na.rm=TRUE)

#Datenaufbereitung ----

data <- data %>%
  mutate_at(c(25:56), funs(car::recode(.,  "'A1' ->  '0'// 'A2' ->  '1'// 'A3' ->  '2'//'A4' ->  '3'//'A5' ->  '4'", as.factor=FALSE, as.numeric=TRUE, to.value="->",separator="//")))
data <- data %>%
  mutate_at(c(58:82), funs(car::recode(.,  "'A2' ->  '0'// 'A3' ->  '1'// 'A4' ->  '2'//'A5' ->  '3'//'A6' ->  '4'", as.factor=FALSE, as.numeric=TRUE, to.value="->",separator="//")))
data$`G3Q00001[SQ009]. The child... [... won't let him-/herself become immersed in sounds.]raw`	<-	data$`G3Q00001[SQ009]. The child... [... won't let him-/herself become immersed in sounds.]`
data$`G3Q00001[SQ010]. The child... [... shows little interest in memorising patterns.]raw` 	<-	data$`G3Q00001[SQ010]. The child... [... shows little interest in memorising patterns.]`
data$`G3Q00001[SQ011]. The child... [... often moves out of sync with music.]raw` 	<-	data$`G3Q00001[SQ011]. The child... [... often moves out of sync with music.]`
data$`G3Q00001[SQ014]. The child... [... does not enjoy spending time in musical environments.]raw` 	<-	data$`G3Q00001[SQ014]. The child... [... does not enjoy spending time in musical environments.]`
data$`G3Q00001[SQ015]. The child... [... does not seek to acquire music related objects.]raw` 	<-	data$`G3Q00001[SQ015]. The child... [... does not seek to acquire music related objects.]`
data$`G3Q00001[SQ016]. The child... [... does not mimic or imitate musicians or singers.]raw` 	<-	data$`G3Q00001[SQ016]. The child... [... does not mimic or imitate musicians or singers.]`
data$`G3Q00001[SQ017]. The child... [... doesn't seem to pick up on emotions conveyed by music.]raw` 	<-	data$`G3Q00001[SQ017]. The child... [... doesn't seem to pick up on emotions conveyed by music.]`
data$`G3Q00001[SQ40126]. The child... [... does not associate music with characters and stories.]raw` 	<-	data$`G3Q00001[SQ40126]. The child... [... does not associate music with characters and stories.]`
data$`G3Q00001[SQ018]. The child... [... has no association between music and routines (e.g. does not have a song for washing his/her hands).]raw` 	<-	data$`G3Q00001[SQ018]. The child... [... has no association between music and routines (e.g. does not have a song for washing his/her hands).]`
data$`G3Q00001[SQ023]. The child... [... does not insist on his/her own will when making and enjoying music.]raw` 	<-	data$`G3Q00001[SQ023]. The child... [... does not insist on his/her own will when making and enjoying music.]`
data$`G3Q00001[SQ024]. The child... [... doesn't repurpose music-neutral objects into musical instruments.]raw` 	<-	data$`G3Q00001[SQ024]. The child... [... doesn't repurpose music-neutral objects into musical instruments.]`
data$`G3Q00001[SQ025]. The child... [...does not seem curious to explore sounds in different environments.]raw` 	<-	data$`G3Q00001[SQ025]. The child... [...does not seem curious to explore sounds in different environments.]`
data$`G4Q00001[A15]. The child...  [... does not recognise melodies.]raw` 	<-	data$`G4Q00001[A15]. The child...  [... does not recognise melodies.]`
data$`G4Q00001[A16]. The child...  [... is easily distracted when engaged with musical activities.]raw` 	<-	data$`G4Q00001[A16]. The child...  [... is easily distracted when engaged with musical activities.]`
data$`G4Q00001[A17]. The child...  [... demonstrates little motivation to make music with others.]raw` 	<-	data$`G4Q00001[A17]. The child...  [... demonstrates little motivation to make music with others.]`
data$`G4Q00001[A20]. The child...  [... has difficulties in recognizing melodies and tone progressions.]raw` 	<-	data$`G4Q00001[A20]. The child...  [... has difficulties in recognizing melodies and tone progressions.]`
data$`G4Q00001[A21]. The child...  [... has issues reproducing melodies he/she has heard before.]raw`	<-	data$`G4Q00001[A21]. The child...  [... has issues reproducing melodies he/she has heard before.]`
data$`G4Q00001[A22]. The child...  [... does not distinguish between different musical instruments.]raw`	<-	data$`G4Q00001[A22]. The child...  [... does not distinguish between different musical instruments.]`
data$`G4Q00001[A25]. The child...  [... does not recognise composers or singers.]raw`	<-	data$`G4Q00001[A25]. The child...  [... does not recognise composers or singers.]`
data$`G4Q00001[A3]. The child...  [... shows difficulties in producing or reproducing music.]raw`	<-	data$`G4Q00001[A3]. The child...  [... shows difficulties in producing or reproducing music.]`
data$`G4Q00001[A4]. The child...  [... pays little attention while making music, so he/she does not realize if it sounds as intended.]` 	<-	data$`G4Q00001[A4]. The child...  [... pays little attention while making music, so he/she does not realize if it sounds as intended.]`
data$`G4Q00001[A6]. The child...  [... displays challenges in experiencing music with an open mind.]raw` 	<-	data$`G4Q00001[A6]. The child...  [... displays challenges in experiencing music with an open mind.]`
data$`G3Q00001[SQ009]. The child... [... won't let him-/herself become immersed in sounds.]`	<- 4-	data$`G3Q00001[SQ009]. The child... [... won't let him-/herself become immersed in sounds.]`
data$`G3Q00001[SQ010]. The child... [... shows little interest in memorising patterns.]` 	<- 4-	data$`G3Q00001[SQ010]. The child... [... shows little interest in memorising patterns.]`
data$`G3Q00001[SQ011]. The child... [... often moves out of sync with music.]` 	<- 4-	data$`G3Q00001[SQ011]. The child... [... often moves out of sync with music.]`
data$`G3Q00001[SQ014]. The child... [... does not enjoy spending time in musical environments.]` 	<- 4-	data$`G3Q00001[SQ014]. The child... [... does not enjoy spending time in musical environments.]`
data$`G3Q00001[SQ015]. The child... [... does not seek to acquire music related objects.]` 	<- 4-	data$`G3Q00001[SQ015]. The child... [... does not seek to acquire music related objects.]`
data$`G3Q00001[SQ016]. The child... [... does not mimic or imitate musicians or singers.]` 	<- 4-	data$`G3Q00001[SQ016]. The child... [... does not mimic or imitate musicians or singers.]`
data$`G3Q00001[SQ017]. The child... [... doesn't seem to pick up on emotions conveyed by music.]` 	<- 4-	data$`G3Q00001[SQ017]. The child... [... doesn't seem to pick up on emotions conveyed by music.]`
data$`G3Q00001[SQ40126]. The child... [... does not associate music with characters and stories.]` 	<- 4-	data$`G3Q00001[SQ40126]. The child... [... does not associate music with characters and stories.]`
data$`G3Q00001[SQ018]. The child... [... has no association between music and routines (e.g. does not have a song for washing his/her hands).]` 	<- 4-	data$`G3Q00001[SQ018]. The child... [... has no association between music and routines (e.g. does not have a song for washing his/her hands).]`
data$`G3Q00001[SQ023]. The child... [... does not insist on his/her own will when making and enjoying music.]` 	<- 4-	data$`G3Q00001[SQ023]. The child... [... does not insist on his/her own will when making and enjoying music.]`
data$`G3Q00001[SQ024]. The child... [... doesn't repurpose music-neutral objects into musical instruments.]` 	<- 4-	data$`G3Q00001[SQ024]. The child... [... doesn't repurpose music-neutral objects into musical instruments.]`
data$`G3Q00001[SQ025]. The child... [...does not seem curious to explore sounds in different environments.]` 	<- 4-	data$`G3Q00001[SQ025]. The child... [...does not seem curious to explore sounds in different environments.]`
data$`G4Q00001[A15]. The child...  [... does not recognise melodies.]` 	<- 4-	data$`G4Q00001[A15]. The child...  [... does not recognise melodies.]`
data$`G4Q00001[A16]. The child...  [... is easily distracted when engaged with musical activities.]` 	<- 4-	data$`G4Q00001[A16]. The child...  [... is easily distracted when engaged with musical activities.]`
data$`G4Q00001[A17]. The child...  [... demonstrates little motivation to make music with others.]` 	<- 4-	data$`G4Q00001[A17]. The child...  [... demonstrates little motivation to make music with others.]`
data$`G4Q00001[A20]. The child...  [... has difficulties in recognizing melodies and tone progressions.]` 	<- 4-	data$`G4Q00001[A20]. The child...  [... has difficulties in recognizing melodies and tone progressions.]`
data$`G4Q00001[A21]. The child...  [... has issues reproducing melodies he/she has heard before.]`	<- 4-	data$`G4Q00001[A21]. The child...  [... has issues reproducing melodies he/she has heard before.]`
data$`G4Q00001[A22]. The child...  [... does not distinguish between different musical instruments.]`	<- 4-	data$`G4Q00001[A22]. The child...  [... does not distinguish between different musical instruments.]`
data$`G4Q00001[A25]. The child...  [... does not recognise composers or singers.]`	<- 4-	data$`G4Q00001[A25]. The child...  [... does not recognise composers or singers.]`
data$`G4Q00001[A3]. The child...  [... shows difficulties in producing or reproducing music.]`	<- 4-	data$`G4Q00001[A3]. The child...  [... shows difficulties in producing or reproducing music.]`
data$`G4Q00001[A4]. The child...  [... pays little attention while making music, so he/she does not realize if it sounds as intended.]` 	<- 4-	data$`G4Q00001[A4]. The child...  [... pays little attention while making music, so he/she does not realize if it sounds as intended.]`

data.german <-  data[data$`startlanguage. Start language`=="de",]

#define groups ----
data$agegroup <- cut(data$`G5Q00001. How old is he/she?`,
                     breaks = c(0,6, 10),
                     labels = c('younger', 'older'))
print(table(data$agegroup,data$`G5Q00001. How old is he/she?`))
print(table(data$agegroup))
print(table(data$`G5Q00003. What type of daycare institution does he/she currently attend?`,data$agegroup))
#A1=school A2=Pre-School A3=Kindergarten/Nursery A4=neither
data$edugroup <- car::recode(data$`G5Q00003. What type of daycare institution does he/she currently attend?`,  "'A1' ->  'higher'/ 'A2' ->  'higher'/ 'A3' ->  'lower'/'A4' ->  NA", as.factor=FALSE, as.numeric=FALSE, to.value="->",separator="/")
print(table(data$edugroup))
print(table(data$agegroup,data$edugroup))
data$relationship <- (data$`G2Q00005[SQ001]. What is your relationship with this child?  [multiple answers possible]  [I am a parent/guardian.]`)*(-20)+data$`G2Q00005[SQ002]. What is your relationship with this child?  [multiple answers possible]  [I am a relative other than parent/guardian.]`*(-20)+data$`G2Q00005[SQ004]. What is your relationship with this child?  [multiple answers possible]  [I teach him/her music in school.]`+data$`G2Q00005[SQ005]. What is your relationship with this child?  [multiple answers possible]  [I am a teacher for a musical instrument in school.]`+data$`G2Q00005[SQ006]. What is your relationship with this child?  [multiple answers possible]  [I teach him/her another subject in school.]`+data$`G2Q00005[SQ007]. What is your relationship with this child?  [multiple answers possible]  [I am a musical educator in kindergarten/pre-school.  ]`+data$`G2Q00005[SQ008]. What is your relationship with this child?  [multiple answers possible]  [I am a non-musical educator in kindergarten/pre-school.]`
data$relationship <- as.numeric(data$relationship)
data$relationship <- cut(data$`relationship`,   breaks = c(-40,0,100),  labels = c('private', 'professional'))
print(table(data$relationship))
write.csv2(data, "Datenaufbereitet.csv")
