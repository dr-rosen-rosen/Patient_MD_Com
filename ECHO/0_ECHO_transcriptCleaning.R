library(tidyverse)
library(xlsx)
library(stringr)
library(here)
library(config)
library(haven)
library(skimr)
library(vader)


Sys.setenv(R_CONFIG_ACTIVE = "salar") # 'default')#
config <- config::get()

ECHO_Transcripts_Complete_TbyT <- read_csv(here(config$ECHO_Transcript_path, config$ECHO_Transcript_name))

#deleting first column of data
ECHO_Transcripts_Complete_TbyT <- select(ECHO_Transcripts_Complete_TbyT, -c(...1))

ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
  mutate(overall_sequence = 1:n())


###########NOTES: BELOW ARE DETAILS ABOUT INDIVIDUAL TRANSCRIPTS###############
#D1 transcripts:   
#JC10P01(D1-D-P all conv- exclude), JC09P06(D1-D-P all conv- exclude),  OC03P01(Lots of D1-D-UF-P conv, exclude), 
#D2 transcripts:  
#NP transcripts: , JC04P03(NP-P conv- exclude), SC18P094 (NP-P conv- exclude)
#PA transcripts: , JC06P06(multiple patients), 
# SC06P075(was this conversation cut too early? maybe exclude),
#JC01P09... Exclude.
#OC03P07- PM and PF in conv. Exclude. 
#SC12P051- Lots of conversation including patient family
# JC05P03- lots of conversation with family
# OC02P04- other speakers involved in conv.
# OC08P06- other speakers involved in conv.
# OC02P06- other speakers involved in conv. 
# OC04P08- others speakers involved in conv
# OC06P03- other speakers involved in conv.
# OC04P06- other speakers involved in conv. 
#SC12P040- multiple speakers throughout conv
#OC05P10- multiple speakers throughout conv
#OC03P02- multiple speakers throughout conv
# OC01P08- multiple speakers throughout conv
# OC04P10- multiple speakers throughout conv
#OC06P09- multiple speakers throughout conv
#SC24P124- multiple speakers throughout conv
#SC18P134- multiple speakers throughout conv
#D1 transcripts: JC11P03, JC08P09, JC08P10, JC11P06, JC08P06, JC11P01, JC11P09, JC08P03, 
#JC11P04, JC11P07, JC11P02, JC10P01, JC09P06, JC11P08, JC11P05, JC08P07, OC03P01, SC24P122,
#SC18P110, SC24P114
#D2 transcripts: JC08P01, JC11P01, JC08P03, JC09P06, JC08P02,
#NP transcripts: JC11P05, JC04P03, SC18P094 
#PA transcripts: JC08P01, JC06P06, JC01P08, OC08P10, OC07P11, 
#OC08P09, OC07P12, OC05P07, SC12P062, SC06P075, SC12P048
#RN transcripts: OC07P02, OC06P02, SC07P006, SC06P101
ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
  #JC11P03: deleting from beginning to end of D1-P conversation
  filter(!(overall_sequence >= 111 & overall_sequence <= 156 | 
             overall_sequence >= 1 & overall_sequence <= 4)) %>%
  #JC08P09: deleting from beginning to end of D1-UM conversation
  filter(!(overall_sequence >= 4591 & overall_sequence <= 4597)) %>%
  #JC08P10: deleting from beginning to end of D1-UM conversation
  filter(!(overall_sequence >= 5760 & overall_sequence <= 5863)) %>%
  #JC11P06: deleting from beginning to end of D-D1-P conversation and
  #at the end of the conversation once the D-P conversation ends
  filter(!(overall_sequence >= 8135 & overall_sequence <= 8141 |
             overall_sequence >= 8361 & overall_sequence <= 8490 |
             overall_sequence >= 8524 & overall_sequence <= 8538)) %>%
  #JC08P06: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 14756 & overall_sequence <= 14789)) %>%
  #JC11P01: deleting from beginning to end of D-D1-P conversation & D-D1-D2-P conv.
  filter(!(overall_sequence >= 15570 & overall_sequence <= 15662 |
             overall_sequence >= 15683 & overall_sequence <= 15794 )) %>%
  #JC11P09: deleting from beginning to end of D1-P conversation
  filter(!(overall_sequence >= 22438 & overall_sequence <= 22506)) %>%
  #JC08P03: deleting sequences with minor speech and D-D1-P conv.
  filter(!(overall_sequence >= 23174 & overall_sequence <= 23200 |
             overall_sequence >= 23347 & overall_sequence <= 23352 |
             overall_sequence >= 23549 & overall_sequence <= 23575 |  
             overall_sequence >= 23574 & overall_sequence <= 23642 |   
             overall_sequence >= 23680 & overall_sequence <= 23777)) %>%
  #JC11P04: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 26221 & overall_sequence <= 26410)) %>%
  #JC11P07: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 27674 & overall_sequence <= 27847 |
             overall_sequence >= 27660 & overall_sequence <= 27663  )) %>%
  #JC11P02: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 34612 & overall_sequence <= 34704)) %>%
  #JC11P08: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 39866 & overall_sequence <= 39953)) %>%
  #JC11P05: deleting from RA/D conv and beginning to end of D-D1-P & D-NP-P conversations
  filter(!(overall_sequence >= 41538 & overall_sequence <= 41545 |
             overall_sequence >= 41820 & overall_sequence <= 41927)) %>%
  #JC08P07: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 44583 & overall_sequence <= 44603)) %>%
  #SC24P122: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 96319 & overall_sequence <= 96324)) %>%
  #SC18P110: deleting from beginning to end of D-D1-P conversation
  filter(!(overall_sequence >= 106608 & overall_sequence <= 106645)) %>%
  #SC24P114: deleting from beginning to end of D-D1-P-UM conversation
  filter(!(overall_sequence >= 121311 & overall_sequence <= 121593)) %>%
  #JC08P01: deleting from beginning to end of D-D1-P & D-D2-P conversation
  filter(!(overall_sequence >= 10747 & overall_sequence <= 10752 |
             overall_sequence >= 10792 & overall_sequence <= 10884)) %>%
  #JC08P02: deleting from beginning to end of D-D2-P conversation
  filter(!(overall_sequence >= 41416 & overall_sequence <= 41513)) %>%
  #JC01P08: deleting from beginning to end of D-PA conversation
  filter(!(overall_sequence >= 48104 & overall_sequence <= 48118)) %>%
  #OC08P10: deleting from beginning to end of D-PA conversation
  filter(!(overall_sequence >= 61639 & overall_sequence <= 61665)) %>%
  #OC07P11: deleting from beginning to end of D-PA-P conversation
  filter(!(overall_sequence >= 64298 & overall_sequence <= 64302)) %>%
  #OC05P07: deleting from beginning to end of P-PA conversation
  filter(!(overall_sequence >= 76586 & overall_sequence <= 76594)) %>%
  #SC12P062: deleting from beginning to end of D-PA conversation
  filter(!(overall_sequence >= 99145 & overall_sequence <= 99149)) %>%
  #SC12P048: deleting from beginning to end of D-RA and D-PA conversations
  filter(!(overall_sequence >= 116497 & overall_sequence <= 116500 |
             overall_sequence >= 116593 & overall_sequence <= 116598)) %>%
  #OC07P02: deleting from beginning to end of RN-P conversation
  filter(!(overall_sequence >= 58104 & overall_sequence <= 58122)) %>%
  #OC06P02: deleting from beginning to end of RN-P conversation
  filter(!(overall_sequence >= 75945 & overall_sequence <= 75949 |
             overall_sequence >= 76170 & overall_sequence <= 76180)) %>%
  #SC06P101: deleting from beginning to end of RN-D conversation
  filter(!(overall_sequence >= 112035 & overall_sequence <= 112037)) %>%
  #JC01P01: deleting from beginning to end of RN-D-P conversation
  filter(!(overall_sequence >= 23129 & overall_sequence <= 23173)) %>%
  #OC07P12: deleting from beginning to end of PA-D conversation
  filter(!(overall_sequence >= 67529 & overall_sequence <= 67531)) %>%
  #OC03P07: deleting from beginning to end of PA-D conversation
  filter(!(overall_sequence >= 67529 & overall_sequence <= 67531)) %>%
  #SC20P068: deleting RA/P & conversation with family members involved
  filter(!(overall_sequence >= 112603 & overall_sequence <= 112610 |
             overall_sequence >= 112998 & overall_sequence <= 113251)) %>%
  #OC07P03: deleting conversation with other speakers
  filter(!(overall_sequence >= 69797 & overall_sequence <= 69806 |
             overall_sequence >= 70094 & overall_sequence <= 70097)) %>%
  #OC03P06: deleting conversation with other speakers
  filter(!(overall_sequence >= 71512 & overall_sequence <= 71516 |
             overall_sequence >= 71824 & overall_sequence <= 71830)) %>%
  #OC04P04: deleting conversation with other speakers
  filter(!(overall_sequence >= 76782 & overall_sequence <= 76784 |
             overall_sequence >= 76810 & overall_sequence <= 76812 |
             overall_sequence >= 76862 & overall_sequence <= 76896)) %>%
  #OC01P07: deleting RA/D/P & conversation at end without doctor
  filter(!(overall_sequence >= 77778 & overall_sequence <= 77793 |
             overall_sequence >= 78190 & overall_sequence <= 78195)) %>%
  #OC03P04: deleting conversation with UM/P & UF & RA/P
  filter(!(overall_sequence >= 80160 & overall_sequence <= 80168 |
             overall_sequence >= 80173 & overall_sequence <= 80178 |
             overall_sequence >= 80446 & overall_sequence <= 80452)) %>%
  #SC13P083: deleting conversation with UM/P & UF
  filter(!(overall_sequence >= 83964 & overall_sequence <= 83969)) %>%
  #SC12P016: deleting conversation with UM/U/P
  filter(!(overall_sequence >= 85819 & overall_sequence <= 85822)) %>%
  #SC23P056: deleting conversation with UM/U/P
  filter(!(overall_sequence >= 84731 & overall_sequence <= 84736)) %>%
  #SC12P038: deleting conversation with D/UF/P & a exchange in spanish
  filter(!(overall_sequence >= 88040 & overall_sequence <= 88046 |
             overall_sequence >= 87789 & overall_sequence <= 87790  )) %>%
  #SC21P133: deleting conversation with D/UM
  filter(!(overall_sequence >= 88933 & overall_sequence <= 88936)) %>%
  #SC15P116: deleting conversation with D/UM/P conv
  filter(!(overall_sequence >= 89400 & overall_sequence <= 89421)) %>%
  #SC15P111: deleting conversation with D/UM/P conv
  filter(!(overall_sequence >= 95704 & overall_sequence <= 95707 |
             overall_sequence >= 95852 & overall_sequence <= 95855)) %>%
  #SC23P046: deleting conversation with D/UM conv
  filter(!(overall_sequence >= 108979 & overall_sequence <= 108994)) %>%
  #SC20P074: deleting conversation with D/UM/P conv
  filter(!(overall_sequence >= 115743 & overall_sequence <= 115768 |
             overall_sequence >= 115780 & overall_sequence <= 115790)) %>%
  #SC20P092: deleting conversation with P/UM conv
  filter(!(overall_sequence >= 122247 & overall_sequence <= 122273)) %>%
  #JC04P09: deleting conversation with P/UM conv
  filter(!(overall_sequence >= 38250 & overall_sequence <= 38284)) %>%
  #SC21P126: deleting conversation with RA/D & P/UM conv
  filter(!(overall_sequence >= 107328 & overall_sequence <= 107330 |
             overall_sequence >= 107565 & overall_sequence <= 107571)) %>%
  #SC19P053: deleting conversation with D/UF conv
  filter(!(overall_sequence >= 120601 & overall_sequence <= 120609)) %>%
  #OC03P05: deleting conversation with RA/P & P/UM conv
  filter(!(overall_sequence >= 60148 & overall_sequence <= 60156 |
             overall_sequence >= 60572 & overall_sequence <= 60574)) %>%
  #OC01P09: deleting conversation with P/UM conv
  filter(!(overall_sequence >= 54868 & overall_sequence <= 54875 |
             overall_sequence >= 54994 & overall_sequence <= 54995)) %>%
  #JC14P01: deleting conversation with P/UM conv
  filter(!(overall_sequence >= 36610 & overall_sequence <= 36614 |
             overall_sequence >= 36748 & overall_sequence <= 36797 |
             overall_sequence >= 37024 & overall_sequence <= 37027  )) %>%
  #JC13P07: deleting conversation with D/UM conv
  filter(!(overall_sequence >= 17001 & overall_sequence <= 17008 |
             overall_sequence >= 17010 & overall_sequence <= 17013)) %>%
  #SC07P099: deleting conversation with  D/UF/P conv
  filter(!(overall_sequence >= 118585 & overall_sequence <= 118638)) %>%
  #SC07P022: deleting conversation with  D/UF conv
  filter(!(overall_sequence >= 123252 & overall_sequence <= 123254)) %>%
  #SC23P058: deleting conversation with  D/UF conv
  filter(!(overall_sequence >= 122808 & overall_sequence <= 122816)) %>%
  #SC20P057: deleting conversation with  D/UF conv
  filter(!(overall_sequence >= 119185 & overall_sequence <= 119191 |
             overall_sequence >= 119637 & overall_sequence <= 119638)) %>%
  #SC24P089: deleting conversation with R/D &  P/UF/RA conv
  filter(!(overall_sequence >= 117377 & overall_sequence <= 117381 |
             overall_sequence >= 117791 & overall_sequence <= 117794)) %>%
  #SC20P138: deleting conversation with   D/UF conv
  filter(!(overall_sequence >= 117237 & overall_sequence <= 117242 |
             overall_sequence >= 117335 & overall_sequence <= 117339)) %>%
  #SC06P047: deleting conversation with D/RA & D/UF conv
  filter(!(overall_sequence >= 110856 & overall_sequence <= 110864 |
             overall_sequence >= 111174 & overall_sequence <= 111184)) %>%
  #SC24P035: deleting conversation with  D/UF conv
  filter(!(overall_sequence >= 109833 & overall_sequence <= 109839)) %>%
  #SC23P024: deleting conversation with  D/UF conv
  filter(!(overall_sequence >= 108530 & overall_sequence <= 108542)) %>%
  #SC18P110: deleting conversation with  D/UF conv
  filter(!(overall_sequence >= 106706 & overall_sequence <= 106715)) %>%
  #SC07P039: deleting conversation after D/P said bye
  filter(!(overall_sequence >= 103671 & overall_sequence <= 103676)) %>%
  #SC23P129: deleting conversation with  D/UF conv
  filter(!(overall_sequence >= 102285 & overall_sequence <= 102290 |
             overall_sequence >= 102916 & overall_sequence <= 102923)) %>%
  #SC23P018: deleting conversation with UF/D/RA convo
  filter(!(overall_sequence >= 100068 & overall_sequence <= 100076)) %>%
  #SC07P141: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 98842 & overall_sequence <= 98845)) %>%
  #SC12P044: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 98652 & overall_sequence <= 98668)) %>%
  #SC07P067: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 93738 & overall_sequence <= 93741)) %>%
  #SC15P095: deleting conversation with  D/RA and D/UF convs
  filter(!(overall_sequence >= 92939 & overall_sequence <= 92941 |
             overall_sequence >= 93094 & overall_sequence <= 93109)) %>%
  #SC24P096: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 92270 & overall_sequence <= 92279)) %>%
  #SC21P109: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 91974 & overall_sequence <= 91976)) %>%
  #SC23P064: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 91382 & overall_sequence <= 91386)) %>%
  #SC15P071: deleting conversation with UF/D/RA/P convo
  filter(!(overall_sequence >= 90067 & overall_sequence <= 90077)) %>%
  #SC12P038: deleting conversation with UM/D and end of visit convo
  filter(!(overall_sequence >= 88167 & overall_sequence <= 88172 |
             overall_sequence >= 88200 & overall_sequence <= 88204)) %>%
  #SC21P118: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 87553 & overall_sequence <= 87557)) %>%
  #SC23P020: deleting conversation with UF/D convo
  filter(!(overall_sequence >= 86545 & overall_sequence <= 86551)) %>%
  #SC19P144: deleting conversation with RA/D & UF/P convo
  filter(!(overall_sequence >= 81821 & overall_sequence <= 81823 |
             overall_sequence >= 81895 & overall_sequence <= 81897 )) %>%
  #OC02P07: deleting conversation with RA/P convo, D/UF, RA/D convos
  filter(!(overall_sequence >= 79837 & overall_sequence <= 79840 |
             overall_sequence >= 80030 & overall_sequence <= 80034 |
             overall_sequence >= 80154 & overall_sequence <= 80159)) %>%
  #OC01P02: deleting conversation with RA/D & UF/P convo
  filter(!(overall_sequence >= 79488 & overall_sequence <= 79492 | 
             overall_sequence >= 79832 & overall_sequence <= 79836)) %>%
  #OC04P01: deleting conversation with UF/P convos
  filter(!(overall_sequence >= 78644 & overall_sequence <= 78649 |
             overall_sequence >= 78970 & overall_sequence <= 78980 )) %>%
  #OC05P11: deleting conversation with UF/D/P convo
  filter(!(overall_sequence >= 73206 & overall_sequence <= 73211)) %>%
  #OC02P08: deleting conversation with UF/P convo at end after D leaves
  filter(!(overall_sequence >= 72921 & overall_sequence <= 72930)) %>%
  #OC03P06: deleting conversation with UF/P convo at end after D leaves
  filter(!(overall_sequence >= 71905 & overall_sequence <= 71911)) %>%
  #OC02P05: deleting conversation with conversation before start & UF/D/P convo 
  filter(!(overall_sequence >= 71142 & overall_sequence <= 71147 | 
             overall_sequence >= 71479 & overall_sequence <= 71487 )) %>%
  #OC08P02: deleting conversation with UF/D 
  filter(!(overall_sequence >= 70868 & overall_sequence <= 70871)) %>%
  #OC01P05: deleting conversation with UF/D/P 
  filter(!(overall_sequence >= 69423 & overall_sequence <= 69433)) %>%
  #OC05P08: deleting conversation with UF/P 
  filter(!(overall_sequence >= 67591 & overall_sequence <= 67594)) %>%
  #OC03P11: deleting conversation with UF/P & RA/P
  filter(!(overall_sequence >= 62982 & overall_sequence <= 62993 | 
             overall_sequence >= 63110 & overall_sequence <= 63118 )) %>%
  #OC05P03: deleting conversation with UF/P 
  filter(!(overall_sequence >= 59336 & overall_sequence <= 59343)) %>%
  #OC06P04: deleting conversation with UF/P 
  filter(!(overall_sequence >= 51413 & overall_sequence <= 51420 | 
             overall_sequence >= 51645 & overall_sequence <= 51655)) %>%
  #JC14P03: deleting conversation with UF/D/P 
  filter(!(overall_sequence >= 45241 & overall_sequence <= 45259)) %>%
  #JC03P10: deleting conversation with RA/D & UF/D/P 
  filter(!(overall_sequence >= 37584 & overall_sequence <= 37587 |
             overall_sequence >= 37892 & overall_sequence <= 37902)) %>%
  #JC01P02: deleting conversation with UF/D/P 
  filter(!(overall_sequence >= 32195 & overall_sequence <= 32214 |
             overall_sequence >= 32396 & overall_sequence <= 32400 |
             overall_sequence >= 32435 & overall_sequence <= 32446 |
             overall_sequence >= 32529 & overall_sequence <= 32532)) %>%
  #JC07P04: deleting conversation with UF/D/P 
  filter(!(overall_sequence >= 28103 & overall_sequence <= 28105)) %>%
  #JC03P09: deleting conversation with RA/P and conversation after P leaves
  filter(!(overall_sequence >= 27145 & overall_sequence <= 27150 |
             overall_sequence >= 27499 & overall_sequence <= 27511)) %>%
  #JC12P10: deleting conversation with UF/D 
  filter(!(overall_sequence >= 21388 & overall_sequence <= 21401)) %>%
  #JC10P05: deleting conversation with UF/P 
  filter(!(overall_sequence >= 9002 & overall_sequence <= 9011)) %>%
  #JC12P03: deleting conversation with UF/P 
  filter(!(overall_sequence >= 7982 & overall_sequence <= 7986)) %>%
  #JC08P04: deleting conversation with RA/D 
  filter(!(overall_sequence >= 3083 & overall_sequence <= 3096)) %>%
  #JC10P11: deleting conversation with RA/D 
  filter(!(overall_sequence >= 4778 & overall_sequence <= 4786)) %>%
  #JC10P08: deleting conversation with RA/D 
  filter(!(overall_sequence >= 5351 & overall_sequence <= 5356)) %>%
  #JC03P08: deleting conversation with RA/D 
  filter(!(overall_sequence >= 5893 & overall_sequence <= 5895)) %>%
  #JC14P08: deleting conversation with RA/D 
  filter(!(overall_sequence >= 6587 & overall_sequence <= 6592)) %>%
  #JC14P05: deleting conversation with RA/D 
  filter(!(overall_sequence >= 11586 & overall_sequence <= 11589)) %>%
  #JC06P01: deleting conversation with RA/D 
  filter(!(overall_sequence >= 15973 & overall_sequence <= 15980)) %>%
  #JC02P10: deleting conversation with RA/D 
  filter(!(overall_sequence >= 21017 & overall_sequence <= 21023)) %>%
  #JC02P04: deleting conversation with RA/D 
  filter(!(overall_sequence >= 23778 & overall_sequence <= 23784)) %>%
  #JC14P07: deleting conversation with RA/D 
  filter(!(overall_sequence >= 23881 & overall_sequence <= 23887)) %>%
  #JC03P07: deleting conversation with RA/D 
  filter(!(overall_sequence >= 24124 & overall_sequence <= 24129)) %>%
  #JC12P02: deleting conversation with RA/D 
  filter(!(overall_sequence >= 30473 & overall_sequence <= 30475)) %>%
  #JC04P01: deleting conversation with RA/D 
  filter(!(overall_sequence >= 30966 & overall_sequence <= 30970)) %>%
  #JC14P04: deleting conversation with RA/D 
  filter(!(overall_sequence >= 31144 & overall_sequence <= 31147)) %>%
  #JC09P03: deleting conversation with RA/D 
  filter(!(overall_sequence >= 31850 & overall_sequence <= 31852)) %>%
  #JC04P04: deleting conversation with RA/D & P/UF
  filter(!(overall_sequence >= 33596 & overall_sequence <= 33599)) %>%
  #JC02P02: deleting conversation with RA/D 
  filter(!(overall_sequence >= 37242 & overall_sequence <= 37247)) %>%
  #JC03P01: deleting conversation with RA/D 
  filter(!(overall_sequence >= 37248 & overall_sequence <= 37255)) %>%
  #JC10P10: deleting conversation with RA/D & D/P/UF
  filter(!(overall_sequence >= 38846 & overall_sequence <= 38858 |
             overall_sequence >= 38934 & overall_sequence <= 38939)) %>%
  #JC02P05: deleting conversation with RA/D 
  filter(!(overall_sequence >= 39965 & overall_sequence <= 39971)) %>%
  #JC07P06: deleting conversation with RA/D 
  filter(!(overall_sequence >= 41948 & overall_sequence <= 41953)) %>%
  #JC09P04: deleting conversation with RA/D/P
  filter(!(overall_sequence >= 44204 & overall_sequence <= 44211)) %>%
  #JC01P05: deleting conversation with RA/D/P
  filter(!(overall_sequence >= 44661 & overall_sequence <= 44669)) %>%
  #JC10P03: deleting conversation with RA/D
  filter(!(overall_sequence >= 48098 & overall_sequence <= 48103)) %>%
  #OC08P03: deleting conversation with RA/P
  filter(!(overall_sequence >= 50747 & overall_sequence <= 50754)) %>%
  #OC01P01: deleting conversation with RA/P
  filter(!(overall_sequence >= 50755 & overall_sequence <= 50757 |
             overall_sequence >= 51133 & overall_sequence <= 51137)) %>%
  #OC07P07: deleting conversation with RA/P
  filter(!(overall_sequence >= 51138 & overall_sequence <= 51143 |
             overall_sequence >= 51398 & overall_sequence <= 51401  )) %>%
  #OC05P01: deleting conversation with RA/D/P & RA/P
  filter(!(overall_sequence >= 52138 & overall_sequence <= 52165 |
             overall_sequence >= 52708 & overall_sequence <= 52716)) %>%
  #OC02P10: deleting conversation with RA/P/D
  filter(!(overall_sequence >= 55330 & overall_sequence <= 55335)) %>%
  #OC05P09: deleting conversation with RA/P &RA/P
  filter(!(overall_sequence >= 55503 & overall_sequence <= 55505 |
             overall_sequence >= 56195 & overall_sequence <= 56200)) %>%
  #OC01P04: deleting conversation with RA/D
  filter(!(overall_sequence >= 56201 & overall_sequence <= 56207)) %>%
  #OC02P01: deleting conversation with RA/D/P
  filter(!(overall_sequence >= 57243 & overall_sequence <= 57256)) %>%
  #OC04P07: deleting conversation with RA/P
  filter(!(overall_sequence >= 57932 & overall_sequence <= 57939)) %>%
  #OC06P01: deleting conversation with RA/P
  filter(!(overall_sequence >= 58499 & overall_sequence <= 58502 |
             overall_sequence >= 58809 & overall_sequence <= 58812)) %>%
  #OC07P05: deleting conversation with RA/P
  filter(!(overall_sequence >= 58813 & overall_sequence <= 58818)) %>%
  #OC06P06: deleting conversation with RA/P
  filter(!(overall_sequence >= 59085 & overall_sequence <= 59089 |
             overall_sequence >= 59328 & overall_sequence <= 59335)) %>%
  #OC02P01: deleting conversation with RA/D/P
  filter(!(overall_sequence >= 57243 & overall_sequence <= 57256)) %>%
  #OC01P03: deleting conversation with RA/D/P & RA/P
  filter(!(overall_sequence >= 60577 & overall_sequence <= 60580 |
             overall_sequence >= 60889 & overall_sequence <= 60899)) %>%
  #OC08P01: deleting conversation with RA/P
  filter(!(overall_sequence >= 61542 & overall_sequence <= 61549)) %>%
  #OC01P12: deleting conversation with RA/P
  filter(!(overall_sequence >= 61803 & overall_sequence <= 61809)) %>%
  #OC07P08: deleting conversation with RA/P
  filter(!(overall_sequence >= 62144 & overall_sequence <= 62154)) %>%
  #OC02P12: deleting conversation with RA/P
  filter(!(overall_sequence >= 63119 & overall_sequence <= 63121 |
             overall_sequence >= 63407 & overall_sequence <= 63415)) %>%
  #OC08P09: deleting conversation with RA/P & D/PA &end of conv
  filter(!(overall_sequence >= 64351 & overall_sequence <= 64353 |
             overall_sequence >= 64377 & overall_sequence <= 64381 |
             overall_sequence >= 64765 & overall_sequence <= 64769)) %>%
  #OC05P06: deleting conversation with RA/P
  filter(!(overall_sequence >= 65029 & overall_sequence <= 65035)) %>%
  #OC04P05: deleting conversation with D/P/RA & RA/P
  filter(!(overall_sequence >= 65036 & overall_sequence <= 65048 |
             overall_sequence >= 65395 & overall_sequence <= 65417)) %>%
  #OC01P06: deleting conversation with RA/P
  filter(!(overall_sequence >= 65935 & overall_sequence <= 65942 |
             overall_sequence >= 66407 & overall_sequence <= 66419)) %>%
  #OC03P03: deleting conversation with RA/P/D & RA/P
  filter(!(overall_sequence >= 68494 & overall_sequence <= 68500 |
             overall_sequence >= 68929 & overall_sequence <= 68937)) %>%
  #OC05P05: deleting conversation with RA/P
  filter(!(overall_sequence >= 70100 & overall_sequence <= 70116)) %>%
  #OC04P03: deleting conversation with RA/P
  filter(!(overall_sequence >= 71912 & overall_sequence <= 71924 |
             overall_sequence >= 72058 & overall_sequence <= 72062)) %>%
  #OC07P06: deleting conversation with RA/P
  filter(!(overall_sequence >= 72063 & overall_sequence <= 72070 | 
             overall_sequence >= 72276 & overall_sequence <= 72279)) %>%
  #OC04P12: deleting conversation with RA/P
  filter(!(overall_sequence >= 72931 & overall_sequence <= 72935)) %>%
  #OC06P08: deleting conversation with RA/P
  filter(!(overall_sequence >= 73730 & overall_sequence <= 73732)) %>%
  #OC08P08: deleting conversation with RA/P
  filter(!(overall_sequence >= 75102 & overall_sequence <= 75106)) %>%
  #OC07P01: deleting conversation with RA/P
  filter(!(overall_sequence >= 75748 & overall_sequence <= 75754)) %>%
  #OC05P02: deleting conversation with RA/P/D
  filter(!(overall_sequence >= 78197 & overall_sequence <= 78203)) %>%
  #OC07P04: deleting conversation with RA/P/D
  filter(!(overall_sequence >= 78981 & overall_sequence <= 78989)) %>%
  #OC06P07: deleting conversation with RA/P/D
  filter(!(overall_sequence >= 79168 & overall_sequence <= 79173 |
             overall_sequence >= 79480 & overall_sequence <= 79487)) %>%
  #OC08P11: deleting conversation with RA/P
  filter(!(overall_sequence >= 81156 & overall_sequence <= 81163)) %>%
  #OC05P13: deleting conversation with RA/D & RA/P
  filter(!(overall_sequence >= 81408 & overall_sequence <= 81410 |
             overall_sequence >= 81811 & overall_sequence <= 81820)) %>%
  #SC19P144: deleting conversation with RA/D
  filter(!(overall_sequence >= 81821 & overall_sequence <= 81823)) %>%
  #SC21P127: deleting conversation with RA/P
  filter(!(overall_sequence >= 82814 & overall_sequence <= 82816)) %>%
  #SC13P002: deleting conversation with RA/P
  filter(!(overall_sequence >= 83270 & overall_sequence <= 83272)) %>%
  #SC12P016: deleting conversation with RA/P
  filter(!(overall_sequence >= 85614 & overall_sequence <= 85616)) %>%
  #SC13P079: deleting conversation with RA/P
  filter(!(overall_sequence >= 91623 & overall_sequence <= 91625)) %>%
  #SC12P060: deleting conversation with RA/P
  filter(!(overall_sequence >= 92516 & overall_sequence <= 92518)) %>%
  #SC20P061: deleting conversation with RA/P
  filter(!(overall_sequence >= 93138 & overall_sequence <= 93140)) %>%
  #SC24P080: deleting conversation with RA/P
  filter(!(overall_sequence >= 96795 & overall_sequence <= 96799)) %>%
  #SC24P066: deleting conversation with RA/P
  filter(!(overall_sequence >= 98166 & overall_sequence <= 98169)) %>%
  #SC06P100: deleting conversation with RA/D
  filter(!(overall_sequence >= 98285 & overall_sequence <= 98291)) %>%
  #SC21P119: deleting conversation with RA/D
  filter(!(overall_sequence >= 101306 & overall_sequence <= 101308 |
             overall_sequence >= 101591 & overall_sequence <= 101593)) %>%
  #SC19P045: deleting conversation with RA/D
  filter(!(overall_sequence >= 103967 & overall_sequence <= 103972)) %>%
  #SC18P004: deleting conversation with RA/D
  filter(!(overall_sequence >= 105949 & overall_sequence <= 105952)) %>%
  #SC18P104: deleting conversation with RA/D
  filter(!(overall_sequence >= 109789 & overall_sequence <= 109791)) %>%
  #SC18P069: deleting conversation with RA/D
  filter(!(overall_sequence >= 111473 & overall_sequence <= 111480)) %>%
  #SC23P023: deleting conversation with RA/D
  filter(!(overall_sequence >= 113458 & overall_sequence <= 113460)) %>%
  #SC06P001: deleting conversation with RA/D
  filter(!(overall_sequence >= 120284 & overall_sequence <= 120294)) %>%
  #SC07P009: deleting conversation with RA/D
  filter(!(overall_sequence >= 121674 & overall_sequence <= 121678)) %>%
  #SC15P018: deleting conversation with RA/D
  filter(!(overall_sequence >= 124232 & overall_sequence <= 124238)) %>%
  #OC08P08: deleting conversation with RA/D
  filter(!(overall_sequence >= 75107 & overall_sequence <= 75108))


ECHO_transcript_exclude_remove <- c('JC10P01', 'JC09P06', 'OC03P01',
                                    'JC04P03', 'SC18P094', 'JC06P06',
                                    'SC06P075', 'JC01P09', 'OC03P07',
                                    'SC12P051', 'JC05P03', 'OC02P04',
                                    'OC08P06', 'OC02P06', 'OC04P08',
                                    'OC06P03','OC04P06', 'SC12P040',
                                    'OC05P10', 'OC03P02', 'OC01P08',
                                    'OC04P10', 'OC06P09', 'SC24P124',
                                    'SC18P134')

#filtering out the list of transcripts in "excluded_transcript" df
ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>% 
  filter(!(File %in% ECHO_transcript_exclude_remove))

#removing rows with: "comment", "RA", "Missing_1", "Missing_2:" for Speaker column...Check with Annie what "Missing" means
ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
  filter(Speaker != "Comment") %>%
  filter(Speaker != "RA") %>%
  filter(Speaker != "Missing_1") %>%
  filter(Speaker != "Missing_2") %>%
  filter(Speaker != "N") %>%
  filter(Speaker != "Q") %>%
  filter(Speaker != "RN") %>%
  filter(Speaker != "D1") %>%
  filter(Speaker != "PA") %>%
  filter(Speaker != "UM") %>%
  filter(Speaker != "UF")






#This is the data that will be further cleaned before analysis
ECHO_Transcripts_Total_final <- ECHO_Transcripts_Complete_TbyT

#removing all brackets with content inside of them
ECHO_Transcripts_Total_final$Text <- gsub("\\[(.*?)\\]", "", ECHO_Transcripts_Total_final$Text)

#need to create word count variable to remove rows without text
ECHO_Transcripts_Total_final$Word_count <- str_count(ECHO_Transcripts_Total_final$Text, "\\w+")


#Removing all rows where there is no speech
ECHO_Transcripts_Total_final <- ECHO_Transcripts_Total_final %>%
  filter(Word_count != 0)

#making a version of the TbyT where one-word and two-word turns are deleted before smoothing/LIWC
#DON'T REMOVE ROWS WITH 1 OR 2 WORDS FOR SENSITIVITY ANALYSIS
ECHO_Transcripts_Total_final_V2 <- ECHO_Transcripts_Total_final %>%
  filter(Word_count != 1) %>%
  filter(Word_count != 2)


############################################################################################################
############# Speaker Smoothing
############################################################################################################

smoothed_tByT_df_V2 <- data.frame( # Empty dataframe to store results
  File = character(),
  Speaker = character(),
  Text = character(),
  Sequence = numeric(),
  overall_sequence = numeric(),
  Word_count = integer()
)

for (f in unique(ECHO_Transcripts_Total_final_V2$File)) { # Iterate through each file
  file_df <- data.frame( # create a df with just that files data; probably ineffecient, but...
    ECHO_Transcripts_Total_final_V2[which(ECHO_Transcripts_Total_final_V2$File == f),]
  )
  first <- TRUE
  smoothed_chunk <- data.frame( # Create empty dataframe to store smoothed speakers for that file
    File = character(),
    Speaker = character(),
    Text = character(),
    Sequence = numeric(),
    overall_sequence = numeric(),
    Word_count = integer()
  )
  for (i in seq_len(nrow(file_df))) { # Iterate through each row in a given file
    if (first == TRUE) { # Just add the first row to the empty df for the file
      smoothed_chunk <- rbind(smoothed_chunk,file_df[i,])
      first <- FALSE
    } else if (file_df[i,'Speaker'] == file_df[i-1,'Speaker']) {
      # This tests to see of the current row's speaker is the same as the previous; if so, it appends the current row's text to the previous one
      smoothed_chunk[nrow(smoothed_chunk),'Text'] <- paste(smoothed_chunk[nrow(smoothed_chunk),'Text'],file_df[i,'Text'])
      smoothed_chunk[nrow(smoothed_chunk),'Word_count'] <- smoothed_chunk[nrow(smoothed_chunk),'Word_count'] + file_df[i,'Word_count']
    } else { # if speakers don't match, just add row to the smoothed df for that file
      smoothed_chunk <- rbind(smoothed_chunk,file_df[i,])
    }
  }
  smoothed_tByT_df_V2 <- rbind(smoothed_tByT_df_V2,smoothed_chunk) # add the smoothed file to overall smoothed results
}

#ECHO_Transcripts_Complete_TbyT_SMOOTHED_V3 is the new smoothed version which includes all 4 sites; V2 was previous version without WSU AND without new transcript cleaning
write.csv(smoothed_tByT_df_V2, "ECHO_Transcripts_Complete_TbyT_SMOOTHED_V2.csv")

#This version does not have turns excluded
# write.csv(smoothed_tByT_df_V2, "ECHO_Transcripts_Complete_TbyT_SMOOTHED_V3.csv")

# write.xlsx(ECHO_Transcripts_Complete_TbyT, file = "ECHO_Transcripts_Complete_TbyT.xlsx")
# write.csv(ECHO_Transcripts_Complete_TbyT, "ECHO_Transcripts_Complete_TbyT.csv")


#conversation level prep for ALL 4 study sites
ECHO_LSM_Prep_final <- ECHO_Transcripts_Total_final %>% 
  group_by(File, Speaker) %>%
  summarise(Text = paste(Text, collapse = " ")) 

#this is saved on OneDrive and then analyzed in LIWC
write.csv(ECHO_LSM_Prep_final, "ECHO_LSM_Prep_final.csv")



#########################################################
########################################################
#################################################################


library(runner)

window_transcripts <- function(transcript_df, window, collapse){
  
  if(missing(collapse)) {collapse <- ' '}
  
  # do some quality control
  if(length(unique(transcript_df$Speaker) )!= 2) {
    print('Too many different speakers...')
  } else {print('Good to go: only 2 speakers')}
  
  windowed_df <- transcript_df %>%
    group_by(File, Speaker) %>%
    mutate(
      text_agg = runner::runner(
        Text,
        f = paste,
        collapse = collapse,
        k = window,
        na_pad = TRUE
      ),
      text_agg_wc = str_count(text_agg, "\\w+")) %>% 
    ungroup()
  
  return(windowed_df)
}

test_window <- window_transcripts(
  transcript_df = smoothed_tByT_df_V2,
  window = 5,
  collapse = ' '
)

skimr::skim(test_window)


write.csv(test_window, "ECHO_rolling_window_5.csv")



##################################################################################################
##################################################################################################
#START HERE AFTER LIWC ANALYSIS DONE
#Calculating LSM, adding back LIWC metrics, merging survey data
##################################################################################################
##################################################################################################

#Opening all files
#this version was for conversation-level LSM using LIWC-2015
#ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_V2_path, config$ECHO_LSM_MLM_V2_name))
ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_V3_path, config$ECHO_LSM_MLM_V3_name))
ECHO_survey_data <- read_dta(here(config$ECHO_survey_data_path, config$ECHO_survey_data_name))
ECHO_ID_key <- read_csv(here(config$ECHO_ID_key_path, config$ECHO_ID_key_name))
# WSU_ECHO_ID_Key <- read_csv(here(config$WSU_ECHO_ID_Key_path, config$WSU_ECHO_ID_Key_name))



# WSU_ECHO_ID_Key_V2 <- WSU_ECHO_ID_Key %>%
#   mutate(File = str_sub(Local_ID, 6, 9)) %>%
#   mutate(tapeid = Global_ID) %>%
#   select(-c(Local_ID,Global_ID))

#this was combining the ID_key documents so that WSU was included
# Combined_ECHO_ID_Key <- bind_rows(ECHO_ID_key, WSU_ECHO_ID_Key_V2)

#Here is the code for creating the LIWC values for patient and doctor as individual variables
ECHO_LSM_LIWC_Components <- ECHO_LSM_MLM%>%
  rename(output_order = '...1') %>%
  select(-one_of(c("Segment", "AllPunc","Period","Comma", "QMark",
                   "Exclam", "Apostro", "OtherP"))) %>%
  ungroup()%>%
  select(-Text) %>%
  select(-output_order) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = c(name, Speaker), values_from = value) 


#This is for calculating LSM for this data
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  rename(output_order = '...1') %>%
  #deleting all punctuations
  select(-one_of(c("Segment", "AllPunc","Period","Comma", "QMark",
                   "Exclam", "Apostro", "OtherP"))) %>%
  #creating LSM scores
  select(-Text) %>%
  select(-output_order) %>%
  pivot_longer(WC:filler) %>%
  pivot_wider(names_from = Speaker, values_from = value) %>%
  mutate(LIWC_new = (1 - abs(D - P) / (D + P + .0001))) %>%
  select(-D, -P) %>%
  pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
  #creating overall LSM metric for functions by getting average of auxiliary verbs, 
  #articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
  #negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
  rowwise() %>%
  mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, 
                                    LSM_negate, LSM_conj, LSM_quantity, LSM_ppron))) %>%
  mutate(conv.psychological.match = mean(c(LSM_Drives, LSM_Cognition,
                                           LSM_Affect, LSM_Social))) %>%
  mutate(conv.auxverb.match = LSM_auxverb,
         conv.article.match = LSM_article,
         conv.adverb.match = LSM_adverb,
         conv.ipron.match = LSM_ipron,
         conv.prep.match = LSM_prep,
         conv.negate.match = LSM_negate,
         conv.conj.match = LSM_conj,
         conv.quantity.match = LSM_quantity,
         conv.ppron.match = LSM_ppron,
         conv.Drives.match = LSM_Drives,
         conv.affiliation.match = LSM_affiliation,
         conv.achieve.match = LSM_achieve,
         conv.power.match = LSM_power,
         conv.Cognition.match = LSM_Cognition,
         conv.allnone.match = LSM_allnone,
         conv.cogproc.match = LSM_cogproc,
         conv.memory.match = LSM_memory,
         conv.Affect.match = LSM_Affect,
         conv.tone_pos.match = LSM_tone_pos,
         conv.tone_neg.match = LSM_tone_neg,
         conv.emotion.match = LSM_emotion,
         conv.emo_pos.match = LSM_emo_pos,
         conv.emo_neg.match = LSM_emo_neg,
         conv.emo_anx.match = LSM_emo_anx,
         conv.emo_anger.match = LSM_emo_anger,
         conv.emo_sad.match = LSM_emo_sad,
         conv.swear.match = LSM_swear,
         conv.Social.match = LSM_Social,
         conv.socbehav.match = LSM_socbehav,
         conv.prosocial.match = LSM_prosocial,
         conv.polite.match = LSM_polite,
         conv.conflict.match = LSM_conflict,
         conv.moral.match = LSM_moral,
         conv.comm.match = LSM_comm,
         conv.socrefs.match = LSM_socrefs,
         conv.Culture.match = LSM_Culture,
         conv.ethnicity.match = LSM_ethnicity,
         conv.Lifestyle.match = LSM_Lifestyle,
         conv.leisure.match = LSM_leisure,
         conv.home.match = LSM_home,
         conv.work.match = LSM_work,
         conv.money.match = LSM_money,
         conv.relig.match = LSM_relig,
         conv.Physical.match = LSM_Physical,
         conv.health.match = LSM_health,
         conv.illness.match = LSM_illness,
         conv.wellness.match = LSM_wellness,
         conv.mental.match = LSM_mental,
         conv.substances.match = LSM_substances,
         conv.death.match = LSM_death,
         conv.need.match = LSM_need,
         conv.want.match = LSM_want,
         conv.acquire.match = LSM_acquire,
         conv.fulfill.match = LSM_fulfill,
         conv.fatigue.match = LSM_fatigue,
         conv.reward.match = LSM_reward,
         conv.risk.match = LSM_risk,
         conv.curiosity.match = LSM_curiosity,
         conv.allure.match = LSM_allure,
         conv.Perception.match = LSM_Perception,
         conv.attention.match = LSM_attention,
         conv.feeling.match = LSM_feeling,
         conv.focuspast.match = LSM_focuspast,
         conv.focuspresent.match = LSM_focuspresent,
         conv.focusfuture.match = LSM_focusfuture,
         conv.Conversation.match = LSM_Conversation)



#Adding LIWC components back to LSM file
ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_LSM_LIWC_Components, by = "File")
#Making the provider_id variable
ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
  mutate(provider_id = str_sub(File, 1, 4))
#merge ECHO_LSM_MLM and ECHO_ID_key by "File"
ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_ID_key, by = "File" ) 
#merge ECHO_LSM_MLM and ECHO_survey_data by "tapeid" to include survey data
ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_survey_data, by = "tapeid")

####################

####################################################################################################
#Use this if you want to use the tbyt file with 1 & 2 word turns EXCLUDED
# df_tbyt_V2 <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_V2_path, config$ECHO_LSM_TbyT_Smoothed_V2_name))

#rolling window analysis for TbyT
df_tbyt_V3 <- read_csv(here(config$ECHO_rolling_window_5_LIWC_path, config$ECHO_rolling_window_5_LIWC_name))

# #Use this if you want to use the tbyt file with 1 & 2 word turns INCLUDED
# df_tbyt_V2 <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_V3_path, config$ECHO_LSM_TbyT_Smoothed_V3_name))

# df_tbyt_V2 <- df_tbyt_V2 %>%
#   select(-'...1') %>%
#   select(-Sequence) %>%
#   select(-overall_sequence) %>%
#   select(-Word_count) %>%
#   select(-Segment)

df_tbyt_V3 <- df_tbyt_V3 %>%
  select(-'...1') %>%
  select(-Text) %>%
  select(-Sequence) %>%
  select(-overall_sequence) %>%
  select(-Word_count) %>%
  select(-text_agg_wc) %>%
  select(-Segment) 

# test <- df_tbyt_V2 %>%
#   dplyr::select(File, Speaker) %>%
#   group_by(File) %>%
#   mutate(
#     Speaker.lag = lag(Speaker)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     speaker_match = if_else(Speaker == Speaker.lag,1,0)
#   )
# table(test$speaker_match)


ECHO_smoothed_rLSM <- df_tbyt_V3 %>%
  dplyr::select(File, text_agg, Speaker, WC, WPS, auxverb, article, adverb, ipron, 
                prep, negate, conj, quantity, ppron) %>%
  # Adding quick way to drop turn by turns from the same speaker
  ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
  group_by(File) %>%
  mutate(
    Speaker.lag = lag(Speaker)
  ) %>%
  ungroup() %>%
  mutate(
    speaker_match = if_else(Speaker == Speaker.lag,1,0)
  ) %>%
  ungroup() %>%
  #filter(speaker_match == 0) %>%
  # End dropping same speaker turns
  
  group_by(File) %>%
  mutate(
    auxverb.orig = auxverb,
    article.orig = article,
    adverb.orig = adverb,
    ipron.orig = ipron,
    prep.orig = prep,
    negate.orig = negate,
    conj.orig = conj,
    quantity.orig = quantity,
    ppron.orig = ppron,
    WC.orig = WC,
    WPS.orig = WPS) %>%
  ungroup() %>%
  #rowwise() %>%
  # This puts the turn before the current one on the same row with a .lag suffix
  #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
  group_by(File) %>%
  mutate(
    auxverb.lag = lag(auxverb),
    article.lag = lag(article),
    adverb.lag = lag(adverb),
    ipron.lag = lag(ipron),
    prep.lag = lag(prep),
    negate.lag = lag(negate),
    conj.lag = lag(conj),
    quantity.lag = lag(quantity),
    ppron.lag = lag(ppron),
    WC.lag = lag(WC),
    WPS.lag = lag(WPS)) %>%
  ungroup() %>%
  # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
  # This makes sure that only liwc categories prersent in the first statement are used for rlsm
  mutate(across(c(auxverb.lag, article.lag,adverb.lag, ipron.lag,
                  prep.lag, negate.lag, conj.lag, quantity.lag, ppron.lag
                  ), 
                ~ if_else(. > 0,.,as.numeric(NA)))) %>%
  # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
  # Per the rLSM paper
  group_by(File) %>%
  mutate(
    auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
    article = if_else(is.na(article.lag),as.numeric(NA),article),
    adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
    ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
    prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
    negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
    conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
    quantity = if_else(is.na(quantity.lag),as.numeric(NA),quantity),
    ppron = if_else(is.na(ppron.lag),as.numeric(NA),ppron)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    auxverb.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
    article.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
    adverb.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
    ipron.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
    prep.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
    negate.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
    conj.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
    quantity.rLSM = 1 - (abs(quantity - quantity.lag) / (quantity + quantity.lag + .0001)),
    ppron.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
  ) %>%
  ungroup() %>%
  #creates an average rLSM across separrate featurers
  #mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
  # mutate(rLSM = ifelse(is.na(rLSM), 0, rLSM)) %>% # replaces NAs with 0
  # group_by(File,Speaker) %>%
  # summarize(rLSM = mean(rLSM)) %>%
  # ungroup() %>%
  group_by(File,Speaker) %>%
  summarize(
    across(contains('.rLSM'), .fns = ~ mean(.x,na.rm=TRUE)),
    WC_sum = sum(WC),
    WPS_avg = mean(WPS)) %>%
  ungroup() %>%
  mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
  pivot_wider(
    id_cols = File,
    names_from = Speaker,
    values_from = c(rLSM,WPS_avg,WC_sum),
    # names_prefix = 'rLSM.'
    names_glue = "{.value}.{Speaker}"
  ) %>%
  # drop_na() %>% # find out where these are coming from
  mutate(
    mean.rLSM = rowMeans(select(.,contains('rLSM')),na.rm = TRUE),
    ratio.rLSM.P = rLSM.P / rLSM.D,
    ratio.rLSM.D = rLSM.D / rLSM.P,
    verb_dom = WC_sum.D / WC_sum.P
  ) %>%
  filter(
    WC_sum.D >= 50 & WC_sum.P >= 50
  ) %>%
  select(-c(WPS_avg.D, WPS_avg.P))

# ECHO_transcript_topbottom5_rLSM_V2 <- c("JC02P03", "JC06P01", "JC05P05", 
#                                      "OC08P10", "SC12P062", "SC15P050", 
#                                      "SC23P064", "OC06P10", "SC15P071", 
#                                      "JC08P08")
# 
# 
# #filtering out the list of transcripts in "excluded_transcript" df
# rLSM_df_turnexclusions <-rLSM_df_V2 %>% 
#   filter(File %in% ECHO_transcript_topbottom5_rLSM_V2)
# 
# 
# rLSM_df_turnexclusions <- rLSM_df_turnexclusions %>%
#   select(-(WPS)) %>%
#   relocate(auxverb.orig:ppron.orig, .before = auxverb)
# 
# readr::write_csv(rLSM_df_turnexclusions , 'rLSM_df_turnexclusions.csv')



#################################################################################################################
##creating tbyt matching score for LIWC variables  
################################################################################################################# 

#Use this if you want to use the tbyt file with 1 & 2 word turns EXCLUDED
#df_tbyt_LIWC <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_V2_path, config$ECHO_LSM_TbyT_Smoothed_V2_name))


df_tbyt_LIWC_V3 <- read_csv(here(config$ECHO_rolling_window_5_LIWC_path, config$ECHO_rolling_window_5_LIWC_name))
#Use this if you want to use the tbyt file with 1 & 2 word turns INCLUDED
# df_tbyt_LIWC <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_V3_path, config$ECHO_LSM_TbyT_Smoothed_V3_name))

# df_tbyt_LIWC <- df_tbyt_LIWC %>%
#   select(-'...1') %>%
#   select(-Sequence) %>%
#   select(-overall_sequence) %>%
#   select(-Word_count) %>%
#   select(-Segment)

df_tbyt_LIWC_V3 <- df_tbyt_LIWC_V3 %>%
  select(-'...1') %>%
  select(-Text) %>%
  select(-Sequence) %>%
  select(-overall_sequence) %>%
  select(-Word_count) %>%
  select(-text_agg_wc) %>%
  select(-Segment) 

# test <- smoothed_tByT_df %>%
#   dplyr::select(File, Speaker) %>%
#   group_by(File) %>%
#   mutate(
#     Speaker.lag = lag(Speaker)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     speaker_match = if_else(Speaker == Speaker.lag,1,0)
#   )
# table(test$speaker_match)

#SK: had to delete sequence from the list right below with select()
ECHO_smoothed_LIWC_matching <- df_tbyt_LIWC_V3 %>%
  dplyr::select(File, text_agg, Speaker, auxverb, article, adverb, ipron, 
                prep, negate, conj, quantity, ppron, Drives, affiliation,
                achieve, power, Cognition, allnone, cogproc, memory, Affect,
                tone_pos, tone_neg, emotion, emo_pos, emo_neg, emo_anx, emo_anger,
                emo_sad, swear, Social, socbehav, prosocial, polite, conflict,
                moral, comm, socrefs, Culture, ethnicity, Lifestyle, leisure,
                home, work, money, relig, Physical, health, illness, wellness, 
                mental, substances, death, need, want, acquire, fulfill, fatigue,
                reward, risk, curiosity, allure, Perception, attention, feeling,
                focuspast, focuspresent, focusfuture, Conversation
 ) %>%
  # Adding quick way to drop turn by turns from the same speaker
  ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
  group_by(File) %>%
  mutate(
    Speaker.lag = lag(Speaker)
  ) %>%
  ungroup() %>%
  mutate(
    speaker_match = if_else(Speaker == Speaker.lag,1,0)
  ) %>%
  ungroup() %>%
  filter(speaker_match == 0) %>%
  # End dropping same speaker turns
  
  # This puts the turn before the current one on the same row with a .lag suffix
  #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
  group_by(File) %>%
  mutate(
    auxverb.lag = lag(auxverb),
    article.lag = lag(article),
    adverb.lag = lag(adverb),
    ipron.lag = lag(ipron),
    prep.lag = lag(prep),
    negate.lag = lag(negate),
    conj.lag = lag(conj),
    quantity.lag = lag(quantity),
    ppron.lag = lag(ppron),
    Drives.lag = lag(Drives),
    affiliation.lag = lag(affiliation),
    achieve.lag = lag(achieve),
    power.lag = lag(power),
    Cognition.lag = lag(Cognition),
    allnone.lag = lag(allnone),
    cogproc.lag = lag(cogproc),
    memory.lag = lag(memory),
    Affect.lag = lag(Affect),
    tone_pos.lag = lag(tone_pos),
    tone_neg.lag = lag(tone_neg),
    emotion.lag = lag(emotion),
    emo_pos.lag = lag(emo_pos),
    emo_neg.lag = lag(emo_neg),
    emo_anx.lag = lag(emo_anx),
    emo_anger.lag = lag(emo_anger),
    emo_sad.lag = lag(emo_sad),
    swear.lag = lag(swear),
    Social.lag = lag(Social),
    socbehav.lag = lag(socbehav),
    prosocial.lag = lag(prosocial),
    polite.lag = lag(polite),
    conflict.lag = lag(conflict),
    moral.lag = lag(moral),
    comm.lag = lag(comm),
    socrefs.lag = lag(socrefs),
    Culture.lag = lag(Culture),
    ethnicity.lag = lag(ethnicity),
    Lifestyle.lag = lag(Lifestyle),
    leisure.lag = lag(leisure),
    home.lag = lag(home),
    work.lag = lag(work),
    money.lag = lag(money),
    relig.lag = lag(relig),
    Physical.lag = lag(Physical),
    health.lag = lag(health),
    illness.lag = lag(illness),
    wellness.lag = lag(wellness),
    mental.lag = lag(mental),
    substances.lag = lag(substances),
    death.lag = lag(death),
    need.lag = lag(need),
    want.lag = lag(want),
    acquire.lag = lag(acquire),
    fulfill.lag = lag(fulfill),
    fatigue.lag = lag(fatigue),
    reward.lag = lag(reward),
    risk.lag = lag(risk),
    curiosity.lag = lag(curiosity),
    allure.lag = lag(allure),
    Perception.lag = lag(Perception),
    attention.lag = lag(attention),
    feeling.lag = lag(feeling),
    focuspast.lag = lag(focuspast),
    focuspresent.lag = lag(focuspresent),
    focusfuture.lag = lag(focusfuture),
    Conversation.lag = lag(Conversation)
  ) %>%
  ungroup() %>%
  # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
  # This makes sure that only liwc categories prersent in the first statement are used for rlsm
  mutate(across(c(auxverb.lag, article.lag, adverb.lag, ipron.lag, 
                  prep.lag, negate.lag, conj.lag, quantity.lag, ppron.lag,
                  Drives.lag, affiliation.lag, achieve.lag, power.lag,
                  Cognition.lag, allnone.lag, cogproc.lag, memory.lag, 
                  Affect.lag, tone_pos.lag, tone_neg.lag, emotion.lag,
                  emo_pos.lag, emo_neg.lag, emo_anx.lag, emo_anger.lag,
                  emo_sad.lag, swear.lag, Social.lag, socbehav.lag, 
                  prosocial.lag, polite.lag, conflict.lag, moral.lag,
                  comm.lag, socrefs.lag, Culture.lag, ethnicity.lag,
                  Lifestyle.lag, leisure.lag, home.lag, work.lag,
                  money.lag, relig.lag, Physical.lag, health.lag,
                  illness.lag, wellness.lag, mental.lag, substances.lag,
                  death.lag, need.lag, want.lag, acquire.lag), 
                ~ if_else(. > 0,.,as.numeric(NA)))) %>%
  # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
  # Per the rLSM paper
  group_by(File) %>%
  mutate(
    auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
    article = if_else(is.na(article.lag),as.numeric(NA),article),
    adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
    ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
    prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
    negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
    conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
    quantity = if_else(is.na(quantity.lag),as.numeric(NA),quantity),
    Drives = if_else(is.na(Drives.lag),as.numeric(NA),Drives),
    affiliation = if_else(is.na(affiliation.lag),as.numeric(NA),affiliation),
    achieve = if_else(is.na(achieve.lag),as.numeric(NA),achieve),
    power = if_else(is.na(power.lag),as.numeric(NA),power),
    Cognition = if_else(is.na(Cognition.lag),as.numeric(NA),Cognition),
    allnone = if_else(is.na(allnone.lag),as.numeric(NA),allnone),
    cogproc = if_else(is.na(cogproc.lag),as.numeric(NA),cogproc),
    memory = if_else(is.na(memory.lag),as.numeric(NA),memory),
    Affect = if_else(is.na(Affect.lag),as.numeric(NA),Affect),
    tone_pos = if_else(is.na(tone_pos.lag),as.numeric(NA),tone_pos),
    tone_neg = if_else(is.na(tone_neg.lag),as.numeric(NA),tone_neg),
    emotion = if_else(is.na(emotion.lag),as.numeric(NA),emotion),
    emo_pos = if_else(is.na(emo_pos.lag),as.numeric(NA),emo_pos),
    emo_neg = if_else(is.na(emo_neg.lag),as.numeric(NA),emo_neg),
    emo_anx = if_else(is.na(emo_anx.lag),as.numeric(NA),emo_anx),
    emo_anger = if_else(is.na(emo_anger.lag),as.numeric(NA),emo_anger),
    emo_sad = if_else(is.na(emo_sad.lag),as.numeric(NA),emo_sad),
    swear = if_else(is.na(swear.lag),as.numeric(NA),swear),
    Social = if_else(is.na(Social.lag),as.numeric(NA),Social),
    socbehav = if_else(is.na(socbehav.lag),as.numeric(NA),socbehav),
    prosocial = if_else(is.na(prosocial.lag),as.numeric(NA),prosocial),
    polite = if_else(is.na(polite.lag),as.numeric(NA),polite),
    conflict = if_else(is.na(conflict.lag),as.numeric(NA),conflict),
    moral = if_else(is.na(moral.lag),as.numeric(NA),moral),
    comm = if_else(is.na(comm.lag),as.numeric(NA),comm),
    socrefs = if_else(is.na(socrefs.lag),as.numeric(NA),socrefs),
    Culture = if_else(is.na(Culture.lag),as.numeric(NA),Culture),
    ethnicity = if_else(is.na(ethnicity.lag),as.numeric(NA),ethnicity),
    Lifestyle = if_else(is.na(Lifestyle.lag),as.numeric(NA),Lifestyle),
    leisure = if_else(is.na(leisure.lag),as.numeric(NA),leisure),
    home = if_else(is.na(home.lag),as.numeric(NA),home),
    work = if_else(is.na(work.lag),as.numeric(NA),work),
    money = if_else(is.na(money.lag),as.numeric(NA),money),
    relig = if_else(is.na(relig.lag),as.numeric(NA),relig),
    Physical = if_else(is.na(Physical.lag),as.numeric(NA),Physical),
    health = if_else(is.na(health.lag),as.numeric(NA),health),
    illness = if_else(is.na(illness.lag),as.numeric(NA),illness),
    wellness = if_else(is.na(wellness.lag),as.numeric(NA),wellness),
    mental = if_else(is.na(mental.lag),as.numeric(NA),mental),
    substances = if_else(is.na(substances.lag),as.numeric(NA),substances),
    death = if_else(is.na(death.lag),as.numeric(NA),death),
    need = if_else(is.na(need.lag),as.numeric(NA),need),
    want = if_else(is.na(want.lag),as.numeric(NA),want),
    acquire = if_else(is.na(acquire.lag),as.numeric(NA),acquire),
    fulfill = if_else(is.na(fulfill.lag),as.numeric(NA),fulfill),
    fatigue = if_else(is.na(fatigue.lag),as.numeric(NA),fatigue),
    reward = if_else(is.na(reward.lag),as.numeric(NA),reward),
    risk = if_else(is.na(risk.lag),as.numeric(NA),risk),
    curiosity = if_else(is.na(curiosity.lag),as.numeric(NA),curiosity),
    allure = if_else(is.na(allure.lag),as.numeric(NA),allure),
    Perception = if_else(is.na(Perception.lag),as.numeric(NA),Perception),
    attention = if_else(is.na(attention.lag),as.numeric(NA),attention),
    feeling = if_else(is.na(feeling.lag),as.numeric(NA),feeling),
    focuspast = if_else(is.na(focuspast.lag),as.numeric(NA),focuspast),
    focuspresent = if_else(is.na(focuspresent.lag),as.numeric(NA),focuspresent),
    focusfuture = if_else(is.na(focusfuture.lag),as.numeric(NA),focusfuture),
    Conversation = if_else(is.na(Conversation.lag),as.numeric(NA),Conversation)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    auxverb.tbytmatch = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
    article.tbytmatch = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
    adverb.tbytmatch = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
    ipron.tbytmatch = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
    prep.tbytmatch = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
    negate.tbytmatch = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
    conj.tbytmatch = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
    quantity.tbytmatch = 1 - (abs(quantity - quantity.lag) / (quantity + quantity.lag + .0001)),
    ppron.tbytmatch = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001)),
    Drives.tbytmatch = 1 - (abs(Drives - Drives.lag) / (Drives + Drives.lag + .0001)),
    affiliation.tbytmatch = 1 - (abs(affiliation - affiliation.lag) / (affiliation + affiliation.lag + .0001)),
    achieve.tbytmatch = 1 - (abs(achieve - achieve.lag) / (achieve + achieve.lag + .0001)),
    power.tbytmatch = 1 - (abs(power - power.lag) / (power + power.lag + .0001)),
    Cognition.tbytmatch = 1 - (abs(Cognition - Cognition.lag) / (Cognition + Cognition.lag + .0001)),
    allnone.tbytmatch = 1 - (abs(allnone - allnone.lag) / (allnone + allnone.lag + .0001)),
    cogproc.tbytmatch = 1 - (abs(cogproc - cogproc.lag) / (cogproc + cogproc.lag + .0001)),
    memory.tbytmatch = 1 - (abs(memory - memory.lag) / (memory + memory.lag + .0001)),
    Affect.tbytmatch = 1 - (abs(Affect - Affect.lag) / (Affect + Affect.lag + .0001)),
    tone_pos.tbytmatch = 1 - (abs(tone_pos - tone_pos.lag) / (tone_pos + tone_pos.lag + .0001)),
    tone_neg.tbytmatch = 1 - (abs(tone_neg - tone_neg.lag) / (tone_neg + tone_neg.lag + .0001)),
    emotion.tbytmatch = 1 - (abs(emotion - emotion.lag) / (emotion + emotion.lag + .0001)),
    emo_pos.tbytmatch = 1 - (abs(emo_pos - emo_pos.lag) / (emo_pos + emo_pos.lag + .0001)),
    emo_neg.tbytmatch = 1 - (abs(emo_neg - emo_neg.lag) / (emo_neg + emo_neg.lag + .0001)),
    emo_anx.tbytmatch = 1 - (abs(emo_anx - emo_anx.lag) / (emo_anx + emo_anx.lag + .0001)),
    emo_anger.tbytmatch = 1 - (abs(emo_anger - emo_anger.lag) / (emo_anger + emo_anger.lag + .0001)),
    emo_sad.tbytmatch = 1 - (abs(emo_sad - emo_sad.lag) / (emo_sad + emo_sad.lag + .0001)),
    swear.tbytmatch = 1 - (abs(swear - swear.lag) / (swear + swear.lag + .0001)),
    Social.tbytmatch = 1 - (abs(Social - Social.lag) / (Social + Social.lag + .0001)),
    socbehav.tbytmatch = 1 - (abs(socbehav - socbehav.lag) / (socbehav + socbehav.lag + .0001)),
    prosocial.tbytmatch = 1 - (abs(prosocial - prosocial.lag) / (prosocial + prosocial.lag + .0001)),
    polite.tbytmatch = 1 - (abs(polite - polite.lag) / (polite + polite.lag + .0001)),
    conflict.tbytmatch = 1 - (abs(conflict - conflict.lag) / (conflict + conflict.lag + .0001)),
    moral.tbytmatch = 1 - (abs(moral - moral.lag) / (moral + moral.lag + .0001)),
    comm.tbytmatch = 1 - (abs(comm - comm.lag) / (comm + comm.lag + .0001)),
    socrefs.tbytmatch = 1 - (abs(socrefs - socrefs.lag) / (socrefs + socrefs.lag + .0001)),
    Culture.tbytmatch = 1 - (abs(Culture - Culture.lag) / (Culture + Culture.lag + .0001)),
    ethnicity.tbytmatch = 1 - (abs(ethnicity - ethnicity.lag) / (ethnicity + ethnicity.lag + .0001)),
    Lifestyle.tbytmatch = 1 - (abs(Lifestyle - Lifestyle.lag) / (Lifestyle + Lifestyle.lag + .0001)),
    leisure.tbytmatch = 1 - (abs(leisure - leisure.lag) / (leisure + leisure.lag + .0001)),
    home.tbytmatch = 1 - (abs(home - home.lag) / (home + home.lag + .0001)),
    work.tbytmatch = 1 - (abs(work - work.lag) / (work + work.lag + .0001)),
    money.tbytmatch = 1 - (abs(money - money.lag) / (money + money.lag + .0001)),
    relig.tbytmatch = 1 - (abs(relig - relig.lag) / (relig + relig.lag + .0001)),
    Physical.tbytmatch = 1 - (abs(Physical - Physical.lag) / (Physical + Physical.lag + .0001)),
    health.tbytmatch = 1 - (abs(health - health.lag) / (health + health.lag + .0001)),
    illness.tbytmatch = 1 - (abs(illness - illness.lag) / (illness + illness.lag + .0001)),
    wellness.tbytmatch = 1 - (abs(wellness - wellness.lag) / (wellness + wellness.lag + .0001)),
    mental.tbytmatch = 1 - (abs(mental - mental.lag) / (mental + mental.lag + .0001)),
    substances.tbytmatch = 1 - (abs(substances - substances.lag) / (substances + substances.lag + .0001)),
    death.tbytmatch = 1 - (abs(death - death.lag) / (death + death.lag + .0001)),
    need.tbytmatch = 1 - (abs(need - need.lag) / (need + need.lag + .0001)),
    want.tbytmatch = 1 - (abs(want - want.lag) / (want + want.lag + .0001)),
    acquire.tbytmatch = 1 - (abs(acquire - acquire.lag) / (acquire + acquire.lag + .0001)),
    fulfill.tbytmatch = 1 - (abs(fulfill - fulfill.lag) / (fulfill + fulfill.lag + .0001)),
    fatigue.tbytmatch = 1 - (abs(fatigue - fatigue.lag) / (fatigue + fatigue.lag + .0001)),
    reward.tbytmatch = 1 - (abs(reward - reward.lag) / (reward + reward.lag + .0001)),
    risk.tbytmatch = 1 - (abs(risk - risk.lag) / (risk + risk.lag + .0001)),
    curiosity.tbytmatch = 1 - (abs(curiosity - curiosity.lag) / (curiosity + curiosity.lag + .0001)),
    allure.tbytmatch = 1 - (abs(allure - allure.lag) / (allure + allure.lag + .0001)),
    Perception.tbytmatch = 1 - (abs(Perception - Perception.lag) / (Perception + Perception.lag + .0001)),
    attention.tbytmatch = 1 - (abs(attention - attention.lag) / (attention + attention.lag + .0001)),
    feeling.tbytmatch = 1 - (abs(feeling - feeling.lag) / (feeling + feeling.lag + .0001)),
    focuspast.tbytmatch = 1 - (abs(focuspast - focuspast.lag) / (focuspast + focuspast.lag + .0001)),
    focuspresent.tbytmatch = 1 - (abs(focuspresent - focuspresent.lag) / (focuspresent + focuspresent.lag + .0001)),
    focusfuture.tbytmatch = 1 - (abs(focusfuture - focusfuture.lag) / (focusfuture + focusfuture.lag + .0001)),
    Conversation.tbytmatch = 1 - (abs(Conversation - Conversation.lag) / (Conversation + Conversation.lag + .0001))
  ) %>%
  ungroup() %>%
  group_by(File,Speaker) %>%
  summarize(
    across(contains('.tbytmatch'), .fns = ~ mean(.x,na.rm=TRUE))) %>%
  ungroup() %>%
  mutate(psychological.tbytmatch = rowMeans(.[, c("Drives.tbytmatch", "Cognition.tbytmatch",
                                                  "Affect.tbytmatch", "Social.tbytmatch")],na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = File,
    names_from = Speaker,
    values_from = c(auxverb.tbytmatch, article.tbytmatch, adverb.tbytmatch, ipron.tbytmatch, prep.tbytmatch,
                    negate.tbytmatch, conj.tbytmatch, quantity.tbytmatch, ppron.tbytmatch, Drives.tbytmatch,
                    affiliation.tbytmatch, achieve.tbytmatch, power.tbytmatch, Cognition.tbytmatch, 
                    allnone.tbytmatch, cogproc.tbytmatch, memory.tbytmatch, Affect.tbytmatch, tone_pos.tbytmatch,
                    tone_neg.tbytmatch, emotion.tbytmatch, emo_pos.tbytmatch, emo_neg.tbytmatch, emo_anx.tbytmatch, 
                    emo_anger.tbytmatch, emo_sad.tbytmatch, swear.tbytmatch, Social.tbytmatch, socbehav.tbytmatch,
                    prosocial.tbytmatch, polite.tbytmatch, conflict.tbytmatch, moral.tbytmatch, comm.tbytmatch, 
                    socrefs.tbytmatch, Culture.tbytmatch, ethnicity.tbytmatch, Lifestyle.tbytmatch, leisure.tbytmatch,
                    home.tbytmatch, work.tbytmatch, money.tbytmatch, relig.tbytmatch, Physical.tbytmatch, health.tbytmatch,
                    illness.tbytmatch, wellness.tbytmatch, mental.tbytmatch, substances.tbytmatch, death.tbytmatch, 
                    need.tbytmatch, want.tbytmatch, acquire.tbytmatch, fulfill.tbytmatch, fatigue.tbytmatch, reward.tbytmatch,
                    risk.tbytmatch, curiosity.tbytmatch, allure.tbytmatch, Perception.tbytmatch, attention.tbytmatch,
                    feeling.tbytmatch, focuspast.tbytmatch, focuspresent.tbytmatch, focusfuture.tbytmatch, 
                    Conversation.tbytmatch, psychological.tbytmatch),
    names_glue = "{.value}.{Speaker}"
  )





###########

#AFTER THIS CODE, YOU ARE READY TO GO TO "0_ECHO_dataCleaning.R" for creating model dfs and then modeling
#LSM Conv, rLSM, and LIWC matching tbyt

ECHO_All_Matching_Measures_V2 <- list(ECHO_LSM_MLM, ECHO_smoothed_rLSM, ECHO_smoothed_LIWC_matching) %>% 
  reduce(full_join, by = "File")


# ############################################################################################################
# ############################################################################################################
# #####Adding chunk variable (based on Word Count and Turns) for ECHO TbyT transcript after processing through LIWC
# ############################################################################################################
# ############################################################################################################
# 


df_tbyt_V3 <- read_csv(here(config$ECHO_rolling_window_5_LIWC_path, config$ECHO_rolling_window_5_LIWC_name))

# #Use this if you want to use the tbyt file with 1 & 2 word turns INCLUDED
# df_tbyt_V2 <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_V3_path, config$ECHO_LSM_TbyT_Smoothed_V3_name))

# df_tbyt_V2 <- df_tbyt_V2 %>%
#   select(-'...1') %>%
#   select(-Sequence) %>%
#   select(-overall_sequence) %>%
#   select(-Word_count) %>%
#   select(-Segment)

df_tbyt_V3 <- df_tbyt_V3 %>%
  select(-'...1') %>%
  select(-Text) %>%
  select(-Sequence) %>%
  select(-overall_sequence) %>%
  select(-Word_count) %>%
  select(-text_agg_wc) %>%
  select(-Segment) 

ECHO_smoothed_chunks <- read_csv(here(config$ECHO_rolling_window_5_LIWC_path, config$ECHO_rolling_window_5_LIWC_name))

ECHO_smoothed_chunks <-ECHO_smoothed_chunks %>%
  select(-'...1') %>%
  select(-Text) %>%
  select(-Sequence) %>%
  select(-overall_sequence) %>%
  select(-Word_count) %>%
  select(-text_agg_wc) %>%
  select(-Segment) 
  
ECHO_smoothed_chunks <-ECHO_smoothed_chunks %>%
  filter(!(WC == 0))
  
  
  
   rename(output_order = A) %>%
  rename(File = B) %>%
  rename(Speaker = C) %>%
  rename(Text = D) %>%
  select(-E) %>%
  select(-F)

   
   
   
   
   
   
   
   

   ECHO_smoothed_chunks_wc <- ECHO_smoothed_chunks %>%
     group_by(File) %>%
     mutate(word_count = str_count(text_agg,"\\w+"),
            cumulative = cumsum(word_count),
            chunk = case_when(cumulative < (max(cumulative)/5) ~ 1,
                              cumulative < (max(cumulative/5))*2 ~ 2,
                              cumulative < (max(cumulative/5))*3 ~ 3,
                              cumulative < (max(cumulative/5))*4 ~ 4,
                              TRUE ~ 5)
     ) %>%
     ungroup() %>%
     relocate(chunk, .before = text_agg) %>%
     select(-c(word_count, cumulative))


####################

   
   ECHO_smoothed_chunks_wc_rLSM <- ECHO_smoothed_chunks_wc
   
   # test <- ECHO_smoothed_chunks_wc_rLSM %>%
   #   dplyr::select(File, Speaker) %>%
   #   group_by(File) %>%
   #   mutate(
   #     Speaker.lag = lag(Speaker)
   #   ) %>%
   #   ungroup() %>%
   #   mutate(
   #     speaker_match = if_else(Speaker == Speaker.lag,1,0)
   #   )
   # table(test$speaker_match)
   
   #SK: had to delete sequence from the list right below with select()
   ECHO_smoothed_chunks_wc_rLSM <- ECHO_smoothed_chunks_wc_rLSM %>%
     dplyr::select(File, Speaker, chunk, WC, auxverb, article, adverb, ipron, 
                   prep, negate, conj, quantity, ppron) %>%
     # Adding quick way to drop turn by turns from the same speaker
     ### WE NEED TO THINK ABOUT FLOW. WHERE WE"RE DOING SMOOTHING, ETC> (here or in cleaning)
     # group_by(File) %>%
     # mutate(
     #   Speaker.lag = lag(Speaker)
     # ) %>%
     # ungroup() %>%
     # mutate(
     #   speaker_match = if_else(Speaker == Speaker.lag,1,0)
     # ) %>%
     # ungroup() %>%
   # filter(speaker_match == 0) %>%
   # End dropping same speaker turns
   
   # group_by(File, Chunk) %>%
   # mutate(
   #   auxverb.orig = auxverb,
   #   article.orig = article,
   #   adverb.orig = adverb,
   #   ipron.orig = ipron,
   #   prep.orig = prep,
   #   negate.orig = negate,
   #   conj.orig = conj,
   #   quant.orig = quant,
   #   ppron.orig = ppron,
   #   WC.orig = WC) %>%
   # ungroup() %>%
   #rowwise() %>%
   # This puts the turn before the current one on the same row with a .lag suffix
   #dplyr::mutate(across(.cols=everything(), .funs = ~ dplyr::lead(.x,order_by=File,n = 1, default = NA), .names = '{.col}_lead')) %>%
   group_by(File, chunk) %>%
     mutate(
       auxverb.lag = lag(auxverb),
       article.lag = lag(article),
       adverb.lag = lag(adverb),
       ipron.lag = lag(ipron),
       prep.lag = lag(prep),
       negate.lag = lag(negate),
       conj.lag = lag(conj),
       quantity.lag = lag(quantity),
       ppron.lag = lag(ppron),
       WC.lag = lag(WC)) %>%
     ungroup() %>%
     # filter(WC > 1 & WC.lag >1) %>% # drops all exchanges with one word utterances
     # This makes sure that only liwc categories prersent in the first statement are used for rlsm
     mutate(across(c(auxverb.lag, article.lag, adverb.lag, ipron.lag, 
                     prep.lag, negate.lag, conj.lag, quantity.lag, ppron.lag), 
                   ~ if_else(. > 0,.,as.numeric(NA)))) %>%
     # This sets liwc categories in the responders speech to NA if that category was NA in the first person's speech
     # Per the rLSM paper
     group_by(File, chunk) %>%
     mutate(
       auxverb = if_else(is.na(auxverb.lag),as.numeric(NA),auxverb),
       article = if_else(is.na(article.lag),as.numeric(NA),article),
       adverb = if_else(is.na(adverb.lag),as.numeric(NA),adverb),
       ipron = if_else(is.na(ipron.lag),as.numeric(NA),ipron),
       prep = if_else(is.na(prep.lag),as.numeric(NA),prep),
       negate = if_else(is.na(negate.lag),as.numeric(NA),negate),
       conj = if_else(is.na(conj.lag),as.numeric(NA),conj),
       quantity = if_else(is.na(quantity.lag),as.numeric(NA),quantity),
       ppron = if_else(is.na(ppron.lag),as.numeric(NA),ppron)
     ) %>%
     ungroup() %>%
     rowwise() %>%
     mutate(
       auxverb.rLSM = 1 - (abs(auxverb - auxverb.lag) / (auxverb + auxverb.lag + .0001)),
       article.rLSM = 1 - (abs(article - article.lag) / (article + article.lag + .0001)),
       adverb.rLSM = 1 - (abs(adverb - adverb.lag) / (adverb + adverb.lag + .0001)),
       ipron.rLSM = 1 - (abs(ipron - ipron.lag) / (ipron + ipron.lag + .0001)),
       prep.rLSM = 1 - (abs(prep - prep.lag) / (prep + prep.lag + .0001)),
       negate.rLSM = 1 - (abs(negate - negate.lag) / (negate + negate.lag + .0001)),
       conj.rLSM = 1 - (abs(conj - conj.lag) / (conj + conj.lag + .0001)),
       quantity.rLSM = 1 - (abs(quantity - quantity.lag) / (quantity + quantity.lag + .0001)),
       ppron.rLSM = 1 - (abs(ppron - ppron.lag) / (ppron + ppron.lag + .0001))
     ) %>%
     ungroup() %>%
     #creates an average rLSM across separrate featurers
     #mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
     # mutate(rLSM = ifelse(is.na(rLSM), 0, rLSM)) %>% # replaces NAs with 0
     # group_by(File,Speaker) %>%
     # summarize(rLSM = mean(rLSM)) %>%
     # ungroup() %>%
     group_by(File, Speaker, chunk) %>%
     summarize(
       across(contains('.rLSM'), .fns = ~ mean(.x,na.rm=TRUE)),
       WC_sum = sum(WC)) %>%
     ungroup() %>%
     mutate(rLSM = rowMeans(select(.,contains('.rLSM')),na.rm = TRUE)) %>%
     pivot_wider(
       id_cols = File,
       names_from = c(chunk,Speaker),
       values_from = c(rLSM,WC_sum),
       # names_prefix = 'rLSM.'
       names_glue = "{.value}.{Speaker}.{chunk}"
     )  %>%
     rowwise()%>%
     mutate(
       mean.rLSM.1 = mean(c(rLSM.D.1, rLSM.P.1)),
       ratio.rLSM.D.1 = rLSM.D.1 / rLSM.P.1,
       ratio.rLSM.P.1 = rLSM.P.1 / rLSM.D.1,
       verb_dom.1 = WC_sum.D.1 / WC_sum.P.1,
       mean.rLSM.2 = mean(c(rLSM.D.2, rLSM.P.2)),
       ratio.rLSM.D.2 = rLSM.D.2 / rLSM.P.2,
       ratio.rLSM.P.2 = rLSM.P.2 / rLSM.D.2,
       verb_dom.2 = WC_sum.D.2 / WC_sum.P.2,
       mean.rLSM.3 = mean(c(rLSM.D.3, rLSM.P.3)),
       ratio.rLSM.D.3 = rLSM.D.3 / rLSM.P.3,
       ratio.rLSM.P.3 = rLSM.P.3 / rLSM.D.3,
       verb_dom.3 = WC_sum.D.3 / WC_sum.P.3,
       mean.rLSM.4 = mean(c(rLSM.D.4, rLSM.P.4)),
       ratio.rLSM.D.4 = rLSM.D.4 / rLSM.P.4,
       ratio.rLSM.P.4 = rLSM.P.4 / rLSM.D.4,
       verb_dom.4 = WC_sum.D.4 / WC_sum.P.4,
       mean.rLSM.5 = mean(c(rLSM.D.5, rLSM.P.5)),
       ratio.rLSM.D.5 = rLSM.D.5 / rLSM.P.5,
       ratio.rLSM.P.5 = rLSM.P.5 / rLSM.D.5,
       verb_dom.5 = WC_sum.D.5 / WC_sum.P.5,
       rLSM_Chunk_Ratio.D = rLSM.D.5/ rLSM.D.1,
       rLSM_Chunk_Ratio.P = rLSM.P.5/ rLSM.P.1) %>%
     # %>%
     #   filter(
     #     WC_sum.D >= 50 & WC_sum.P >= 50
     #   )
     #adding "_wc" at the end of variables since this chunking was based on word count
     rename_at(vars(-(File)), ~ paste0(., '_wc')) 
   
   
   

   
   
#####################################################


   
     rLSM.D.WC <- ECHO_smoothed_chunks_wc_rLSM %>%
       select(c(File, rLSM.D.1_wc, rLSM.D.2_wc, rLSM.D.3_wc, rLSM.D.4_wc, rLSM.D.5_wc)) %>%
       pivot_longer(
         cols= starts_with("rLSM"),
         names_to='chunk',
         values_to='rLSM.D') %>%
       mutate(provider_id = str_sub(File, 1, 4)) %>%
       mutate(site_name = str_sub(provider_id, 1, 2)) %>%
     filter(!(File == "JC08P08"))
     
     
     
     #creating the plot
     ggplot(data= rLSM.D.WC, mapping = aes(x = chunk, y = rLSM.D)) +
       geom_line(color="gray", aes(group = File)) + theme_light() + geom_point() +
       stat_summary(geom= "line", fun = "mean", color="red", size=1.5, linetype="dashed", group = 1) +
       facet_wrap(~ site_name)
     
##############################

     rLSM.P.WC <- ECHO_smoothed_chunks_wc_rLSM %>%
       select(c(File, rLSM.P.1_wc, rLSM.P.2_wc, rLSM.P.3_wc, rLSM.P.4_wc, rLSM.P.5_wc)) %>%
       pivot_longer(
         cols= starts_with("rLSM"),
         names_to='chunk',
         values_to='rLSM.P') %>%
       mutate(provider_id = str_sub(File, 1, 4)) %>%
       mutate(site_name = str_sub(provider_id, 1, 2)) %>%
       filter(!(File == "JC08P08"))
     
    
     
     
     
     #creating the plot
     ggplot(data= rLSM.P.WC, mapping = aes(x = chunk, y = rLSM.P)) +
       geom_line(color="gray", aes(group = File)) + theme_light() + geom_point() +
       stat_summary(geom= "line", fun = "mean", color="red", size=1.5, linetype="dashed", group = 1) +
       facet_wrap(~ site_name)
     


###############################
     
     ##############################
     
     ratio.rLSM.P.WC <- ECHO_smoothed_chunks_wc_rLSM %>%
       select(c(File, ratio.rLSM.P.1_wc, ratio.rLSM.P.2_wc, ratio.rLSM.P.3_wc, ratio.rLSM.P.4_wc, ratio.rLSM.P.5_wc)) %>%
       pivot_longer(
         cols= starts_with("ratio"),
         names_to='chunk',
         values_to='ratio.rLSM.P') %>%
       mutate(provider_id = str_sub(File, 1, 4)) %>%
       mutate(site_name = str_sub(provider_id, 1, 2)) %>%
       filter(!(File == "JC08P08"))
     
     
     
     #creating the plot
     ggplot(data= ratio.rLSM.P.WC, mapping = aes(x = chunk, y = ratio.rLSM.P)) +
       geom_line(color="gray", aes(group = File)) + theme_light() + geom_point() +
       stat_summary(geom= "line", fun = "mean", color="red", size=1.5, linetype="dashed", group = 1) +
       facet_wrap(~ site_name)

     
#########################
#########################    
     
     

     ratio.rLSM.D.WC <- ECHO_smoothed_chunks_wc_rLSM %>%
       select(c(File, ratio.rLSM.D.1_wc, ratio.rLSM.D.2_wc, ratio.rLSM.D.3_wc, ratio.rLSM.D.4_wc, ratio.rLSM.D.5_wc)) %>%
       pivot_longer(
         cols= starts_with("ratio"),
         names_to='chunk',
         values_to='ratio.rLSM.D') %>%
       mutate(provider_id = str_sub(File, 1, 4)) %>%
       mutate(site_name = str_sub(provider_id, 1, 2)) %>%
       filter(!(File == "JC08P08"))
     
     
     
     #creating the plot
     ggplot(data= ratio.rLSM.D.WC, mapping = aes(x = chunk, y = ratio.rLSM.D)) +
       geom_line(color="gray", aes(group = File)) + theme_light() + geom_point() +
       stat_summary(geom= "line", fun = "mean", color="red", size=1.5, linetype="dashed", group = 1) +
       facet_wrap(~ site_name)
     
#########################
#########################      
     verb_dom.WC <- ECHO_smoothed_chunks_wc_rLSM %>%
       select(c(File, verb_dom.1_wc, verb_dom.2_wc, verb_dom.3_wc, verb_dom.4_wc, verb_dom.5_wc)) %>%
       pivot_longer(
         cols= starts_with("verb"),
         names_to='chunk',
         values_to='verb_dom') %>%
       mutate(provider_id = str_sub(File, 1, 4)) %>%
       mutate(site_name = str_sub(provider_id, 1, 2)) %>%
       filter(!(File == "JC08P08"))
     

     
     #creating the plot
     ggplot(data= verb_dom.WC, mapping = aes(x = chunk, y = verb_dom)) +
       geom_line(color="gray", aes(group = File)) + theme_light() + geom_point() +
       stat_summary(geom= "line", fun = "mean", color="red", size=1.5, linetype="dashed", group = 1) +
       facet_wrap(~ site_name)
     
     
#########################
#########################  
     
JC08P08_transcript <- ECHO_smoothed_chunks_wc %>%
       filter(File == "JC08P08") %>%
       select(c(File, Speaker, chunk, text_agg))
     
     
     JC08P08_transcript <- ECHO_smoothed_chunks_wc %>%
       filter(File == "JC08P08") %>%
       select(c(File, Speaker, chunk, text_agg))
     ###############################

test_sample <- tribble(
  ~Participant_ID, ~timepoint_1,  ~timepoint_2, ~timepoint_3, ~timepoint_4, ~timepoint_5,
  "a1", .1,  .3, .2, .4, .5,
  "a2", .2,  .4, .5, .3, .6,
  "a3", .1,  .3, .2, .4, .5,
  "a4", .25,  .3, .35, .44, .25,
  "a6", .11,  .23, .24, .54, .55,
  "a7", .21,  .32, .22, .46, .45,
  "a8", .13,  .36, .42, .49, .75,
  "a9", .28,  .13, .22, .44, .55
)

#changing the data into long format
test_sample <- test_sample %>%
  pivot_longer(
    cols= starts_with("timepoint"),
               names_to='timepoint',
               values_to='score')

#creating the plot
ggplot(data= test_sample, mapping = aes(x = timepoint, y = score)) +
  geom_line(color="gray", aes(group = Participant_ID)) + theme_light() + geom_point() +
  stat_summary(geom= "line", fun = "mean", color="red", size=2, linetype="dashed", group = 1) 










###########################################################################################
#OLD CODE###
###########################################################################################

# Sys.setenv(R_CONFIG_ACTIVE = "salar") # 'default')#
# config <- config::get()
# 
# ECHO_Transcripts_Complete_TbyT <- read_csv(here(config$ECHO_Transcript_path, config$ECHO_Transcript_name))
# 
# #deleting first column of data
# ECHO_Transcripts_Complete_TbyT <- select(ECHO_Transcripts_Complete_TbyT, -c(...1))
# 
# ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
#   mutate(overall_sequence = 1:n())
# 
# 
# ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
#   #JC11P03: deleting from beginning to end of D1-P conversation
#   filter(!(overall_sequence >= 111 & overall_sequence <= 156 | 
#            overall_sequence >= 1 & overall_sequence <= 4)) %>%
#   #JC08P09: deleting from beginning to end of D1-UM conversation
#   filter(!(overall_sequence >= 4591 & overall_sequence <= 4597)) %>%
#   #JC08P10: deleting from beginning to end of D1-UM conversation
#   filter(!(overall_sequence >= 5760 & overall_sequence <= 5863)) %>%
#   #JC11P06: deleting from beginning to end of D-D1-P conversation and
#   #at the end of the conversation once the D-P conversation ends
#   filter(!(overall_sequence >= 8135 & overall_sequence <= 8141 |
#              overall_sequence >= 8361 & overall_sequence <= 8490 |
#              overall_sequence >= 8524 & overall_sequence <= 8538)) %>%
#   #JC08P06: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 14756 & overall_sequence <= 14789)) %>%
#   #JC11P01: deleting from beginning to end of D-D1-P conversation & D-D1-D2-P conv.
#   filter(!(overall_sequence >= 15570 & overall_sequence <= 15662 |
#           overall_sequence >= 15683 & overall_sequence <= 15794 )) %>%
#   #JC11P09: deleting from beginning to end of D1-P conversation
#   filter(!(overall_sequence >= 22438 & overall_sequence <= 22506)) %>%
#   #JC08P03: deleting sequences with minor speech and D-D1-P conv.
#   filter(!(overall_sequence >= 23174 & overall_sequence <= 23200 |
#            overall_sequence >= 23347 & overall_sequence <= 23352 |
#            overall_sequence >= 23549 & overall_sequence <= 23575 |  
#            overall_sequence >= 23574 & overall_sequence <= 23642 |   
#            overall_sequence >= 23680 & overall_sequence <= 23777)) %>%
#   #JC11P04: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 26221 & overall_sequence <= 26410)) %>%
#   #JC11P07: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 27674 & overall_sequence <= 27847 |
#            overall_sequence >= 27660 & overall_sequence <= 27663  )) %>%
#   #JC11P02: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 34612 & overall_sequence <= 34704)) %>%
#   #JC11P08: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 39866 & overall_sequence <= 39953)) %>%
#   #JC11P05: deleting from RA/D conv and beginning to end of D-D1-P & D-NP-P conversations
#   filter(!(overall_sequence >= 41538 & overall_sequence <= 41545 |
#            overall_sequence >= 41820 & overall_sequence <= 41927)) %>%
#   #JC08P07: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 44583 & overall_sequence <= 44603)) %>%
#   #SC24P122: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 96319 & overall_sequence <= 96324)) %>%
#   #SC18P110: deleting from beginning to end of D-D1-P conversation
#   filter(!(overall_sequence >= 106608 & overall_sequence <= 106645)) %>%
#   #SC24P114: deleting from beginning to end of D-D1-P-UM conversation
#   filter(!(overall_sequence >= 121311 & overall_sequence <= 121593)) %>%
#   #JC08P01: deleting from beginning to end of D-D1-P & D-D2-P conversation
#   filter(!(overall_sequence >= 10747 & overall_sequence <= 10752 |
#            overall_sequence >= 10792 & overall_sequence <= 10884)) %>%
#   #JC08P02: deleting from beginning to end of D-D2-P conversation
#   filter(!(overall_sequence >= 41416 & overall_sequence <= 41513)) %>%
#   #JC01P08: deleting from beginning to end of D-PA conversation
#   filter(!(overall_sequence >= 48104 & overall_sequence <= 48118)) %>%
#   #OC08P10: deleting from beginning to end of D-PA conversation
#   filter(!(overall_sequence >= 61639 & overall_sequence <= 61665)) %>%
#   #OC07P11: deleting from beginning to end of D-PA-P conversation
#   filter(!(overall_sequence >= 64298 & overall_sequence <= 64302)) %>%
#   #OC05P07: deleting from beginning to end of P-PA conversation
#   filter(!(overall_sequence >= 76586 & overall_sequence <= 76594)) %>%
#   #SC12P062: deleting from beginning to end of D-PA conversation
#   filter(!(overall_sequence >= 99145 & overall_sequence <= 99149)) %>%
#   #SC12P048: deleting from beginning to end of D-RA and D-PA conversations
#   filter(!(overall_sequence >= 116497 & overall_sequence <= 116500 |
#            overall_sequence >= 116593 & overall_sequence <= 116598)) %>%
#   #OC07P02: deleting from beginning to end of RN-P conversation
#   filter(!(overall_sequence >= 58104 & overall_sequence <= 58122)) %>%
#   #OC06P02: deleting from beginning to end of RN-P conversation
#   filter(!(overall_sequence >= 75945 & overall_sequence <= 75949 |
#     overall_sequence >= 76170 & overall_sequence <= 76180)) %>%
#   #SC06P101: deleting from beginning to end of RN-D conversation
#   filter(!(overall_sequence >= 112035 & overall_sequence <= 112037)) %>%
#   #JC01P01: deleting from beginning to end of RN-D-P conversation
#   filter(!(overall_sequence >= 23129 & overall_sequence <= 23173)) %>%
#   #OC07P12: deleting from beginning to end of PA-D conversation
#   filter(!(overall_sequence >= 67529 & overall_sequence <= 67531)) %>%
#   #OC03P07: deleting from beginning to end of PA-D conversation
#   filter(!(overall_sequence >= 67529 & overall_sequence <= 67531)) %>%
#   #SC20P068: deleting RA/P & conversation with family members involved
#   filter(!(overall_sequence >= 112603 & overall_sequence <= 112610 |
#     overall_sequence >= 112998 & overall_sequence <= 113251)) %>%
#   #OC07P03: deleting conversation with other speakers
#   filter(!(overall_sequence >= 69797 & overall_sequence <= 69806 |
#            overall_sequence >= 70094 & overall_sequence <= 70097)) %>%
#   #OC03P06: deleting conversation with other speakers
#   filter(!(overall_sequence >= 71512 & overall_sequence <= 71516 |
#              overall_sequence >= 71824 & overall_sequence <= 71830)) %>%
#   #OC04P04: deleting conversation with other speakers
#   filter(!(overall_sequence >= 76782 & overall_sequence <= 76784 |
#              overall_sequence >= 76810 & overall_sequence <= 76812 |
#              overall_sequence >= 76862 & overall_sequence <= 76896)) %>%
#   #OC01P07: deleting RA/D/P & conversation at end without doctor
#   filter(!(overall_sequence >= 77778 & overall_sequence <= 77793 |
#     overall_sequence >= 78190 & overall_sequence <= 78195)) %>%
#   #OC03P04: deleting conversation with UM/P & UF & RA/P
#   filter(!(overall_sequence >= 80160 & overall_sequence <= 80168 |
#            overall_sequence >= 80173 & overall_sequence <= 80178 |
#            overall_sequence >= 80446 & overall_sequence <= 80452)) %>%
#   #SC13P083: deleting conversation with UM/P & UF
#   filter(!(overall_sequence >= 83964 & overall_sequence <= 83969)) %>%
#   #SC12P016: deleting conversation with UM/U/P
#   filter(!(overall_sequence >= 85819 & overall_sequence <= 85822)) %>%
#   #SC23P056: deleting conversation with UM/U/P
#   filter(!(overall_sequence >= 84731 & overall_sequence <= 84736)) %>%
#   #SC12P038: deleting conversation with D/UF/P & a exchange in spanish
#   filter(!(overall_sequence >= 88040 & overall_sequence <= 88046 |
#            overall_sequence >= 87789 & overall_sequence <= 87790  )) %>%
#   #SC21P133: deleting conversation with D/UM
#   filter(!(overall_sequence >= 88933 & overall_sequence <= 88936)) %>%
#   #SC15P116: deleting conversation with D/UM/P conv
#   filter(!(overall_sequence >= 89400 & overall_sequence <= 89421)) %>%
#   #SC15P111: deleting conversation with D/UM/P conv
#   filter(!(overall_sequence >= 95704 & overall_sequence <= 95707 |
#            overall_sequence >= 95852 & overall_sequence <= 95855)) %>%
#   #SC23P046: deleting conversation with D/UM conv
#   filter(!(overall_sequence >= 108979 & overall_sequence <= 108994)) %>%
#   #SC20P074: deleting conversation with D/UM/P conv
#   filter(!(overall_sequence >= 115743 & overall_sequence <= 115768 |
#              overall_sequence >= 115780 & overall_sequence <= 115790)) %>%
#   #SC20P092: deleting conversation with P/UM conv
#   filter(!(overall_sequence >= 122247 & overall_sequence <= 122273)) %>%
#   #JC04P09: deleting conversation with P/UM conv
#   filter(!(overall_sequence >= 38250 & overall_sequence <= 38284)) %>%
#   #SC21P126: deleting conversation with RA/D & P/UM conv
#   filter(!(overall_sequence >= 107328 & overall_sequence <= 107330 |
#            overall_sequence >= 107565 & overall_sequence <= 107571)) %>%
#   #SC19P053: deleting conversation with D/UF conv
#   filter(!(overall_sequence >= 120601 & overall_sequence <= 120609)) %>%
#   #OC03P05: deleting conversation with RA/P & P/UM conv
#   filter(!(overall_sequence >= 60148 & overall_sequence <= 60156 |
#            overall_sequence >= 60572 & overall_sequence <= 60574)) %>%
#   #OC01P09: deleting conversation with P/UM conv
#   filter(!(overall_sequence >= 54868 & overall_sequence <= 54875 |
#            overall_sequence >= 54994 & overall_sequence <= 54995)) %>%
#   #JC14P01: deleting conversation with P/UM conv
#   filter(!(overall_sequence >= 36610 & overall_sequence <= 36614 |
#            overall_sequence >= 36748 & overall_sequence <= 36797 |
#            overall_sequence >= 37024 & overall_sequence <= 37027  )) %>%
#   #JC13P07: deleting conversation with D/UM conv
#   filter(!(overall_sequence >= 17001 & overall_sequence <= 17008 |
#     overall_sequence >= 17010 & overall_sequence <= 17013)) %>%
#   #SC07P099: deleting conversation with  D/UF/P conv
#   filter(!(overall_sequence >= 118585 & overall_sequence <= 118638)) %>%
#   #SC07P022: deleting conversation with  D/UF conv
#   filter(!(overall_sequence >= 123252 & overall_sequence <= 123254)) %>%
#   #SC23P058: deleting conversation with  D/UF conv
#   filter(!(overall_sequence >= 122808 & overall_sequence <= 122816)) %>%
#   #SC20P057: deleting conversation with  D/UF conv
#   filter(!(overall_sequence >= 119185 & overall_sequence <= 119191 |
#            overall_sequence >= 119637 & overall_sequence <= 119638)) %>%
#   #SC24P089: deleting conversation with R/D &  P/UF/RA conv
#   filter(!(overall_sequence >= 117377 & overall_sequence <= 117381 |
#              overall_sequence >= 117791 & overall_sequence <= 117794)) %>%
#   #SC20P138: deleting conversation with   D/UF conv
#   filter(!(overall_sequence >= 117237 & overall_sequence <= 117242 |
#              overall_sequence >= 117335 & overall_sequence <= 117339)) %>%
#   #SC06P047: deleting conversation with D/RA & D/UF conv
#   filter(!(overall_sequence >= 110856 & overall_sequence <= 110864 |
#              overall_sequence >= 111174 & overall_sequence <= 111184)) %>%
#   #SC24P035: deleting conversation with  D/UF conv
#   filter(!(overall_sequence >= 109833 & overall_sequence <= 109839)) %>%
#   #SC23P024: deleting conversation with  D/UF conv
#   filter(!(overall_sequence >= 108530 & overall_sequence <= 108542)) %>%
#   #SC18P110: deleting conversation with  D/UF conv
#   filter(!(overall_sequence >= 106706 & overall_sequence <= 106715)) %>%
#   #SC07P039: deleting conversation after D/P said bye
#   filter(!(overall_sequence >= 103671 & overall_sequence <= 103676)) %>%
#   #SC23P129: deleting conversation with  D/UF conv
#   filter(!(overall_sequence >= 102285 & overall_sequence <= 102290 |
#              overall_sequence >= 102916 & overall_sequence <= 102923)) %>%
#   #SC23P018: deleting conversation with UF/D/RA convo
#   filter(!(overall_sequence >= 100068 & overall_sequence <= 100076)) %>%
#   #SC07P141: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 98842 & overall_sequence <= 98845)) %>%
#   #SC12P044: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 98652 & overall_sequence <= 98668)) %>%
#   #SC07P067: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 93738 & overall_sequence <= 93741)) %>%
#   #SC15P095: deleting conversation with  D/RA and D/UF convs
#   filter(!(overall_sequence >= 92939 & overall_sequence <= 92941 |
#              overall_sequence >= 93094 & overall_sequence <= 93109)) %>%
#   #SC24P096: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 92270 & overall_sequence <= 92279)) %>%
#   #SC21P109: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 91974 & overall_sequence <= 91976)) %>%
#   #SC23P064: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 91382 & overall_sequence <= 91386)) %>%
#   #SC15P071: deleting conversation with UF/D/RA/P convo
#   filter(!(overall_sequence >= 90067 & overall_sequence <= 90077)) %>%
#   #SC12P038: deleting conversation with UM/D and end of visit convo
#   filter(!(overall_sequence >= 88167 & overall_sequence <= 88172 |
#            overall_sequence >= 88200 & overall_sequence <= 88204)) %>%
#   #SC21P118: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 87553 & overall_sequence <= 87557)) %>%
#   #SC23P020: deleting conversation with UF/D convo
#   filter(!(overall_sequence >= 86545 & overall_sequence <= 86551)) %>%
#   #SC19P144: deleting conversation with RA/D & UF/P convo
#   filter(!(overall_sequence >= 81821 & overall_sequence <= 81823 |
#            overall_sequence >= 81895 & overall_sequence <= 81897 )) %>%
#   #OC02P07: deleting conversation with RA/P convo, D/UF, RA/D convos
#   filter(!(overall_sequence >= 79837 & overall_sequence <= 79840 |
#            overall_sequence >= 80030 & overall_sequence <= 80034 |
#            overall_sequence >= 80154 & overall_sequence <= 80159)) %>%
#   #OC01P02: deleting conversation with RA/D & UF/P convo
#   filter(!(overall_sequence >= 79488 & overall_sequence <= 79492 | 
#            overall_sequence >= 79832 & overall_sequence <= 79836)) %>%
#   #OC04P01: deleting conversation with UF/P convos
#   filter(!(overall_sequence >= 78644 & overall_sequence <= 78649 |
#            overall_sequence >= 78970 & overall_sequence <= 78980 )) %>%
#   #OC05P11: deleting conversation with UF/D/P convo
#   filter(!(overall_sequence >= 73206 & overall_sequence <= 73211)) %>%
#   #OC02P08: deleting conversation with UF/P convo at end after D leaves
#   filter(!(overall_sequence >= 72921 & overall_sequence <= 72930)) %>%
#   #OC03P06: deleting conversation with UF/P convo at end after D leaves
#   filter(!(overall_sequence >= 71905 & overall_sequence <= 71911)) %>%
#   #OC02P05: deleting conversation with conversation before start & UF/D/P convo 
#   filter(!(overall_sequence >= 71142 & overall_sequence <= 71147 | 
#            overall_sequence >= 71479 & overall_sequence <= 71487 )) %>%
#   #OC08P02: deleting conversation with UF/D 
#   filter(!(overall_sequence >= 70868 & overall_sequence <= 70871)) %>%
#   #OC01P05: deleting conversation with UF/D/P 
#   filter(!(overall_sequence >= 69423 & overall_sequence <= 69433)) %>%
#   #OC05P08: deleting conversation with UF/P 
#   filter(!(overall_sequence >= 67591 & overall_sequence <= 67594)) %>%
#   #OC03P11: deleting conversation with UF/P & RA/P
#   filter(!(overall_sequence >= 62982 & overall_sequence <= 62993 | 
#            overall_sequence >= 63110 & overall_sequence <= 63118 )) %>%
#   #OC05P03: deleting conversation with UF/P 
#   filter(!(overall_sequence >= 59336 & overall_sequence <= 59343)) %>%
#   #OC06P04: deleting conversation with UF/P 
#   filter(!(overall_sequence >= 51413 & overall_sequence <= 51420 | 
#            overall_sequence >= 51645 & overall_sequence <= 51655)) %>%
#   #JC14P03: deleting conversation with UF/D/P 
#   filter(!(overall_sequence >= 45241 & overall_sequence <= 45259)) %>%
#   #JC03P10: deleting conversation with RA/D & UF/D/P 
#   filter(!(overall_sequence >= 37584 & overall_sequence <= 37587 |
#            overall_sequence >= 37892 & overall_sequence <= 37902)) %>%
#   #JC01P02: deleting conversation with UF/D/P 
#   filter(!(overall_sequence >= 32195 & overall_sequence <= 32214 |
#            overall_sequence >= 32396 & overall_sequence <= 32400 |
#            overall_sequence >= 32435 & overall_sequence <= 32446 |
#            overall_sequence >= 32529 & overall_sequence <= 32532)) %>%
#   #JC07P04: deleting conversation with UF/D/P 
#   filter(!(overall_sequence >= 28103 & overall_sequence <= 28105)) %>%
#   #JC03P09: deleting conversation with RA/P and conversation after P leaves
#   filter(!(overall_sequence >= 27145 & overall_sequence <= 27150 |
#            overall_sequence >= 27499 & overall_sequence <= 27511)) %>%
#   #JC12P10: deleting conversation with UF/D 
#   filter(!(overall_sequence >= 21388 & overall_sequence <= 21401)) %>%
#   #JC10P05: deleting conversation with UF/P 
#   filter(!(overall_sequence >= 9002 & overall_sequence <= 9011)) %>%
#   #JC12P03: deleting conversation with UF/P 
#   filter(!(overall_sequence >= 7982 & overall_sequence <= 7986)) %>%
#   #JC08P04: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 3083 & overall_sequence <= 3096)) %>%
#   #JC10P11: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 4778 & overall_sequence <= 4786)) %>%
#   #JC10P08: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 5351 & overall_sequence <= 5356)) %>%
#   #JC03P08: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 5893 & overall_sequence <= 5895)) %>%
#   #JC14P08: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 6587 & overall_sequence <= 6592)) %>%
#   #JC14P05: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 11586 & overall_sequence <= 11589)) %>%
#   #JC06P01: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 15973 & overall_sequence <= 15980)) %>%
#   #JC02P10: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 21017 & overall_sequence <= 21023)) %>%
#   #JC02P04: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 23778 & overall_sequence <= 23784)) %>%
#   #JC14P07: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 23881 & overall_sequence <= 23887)) %>%
#   #JC03P07: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 24124 & overall_sequence <= 24129)) %>%
#   #JC12P02: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 30473 & overall_sequence <= 30475)) %>%
#   #JC04P01: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 30966 & overall_sequence <= 30970)) %>%
#   #JC14P04: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 31144 & overall_sequence <= 31147)) %>%
#   #JC09P03: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 31850 & overall_sequence <= 31852)) %>%
#   #JC04P04: deleting conversation with RA/D & P/UF
#   filter(!(overall_sequence >= 33596 & overall_sequence <= 33599)) %>%
#   #JC02P02: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 37242 & overall_sequence <= 37247)) %>%
#   #JC03P01: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 37248 & overall_sequence <= 37255)) %>%
#   #JC10P10: deleting conversation with RA/D & D/P/UF
#   filter(!(overall_sequence >= 38846 & overall_sequence <= 38858 |
#            overall_sequence >= 38934 & overall_sequence <= 38939)) %>%
#   #JC02P05: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 39965 & overall_sequence <= 39971)) %>%
#   #JC07P06: deleting conversation with RA/D 
#   filter(!(overall_sequence >= 41948 & overall_sequence <= 41953)) %>%
#   #JC09P04: deleting conversation with RA/D/P
#   filter(!(overall_sequence >= 44204 & overall_sequence <= 44211)) %>%
#   #JC01P05: deleting conversation with RA/D/P
#   filter(!(overall_sequence >= 44661 & overall_sequence <= 44669)) %>%
#   #JC10P03: deleting conversation with RA/D
#   filter(!(overall_sequence >= 48098 & overall_sequence <= 48103)) %>%
#   #OC08P03: deleting conversation with RA/P
#   filter(!(overall_sequence >= 50747 & overall_sequence <= 50754)) %>%
#   #OC01P01: deleting conversation with RA/P
#   filter(!(overall_sequence >= 50755 & overall_sequence <= 50757 |
#            overall_sequence >= 51133 & overall_sequence <= 51137)) %>%
#   #OC07P07: deleting conversation with RA/P
#   filter(!(overall_sequence >= 51138 & overall_sequence <= 51143 |
#            overall_sequence >= 51398 & overall_sequence <= 51401  )) %>%
#   #OC05P01: deleting conversation with RA/D/P & RA/P
#   filter(!(overall_sequence >= 52138 & overall_sequence <= 52165 |
#            overall_sequence >= 52708 & overall_sequence <= 52716)) %>%
#   #OC02P10: deleting conversation with RA/P/D
#   filter(!(overall_sequence >= 55330 & overall_sequence <= 55335)) %>%
#   #OC05P09: deleting conversation with RA/P &RA/P
#   filter(!(overall_sequence >= 55503 & overall_sequence <= 55505 |
#            overall_sequence >= 56195 & overall_sequence <= 56200)) %>%
#   #OC01P04: deleting conversation with RA/D
#   filter(!(overall_sequence >= 56201 & overall_sequence <= 56207)) %>%
#   #OC02P01: deleting conversation with RA/D/P
#   filter(!(overall_sequence >= 57243 & overall_sequence <= 57256)) %>%
#   #OC04P07: deleting conversation with RA/P
#   filter(!(overall_sequence >= 57932 & overall_sequence <= 57939)) %>%
#   #OC06P01: deleting conversation with RA/P
#   filter(!(overall_sequence >= 58499 & overall_sequence <= 58502 |
#            overall_sequence >= 58809 & overall_sequence <= 58812)) %>%
#   #OC07P05: deleting conversation with RA/P
#   filter(!(overall_sequence >= 58813 & overall_sequence <= 58818)) %>%
#   #OC06P06: deleting conversation with RA/P
#   filter(!(overall_sequence >= 59085 & overall_sequence <= 59089 |
#            overall_sequence >= 59328 & overall_sequence <= 59335)) %>%
#   #OC02P01: deleting conversation with RA/D/P
#   filter(!(overall_sequence >= 57243 & overall_sequence <= 57256)) %>%
#   #OC01P03: deleting conversation with RA/D/P & RA/P
#   filter(!(overall_sequence >= 60577 & overall_sequence <= 60580 |
#            overall_sequence >= 60889 & overall_sequence <= 60899)) %>%
#   #OC08P01: deleting conversation with RA/P
#   filter(!(overall_sequence >= 61542 & overall_sequence <= 61549)) %>%
#   #OC01P12: deleting conversation with RA/P
#   filter(!(overall_sequence >= 61803 & overall_sequence <= 61809)) %>%
#   #OC07P08: deleting conversation with RA/P
#   filter(!(overall_sequence >= 62144 & overall_sequence <= 62154)) %>%
#   #OC02P12: deleting conversation with RA/P
#   filter(!(overall_sequence >= 63119 & overall_sequence <= 63121 |
#            overall_sequence >= 63407 & overall_sequence <= 63415)) %>%
#   #OC08P09: deleting conversation with RA/P & D/PA &end of conv
#   filter(!(overall_sequence >= 64351 & overall_sequence <= 64353 |
#            overall_sequence >= 64377 & overall_sequence <= 64381 |
#            overall_sequence >= 64765 & overall_sequence <= 64769)) %>%
#   #OC05P06: deleting conversation with RA/P
#   filter(!(overall_sequence >= 65029 & overall_sequence <= 65035)) %>%
#   #OC04P05: deleting conversation with D/P/RA & RA/P
#   filter(!(overall_sequence >= 65036 & overall_sequence <= 65048 |
#            overall_sequence >= 65395 & overall_sequence <= 65417)) %>%
#   #OC01P06: deleting conversation with RA/P
#   filter(!(overall_sequence >= 65935 & overall_sequence <= 65942 |
#            overall_sequence >= 66407 & overall_sequence <= 66419)) %>%
#   #OC03P03: deleting conversation with RA/P/D & RA/P
#   filter(!(overall_sequence >= 68494 & overall_sequence <= 68500 |
#            overall_sequence >= 68929 & overall_sequence <= 68937)) %>%
#   #OC05P05: deleting conversation with RA/P
#   filter(!(overall_sequence >= 70100 & overall_sequence <= 70116)) %>%
#   #OC04P03: deleting conversation with RA/P
#   filter(!(overall_sequence >= 71912 & overall_sequence <= 71924 |
#            overall_sequence >= 72058 & overall_sequence <= 72062)) %>%
#   #OC07P06: deleting conversation with RA/P
#   filter(!(overall_sequence >= 72063 & overall_sequence <= 72070 | 
#            overall_sequence >= 72276 & overall_sequence <= 72279)) %>%
#   #OC04P12: deleting conversation with RA/P
#   filter(!(overall_sequence >= 72931 & overall_sequence <= 72935)) %>%
#   #OC06P08: deleting conversation with RA/P
#   filter(!(overall_sequence >= 73730 & overall_sequence <= 73732)) %>%
#   #OC08P08: deleting conversation with RA/P
#   filter(!(overall_sequence >= 75102 & overall_sequence <= 75106)) %>%
#   #OC07P01: deleting conversation with RA/P
#   filter(!(overall_sequence >= 75748 & overall_sequence <= 75754)) %>%
#   #OC05P02: deleting conversation with RA/P/D
#   filter(!(overall_sequence >= 78197 & overall_sequence <= 78203)) %>%
#   #OC07P04: deleting conversation with RA/P/D
#   filter(!(overall_sequence >= 78981 & overall_sequence <= 78989)) %>%
#   #OC06P07: deleting conversation with RA/P/D
#   filter(!(overall_sequence >= 79168 & overall_sequence <= 79173 |
#            overall_sequence >= 79480 & overall_sequence <= 79487)) %>%
#   #OC08P11: deleting conversation with RA/P
#   filter(!(overall_sequence >= 81156 & overall_sequence <= 81163)) %>%
#   #OC05P13: deleting conversation with RA/D & RA/P
#   filter(!(overall_sequence >= 81408 & overall_sequence <= 81410 |
#            overall_sequence >= 81811 & overall_sequence <= 81820)) %>%
#   #SC19P144: deleting conversation with RA/D
#   filter(!(overall_sequence >= 81821 & overall_sequence <= 81823)) %>%
#   #SC21P127: deleting conversation with RA/P
#   filter(!(overall_sequence >= 82814 & overall_sequence <= 82816)) %>%
#   #SC13P002: deleting conversation with RA/P
#   filter(!(overall_sequence >= 83270 & overall_sequence <= 83272)) %>%
#   #SC12P016: deleting conversation with RA/P
#   filter(!(overall_sequence >= 85614 & overall_sequence <= 85616)) %>%
#   #SC13P079: deleting conversation with RA/P
#   filter(!(overall_sequence >= 91623 & overall_sequence <= 91625)) %>%
#   #SC12P060: deleting conversation with RA/P
#   filter(!(overall_sequence >= 92516 & overall_sequence <= 92518)) %>%
#   #SC20P061: deleting conversation with RA/P
#   filter(!(overall_sequence >= 93138 & overall_sequence <= 93140)) %>%
#   #SC24P080: deleting conversation with RA/P
#   filter(!(overall_sequence >= 96795 & overall_sequence <= 96799)) %>%
#   #SC24P066: deleting conversation with RA/P
#   filter(!(overall_sequence >= 98166 & overall_sequence <= 98169)) %>%
#   #SC06P100: deleting conversation with RA/D
#   filter(!(overall_sequence >= 98285 & overall_sequence <= 98291)) %>%
#   #SC21P119: deleting conversation with RA/D
#   filter(!(overall_sequence >= 101306 & overall_sequence <= 101308 |
#            overall_sequence >= 101591 & overall_sequence <= 101593)) %>%
#   #SC19P045: deleting conversation with RA/D
#   filter(!(overall_sequence >= 103967 & overall_sequence <= 103972)) %>%
#   #SC18P004: deleting conversation with RA/D
#   filter(!(overall_sequence >= 105949 & overall_sequence <= 105952)) %>%
#   #SC18P104: deleting conversation with RA/D
#   filter(!(overall_sequence >= 109789 & overall_sequence <= 109791)) %>%
#   #SC18P069: deleting conversation with RA/D
#   filter(!(overall_sequence >= 111473 & overall_sequence <= 111480)) %>%
#   #SC23P023: deleting conversation with RA/D
#   filter(!(overall_sequence >= 113458 & overall_sequence <= 113460)) %>%
#   #SC06P001: deleting conversation with RA/D
#   filter(!(overall_sequence >= 120284 & overall_sequence <= 120294)) %>%
#   #SC07P009: deleting conversation with RA/D
#   filter(!(overall_sequence >= 121674 & overall_sequence <= 121678)) %>%
#   #SC15P018: deleting conversation with RA/D
#   filter(!(overall_sequence >= 124232 & overall_sequence <= 124238)) %>%
#   #OC08P08: deleting conversation with RA/D
#   filter(!(overall_sequence >= 75107 & overall_sequence <= 75108))
#   
# 
# ECHO_transcript_exclude_remove <- c('JC10P01', 'JC09P06', 'OC03P01',
#                                     'JC04P03', 'SC18P094', 'JC06P06',
#                                     'SC06P075', 'JC01P09', 'OC03P07',
#                                     'SC12P051', 'JC05P03', 'OC02P04',
#                                     'OC08P06', 'OC02P06', 'OC04P08',
#                                     'OC06P03','OC04P06', 'SC12P040',
#                                     'OC05P10', 'OC03P02', 'OC01P08',
#                                     'OC04P10', 'OC06P09', 'SC24P124',
#                                     'SC18P134')
# 
# #filtering out the list of transcripts in "excluded_transcript" df
# ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>% 
#   filter(!(File %in% ECHO_transcript_exclude_remove))
# 
# #removing rows with: "comment", "RA", "Missing_1", "Missing_2:" for Speaker column...Check with Annie what "Missing" means
# ECHO_Transcripts_Complete_TbyT <- ECHO_Transcripts_Complete_TbyT %>%
#   filter(Speaker != "Comment") %>%
#   filter(Speaker != "RA") %>%
#   filter(Speaker != "Missing_1") %>%
#   filter(Speaker != "Missing_2") %>%
#   filter(Speaker != "N") %>%
#   filter(Speaker != "Q") %>%
#   filter(Speaker != "RN") %>%
#   filter(Speaker != "D1") %>%
#   filter(Speaker != "PA") %>%
#   filter(Speaker != "UM") %>%
#   filter(Speaker != "UF")
# 
# 
# ###########NOTES: BELOW ARE DETAILS ABOUT INDIVIDUAL TRANSCRIPTS###############
# #D1 transcripts:   
# #JC10P01(D1-D-P all conv- exclude), JC09P06(D1-D-P all conv- exclude),  OC03P01(Lots of D1-D-UF-P conv, exclude), 
# #D2 transcripts:  
# #NP transcripts: , JC04P03(NP-P conv- exclude), SC18P094 (NP-P conv- exclude)
# #PA transcripts: , JC06P06(multiple patients), 
#  # SC06P075(was this conversation cut too early? maybe exclude),
# #JC01P09... Exclude.
# #OC03P07- PM and PF in conv. Exclude. 
# #SC12P051- Lots of conversation including patient family
# # JC05P03- lots of conversation with family
# # OC02P04- other speakers involved in conv.
# # OC08P06- other speakers involved in conv.
# # OC02P06- other speakers involved in conv. 
# # OC04P08- others speakers involved in conv
# # OC06P03- other speakers involved in conv.
# # OC04P06- other speakers involved in conv. 
# #SC12P040- multiple speakers throughout conv
# #OC05P10- multiple speakers throughout conv
# #OC03P02- multiple speakers throughout conv
# # OC01P08- multiple speakers throughout conv
# # OC04P10- multiple speakers throughout conv
# #OC06P09- multiple speakers throughout conv
# #SC24P124- multiple speakers throughout conv
# #SC18P134- multiple speakers throughout conv
# #D1 transcripts: JC11P03, JC08P09, JC08P10, JC11P06, JC08P06, JC11P01, JC11P09, JC08P03, 
# #JC11P04, JC11P07, JC11P02, JC10P01, JC09P06, JC11P08, JC11P05, JC08P07, OC03P01, SC24P122,
# #SC18P110, SC24P114
# #D2 transcripts: JC08P01, JC11P01, JC08P03, JC09P06, JC08P02,
# #NP transcripts: JC11P05, JC04P03, SC18P094 
# #PA transcripts: JC08P01, JC06P06, JC01P08, OC08P10, OC07P11, 
# #OC08P09, OC07P12, OC05P07, SC12P062, SC06P075, SC12P048
# #RN transcripts: OC07P02, OC06P02, SC07P006, SC06P101
# 
# 
# 
# 
# ################################################################################
# # #Cleaning WSU transcripts- EXCLUDING FROM ANALYSIS BUT KEEPING SCRIPT FOR NOW
# # 
# # 
# # 
# # ECHO_WSU_Transcripts_Complete_TbyT <- read_csv(here(config$ECHO_WSU_Transcripts_path, config$ECHO_WSU_Transcripts_name))
# # WSU_ECHO_ID_Key <- read_csv(here(config$WSU_ECHO_ID_Key_path, config$WSU_ECHO_ID_Key_name))
# # 
# # 
# # WSU_ECHO_ID_Key <- WSU_ECHO_ID_Key %>%
# #   mutate(tapeid = Global_ID) %>%
# #   mutate(provider_id = str_sub(Local_ID, 1, 4))
# # 
# # ECHO_WSU_Transcripts_Complete_TbyT <- ECHO_WSU_Transcripts_Complete_TbyT %>%
# #   mutate(overall_sequence = 1:n())
# # 
# # 
# # ECHO_WSU_Transcripts_Complete_TbyT <- ECHO_WSU_Transcripts_Complete_TbyT %>%
# #   #100J: deleting from beginning to end of D2-P conversation
# #   filter(!(overall_sequence >= 5412 & overall_sequence <= 5639)) %>%
# #   #100Z: deleting from beginning to end of UF-P, UM-D, & RN-P conversation
# #   filter(!(overall_sequence >= 16268 & overall_sequence <= 16283 |
# #              overall_sequence >= 16350 & overall_sequence <= 16352 | 
# #              overall_sequence >= 16532 & overall_sequence <= 16661)) %>%
# #   #100Q: deleting from beginning to end of RA/D & UF/D conversation
# #   filter(!(overall_sequence >= 1176 & overall_sequence <= 1186 |
# #              overall_sequence >= 1466 & overall_sequence <= 1472)) %>%
# #   #300A: deleting from beginning to end of UF/RA & RA/P conversation
# #   filter(!(overall_sequence >= 3315 & overall_sequence <= 3319 |
# #              overall_sequence >= 3551 & overall_sequence <= 3563)) %>%
# #   #300L: deleting from beginning to end of RA/P conversation
# #   filter(!(overall_sequence >= 3880 & overall_sequence <= 3888 |
# #              overall_sequence >= 3893 & overall_sequence <= 3895)) %>%
# #   #300C: deleting from beginning to end of UF/P conversation
# #   filter(!(overall_sequence >= 10223 & overall_sequence <= 10232)) %>%
# #   #300B: deleting from beginning to end of UF/D conversation
# #   filter(!(overall_sequence >= 16662 & overall_sequence <= 16669)) %>%
# #   #200D: deleting from beginning to end of RA/P/UM/D & UM/P conversation
# #   filter(!(overall_sequence >= 21044 & overall_sequence <= 21067 |
# #              overall_sequence >= 21429 & overall_sequence <= 21434)) %>%
# #   #200Z: deleting from beginning to end of RA/P conversation
# #   filter(!(overall_sequence >= 20248 & overall_sequence <= 20259)) %>%
# #   #200D: deleting from beginning to end of RA/P & RA/D conversations
# #   filter(!(overall_sequence >= 15671 & overall_sequence <= 15681 |
# #              overall_sequence >= 15742 & overall_sequence <= 15749)) %>%
# #   #100V: deleting from beginning to end of RA/P & RA/D conversations
# #   filter(!(overall_sequence >= 10233 & overall_sequence <= 10239 |
# #              overall_sequence >= 10359 & overall_sequence <= 10367)) %>%
# #   #300R: deleting from beginning to end of RA/P conversation
# #   filter(!(overall_sequence >= 5640 & overall_sequence <= 5755))
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # ######NOTES ABOUT TRANSCRIPTS
# # #Exclude NP transcripts:'300I', '300D', '300F','300N', '300H', '100N','300J'
# # #Exclude PA transcripts: 200O, 200S, 200N, 300Q
# # #Multiple Doctors: 200J, 100S(and lots of crosstalk from UM), 
# # #300K(multiple doctors and lots of crosstalk from other individuals),
# # #200P(multiple doctors, social worker, and other speakers),
# # #100U(multiple doctors for conv)
# # #100P(multiple doctors and other speakers)
# # #100I(multiple doctors across all convo)
# # #300G(second doctor spoke for a large part of convo)
# # #Multiple RNs: 200W, 100A, 100X
# # #Multiple speakers throughout convo: 100B, 200Q
# # 
# # ECHO_WSU_transcript_exclude_remove <- c('300I', '300D', '300F', 
# #                                         '300N', '300H', '100N', 
# #                                         '300J', '200O', '200S', 
# #                                         '200N', '300Q', '200J',
# #                                         '100S', '300K', '200P',
# #                                         '100U', '100P', '100I',
# #                                         '300G', '100X', '100A',
# #                                         '100B', '200Q', '200W')
# # 
# # 
# # #filtering out the list of transcripts in "excluded_transcript" df
# # ECHO_WSU_Transcripts_Complete_TbyT <- ECHO_WSU_Transcripts_Complete_TbyT %>% 
# #   filter(!(File %in% ECHO_WSU_transcript_exclude_remove))
# # 
# # 
# # #removing rows with: "comment", "RA", "Missing_1", "Missing_2:" for Speaker column...Check with Annie what "Missing" means
# # ECHO_WSU_Transcripts_Complete_TbyT <- ECHO_WSU_Transcripts_Complete_TbyT %>%
# #   filter(Speaker != "Comment") %>%
# #   filter(Speaker != "RA") %>%
# #   filter(Speaker != "Missing_2") %>%
# #   filter(Speaker != "PA") %>%
# #   filter(Speaker != "UM") %>%
# #   filter(Speaker != "UF")
# # 
# # 
# # ################################################################################
# # 
# # 
# # #combining WSU with other transcripts from previous file
# # ECHO_Transcripts_Total_final <- bind_rows(ECHO_Transcripts_Complete_TbyT, ECHO_WSU_Transcripts_Complete_TbyT)
# 
# #This is the data that will be further cleaned before analysis
# ECHO_Transcripts_Total_final <- ECHO_Transcripts_Complete_TbyT
# 
# #removing all brackets with content inside of them
# ECHO_Transcripts_Total_final$Text <- gsub("\\[(.*?)\\]", "", ECHO_Transcripts_Total_final$Text)
# 
# #need to create word count variable to remove rows without text
# ECHO_Transcripts_Total_final$Word_count <- str_count(ECHO_Transcripts_Total_final$Text, "\\w+")
# 
# 
# #Removing all rows where there is no speech
# ECHO_Transcripts_Total_final <- ECHO_Transcripts_Total_final %>%
#   filter(Word_count != 0)
# 
# #making a version of the TbyT where one-word and two-word turns are deleted before smoothing/LIWC
# #DON'T REMOVE ROWS WITH 1 OR 2 WORDS FOR SENSITIVITY ANALYSIS
# ECHO_Transcripts_Total_final_V2 <- ECHO_Transcripts_Total_final %>%
#   filter(Word_count != 1) %>%
#   filter(Word_count != 2)
# 
# 
# ############################################################################################################
# ############# Speaker Smoothing
# ############################################################################################################
# 
# smoothed_tByT_df_V2 <- data.frame( # Empty dataframe to store results
#   File = character(),
#   Speaker = character(),
#   Text = character(),
#   Sequence = numeric(),
#   overall_sequence = numeric(),
#   Word_count = integer()
# )
# 
# for (f in unique(ECHO_Transcripts_Total_final_V2$File)) { # Iterate through each file
#   file_df <- data.frame( # create a df with just that files data; probably ineffecient, but...
#     ECHO_Transcripts_Total_final_V2[which(ECHO_Transcripts_Total_final_V2$File == f),]
#   )
#   first <- TRUE
#   smoothed_chunk <- data.frame( # Create empty dataframe to store smoothed speakers for that file
#     File = character(),
#     Speaker = character(),
#     Text = character(),
#     Sequence = numeric(),
#     overall_sequence = numeric(),
#     Word_count = integer()
#   )
#   for (i in seq_len(nrow(file_df))) { # Iterate through each row in a given file
#     if (first == TRUE) { # Just add the first row to the empty df for the file
#       smoothed_chunk <- rbind(smoothed_chunk,file_df[i,])
#       first <- FALSE
#     } else if (file_df[i,'Speaker'] == file_df[i-1,'Speaker']) {
#       # This tests to see of the current row's speaker is the same as the previous; if so, it appends the current row's text to the previous one
#       smoothed_chunk[nrow(smoothed_chunk),'Text'] <- paste(smoothed_chunk[nrow(smoothed_chunk),'Text'],file_df[i,'Text'])
#       smoothed_chunk[nrow(smoothed_chunk),'Word_count'] <- smoothed_chunk[nrow(smoothed_chunk),'Word_count'] + file_df[i,'Word_count']
#     } else { # if speakers don't match, just add row to the smoothed df for that file
#       smoothed_chunk <- rbind(smoothed_chunk,file_df[i,])
#     }
#   }
#   smoothed_tByT_df_V2 <- rbind(smoothed_tByT_df_V2,smoothed_chunk) # add the smoothed file to overall smoothed results
# }
# 
# #ECHO_Transcripts_Complete_TbyT_SMOOTHED_V3 is the new smoothed version which includes all 4 sites; V2 was previous version without WSU
# write.csv(smoothed_tByT_df_V2, "ECHO_Transcripts_Complete_TbyT_SMOOTHED_V2.csv")
# # write.csv(smoothed_tByT_df_V2, "ECHO_Transcripts_Complete_TbyT_SMOOTHED_V3.csv")
# 
# #write.xlsx(ECHO_Transcripts_Complete_TbyT, file = "ECHO_Transcripts_Complete_TbyT.xlsx")
# write.csv(ECHO_Transcripts_Complete_TbyT, "ECHO_Transcripts_Complete_TbyT.csv")
# 
# 
# #conversation level prep for ALL 4 study sites
# ECHO_LSM_Prep_final <- ECHO_Transcripts_Total_final %>% 
#   group_by(File, Speaker) %>%
#   summarise(Text = paste(Text, collapse = " ")) 
# 
# #this is saved on OneDrive and then analyzed in LIWC
# write.csv(ECHO_LSM_Prep_final, "ECHO_LSM_Prep_final.csv")
# 
# 
# 
# ##################################################################################################
# ##################################################################################################
# #Calculating LSM, adding back LIWC metrics, merging survey data
# ##################################################################################################
# ##################################################################################################
# 
# #Opening all files
# ECHO_LSM_MLM <- read_csv(here(config$ECHO_LSM_MLM_V2_path, config$ECHO_LSM_MLM_V2_name))
# ECHO_survey_data <- read_dta(here(config$ECHO_survey_data_path, config$ECHO_survey_data_name))
# ECHO_ID_key <- read_csv(here(config$ECHO_ID_key_path, config$ECHO_ID_key_name))
# # WSU_ECHO_ID_Key <- read_csv(here(config$WSU_ECHO_ID_Key_path, config$WSU_ECHO_ID_Key_name))
# 
# 
# # WSU_ECHO_ID_Key_V2 <- WSU_ECHO_ID_Key %>%
# #   mutate(File = str_sub(Local_ID, 6, 9)) %>%
# #   mutate(tapeid = Global_ID) %>%
# #   select(-c(Local_ID,Global_ID))
# 
# #this was combining the ID_key documents so that WSU was included
# # Combined_ECHO_ID_Key <- bind_rows(ECHO_ID_key, WSU_ECHO_ID_Key_V2)
# 
# #Here is the code for creating the LIWC values for patient and doctor as individual variables
# ECHO_LSM_LIWC_Components <- ECHO_LSM_MLM%>%
#   rename(output_order = 'Source (A)') %>%
#   rename(File = 'Source (B)') %>%
#   rename(Speaker = 'Source (C)') %>%
#   rename(Text = 'Source (D)') %>%
#   select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
#                    "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
#   ungroup()%>%
#   select(-Text) %>%
#   select(-output_order) %>%
#   pivot_longer(WC:filler) %>%
#   pivot_wider(names_from = c(name, Speaker), values_from = value) 
# 
# 
# #This is for calculating LSM for this data
# ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
#   rename(output_order = 'Source (A)') %>%
#   rename(File = 'Source (B)') %>%
#   rename(Speaker = 'Source (C)') %>%
#   rename(Text = 'Source (D)') %>%
#   #deleting all punctuations
#   select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
#                    "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
# #renamed function variable to funct as indicated on LIWC manual
# rename(funct = "function") %>%
# #creating LSM scores
#   select(-Text) %>%
#   select(-output_order) %>%
#   pivot_longer(WC:filler) %>%
#   pivot_wider(names_from = Speaker, values_from = value) %>%
#   mutate(LIWC_new = (1 - abs(D - P) / (D + P + .0001))) %>%
#   select(-D, -P) %>%
#   pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
# #creating overall LSM metric for functions by getting average of auxiliary verbs, 
# #articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
# #negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
#   rowwise() %>%
#   mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, 
#                                     LSM_negate, LSM_conj, LSM_quant, LSM_ppron))) %>%
#   mutate(conv.affect.match = LSM_affect,
#          conv.social.match = LSM_social,
#          conv.cogproc.match = LSM_cogproc,
#          conv.percept.match = LSM_percept,
#          conv.negemo.match = LSM_negemo,
#          conv.bio.match = LSM_bio,
#          conv.drives.match = LSM_drives,
#          conv.relativ.match = LSM_relativ,
#          conv.informal.match = LSM_informal)
#   
# 
# 
# #Adding LIWC components back to LSM file
# ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_LSM_LIWC_Components, by = "File")
# #Making the provider_id variable
# # ECHO_LSM_MLM <- ECHO_LSM_MLM %>%
# #   mutate(provider_id = str_sub(File, 1, 4))
# #merge ECHO_LSM_MLM and ECHO_ID_key by "File"
# ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_ID_key, by = "File" ) 
# #merge ECHO_LSM_MLM and ECHO_survey_data by "tapeid" to include survey data
# ECHO_LSM_MLM <- left_join(ECHO_LSM_MLM, ECHO_survey_data, by = "tapeid")
# 
# 
# ############################################################################################################
# ############################################################################################################
# #####Making text chunks based on Word Count and Turns
# ############################################################################################################
# ############################################################################################################
# 
# 
# #Script for creating the chunks which will be used of studying linguistic accommodation
# #chunks created BASED ON WORD COUNT
# ECHO_Transcript_chunks_wc <- ECHO_Transcripts_Complete_TbyT %>% 
#   group_by(File) %>%
#   mutate(word_count = str_count(Text,"\\w+"), 
#          cumulative = cumsum(word_count), 
#          chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
#                            cumulative < (max(cumulative/3))*2 ~ 2, 
#                            TRUE ~ 3) 
#   ) %>%
#   ungroup() 
# 
# ECHO_Transcript_chunks_wc <- ECHO_Transcript_chunks_wc %>%
#   group_by(File, chunk, Speaker) %>%
#   summarise(words = paste(Text,collapse = " ")) %>%
#   ungroup()
# 
# write.csv(ECHO_Transcript_chunks_wc, "ECHO_Transcript_chunks_wc.csv")
# 
# #Script for creating the chunks which will be used of studying linguistic accommodation
# #chunks created BASED ON TURNS
# ECHO_Transcript_chunks_turns <- ECHO_Transcripts_Complete_TbyT %>% 
#   mutate(turn = 1) %>%
#   group_by(File) %>%
#   mutate(cumulative = cumsum(turn),
#          chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
#                            cumulative < (max(cumulative)/3)*2 ~ 2, 
#                            TRUE ~ 3)
#   ) %>%
#   ungroup() 
# 
# ECHO_Transcript_chunks_turns <- ECHO_Transcript_chunks_turns %>%
#   group_by(File, chunk, Speaker) %>%
#   summarise(words = paste(Text,collapse = " ")) %>%
#   ungroup()
# 
# 
# write.csv(ECHO_Transcript_chunks_turns, "ECHO_Transcript_chunks_turns.csv")
# 
# 
# ############################################################################################################
# ############################################################################################################
# #####Adding chunk variable (based on Word Count and Turns) for ECHO TbyT transcript after processing through LIWC
# ############################################################################################################
# ############################################################################################################
# 
# ECHO_smoothed_chunks <- read_csv(here(config$ECHO_LSM_TbyT_Smoothed_path, config$ECHO_LSM_TbyT_Smoothed_name))
# 
# ECHO_smoothed_chunks <-ECHO_smoothed_chunks %>%
#   rename(output_order = A) %>%
#   rename(File = B) %>%
#   rename(Speaker = C) %>%
#   rename(Text = D) %>%
#   select(-E) %>%
#   select(-F)
# 
# 
# ECHO_smoothed_chunks_turns <- ECHO_smoothed_chunks %>% 
#   mutate(turn = 1) %>%
#   group_by(File) %>%
#   mutate(cumulative = cumsum(turn),
#          chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
#                            cumulative < (max(cumulative)/3)*2 ~ 2, 
#                            TRUE ~ 3)
#   ) %>%
#   ungroup() %>%
# relocate(chunk, .before = Text) %>%
#   select(-c(turn, cumulative))
# 
# 
# 
# 
# #Script for creating the chunks which will be used of studying linguistic accommodation
# #chunks created BASED ON WORD COUNT
# ECHO_smoothed_chunks_wc <- ECHO_smoothed_chunks %>% 
#   group_by(File) %>%
#   mutate(word_count = str_count(Text,"\\w+"), 
#          cumulative = cumsum(word_count), 
#          chunk = case_when(cumulative < (max(cumulative)/3) ~ 1, 
#                            cumulative < (max(cumulative/3))*2 ~ 2, 
#                            TRUE ~ 3) 
#   ) %>%
#   ungroup() %>%
# relocate(chunk, .before = Text) %>%
#   select(-c(word_count, cumulative))
# 
# 
# 
# ##################################################################################################
# ##################################################################################################
# #Running LSM on two different transcript chunk versions
# #Based on WC & Turns
# ##################################################################################################
# ##################################################################################################
# 
# #Opening all files
# ECHO_LSM_MLM_chunks_wc <- read_csv(here(config$ECHO_LSM_MLM_chunks_wc_path, config$ECHO_LSM_MLM_chunks_wc_name))
# ECHO_LSM_MLM_chunks_turns <- read_csv(here(config$ECHO_LSM_MLM_chunks_turns_path, config$ECHO_LSM_MLM_chunks_turns_name))
# ECHO_survey_data <- read_dta(here(config$ECHO_survey_data_path, config$ECHO_survey_data_name))
# ECHO_ID_key <- read_csv(here(config$ECHO_ID_key_path, config$ECHO_ID_key_name))
# 
# 
# #FIRST IS LSM ON CHUNKS BASED ON WORD COUNT (WC)
# #Here is the code for creating the LIWC values for patient and doctor as individual variables
# ECHO_LSM_LIWC_Components_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
#   rename(output_order = 'Source (A)') %>%
#   rename(File = 'Source (B)') %>%
#   rename(Chunk = 'Source (C)') %>%
#   rename(Speaker = 'Source (D)') %>%
#   rename(Text = 'Source (E)') %>%
#   select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
#                    "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
#   ungroup()%>%
#   select(-Text) %>%
#   select(-output_order) %>%
#   pivot_longer(WC:filler) %>%
#   pivot_wider(names_from = c(name, Speaker), values_from = value)  %>%
#   #the next two lines of code adds a separate LIWC variable for each chunk
#   pivot_longer(WC_D:filler_P) %>%
#   pivot_wider(names_from = c(name, Chunk), values_from = value)
# 
# 
# #This is for calculating LSM for this data
# ECHO_LSM_MLM_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
#   rename(output_order = 'Source (A)') %>%
#   rename(File = 'Source (B)') %>%
#   rename(Chunk = 'Source (C)') %>%
#   rename(Speaker = 'Source (D)') %>%
#   rename(Text = 'Source (E)') %>%
#   #deleting all punctuation categories
#   select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
#                    "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
# 
# #renamed function variable to funct as indicated on LIWC manual
# rename(funct = "function") %>%
# #creating LSM values
# select(-Text) %>%
# select(-output_order) %>%
# pivot_longer(WC:filler) %>%
# pivot_wider(names_from = Speaker, values_from = value) %>%
# mutate(LIWC_new = (1 - abs(D - P) / (D + P + .0001))) %>%
# select(-D, -P) %>%
# pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
# #creating overall LSM metric for functions by getting average of auxiliary verbs, 
# #articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
# #negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
# rowwise() %>%
# mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, 
#                                   LSM_negate, LSM_conj, LSM_quant, LSM_ppron))) %>%
#   mutate(conv.affect.match = LSM_affect,
#          conv.social.match = LSM_social,
#          conv.cogproc.match = LSM_cogproc,
#          conv.percept.match = LSM_percept,
#          conv.negemo.match = LSM_negemo,
#          conv.bio.match = LSM_bio,
#          conv.drives.match = LSM_drives,
#          conv.relativ.match = LSM_relativ,
#          conv.informal.match = LSM_informal) %>%
# pivot_longer(LSM_WC:conv.informal.match) %>%
# pivot_wider(names_from = c(name, Chunk), values_from = value)
# 
# 
# #Adding LIWC components back to LSM file
# ECHO_LSM_MLM_chunks_wc <- left_join(ECHO_LSM_MLM_chunks_wc, ECHO_LSM_LIWC_Components_chunks_wc, by = "File")
# 
# #Making the provider_id variable
# # ECHO_LSM_MLM_chunks_wc <- mutate(ECHO_LSM_MLM_chunks_wc,
# #                        provider_id = str_sub(File, 1, 4))
# 
# #merge ECHO_LSM_MLM_chunks_wc and ECHO_ID_key by "File"
# #ECHO_LSM_MLM_chunks_wc <- left_join(ECHO_LSM_MLM_chunks_wc, ECHO_ID_key, by = "File" )
# 
# #merge ECHO_LSM_MLM_chunks_wc and ECHO_survey_data by "tapeid" to include survey data
# #ECHO_LSM_MLM_chunks_wc <- left_join(ECHO_LSM_MLM_chunks_wc, ECHO_survey_data, by = "tapeid")
# 
# #making chunk ratio to be used in analysis
# ECHO_LSM_MLM_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
#   rowwise() %>%
#   mutate(LSM_function_mean_chunkratio = (LSM_function_mean_3/LSM_function_mean_1))
# 
# ECHO_LSM_MLM_chunks_wc <- ECHO_LSM_MLM_chunks_wc %>%
#   rename_at(vars(-(File)), ~ paste0(., '_wc')) %>%
#   mutate(affect_chunkratio_wc = conv.affect.match_3_wc/ conv.affect.match_1_wc,
#          social_chunkratio_wc = conv.social.match_3_wc/ conv.social.match_1_wc,
#          cogproc_chunkratio_wc = conv.cogproc.match_3_wc/ conv.cogproc.match_1_wc,
#          percept_chunkratio_wc = conv.percept.match_3_wc/ conv.percept.match_1_wc,
#          negemo_chunkratio_wc = conv.negemo.match_3_wc/ conv.negemo.match_1_wc,
#          bio_chunkratio_wc = conv.bio.match_3_wc/ conv.bio.match_1_wc,
#          drives_chunkratio_wc = conv.drives.match_3_wc/ conv.drives.match_1_wc,
#          relativ_chunkratio_wc = conv.relativ.match_3_wc/ conv.relativ.match_1_wc,
#          informal_chunkratio_wc = conv.informal.match_3_wc/ conv.informal.match_1_wc)
# ######################################################################################################
# #THIS SECTION IS LSM ON CHUNKS BASED ON TURNS
# #Here is the code for creating the LIWC values for patient and doctor as individual variables
# ECHO_LSM_LIWC_Components_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
#   rename(output_order = 'Source (A)') %>%
#   rename(File = 'Source (B)') %>%
#   rename(Chunk = 'Source (C)') %>%
#   rename(Speaker = 'Source (D)') %>%
#   rename(Text = 'Source (E)') %>%
#   select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
#                    "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
#   ungroup() %>%
#   select(-Text) %>%
#   select(-output_order) %>%
#   pivot_longer(WC:filler) %>%
#   pivot_wider(names_from = c(name, Speaker), values_from = value) %>%
#   pivot_longer(WC_D:filler_P) %>%
#   pivot_wider(names_from = c(name, Chunk), values_from = value)
# 
# 
# #This is for calculating LSM for this data
# ECHO_LSM_MLM_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
#   rename(output_order = 'Source (A)') %>%
#   rename(File = 'Source (B)') %>%
#   rename(Chunk = 'Source (C)') %>%
#   rename(Speaker = 'Source (D)') %>%
#   rename(Text = 'Source (E)') %>%
#   #deleting all punctuation variables
#   select(-one_of(c("AllPunc","Period","Comma", "Colon", "SemiC","QMark",
#                    "Exclam", "Dash", "Quote", "Apostro", "Parenth", "OtherP"))) %>%
# 
# #renamed function variable to funct as indicated on LIWC manual
# rename(funct = "function") %>%
# #creating LSM values
# select(-Text) %>%
# select(-output_order) %>%
# pivot_longer(WC:filler) %>%
# pivot_wider(names_from = Speaker, values_from = value) %>%
# mutate(LIWC_new = (1 - abs(D - P) / (D + P + .0001))) %>%
# select(-D, -P) %>%
# pivot_wider(names_from = name, names_prefix = "LSM_", values_from = LIWC_new) %>%
#   
# #creating overall LSM metric for functions by getting average of auxiliary verbs, 
# #articles, common adverbs, personal pronouns, indefinite pronouns, prepositions, 
# #negations, conjunctions, quantifiers (missing LSM_ppronoun for now)
# rowwise() %>%
# mutate(LSM_function_mean = mean(c(LSM_auxverb, LSM_article, LSM_adverb, LSM_ipron, LSM_prep, 
#                                   LSM_negate, LSM_conj, LSM_quant, LSM_ppron))) %>%
#   mutate(conv.affect.match = LSM_affect,
#          conv.social.match = LSM_social,
#          conv.cogproc.match = LSM_cogproc,
#          conv.percept.match = LSM_percept,
#          conv.negemo.match = LSM_negemo,
#          conv.bio.match = LSM_bio,
#          conv.drives.match = LSM_drives,
#          conv.relativ.match = LSM_relativ,
#          conv.informal.match = LSM_informal) %>%
# pivot_longer(LSM_WC:conv.informal.match) %>%
# pivot_wider(names_from = c(name, Chunk), values_from = value)
# 
# 
# #Adding LIWC components back to LSM file
# ECHO_LSM_MLM_chunks_turns <- left_join(ECHO_LSM_MLM_chunks_turns, ECHO_LSM_LIWC_Components_chunks_turns, by = "File")
# 
# #Making the provider_id variable
# # ECHO_LSM_MLM_chunks_turns <- mutate(ECHO_LSM_MLM_chunks_turns,
# #                                  provider_id = str_sub(File, 1, 4))
# 
# #merge ECHO_LSM_MLM_chunks_turns and ECHO_ID_key by "File"
# #ECHO_LSM_MLM_chunks_turns <- left_join(ECHO_LSM_MLM_chunks_turns, ECHO_ID_key, by = "File" )
# 
# #merge ECHO_LSM_MLM_chunks_turns and ECHO_survey_data by "tapeid" to include survey data
# #ECHO_LSM_MLM_chunks_turns <- left_join(ECHO_LSM_MLM_chunks_turns, ECHO_survey_data, by = "tapeid")
# 
# #making chunk ratio to be used in analysis
# ECHO_LSM_MLM_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
#   rowwise() %>%
#   mutate(LSM_function_mean_chunkratio = (LSM_function_mean_3/LSM_function_mean_1))
# 
# ECHO_LSM_MLM_chunks_turns <- ECHO_LSM_MLM_chunks_turns %>%
#   rename_at(vars(-(File)), ~ paste0(., '_turns')) %>%
#   mutate(affect_chunkratio_turns = conv.affect.match_3_turns/ conv.affect.match_1_turns,
#          social_chunkratio_turns = conv.social.match_3_turns/ conv.social.match_1_turns,
#          cogproc_chunkratio_turns = conv.cogproc.match_3_turns/ conv.cogproc.match_1_turns,
#          percept_chunkratio_turns = conv.percept.match_3_turns/ conv.percept.match_1_turns,
#          negemo_chunkratio_turns = conv.negemo.match_3_turns/ conv.negemo.match_1_turns,
#          bio_chunkratio_turns = conv.bio.match_3_turns/ conv.bio.match_1_turns,
#          drives_chunkratio_turns = conv.drives.match_3_turns/ conv.drives.match_1_turns,
#          relativ_chunkratio_turns = conv.relativ.match_3_turns/ conv.relativ.match_1_turns,
#          informal_chunkratio_turns = conv.informal.match_3_turns/ conv.informal.match_1_turns)
# 
# ##################################################################
# ##################################################################
# ###VADER Analysis
# ##################################################################
# ##################################################################
# 
# #NOTE: COMMENTED THE VADER SCRIPT OUT SO WE DON'T ACCIDENTALLY RUN IT UNLESS WE WANT UPDATE IT.
# # #tbyt transcripts
# # smoothed_tByT_VADER_df <- smoothed_tByT_df
# # 
# # #getting tbyt vader scores
# # smoothed_tByT_VADER_scores <- vader_df(smoothed_tByT_VADER_df$Text, incl_nt = T, neu_set = T, rm_qm = T)
# # 
# # #binding the VADER scores with original df (which has ID and text)
# # smoothed_tByT_VADER_complete_df <- bind_cols(smoothed_tByT_VADER_df,smoothed_tByT_VADER_scores)
# # 
# # write.csv(smoothed_tByT_VADER_complete_df, "smoothed_tByT_VADER_complete_df.csv")
# # 
# # 
# # #conv level transcripts
# # ECHO_LSM_Prep_VADER <- ECHO_LSM_Prep 
# #   
# # #getting conv. level VADER scores --> had ERRORS for some transcript...looking into this..
# # ECHO_LSM_Prep_VADER$Text <- vader_df(ECHO_LSM_Prep_VADER$Text, incl_nt = T, neu_set = T, rm_qm = T)
# 
# ##################################################################
# #####Making one large df with all different matching variables
# # ****RUN THE SCRIPT BELOW AFTER YOU RUN THE "4_turnbyturrnLSM.R" script
# ##################################################################
# 
# 
# 
# #merging all matching measures into one large df
# ECHO_All_Matching_Measures <- list(ECHO_LSM_MLM, ECHO_smoothed_rLSM, ECHO_smoothed_LIWC_matching, 
#                ECHO_LSM_MLM_chunks_turns, ECHO_LSM_MLM_chunks_wc, 
#                ECHO_smoothed_chunks_turns_rLSM, ECHO_smoothed_chunks_wc_rLSM,
#                ECHO_smoothed_chunks_wc_LIWC_matching, ECHO_smoothed_chunks_turns_LIWC_matching,
#                ECHO_smoothed_VADER_matching) %>% 
#   reduce(full_join, by = "File")
# 
# 
# #making dataframe for all variables in the overall matching document
# ECHO_All_Matching_Measures_varnames <- as.data.frame(colnames(ECHO_All_Matching_Measures))
# 
# write.csv(ECHO_All_Matching_Measures, "ECHO_All_Matching_Measures.csv")
# write.csv(ECHO_All_Matching_Measures_varnames, "ECHO_All_Matching_Measures_varnames.csv")
# 
# 
# #LSM Conv, rLSM, and LIWC matching tbyt
# ECHO_All_Matching_Measures_V2 <- list(ECHO_LSM_MLM, ECHO_smoothed_rLSM, ECHO_smoothed_LIWC_matching) %>% 
#   reduce(full_join, by = "File")







