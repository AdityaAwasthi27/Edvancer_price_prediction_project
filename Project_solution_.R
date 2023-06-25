


setwd("C:/R")
getwd()

htrain=read.csv("housing_train.csv",stringsAsFactors = F)
htest=read.csv("housing_test.csv",stringsAsFactors = F)

#glimpse(htest)
#View(htest)


table(htrain$Suburb > 100)

library(dplyr)
glimpse(htrain)
glimpse(htest)

#adding Price to combine both
htest$Price=0

htrain$data='train'
htest$data='test'

#combine both dataSet
hsg_all=rbind(htrain,htest)


# checking NA's in the data


apply(hsg_all,2,function(x) sum(is.na(x)))

?mean
#for Bedroom
#mean(hsg_all$Bedroom2,na.rm=T)
hsg_all$Bedroom2[is.na(hsg_all$Bedroom2)]=round(mean(hsg_all$Bedroom2,na.rm=T),0)


apply(hsg_all,2,function(x) sum(is.na(x)))

glimpse(hsg_all$Bathroom)

#for Bathroom
hsg_all$Bathroom[is.na(hsg_all$Bathroom)]=round(mean(hsg_all$Bathroom,na.rm=T),0)

apply(hsg_all,2,function(x) sum(is.na(x)))
#for Car

hsg_all$Car[is.na(hsg_all$Car)]=round(mean(hsg_all$Car,na.rm=T),0)
apply(hsg_all,2,function(x) sum(is.na(x)))

#for landsize
hsg_all$Landsize[is.na(hsg_all$Landsize)]=round(mean(hsg_all$Landsize,na.rm=T),0)

apply(hsg_all,2,function(x) sum(is.na(x)))


#for buildingArea
hsg_all$BuildingArea[is.na(hsg_all$BuildingArea)]=round(mean(hsg_all$BuildingArea,na.rm=T),0)
apply(hsg_all,2,function(x) sum(is.na(x)))

#for YearBuilt
#View(hsg_all$YearBuilt)
table(hsg_all$YearBuilt)
length(unique(hsg_all$YearBuilt))

library(tidyr)
#View(hsg_all)
hsg_all <- hsg_all %>% fill(YearBuilt)

apply(hsg_all,2,function(x) sum(is.na(x)))

glimpse(hsg_all)

#craeting Dummies

#combining same frequencies dummy
#View(table(hsg_all$Suburb))

t1=round(tapply(hsg_all$Price,hsg_all$Suburb,mean,na.rm=T),0)
#View(t1)
t1=sort(t1)

hsg_all <- hsg_all %>%
  mutate(
    # sub_Kooyong=as.numeric(Suburb=="Kooyong"),
    sub_Princes_Hill=as.numeric(Suburb=="Princes Hill"),
    sub_Burnley=as.numeric(Suburb=="Burnley"),
    sub_Docklands=as.numeric(Suburb=="Docklands"),
    sub_Campbellfield=as.numeric(Suburb=="Campbellfield"),
    sub_Gardenvale=as.numeric(Suburb=="Gardenvale"),
    sub_Travancore=as.numeric(Suburb=="Travancore"),
    sub_Seaholme=as.numeric(Suburb=="Seaholme"),
    sub_Caulfield=as.numeric(Suburb=="Caulfield"),
    sub_Ripponlea=as.numeric(Suburb=="Ripponlea"),
    sub_Strathmore_Heights=as.numeric(Suburb=="Strathmore Heights"),
    sub_Caulfield_East=as.numeric(Suburb=="Caulfield East"),
    sub_Bellfield=as.numeric(Suburb=="Bellfield"),
    sub_Essendon_North=as.numeric(Suburb=="Essendon North"),
    sub_Kingsbury=as.numeric(Suburb=="Kingsbury"),
    sub_Williamstown_North=as.numeric(Suburb=="Williamstown North"),
    sub_Brooklyn=as.numeric(Suburb=="Brooklyn"),
    sub_Keilor_Park=as.numeric(Suburb=="Keilor Park"),
    sub_South_Kingsville=as.numeric(Suburb=="South Kingsville"),
    sub_Jacana=as.numeric(Suburb=="Jacana"),
    sub_Kealba=as.numeric(Suburb=="Kealba"),
    sub_Balaclava=as.numeric(Suburb=="Balaclava"),
    sub_Eaglemont=as.numeric(Suburb=="Eaglemont"),
    sub_East_Melbourne=as.numeric(Suburb=="East Melbourne"),
    sub_Ivanhoe_East=as.numeric(Suburb=="Ivanhoe East"),
    sub_Cremorne=as.numeric(Suburb=="Cremorne"),
    sub_Middle_Park=as.numeric(Suburb=="Middle Park"),
    sub_West_Melbourne=as.numeric(Suburb=="West Melbourne"),
    sub_Essendon_West=as.numeric(Suburb=="Essendon West"),
    sub_Gowanbrae=as.numeric(Suburb=="Gowanbrae"),
    sub_Spotswood=as.numeric(Suburb=="Spotswood"),
    sub_Glen_Huntly=as.numeric(Suburb=="Glen Huntly"),
    sub_Yallambie=as.numeric(Suburb=="Yallambie"),
    sub_Albion=as.numeric(Suburb=="Albion"),
    sub_Kingsville=as.numeric(Suburb=="Kingsville"),
    sub_Caulfield_North=as.numeric(Suburb=="Caulfield North"),
    sub_Parkville=as.numeric(Suburb=="Parkville"),
    sub_Aberfeldie=as.numeric(Suburb=="Aberfeldie"),
    sub_Alphington=as.numeric(Suburb=="Alphington"),
    sub_Hughesdale=as.numeric(Suburb=="Hughesdale"),
    sub_Carlton_North=as.numeric(Suburb=="Carlton North"),
    sub_Southbank=as.numeric(Suburb=="Southbank"),
    sub_Viewbank=as.numeric(Suburb=="Viewbank"),
    sub_Mont_Albert=as.numeric(Suburb=="Mont Albert"),
    sub_Watsonia=as.numeric(Suburb=="Watsonia"),
    sub_Fairfield=as.numeric(Suburb=="Fairfield"),
    sub_Hampton_East=as.numeric(Suburb=="Hampton East"),
    sub_Heidelberg=as.numeric(Suburb=="Heidelberg"),
    sub_Oakleigh=as.numeric(Suburb=="Oakleigh"),
    sub_Caulfield_South=as.numeric(Suburb=="Caulfield South"),
    sub_Fitzroy=as.numeric(Suburb=="Fitzroy"),
    sub_Braybrook=as.numeric(Suburb=="Braybrook"),
    sub_Carlton=as.numeric(Suburb=="Carlton"),
    sub_Canterbury=as.numeric(Suburb=="Canterbury"),
    sub_Chadstone=as.numeric(Suburb=="Chadstone"),
    sub_Clifton_Hill=as.numeric(Suburb=="Clifton Hill"),
    sub_Flemington=as.numeric(Suburb=="Flemington"),
    sub_Kew_East=as.numeric(Suburb=="Kew East"),
    sub_Albert_Park=as.numeric(Suburb=="Albert Park"),
    sub_Box_Hill=as.numeric(Suburb=="Box Hill"),
    sub_Seddon=as.numeric(Suburb=="Seddon"),
    sub_Ashwood=as.numeric(Suburb=="Ashwood"),
    sub_Windsor=as.numeric(Suburb=="Windsor"),
    sub_Elsternwick=as.numeric(Suburb=="Elsternwick"),
    sub_Collingwood=as.numeric(Suburb=="Collingwood"),
    sub_Oak_Park=as.numeric(Suburb=="Oak Park"),
    sub_Altona=as.numeric(Suburb=="Altona"),
    sub_Hadfield=as.numeric(Suburb=="Hadfield"),
    sub_Abbotsford=as.numeric(Suburb=="Abbotsford"),
    sub_Heidelberg_West=as.numeric(Suburb=="Heidelberg West"),
    sub_North_Melbourne=as.numeric(Suburb=="North Melbourne"),
    sub_Oakleigh_South=as.numeric(Suburb=="Oakleigh South"),
    sub_Coburg_North=as.numeric(Suburb=="Coburg North"),
    sub_Murrumbeena=as.numeric(Suburb=="Murrumbeena"),
    sub_Heidelberg_Heights=as.numeric(Suburb=="Heidelberg Heights"),
    sub_Malvern=as.numeric(Suburb=="Malvern"),
    sub_Moorabbin=as.numeric(Suburb=="Moorabbin"),
    sub_South_Melbourne=as.numeric(Suburb=="South Melbourne"),
    sub_Ashburton=as.numeric(Suburb=="Ashburton"),
    sub_Rosanna=as.numeric(Suburb=="Rosanna"),
    sub_Brunswick_East=as.numeric(Suburb=="Brunswick East"),
    sub_Niddrie=as.numeric(Suburb=="Niddrie"),
    sub_Maidstone=as.numeric(Suburb=="Maidstone"),
    sub_Airport_West=as.numeric(Suburb=="Airport West"),
    sub_Fitzroy_North=as.numeric(Suburb=="Fitzroy North"),
    sub_Bulleen=as.numeric(Suburb=="Bulleen"),
    sub_Ormond=as.numeric(Suburb=="Ormond"),
    sub_Strathmore=as.numeric(Suburb=="Strathmore"),
    sub_Sunshine_North=as.numeric(Suburb=="Sunshine North"),
    sub_West_Footscray=as.numeric(Suburb=="West Footscray"),
    sub_Avondale_Heights=as.numeric(Suburb=="Avondale Heights"),
    sub_Fawkner=as.numeric(Suburb=="Fawkner"),
    sub_Altona_North=as.numeric(Suburb=="Altona North"),
    sub_Armadale=as.numeric(Suburb=="Armadale"),
    sub_Burwood=as.numeric(Suburb=="Burwood"),
    sub_Williamstown=as.numeric(Suburb=="Williamstown"),
    sub_Melbourne=as.numeric(Suburb=="Melbourne"),
    sub_Sunshine_West=as.numeric(Suburb=="Sunshine West"),
    sub_Ivanhoe=as.numeric(Suburb=="Ivanhoe"),
    sub_Templestowe_Lower=as.numeric(Suburb=="Templestowe Lower"),
    sub_Brunswick_West=as.numeric(Suburb=="Brunswick West"),
    sub_Keilor_East=as.numeric(Suburb=="Keilor East"),
    sub_Hawthorn_East=as.numeric(Suburb=="Hawthorn East"),
    sub_Prahran=as.numeric(Suburb=="Prahran"),
    sub_Surrey_Hills=as.numeric(Suburb=="Surrey Hills"),
    sub_Kensington=as.numeric(Suburb=="Kensington"),
    sub_Sunshine=as.numeric(Suburb=="Sunshine"),
    sub_Toorak=as.numeric(Suburb=="Toorak"),
    sub_Elwood=as.numeric(Suburb=="Elwood"),
    sub_Maribyrnong=as.numeric(Suburb=="Maribyrnong"),
    sub_Newport=as.numeric(Suburb=="Newport"),
    sub_Doncaster=as.numeric(Suburb=="Doncaster"),
    sub_Ascot_Vale=as.numeric(Suburb=="Ascot Vale"),
    sub_Footscray=as.numeric(Suburb=="Footscray"),
    sub_Moonee_Ponds=as.numeric(Suburb=="Moonee Ponds"),
    sub_Thornbury=as.numeric(Suburb=="Thornbury"),
    sub_Hampton=as.numeric(Suburb=="Hampton"),
    sub_Yarraville=as.numeric(Suburb=="Yarraville"),
    sub_Balwyn=as.numeric(Suburb=="Balwyn"),
    sub_Malvern_East=as.numeric(Suburb=="Malvern East"),
    sub_Camberwell=as.numeric(Suburb=="Camberwell"),
    sub_Carnegie=as.numeric(Suburb=="Carnegie"),
    sub_Port_Melbourne=as.numeric(Suburb=="Port Melbourne"),
    sub_Bentleigh=as.numeric(Suburb=="Bentleigh"),
    sub_Pascoe_Vale=as.numeric(Suburb=="Pascoe Vale"),
    sub_Brighton_East=as.numeric(Suburb=="Brighton East"),
    sub_Hawthorn=as.numeric(Suburb=="Hawthorn"),
    sub_Balwyn_North=as.numeric(Suburb=="Balwyn North"),
    sub_Coburg=as.numeric(Suburb=="Coburg"),
    sub_Northcote=as.numeric(Suburb=="Northcote"),
    sub_Kew=as.numeric(Suburb=="Kew"),
    sub_Brighton=as.numeric(Suburb=="Brighton"),
    sub_Glenroy=as.numeric(Suburb=="Glenroy"),
    sub_Glen_Iris=as.numeric(Suburb=="Glen Iris"),
    sub_Essendon=as.numeric(Suburb=="Essendon"),
    sub_Brunswick=as.numeric(Suburb=="Brunswick"),
    sub_South_Yarra=as.numeric(Suburb=="South Yarra"),
    sub_St_Kilda=as.numeric(Suburb=="St Kilda"),
    sub_Preston=as.numeric(Suburb=="Preston"),
    sub_Richmond=as.numeric(Suburb=="Richmond"),
    sub_Bentleigh_East=as.numeric(Suburb=="Bentleigh East"),
    sub_Reservoir=as.numeric(Suburb=="Reservoir")
    
  ) %>%
  select(-Suburb)

glimpse(hsg_all)


#Address  - remove as all ahve freq. 1
table(hsg_all$Address)
hsg_all <- hsg_all %>%
  select(-Address)

glimpse(hsg_all)

#Type

table(hsg_all$Type)
sort(table(hsg_all$Type),decreasing = TRUE)

hsg_all <- hsg_all %>%
  mutate(
    Type_h=as.numeric(Type=="h"),
    Type_u=as.numeric(Type=="u")
  ) %>%
  select(-Type)

glimpse(hsg_all)

#Method::::

table(hsg_all$Method)
sort(table(hsg_all$Method),decreasing = TRUE)

hsg_all <- hsg_all %>%
  mutate(
    Method_S=as.numeric(Method=="S"),
    Method_PI=as.numeric(Method=="PI"),
    Method_SP=as.numeric(Method=="SP"),
    Method_VB=as.numeric(Method=="VB")
  ) %>%
  select(-Method)

glimpse(hsg_all)

#SellerG
#View(table(hsg_all$SellerG))
sort(table(hsg_all$SellerG),decreasing = TRUE)

hsg_all <- hsg_all %>% 
  mutate(
    sellerG_Airport=as.numeric(SellerG=="Airport"),
    sellerG_Allan=as.numeric(SellerG=="Allan"),
    sellerG_Appleby=as.numeric(SellerG=="Appleby"),
    sellerG_Batty=as.numeric(SellerG=="Batty"),
    sellerG_Blue=as.numeric(SellerG=="Blue"),
    sellerG_Bustin=as.numeric(SellerG=="Bustin"),
    sellerG_Buxton_Find=as.numeric(SellerG=="Buxton/Find"),
    sellerG_CASTRAN=as.numeric(SellerG=="CASTRAN"),
    sellerG_Century=as.numeric(SellerG=="Century"),
    sellerG_Clairmont=as.numeric(SellerG=="Clairmont"),
    sellerG_Coventry=as.numeric(SellerG=="Coventry"),
    sellerG_Del=as.numeric(SellerG=="Del"),
    sellerG_Direct=as.numeric(SellerG=="Direct"),
    sellerG_Elite=as.numeric(SellerG=="Elite"),
    sellerG_Fletchers_Fletchers=as.numeric(SellerG=="Fletchers/Fletchers"),
    sellerG_Fletchers_One=as.numeric(SellerG=="Fletchers/One"),
    sellerG_Geoff=as.numeric(SellerG=="Geoff"),
    sellerG_Ham=as.numeric(SellerG=="Ham"),
    sellerG_hockingstuart_Advantage=as.numeric(SellerG=="hockingstuart/Advantage"),
    sellerG_hockingstuart_Barry=as.numeric(SellerG=="hockingstuart/Barry"),
    sellerG_hockingstuart_Buxton=as.numeric(SellerG=="hockingstuart/Buxton"),
    sellerG_hockingstuart_Village=as.numeric(SellerG=="hockingstuart/Village"),
    sellerG_Homes=as.numeric(SellerG=="Homes"),
    sellerG_Hooper=as.numeric(SellerG=="Hooper"),
    sellerG_Iconek=as.numeric(SellerG=="Iconek"),
    sellerG_iOne=as.numeric(SellerG=="iOne"),
    sellerG_iTRAK=as.numeric(SellerG=="iTRAK"),
    sellerG_Joe=as.numeric(SellerG=="Joe"),
    sellerG_Johnston=as.numeric(SellerG=="Johnston"),
    sellerG_Joseph=as.numeric(SellerG=="Joseph"),
    sellerG_Karen=as.numeric(SellerG=="Karen"),
    sellerG_Lucas=as.numeric(SellerG=="Lucas"),
    sellerG_Luxe=as.numeric(SellerG=="Luxe"),
    sellerG_Luxton=as.numeric(SellerG=="Luxton"),
    sellerG_Mandy=as.numeric(SellerG=="Mandy"),
    sellerG_Mason=as.numeric(SellerG=="Mason"),
    sellerG_Meadows=as.numeric(SellerG=="Meadows"),
    sellerG_Naison=as.numeric(SellerG=="Naison"),
    sellerG_Nardella=as.numeric(SellerG=="Nardella"),
    sellerG_North=as.numeric(SellerG=="North"),
    sellerG_Oak=as.numeric(SellerG=="Oak"),
    sellerG_One=as.numeric(SellerG=="One"),
    sellerG_Parkinson=as.numeric(SellerG=="Parkinson"),
    sellerG_Private_Tiernan_s=as.numeric(SellerG=="Private/Tiernan's"),
    sellerG_Professionals=as.numeric(SellerG=="Professionals"),
    sellerG_Property=as.numeric(SellerG=="Property"),
    sellerG_Propertyau=as.numeric(SellerG=="Propertyau"),
    sellerG_Prowse=as.numeric(SellerG=="Prowse"),
    sellerG_R_H=as.numeric(SellerG=="R&H"),
    sellerG_Reach=as.numeric(SellerG=="Reach"),
    sellerG_S_L=as.numeric(SellerG=="S&L"),
    sellerG_Steveway=as.numeric(SellerG=="Steveway"),
    sellerG_Tiernan_s=as.numeric(SellerG=="Tiernan's"),
    sellerG_Vic=as.numeric(SellerG=="Vic"),
    sellerG_Weast=as.numeric(SellerG=="Weast"),
    sellerG_Win=as.numeric(SellerG=="Win"),
    sellerG_Zahn=as.numeric(SellerG=="Zahn"),
    sellerG_Allens=as.numeric(SellerG=="Allens"),
    sellerG_Australian=as.numeric(SellerG=="Australian"),
    sellerG_Besser=as.numeric(SellerG=="Besser"),
    sellerG_Buxton_Advantage=as.numeric(SellerG=="Buxton/Advantage"),
    sellerG_Calder=as.numeric(SellerG=="Calder"),
    sellerG_Changing=as.numeric(SellerG=="Changing"),
    sellerG_Charlton=as.numeric(SellerG=="Charlton"),
    sellerG_Crane=as.numeric(SellerG=="Crane"),
    sellerG_David=as.numeric(SellerG=="David"),
    sellerG_Dixon=as.numeric(SellerG=="Dixon"),
    sellerG_Galldon=as.numeric(SellerG=="Galldon"),
    sellerG_Grantham=as.numeric(SellerG=="Grantham"),
    sellerG_JMRE=as.numeric(SellerG=="JMRE"),
    sellerG_Ken=as.numeric(SellerG=="Ken"),
    sellerG_LJ=as.numeric(SellerG=="LJ"),
    sellerG_Nguyen=as.numeric(SellerG=="Nguyen"),
    sellerG_RE=as.numeric(SellerG=="RE"),
    sellerG_Red=as.numeric(SellerG=="Red"),
    sellerG_Redina=as.numeric(SellerG=="Redina"),
    sellerG_Ross=as.numeric(SellerG=="Ross"),
    sellerG_Scott=as.numeric(SellerG=="Scott"),
    sellerG_Sweeney_Advantage=as.numeric(SellerG=="Sweeney/Advantage"),
    sellerG_VICPROP=as.numeric(SellerG=="VICPROP"),
    sellerG_Walsh=as.numeric(SellerG=="Walsh"),
    sellerG_Wood=as.numeric(SellerG=="Wood"),
    sellerG_Ascend=as.numeric(SellerG=="Ascend"),
    sellerG_ASL=as.numeric(SellerG=="ASL"),
    sellerG_Assisi=as.numeric(SellerG=="Assisi"),
    sellerG_Bayside=as.numeric(SellerG=="Bayside"),
    sellerG_Compton=as.numeric(SellerG=="Compton"),
    sellerG_Garvey=as.numeric(SellerG=="Garvey"),
    sellerG_Hamilton=as.numeric(SellerG=="Hamilton"),
    sellerG_Jason=as.numeric(SellerG=="Jason"),
    sellerG_Kelly=as.numeric(SellerG=="Kelly"),
    sellerG_Leased=as.numeric(SellerG=="Leased"),
    sellerG_Maddison=as.numeric(SellerG=="Maddison"),
    sellerG_New=as.numeric(SellerG=="New"),
    sellerG_Owen=as.numeric(SellerG=="Owen"),
    sellerG_Thomas=as.numeric(SellerG=="Thomas"),
    sellerG_Weda=as.numeric(SellerG=="Weda"),
    sellerG_Anderson=as.numeric(SellerG=="Anderson"),
    sellerG_First=as.numeric(SellerG=="First"),
    sellerG_Morrison=as.numeric(SellerG=="Morrison"),
    sellerG_Nicholson=as.numeric(SellerG=="Nicholson"),
    sellerG_O_Brien=as.numeric(SellerG=="O'Brien"),
    sellerG_Prof_=as.numeric(SellerG=="Prof."),
    sellerG_Raine_Horne=as.numeric(SellerG=="Raine&Horne"),
    sellerG_D_Aprano=as.numeric(SellerG=="D'Aprano"),
    sellerG_Domain=as.numeric(SellerG=="Domain"),
    sellerG_Holland=as.numeric(SellerG=="Holland"),
    sellerG_Matthew=as.numeric(SellerG=="Matthew"),
    sellerG_Parkes=as.numeric(SellerG=="Parkes"),
    sellerG_Bekdon=as.numeric(SellerG=="Bekdon"),
    sellerG_FN=as.numeric(SellerG=="FN"),
    sellerG_Re=as.numeric(SellerG=="Re"),
    sellerG_Sotheby_s=as.numeric(SellerG=="Sotheby's"),
    sellerG_HAR=as.numeric(SellerG=="HAR"),
    sellerG_Morleys=as.numeric(SellerG=="Morleys"),
    sellerG_Pagan=as.numeric(SellerG=="Pagan"),
    sellerG_W_B_=as.numeric(SellerG=="W.B."),
    sellerG_William=as.numeric(SellerG=="William"),
    sellerG_Christopher=as.numeric(SellerG=="Christopher"),
    sellerG_O_Donoghues=as.numeric(SellerG=="O'Donoghues"),
    sellerG_Chambers=as.numeric(SellerG=="Chambers"),
    sellerG_J=as.numeric(SellerG=="J"),
    sellerG_Gunn_Co=as.numeric(SellerG=="Gunn&Co"),
    sellerG_Hunter=as.numeric(SellerG=="Hunter"),
    sellerG_Pride=as.numeric(SellerG=="Pride"),
    sellerG_Trimson=as.numeric(SellerG=="Trimson"),
    sellerG_Brace=as.numeric(SellerG=="Brace"),
    sellerG_Castran=as.numeric(SellerG=="Castran"),
    sellerG_Darren=as.numeric(SellerG=="Darren"),
    sellerG_Melbourne=as.numeric(SellerG=="Melbourne"),
    sellerG_Rodney=as.numeric(SellerG=="Rodney"),
    sellerG_Tim=as.numeric(SellerG=="Tim"),
    sellerG_Whiting=as.numeric(SellerG=="Whiting"),
    sellerG_Caine=as.numeric(SellerG=="Caine"),
    sellerG_Haughton=as.numeric(SellerG=="Haughton"),
    sellerG_Lindellas=as.numeric(SellerG=="Lindellas"),
    sellerG_MICM=as.numeric(SellerG=="MICM"),
    sellerG_GL=as.numeric(SellerG=="GL"),
    sellerG_Beller=as.numeric(SellerG=="Beller"),
    sellerG_Harrington=as.numeric(SellerG=="Harrington"),
    sellerG_Paul=as.numeric(SellerG=="Paul"),
    sellerG_Purplebricks=as.numeric(SellerG=="Purplebricks"),
    sellerG_Abercromby_s=as.numeric(SellerG=="Abercromby's"),
    sellerG_Barlow=as.numeric(SellerG=="Barlow"),
    sellerG_Wilson=as.numeric(SellerG=="Wilson"),
    sellerG_Philip=as.numeric(SellerG=="Philip"),
    sellerG_Buckingham=as.numeric(SellerG=="Buckingham"),
    sellerG_Walshe=as.numeric(SellerG=="Walshe"),
    sellerG_Edward=as.numeric(SellerG=="Edward"),
    sellerG_McDonald=as.numeric(SellerG=="McDonald"),
    sellerG_Alexkarbon=as.numeric(SellerG=="Alexkarbon"),
    sellerG_RW=as.numeric(SellerG=="RW"),
    sellerG_Bells=as.numeric(SellerG=="Bells"),
    sellerG_C21=as.numeric(SellerG=="C21"),
    sellerG_Considine=as.numeric(SellerG=="Considine"),
    sellerG_Eview=as.numeric(SellerG=="Eview"),
    sellerG_Frank=as.numeric(SellerG=="Frank"),
    sellerG_Thomson=as.numeric(SellerG=="Thomson"),
    sellerG_Burnham=as.numeric(SellerG=="Burnham"),
    sellerG_Peter=as.numeric(SellerG=="Peter"),
    sellerG_Dingle=as.numeric(SellerG=="Dingle"),
    sellerG_YPA=as.numeric(SellerG=="YPA"),
    sellerG_Moonee=as.numeric(SellerG=="Moonee"),
    sellerG_LITTLE=as.numeric(SellerG=="LITTLE"),
    sellerG_Nick=as.numeric(SellerG=="Nick"),
    sellerG_Harcourts=as.numeric(SellerG=="Harcourts"),
    sellerG_Cayzer=as.numeric(SellerG=="Cayzer"),
    sellerG_Collins=as.numeric(SellerG=="Collins"),
    sellerG_Chisholm=as.numeric(SellerG=="Chisholm"),
    sellerG_Rendina=as.numeric(SellerG=="Rendina"),
    sellerG_Raine=as.numeric(SellerG=="Raine"),
    sellerG_Love=as.numeric(SellerG=="Love"),
    sellerG_Douglas=as.numeric(SellerG=="Douglas"),
    sellerG_Williams=as.numeric(SellerG=="Williams"),
    sellerG_Village=as.numeric(SellerG=="Village"),
    sellerG_Stockdale=as.numeric(SellerG=="Stockdale"),
    sellerG_Kay=as.numeric(SellerG=="Kay"),
    sellerG_Hodges=as.numeric(SellerG=="Hodges"),
    sellerG_McGrath=as.numeric(SellerG=="McGrath"),
    sellerG_Noel=as.numeric(SellerG=="Noel"),
    sellerG_Gary=as.numeric(SellerG=="Gary"),
    sellerG_Jas=as.numeric(SellerG=="Jas"),
    sellerG_Miles=as.numeric(SellerG=="Miles"),
    sellerG_Greg=as.numeric(SellerG=="Greg"),
    sellerG_Sweeney=as.numeric(SellerG=="Sweeney"),
    sellerG_RT=as.numeric(SellerG=="RT"),
    sellerG_Fletchers=as.numeric(SellerG=="Fletchers"),
    sellerG_Woodards=as.numeric(SellerG=="Woodards"),
    sellerG_Brad=as.numeric(SellerG=="Brad"),
    sellerG_Biggin=as.numeric(SellerG=="Biggin"),
    sellerG_Ray=as.numeric(SellerG=="Ray"),
    sellerG_Buxton=as.numeric(SellerG=="Buxton"),
    sellerG_Marshall=as.numeric(SellerG=="Marshall"),
    sellerG_Barry=as.numeric(SellerG=="Barry"),
    sellerG_hockingstuart=as.numeric(SellerG=="hockingstuart"),
    sellerG_Jellis=as.numeric(SellerG=="Jellis"),
    sellerG_Nelson=as.numeric(SellerG=="Nelson")
    
    
    
  )%>%
  select(-SellerG)


glimpse(hsg_all)

#Council Area

sort(table(hsg_all$CouncilArea),decreasing = TRUE)
#View(table(hsg_all$CouncilArea))


hsg_all <- hsg_all %>% 
  mutate(
    CouncilArea_Banyule=as.numeric(CouncilArea=="Banyule"),
    CouncilArea_Bayside=as.numeric(CouncilArea=="Bayside"),
    CouncilArea_Boroondara=as.numeric(CouncilArea=="Boroondara"),
    CouncilArea_Brimbank=as.numeric(CouncilArea=="Brimbank"),
    CouncilArea_Darebin=as.numeric(CouncilArea=="Darebin"),
    CouncilArea_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
    CouncilArea_Hobsons_Bay=as.numeric(CouncilArea=="Hobsons Bay"),
    #CouncilArea_Hume=as.numeric(CouncilArea=="Hume"),
    CouncilArea_Kingston=as.numeric(CouncilArea=="Kingston"),
    CouncilArea_Manningham=as.numeric(CouncilArea=="Manningham"),
    CouncilArea_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
    CouncilArea_Melbourne=as.numeric(CouncilArea=="Melbourne"),
    CouncilArea_Monash=as.numeric(CouncilArea=="Monash"),
    CouncilArea_Moonee_Valley=as.numeric(CouncilArea=="Moonee Valley"),
    CouncilArea_Moreland=as.numeric(CouncilArea=="Moreland"),
    CouncilArea_Port_Phillip=as.numeric(CouncilArea=="Port Phillip"),
    CouncilArea_Stonnington=as.numeric(CouncilArea=="Stonnington"),
    CouncilArea_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
    CouncilArea_Yarra=as.numeric(CouncilArea=="Yarra")
    
  )%>%
  select(-CouncilArea)

glimpse(hsg_all)


#data Prep complete now seperating test and train dataset

hsg_train=hsg_all %>% 
  filter(data=='train') %>% 
  select(-data)

apply(hsg_train,2,function(x) sum(is.na(x)))
#View(hsg_train)

hsg_test=hsg_all %>% 
  filter(data=='test') %>% 
  select(-data)

apply(hsg_test,2,function(x) sum(is.na(x)))
#View(hsg_test)



#model

library(car)

fit=lm(Price~. , data=hsg_train)
summary(fit)

vif(fit)

z=vif(fit$coefficients)
sort(z,decreasing=T)[1:3]



alias(lm(Price~., data=hsg_train))
aliasWala<- attributes(alias(fit)$Complete)$dimnames[[1]]

#removing aliased coefficient

View(attributes(alias(fit)$Complete)$dimnames[[1]])


fit=lm(Price~. , data=hsg_train)
attributes(alias(fit)$Complete)$dimnames[[1]]



#

hsg_train = hsg_train %>%
  select(
    --sub_Bentleigh_East,
    -sub_Reservoir,
    -sellerG_Batty,
    -sellerG_Bustin,
    -sellerG_Buxton_Find,
    -sellerG_Clairmont,
    -sellerG_Del,
    -sellerG_Fletchers_Fletchers,
    -sellerG_hockingstuart_Barry,
    -sellerG_hockingstuart_Buxton,
    -sellerG_Joe,
    -sellerG_Joseph,
    -sellerG_Mandy,
    -sellerG_Mason,
    -sellerG_North,
    -sellerG_Oak,
    -sellerG_One,
    -sellerG_Parkinson,
    -sellerG_Private_Tiernan_s,
    -sellerG_Professionals,
    -sellerG_Property,
    -sellerG_Propertyau,
    -sellerG_R_H,
    -sellerG_Reach,
    -sellerG_Steveway,
    -sellerG_Tiernan_s,
    -sellerG_Vic,
    -sellerG_Australian,
    -sellerG_Buxton_Advantage,
    -sellerG_Charlton,
    -sellerG_Crane,
    -sellerG_Scott,
    -sellerG_Sweeney_Advantage,
    -sellerG_Wood,
    -sellerG_Nelson
    
  )

#all aliased Removed
fit=lm(Price~. , data=hsg_train)

vif(fit)

#View(attributes(alias(fit)$Complete)$dimnames[[1]])


#Split Train Data

set.seed(2)
s=sample(1:nrow(hsg_train),0.7*nrow(hsg_train))
hsg_train_set=hsg_train[s,]
hsg_train_validation=hsg_train[-s,]

glimpse(hsg_train_set)
glimpse(hsg_train_validation)

apply(hsg_train_set,2,function(x) sum(is.na(x)))
apply(hsg_train_validation,2,function(x) sum(is.na(x)))


#model for hsg_train_set

fit=lm(Price~. , data=hsg_train_set)

z=vif(fit)

#removing  variable vif>5
sort(z,decreasing = T)[1:3]

fit=lm(Price~. , data=hsg_train_set)

z=vif(fit)

#removing  variable vif>5
fit=lm(Price~.-Distance, data=hsg_train_set)

z=vif(fit)
sort(z,decreasing = T)[1:3]
#
fit=lm(Price~.-Distance-Postcode, data=hsg_train_set)

z=vif(fit)
sort(z,decreasing = T)[1:3]

#
fit=lm(Price~.-Distance-Postcode-Method_S, data=hsg_train_set)

z=vif(fit)
sort(z,decreasing = T)[1:3]

#
fit=lm(Price~.-Distance-Postcode-Method_S-CouncilArea_Maribyrnong, data=hsg_train_set)

z=vif(fit)
sort(z,decreasing = T)[1:3]

#
fit=lm(Price~.-Distance-Postcode-Method_S-CouncilArea_Maribyrnong-CouncilArea_Yarra, data=hsg_train_set)

z=vif(fit)
sort(z,decreasing = T)[1:3]

# (full)
fit=lm(Price~.-Distance-Postcode-Method_S-CouncilArea_Maribyrnong-CouncilArea_Yarra
       -CouncilArea_Hobsons_Bay-CouncilArea_Brimbank-CouncilArea_Banyule, data=hsg_train_set)

z=vif(fit)
sort(z,decreasing = T)[1:3]

summary(fit)


#removal by P-value >0.05
fit=lm(Price~.-Distance-Postcode-Method_S-CouncilArea_Maribyrnong-CouncilArea_Yarra
       -CouncilArea_Hobsons_Bay-CouncilArea_Brimbank-CouncilArea_Banyule-sub_Caulfield
       -sub_Brooklyn-sub_Travancore-sub_Seaholme-sub_Ripponlea-sub_Strathmore_Heights
       -sub_Caulfield_East-sub_Bellfield-sub_Essendon_North-sub_Keilor_Park-sub_South_Kingsville
       -sub_Kingsbury-sub_Essendon_West-sub_Spotswood-sub_Yallambie-sub_Albion-sub_Kingsville
       -sub_Williamstown_North-sub_Gardenvale-sub_Glen_Huntly-sub_Southbank-sub_Burnley
       -sub_Docklands-sub_Viewbank-sub_Watsonia-sub_Chadstone-sub_Oakleigh-sub_Ashwood
       -sub_Altona-sub_Heidelberg_West-sub_Oakleigh_South-sub_Heidelberg_Heights
       -sub_Rosanna-sub_Niddrie-sub_Moorabbin-sub_Maidstone-sub_Airport_West-sub_Bulleen
       -sub_Strathmore-sub_West_Footscray-sub_Templestowe_Lower-sub_Kensington
       -sub_Sunshine-sub_Maribyrnong-sub_Newport-sub_Footscray-sub_Pascoe_Vale-sub_Oak_Park
       -sub_Coburg_North-sub_Coburg-sub_Preston-sellerG_Airport-sellerG_Allan-sellerG_Appleby
       -sellerG_CASTRAN-sellerG_Century-sellerG_Coventry-sellerG_Direct-sellerG_Elite
       -sellerG_Fletchers_One-sellerG_Geoff-sellerG_Ham-sellerG_hockingstuart_Advantage
       -sellerG_hockingstuart_Village-sellerG_Homes-sellerG_Iconek-sellerG_iOne
       -sellerG_iTRAK-sellerG_Johnston-sellerG_Karen-sellerG_Lucas-sellerG_Luxe-sellerG_Luxton
       -sellerG_Meadows-sellerG_Naison-sellerG_Nardella-sellerG_Prowse-sellerG_S_L
       -sellerG_Win-sellerG_Zahn-sellerG_Allens-sellerG_Besser-sellerG_Calder-sellerG_Changing
       -sellerG_David-sellerG_Dixon-sellerG_Galldon-sellerG_Grantham-sellerG_JMRE-sellerG_Ken
       -sellerG_LJ-sellerG_Nguyen-sellerG_RE-sellerG_Red-sellerG_Redina-sellerG_Ross
       -sellerG_VICPROP-sellerG_Walsh-sellerG_Ascend-sellerG_ASL-sellerG_Bayside
       -sellerG_Compton-sellerG_Garvey-sellerG_Hamilton-sellerG_Jason-sellerG_Kelly
       -sellerG_Leased-sellerG_Maddison-sellerG_New-sellerG_Owen-sellerG_Thomas
       -sellerG_Weda-sellerG_Anderson-sellerG_First-sellerG_Morrison-sellerG_Nicholson
       -sellerG_O_Brien-sellerG_Prof_-sellerG_Raine_Horne-sellerG_D_Aprano-sellerG_Domain
       -sellerG_Holland-sellerG_Matthew-sellerG_Parkes-sellerG_Bekdon-sellerG_FN-sellerG_Re
       -sellerG_Sotheby_s-sellerG_HAR-sellerG_Morleys-sub_Seddon-sub_Yarraville-sellerG_Pagan
       -sellerG_W_B_-sellerG_William-sellerG_Christopher-sellerG_O_Donoghues-sellerG_Chambers
       -sellerG_J-sellerG_Gunn_Co-sellerG_Hunter-sellerG_Pride-sellerG_Trimson-sellerG_Brace
       -sellerG_Darren-sellerG_Melbourne-sellerG_Rodney-sellerG_Tim-sellerG_Whiting
       -sellerG_Caine-sellerG_MICM-sellerG_GL-sellerG_Beller-sellerG_Harrington
       -sellerG_Purplebricks-sellerG_Barlow-sellerG_Wilson-sellerG_Philip-sellerG_Buckingham
       -sellerG_Walshe-sellerG_Edward-sellerG_McDonald-sellerG_Alexkarbon-sellerG_RW
       -sellerG_Bells-sub_Flemington-sub_Ascot_Vale-sellerG_C21-sellerG_Considine
       -sellerG_Eview-sellerG_Frank-sellerG_Thomson-sellerG_Peter-sellerG_Dingle
       -sellerG_YPA-sellerG_Moonee-sellerG_LITTLE-sellerG_Nick-sellerG_Harcourts
       -sellerG_Chisholm-sellerG_Rendina-sellerG_Raine-sellerG_Love-sellerG_Douglas
       -sellerG_Williams-sellerG_Stockdale-sellerG_Hodges-sellerG_McGrath-sellerG_Noel
       -sellerG_Gary-sellerG_Miles-sellerG_Sweeney-sellerG_Fletchers-sub_Princes_Hill
       -sellerG_Woodards-sellerG_Brad-sellerG_Jas-sellerG_Biggin-sellerG_Buxton
       -sellerG_Burnham-sellerG_Paul-sellerG_Barry-CouncilArea_Bayside-CouncilArea_Boroondara
       -CouncilArea_Darebin-CouncilArea_Kingston-CouncilArea_Manningham-CouncilArea_Moreland
       -CouncilArea_Stonnington-CouncilArea_Whitehorse,data=hsg_train_set)

z=vif(fit)
summary(fit)

#Prediction

predict_hsg_train_val= predict(fit,newdata = hsg_train_validation)
predict_hsg_train_val_roundoff=round(predict_hsg_train_val,1)
class(predict_hsg_train_val_roundoff)

#plot
plot(hsg_train_validation$Price,predict_hsg_train_val_roundoff)

#rmse
res=hsg_train_validation$Price-predict_hsg_train_val_roundoff

rmse_val=sqrt(mean(res^2))
rmse_val
#OR
rmse1= mean((hsg_train_validation$Price-predict_hsg_train_val_roundoff)**2) %>%
  sqrt()
rmse1




