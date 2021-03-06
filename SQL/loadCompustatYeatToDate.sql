create table compustat.compustat_qtrly_yeartodate
(
uniqueid    int not null auto_increment, primary key(uniqueid),
gvkey	      int	, index (gvkey),
datadate	   int	, index (gvkey),
fyearq	   int	,
fqtr	      int	,
fyr	      int	,
indfmt	   varchar(50)	,
consol	   varchar(50)	,
popsrc	   varchar(50)	,
datafmt	   varchar(50)	,
tic	      varchar(50)	, index (gvkey),
cusip	      varchar(50)	,
conm	      varchar(50)	,
curcdq	   varchar(50)	,
datacqtr	   varchar(50)	,
datafqtr	   varchar(50)	,
acchgy	   varchar(50)	,
accliy	   varchar(50)	,
acqdisny	   varchar(50)	,
acqdisoy	   varchar(50)	,
adpacy	   varchar(50)	,
afudccy	   varchar(50)	,
afudciy	   varchar(50)	,
amcy	      varchar(50)	,
amy	      varchar(50)	,
aolochy	   varchar(50)	,
apalchy	   varchar(50)	,
apchy	      varchar(50)	,
aqay	      varchar(50)	,
aqcy	      varchar(50)	,
aqdy	      varchar(50)	,
aqepsy	   varchar(50)	,
aqpy	      varchar(50)	,
arcedy	   varchar(50)	,
arceepsy	   varchar(50)	,
arcey	      varchar(50)	,
asdisy	   varchar(50)	,
asinvy	   varchar(50)	,
atochy	   varchar(50)	,
autxry	   varchar(50)	,
bcefy	      varchar(50)	,
bcty	      varchar(50)	,
bdiy	      varchar(50)	,
capcsty	   varchar(50)	,
capfly	   varchar(50)	,
capxfiy	   varchar(50)	,
capxy	      varchar(50)	,
cdvcy	      varchar(50)	,
cfbdy	      varchar(50)	,
cferey	   varchar(50)	,
cflaothy	   varchar(50)	,
cfoy	      varchar(50)	,
cfpdoy	   varchar(50)	,
chechy	   varchar(50)	,
chenfdy	   varchar(50)	,
cibegniy	   varchar(50)	,
cicurry	   varchar(50)	,
cidergly	   varchar(50)	,
ciothery	   varchar(50)	,
cipeny	   varchar(50)	,
cisecgly	   varchar(50)	,
citotaly	   varchar(50)	,
cogsy	      varchar(50)	,
cshfdy	   varchar(50)	,
cshpry	   varchar(50)	,
cstkey	   varchar(50)	,
dcsfdy	   varchar(50)	,
dcufdy	   varchar(50)	,
depcy	      varchar(50)	,
dfxay	      varchar(50)	,
dilady	   varchar(50)	,
dilavy	   varchar(50)	,
dispochy	   varchar(50)	,
dity	      varchar(50)	,
dlcchy	   varchar(50)	,
dltisy	   varchar(50)	,
dltry	      varchar(50)	,
docy	      varchar(50)	,
doy	      varchar(50)	,
dpcy	      varchar(50)	,
dprety	   varchar(50)	,
dpy	      varchar(50)	,
dteay	      varchar(50)	,
dtedy	      varchar(50)	,
dteepsy	   varchar(50)	,
dtepy	      varchar(50)	,
dvpdpy	   varchar(50)	,
dvpy	      varchar(50)	,
dvrecy	   varchar(50)	,
dvrrey	   varchar(50)	,
dvty	      varchar(50)	,
dvy	      varchar(50)	,
eieacy	   varchar(50)	,
epsfiy	   varchar(50)	,
epsfxy	   varchar(50)	,
epspiy	   varchar(50)	,
epspxy	   varchar(50)	,
eqdivpy	   varchar(50)	,
esubcy	   varchar(50)	,
esuby	      varchar(50)	,
exresy	   varchar(50)	,
exreuy	   varchar(50)	,
exrey	      varchar(50)	,
fcay	      varchar(50)	,
ffoy	      varchar(50)	,
fiaoy	      varchar(50)	,
fincfy	   varchar(50)	,
finincy	   varchar(50)	,
finley	   varchar(50)	,
finrey	   varchar(50)	,
finvaoy	   varchar(50)	,
fopoxy	   varchar(50)	,
fopoy	      varchar(50)	,
fopty	      varchar(50)	,
fsrcopoy	   varchar(50)	,
fsrcopty	   varchar(50)	,
fsrcoy	   varchar(50)	,
fsrcty	   varchar(50)	,
fuseoy	   varchar(50)	,
fusety	   varchar(50)	,
gdwlamy	   varchar(50)	,
gdwliay	   varchar(50)	,
gdwlidy	   varchar(50)	,
gdwliepsy	varchar(50)	,
gdwlipy	   varchar(50)	,
glay	      varchar(50)	,
glceay	   varchar(50)	,
glcedy	   varchar(50)	,
glceepsy	   varchar(50)	,
glcepy	   varchar(50)	,
gldy	      varchar(50)	,
glepsy	   varchar(50)	,
glpy	      varchar(50)	,
gpy	      varchar(50)	,
hedgegly	   varchar(50)	,
ibadjy	   varchar(50)	,
ibcomy	   varchar(50)	,
ibcy	      varchar(50)	,
ibkiy	      varchar(50)	,
iby	      varchar(50)	,
idity	      varchar(50)	,
iirey	      varchar(50)	,
iity	      varchar(50)	,
intandy	   varchar(50)	,
intanpy	   varchar(50)	,
intcy	      varchar(50)	,
intfacty	   varchar(50)	,
intfly	   varchar(50)	,
intiacty	   varchar(50)	,
intoacty	   varchar(50)	,
intpdy	   varchar(50)	,
intpny	   varchar(50)	,
intrcy	   varchar(50)	,
invchy	   varchar(50)	,
invdspy	   varchar(50)	,
invsvcy	   varchar(50)	,
iobdy	      varchar(50)	,
ioiy	      varchar(50)	,
iorey	      varchar(50)	,
iptiy	      varchar(50)	,
isgty	      varchar(50)	,
itccy	      varchar(50)	,
ivacoy	   varchar(50)	,
ivchy	      varchar(50)	,
iviy	      varchar(50)	,
ivncfy	   varchar(50)	,
ivstchy	   varchar(50)	,
liqresny	   varchar(50)	,
liqresoy	   varchar(50)	,
lndepy	   varchar(50)	,
lnincy	   varchar(50)	,
lnmdy 	   varchar(50)	,
lnrepy	   varchar(50)	,
ltdchy	   varchar(50)	,
ltdlchy	   varchar(50)	,
ltloy	      varchar(50)	,
micy	      varchar(50)	,
miiy	      varchar(50)	,
miseqy	   varchar(50)	,
ncfliqy	   varchar(50)	,
ncoy	      varchar(50)	,
neqmiy	   varchar(50)	,
niity	      varchar(50)	,
nimy	      varchar(50)	,
nity	      varchar(50)	,
niy	      varchar(50)	,
noasuby	   varchar(50)	,
nopioy	   varchar(50)	,
nopiy	      varchar(50)	,
nrtxtdy	   varchar(50)	,
nrtxtepsy	varchar(50)	,
nrtxty	   varchar(50)	,
oancfcy	   varchar(50)	,
oancfdy	   varchar(50)	,
oancfy	   varchar(50)	,
oepsxy	   varchar(50)	,
oiadpy	   varchar(50)	,
oibdpy	   varchar(50)	,
opepsy	   varchar(50)	,
opprfty	   varchar(50)	,
oproy	      varchar(50)	,
optdry	   varchar(50)	,
optfvgry	   varchar(50)	,
optlifey	   varchar(50)	,
optrfry	   varchar(50)	,
optvoly	   varchar(50)	,
pcly	      varchar(50)	,
pdvcy	      varchar(50)	,
piy	      varchar(50)	,
pliachy	   varchar(50)	,
plly	      varchar(50)	,
pncdy	      varchar(50)	,
pncepsy	   varchar(50)	,
pnciapy	   varchar(50)	,
pnciay	   varchar(50)	,
pncidpy	   varchar(50)	,
pncidy	   varchar(50)	,
pnciepspy	varchar(50)	,
pnciepsy	   varchar(50)	,
pncippy	   varchar(50)	,
pncipy	   varchar(50)	,
pncpdy	   varchar(50)	,
pncpepsy	   varchar(50)	,
pncpy	      varchar(50)	,
pncwiapy	   varchar(50)	,
pncwiay	   varchar(50)	,
pncwidpy	   varchar(50)	,
pncwidy	   varchar(50)	,
pncwiepsy	varchar(50)	,
pncwiepy	   varchar(50)	,
pncwippy	   varchar(50)	,
pncwipy	   varchar(50)	,
pncy	      varchar(50)	,
prcay	      varchar(50)	,
prcdy	      varchar(50)	,
prcepsy	   varchar(50)	,
prcpdy	   varchar(50)	,
prcpepsy	   varchar(50)	,
prcpy	      varchar(50)	,
prosaiy	   varchar(50)	,
prstkccy	   varchar(50)	,
prstkcy	   varchar(50)	,
prstkpcy	   varchar(50)	,
prvy	      varchar(50)	,
psfixy	   varchar(50)	,
ptrany	   varchar(50)	,
purtshry	   varchar(50)	,
pvoy	      varchar(50)	,
rawmsmy	   varchar(50)	,
rcay	      varchar(50)	,
rcdy	      varchar(50)	,
rcepsy	   varchar(50)	,
rcpy	      varchar(50)	,
rdipay	   varchar(50)	,
rdipdy	   varchar(50)	,
rdipepsy	   varchar(50)	,
rdipy	      varchar(50)	,
recchy	   varchar(50)	,
reity	      varchar(50)	,
revty	      varchar(50)	,
risy	      varchar(50)	,
rray	      varchar(50)	,
rrdy	      varchar(50)	,
rrepsy	   varchar(50)	,
rrpy	      varchar(50)	,
rvy	      varchar(50)	,
saley       varchar(50)	,
scstkcy	   varchar(50)	,
setay	      varchar(50)	,
setdy	      varchar(50)	,
setepsy	   varchar(50)	,
setpy	      varchar(50)	,
shrcapy	   varchar(50)	,
sivy	      varchar(50)	,
spcedpy	   varchar(50)	,
spcedy	   varchar(50)	,
spceepspy	varchar(50)	,
spceepsy	   varchar(50)	,
spcepy	   varchar(50)	,
spcey	      varchar(50)	,
spidy	      varchar(50)	,
spiepsy	   varchar(50)	,
spioay	   varchar(50)	,
spiopy	   varchar(50)	,
spiy	      varchar(50)	,
sppchy	   varchar(50)	,
sppey	      varchar(50)	,
sppivy	   varchar(50)	,
spstkcy	   varchar(50)	,
srety	      varchar(50)	,
sstky	      varchar(50)	,
stfixay	   varchar(50)	,
stinvy	   varchar(50)	,
stkchy	   varchar(50)	,
stkcoy	   varchar(50)	,
stkcpay	   varchar(50)	,
subdisy	   varchar(50)	,
subpury	   varchar(50)	,
tdcy	      varchar(50)	,
tdsgy	      varchar(50)	,
tfvcey	   varchar(50)	,
tiey	      varchar(50)	,
tiiy	      varchar(50)	,
tsafcy	   varchar(50)	,
txachy	   varchar(50)	,
txbcofy	   varchar(50)	,
txbcoy	   varchar(50)	,
txdcy	      varchar(50)	,
txdiy	      varchar(50)	,
txopy	      varchar(50)	,
txpdy	      varchar(50)	,
txty	      varchar(50)	,
txwy	      varchar(50)	,
txy	      varchar(50)	,
uaolochy	   varchar(50)	,
udfccy	   varchar(50)	,
udvpy	      varchar(50)	,
ufretsdy	   varchar(50)	,
ugiy	      varchar(50)	,
uniamiy	   varchar(50)	,
unopincy	   varchar(50)	,
unwccy	   varchar(50)	,
uoisy	      varchar(50)	,
updvpy	   varchar(50)	,
uptacy	   varchar(50)	,
uspiy	      varchar(50)	,
ustdncy	   varchar(50)	,
usubdvpy	   varchar(50)	,
utfdocy	   varchar(50)	,
utfoscy	   varchar(50)	,
utmey	      varchar(50)	,
uwkcapcy	   varchar(50)	,
wcapchcy	   varchar(50)	,
wcapchy	   varchar(50)	,
wcapcy	   varchar(50)	,
wcapopcy	   varchar(50)	,
wcapsay	   varchar(50)	,
wcapsuy	   varchar(50)	,
wcapsy	   varchar(50)	,
wcapty	   varchar(50)	,
wcapuy	   varchar(50)	,
wday	      varchar(50)	,
wddy	      varchar(50)	,
wdepsy	   varchar(50)	,
wdpy	      varchar(50)	,
xagty	      varchar(50)	,
xbdty	      varchar(50)	,
xcomiy	   varchar(50)	,
xcomy	      varchar(50)	,
xdvrey	   varchar(50)	,
xidocy	   varchar(50)	,
xidoy	      varchar(50)	,
xinty	      varchar(50)	,
xioy	      varchar(50)	,
xiviy	      varchar(50)	,
xivrey	   varchar(50)	,
xiy	      varchar(50)	,
xobdy	      varchar(50)	,
xoiy	      varchar(50)	,
xoproy	   varchar(50)	,
xopry	      varchar(50)	,
xoptdqpy	   varchar(50)	,
xoptdy	   varchar(50)	,
xoptepsqpy	varchar(50)	,
xoptepsy	   varchar(50)	,
xoptqpy	   varchar(50)	,
xopty	      varchar(50)	,
xorey	      varchar(50)	,
xrdy	      varchar(50)	,
xrety	      varchar(50)	,
xsgay	      varchar(50)	,
xstoy	      varchar(50)	,
xsty	      varchar(50)	,
xsy	      varchar(50)	,
xty	      varchar(50)	,
exchg	      varchar(50)	,
cik	      varchar(50)	,
costat	   varchar(50)	,
fic      	varchar(50)	
);

load data local infile 'F:\\Data\\ContextPaper\\compustat_qtrly_yeartodate_fields.csv'
into table compustat.compustat_qtrly_yeartodate
fields terminated by ','
enclosed by '"'
lines terminated by '\n'
ignore 1 lines
(gvkey,datadate,fyearq,fqtr,fyr,indfmt,consol,popsrc,datafmt,tic,cusip,conm,curcdq,datacqtr,datafqtr,acchgy,accliy,acqdisny,acqdisoy,adpacy,afudccy,afudciy,amcy,amy,aolochy,apalchy,apchy,aqay,aqcy,aqdy,aqepsy,aqpy,arcedy,arceepsy,arcey,asdisy,asinvy,atochy,autxry,bcefy,bcty,bdiy,capcsty,capfly,capxfiy,capxy,cdvcy,cfbdy,cferey,cflaothy,cfoy,cfpdoy,chechy,chenfdy,cibegniy,cicurry,cidergly,ciothery,cipeny,cisecgly,citotaly,cogsy,cshfdy,cshpry,cstkey,dcsfdy,dcufdy,depcy,dfxay,dilady,dilavy,dispochy,dity,dlcchy,dltisy,dltry,docy,doy,dpcy,dprety,dpy,dteay,dtedy,dteepsy,dtepy,dvpdpy,dvpy,dvrecy,dvrrey,dvty,dvy,eieacy,epsfiy,epsfxy,epspiy,epspxy,eqdivpy,esubcy,esuby,exresy,exreuy,exrey,fcay,ffoy,fiaoy,fincfy,finincy,finley,finrey,finvaoy,fopoxy,fopoy,fopty,fsrcopoy,fsrcopty,fsrcoy,fsrcty,fuseoy,fusety,gdwlamy,gdwliay,gdwlidy,gdwliepsy,gdwlipy,glay,glceay,glcedy,glceepsy,glcepy,gldy,glepsy,glpy,gpy,hedgegly,ibadjy,ibcomy,ibcy,ibkiy,iby,idity,iirey,iity,intandy,intanpy,intcy,intfacty,intfly,intiacty,intoacty,intpdy,intpny,intrcy,invchy,invdspy,invsvcy,iobdy,ioiy,iorey,iptiy,isgty,itccy,ivacoy,ivchy,iviy,ivncfy,ivstchy,liqresny,liqresoy,lndepy,lnincy,lnmdy,lnrepy,ltdchy,ltdlchy,ltloy,micy,miiy,miseqy,ncfliqy,ncoy,neqmiy,niity,nimy,nity,niy,noasuby,nopioy,nopiy,nrtxtdy,nrtxtepsy,nrtxty,oancfcy,oancfdy,oancfy,oepsxy,oiadpy,oibdpy,opepsy,opprfty,oproy,optdry,optfvgry,optlifey,optrfry,optvoly,pcly,pdvcy,piy,pliachy,plly,pncdy,pncepsy,pnciapy,pnciay,pncidpy,pncidy,pnciepspy,pnciepsy,pncippy,pncipy,pncpdy,pncpepsy,pncpy,pncwiapy,pncwiay,pncwidpy,pncwidy,pncwiepsy,pncwiepy,pncwippy,pncwipy,pncy,prcay,prcdy,prcepsy,prcpdy,prcpepsy,prcpy,prosaiy,prstkccy,prstkcy,prstkpcy,prvy,psfixy,ptrany,purtshry,pvoy,rawmsmy,rcay,rcdy,rcepsy,rcpy,rdipay,rdipdy,rdipepsy,rdipy,recchy,reity,revty,risy,rray,rrdy,rrepsy,rrpy,rvy,saley,scstkcy,setay,setdy,setepsy,setpy,shrcapy,sivy,spcedpy,spcedy,spceepspy,spceepsy,spcepy,spcey,spidy,spiepsy,spioay,spiopy,spiy,sppchy,sppey,sppivy,spstkcy,srety,sstky,stfixay,stinvy,stkchy,stkcoy,stkcpay,subdisy,subpury,tdcy,tdsgy,tfvcey,tiey,tiiy,tsafcy,txachy,txbcofy,txbcoy,txdcy,txdiy,txopy,txpdy,txty,txwy,txy,uaolochy,udfccy,udvpy,ufretsdy,ugiy,uniamiy,unopincy,unwccy,uoisy,updvpy,uptacy,uspiy,ustdncy,usubdvpy,utfdocy,utfoscy,utmey,uwkcapcy,wcapchcy,wcapchy,wcapcy,wcapopcy,wcapsay,wcapsuy,wcapsy,wcapty,wcapuy,wday,wddy,wdepsy,wdpy,xagty,xbdty,xcomiy,xcomy,xdvrey,xidocy,xidoy,xinty,xioy,xiviy,xivrey,xiy,xobdy,xoiy,xoproy,xopry,xoptdqpy,xoptdy,xoptepsqpy,xoptepsy,xoptqpy,xopty,xorey,xrdy,xrety,xsgay,xstoy,xsty,xsy,xty,exchg,cik,costat,fic);

