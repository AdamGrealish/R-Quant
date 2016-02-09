create table compustat.compustat_monthly_pricing
(
uniqueid    int not null auto_increment, primary key(uniqueid),
gvkey       int        , index (gvkey),
iid         varchar(50),
datadate    int        , index (datadate),
tic         varchar(50), index (tic),
cusip       varchar(50), index (cusip),
conm        varchar(50),
ajexm       varchar(50),
ajpm        varchar(50),
isalrt      varchar(50),
primiss     varchar(50),
spgim       varchar(50),
spiim       varchar(50),
spmim       varchar(50),
cheqvm      varchar(50),
curcddvm    varchar(50),
dvpspm      varchar(50),
dvpsxm      varchar(50),
dvrate      varchar(50),
csfsm       varchar(50),
cshtrm      varchar(50),
curcdm      varchar(50),
navm        varchar(50),
prccm       varchar(50),
prchm       varchar(50),
prclm       varchar(50),
shortint    varchar(50),
shortintme  varchar(50),
trfm        varchar(50),
trt1m       varchar(50),
rawpm       varchar(50),
rawxm       varchar(50),
sph100      varchar(50),
sphcusip    varchar(50),
sphiid      varchar(50),
sphmid      varchar(50),
sphname     varchar(50),
sphsec      varchar(50),
sphtic      varchar(50),
sphvg       varchar(50),
cshoq       varchar(50),
exchg       varchar(50),
secstat     varchar(50),
tpci        varchar(50),
cik         varchar(50),
fic         varchar(50)   
);




load data local infile 'F:\\Data\\ContextPaper\\compustat_monthly_pricing.csv'
into table compustat.compustat_monthly_pricing
fields terminated by ','
enclosed by '"'
lines terminated by '\n'
ignore 1 lines
(gvkey,iid,datadate,tic,cusip,conm,ajexm,ajpm,isalrt,primiss,spgim,spiim,spmim,cheqvm,curcddvm,dvpspm,dvpsxm,dvrate,csfsm,cshtrm,curcdm,navm,prccm,prchm,prclm,shortint,shortintme,trfm,trt1m,rawpm,rawxm,sph100,sphcusip,sphiid,sphmid,sphname,sphsec,sphtic,sphvg,cshoq,exchg,secstat,tpci,cik,fic);

