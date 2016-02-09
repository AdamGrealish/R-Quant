create table compustat.index_constituents
(
uniqueid    int not null auto_increment, primary key(uniqueid),
gvkey       int, index (gvkey),
gvkeyx      varchar(50),
from_       int, index(from_),
thru        int, index(thru),
conm        varchar(50),
indextype   varchar(50),
tic         varchar(50),
spmi        int, index (spmi),
gic         varchar(50),
co_conm     varchar(50),
co_tic      varchar(50),
co_cusip    varchar(50),
co_cik      varchar(50),
co_sic      varchar(50),
co_naics    varchar(50)
);


load data local infile 'F:\\Data\\ContextPaper\\index_constituents.csv'
into table compustat.index_constituents
fields terminated by ','
enclosed by '"'
lines terminated by '\n'
ignore 1 lines
(gvkey,gvkeyx,from_,thru,conm,indextype,tic,spmi,gic,co_conm,co_tic,co_cusip,co_cik,co_sic,co_naics);
