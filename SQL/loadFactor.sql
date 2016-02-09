create table factors.factor_scores
(
uniqueid    int not null auto_increment, primary key(uniqueid),
fid         int, index (fid),
funcname    varchar(50),
descr       varchar(100),
dt          int, index (dt),
gvkey       int, index (gvkey),
score       varchar(50),
blank       varchar(50)  
);



load data local infile 'F:\\Data\\Factors\\SP1500\\bookToMV.csv'
into table factors.factor_scores
fields terminated by ','
enclosed by '"'
lines terminated by '\n'
ignore 1 lines
(fid,funcname,descr,dt,gvkey,score,blank);


