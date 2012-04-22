--
-- step 1. histogram of words.
--

create temporary function rowSequence as 'org.apache.hadoop.hive.contrib.udf.UDFRowSequence';
set hive.auto.convert.join=true; -- convert joins as necessary
set hive.exec.mode.local.auto=true; -- move small jobs to the master node

-- as of 1 Apr 2012, the first created table in EMR can't be a "create table as".
create table tmp (x int);

-- as per http://aws.amazon.com/articles/5249664154115844
create external table e2grams (gram string, year int, freq int, pagefreq int, bookfreq int) row format delimited fields terminated by '\t' stored as sequencefile location 's3://datasets.elasticmapreduce/ngrams/books/20090715/eng-all/2gram/';
create external table e3grams (gram string, year int, freq int, pagefreq int, bookfreq int) row format delimited fields terminated by '\t' stored as sequencefile location 's3://datasets.elasticmapreduce/ngrams/books/20090715/eng-all/3gram/';
create external table e4grams (gram string, year int, freq int, pagefreq int, bookfreq int) row format delimited fields terminated by '\t' stored as sequencefile location 's3://datasets.elasticmapreduce/ngrams/books/20090715/eng-all/4gram/';
create external table e5grams (gram string, year int, freq int, pagefreq int, bookfreq int) row format delimited fields terminated by '\t' stored as sequencefile location 's3://datasets.elasticmapreduce/ngrams/books/20090715/eng-all/5gram/';

create table grams as select g as word, count(*) as freq from (select explode(split(gram," ")) as g from e2grams union all select explode(split(gram," ")) as g from e3grams union all select explode(split(gram," ")) as g from e4grams union all select explode(split(gram," ")) as g from e5grams) x group by g;

create table topgrams as select * from grams order by freq desc limit 500000;

create table words as select word, rowSequence() as id from topgrams;  -- ensure this runs on 1 node only.

create view g2t as select split(gram," ")[0] as atxt, split(gram," ")[1] as btxt, sum(freq) as tot from e2grams where year > 1800 group by gram;
create view g3t as select split(gram," ")[0] as atxt, split(gram," ")[1] as btxt, split(gram," ")[2] as ctxt, sum(freq) as tot from e3grams where year > 1800 group by gram;
create view g4t as select split(gram," ")[0] as atxt, split(gram," ")[1] as btxt, split(gram," ")[2] as ctxt, split(gram," ")[3] as dtxt, sum(freq) as tot from e4grams where year > 1800 group by gram;
create view g5t as select split(gram," ")[0] as atxt, split(gram," ")[1] as btxt, split(gram," ")[2] as ctxt, split(gram," ")[3] as dtxt, split(gram," ")[4] as etxt, sum(freq) as tot from e5grams where year > 1800 group by gram;

create table g2 as
  select /* +MAPJOIN(ga,gb) */ ga.id as a, gb.id as b, tot
    from g2t join words ga on ga.word = g2t.atxt
             join words gb on gb.word = g2t.btxt;

create table g3 as
  select /* +MAPJOIN(ga,gb,gc) */ ga.id as a, gb.id as b, gc.id as c, tot
    from g3t join words ga on ga.word = g3t.atxt
             join words gb on gb.word = g3t.btxt
             join words gc on gc.word = g3t.ctxt;

create table g4 as
  select /* +MAPJOIN(ga,gb,gc,gd) */ ga.id as a, gb.id as b, gc.id as c, gd.id as d, tot
    from g4t join words ga on ga.word = g4t.atxt
             join words gb on gb.word = g4t.btxt
             join words gc on gc.word = g4t.ctxt
             join words gd on gd.word = g4t.dtxt;

create table g5 as
  select /* +MAPJOIN(ga,gb,gc,gd,ge) */ ga.id as a, gb.id as b, gc.id as c, gd.id as d, ge.id as e, tot
    from g5t join words ga on ga.word = g5t.atxt
             join words gb on gb.word = g5t.btxt
             join words gc on gc.word = g5t.ctxt
             join words gd on gd.word = g5t.dtxt
             join words ge on ge.word = g5t.etxt;

--
-- step 2. denormalization.
--

create table denorm2 as
  select fst.a as a, fst.b as b,
           sum(if(snd.b = fst.b, snd.tot, 0)) as eq,
           sum(if(snd.b > fst.b, snd.tot, 0)) as lt,
           sum(if(snd.b < fst.b, snd.tot, 0)) as gt

  from g2 fst join g2 snd on fst.a = snd.a
  group by fst.a, fst.b;

create table denorm3 as
  select fst.a as a, fst.b as b, fst.c as c,
           sum(if(snd.c = fst.c, snd.tot, 0)) as eq,
           sum(if(snd.c > fst.c, snd.tot, 0)) as lt,
           sum(if(snd.c < fst.c, snd.tot, 0)) as gt

   from g3 fst join g3 snd on fst.a = snd.b and fst.b = snd.b
   group by fst.a, fst.b, fst.c;

create table denorm4 as
  select fst.a as a, fst.b as b, fst.c as c, fst.d,
           sum(if(snd.d = fst.d, snd.tot, 0)) as eq,
           sum(if(snd.d > fst.d, snd.tot, 0)) as lt,
           sum(if(snd.d < fst.d, snd.tot, 0)) as gt

  from g4 fst join g4 snd on fst.a = snd.a and fst.b = snd.b and fst.c = snd.c
  group by fst.a, fst.b, fst.c, fst.d;

create external table d4 like denorm4 location "s3n://twotofive/d4";
insert overwrite table d4 select * from denorm4;

create table denorm5 as
  select fst.a as a, fst.b as b, fst.c as c, fst.d as d, fst.e as e,
           sum(if(snd.e = fst.e, snd.tot, 0)) as eq,
           sum(if(snd.e < fst.e, snd.tot, 0)) as lt,
           sum(if(snd.e > fst.e, snd.tot, 0)) as gt
  from g5 fst join g5 snd on fst.a = snd.a and fst.b = snd.b and fst.c = snd.c and fst.d = snd.d
  group by fst.a, fst.b, fst.c, fst.d, fst.e;

create external table d5 like denorm5 location "s3n://twotofive/d5";
insert overwrite table d5 select * from denorm5;





select sum(c*c) from (select count(1) as c from g5 group by a, b, c, d) x; -- 40 Billion
select sum(c*c) from (select count(1) as c from g4 group by a, b, c) x;  -- 24 Billion
select sum(c*c) from (select count(1) as c from g3 group by a, b) x; -- 1100 Billion
select sum(c*c) from (select count(1) as c from g2 group by a) x; -- 2200 Billion

