set feedback off
column program heading "Program" format a29
column osuser heading "OS User" format a8
column process heading "OS pid" format a7
column owner heading "Owner" format a7
column type heading "Type" format a8
column object heading object format a15
column lmode heading "Lock mode" format a8
column vlock heading "VLock" format a10
column type heading "Type" format a10
column sql heading "Session SQL" format a80

drop table t_lock;
drop table t_session;

create table t_lock as select * from v$lock;
create table t_session as select * from v$session;
select 
  s.program, s.osuser, s.process,
  substr(owner,1,7) owner, substr(object_type,1,8) type, substr(object_name,1,15) object,
  decode (l.lmode,1,'Null',
                  2,'Row-S',
                  3,'Row-X',
                  4,'Share',
                  5,'S/Row-X',
                  6,'Exclusive', 
		LTRIM(TO_CHAR(l.Lmode,'990'))) lmode,
       DECODE(l.type,
              'TX','TRANSACTION ROW-LEVEL'     ,
              'RT','REDO-LOG'                  ,
              'TS','TEMPORARY SEGMENT '        ,
              'TD','TABLE LOCK'                ,
              'TM','ROW LOCK'                  ,
                   l.type                     )   vlock,
       DECODE(l.type,
              'TX','DML LOCK'                  ,
              'RT','REDO LOG'                  ,
              'TS','TEMPORARY SEGMENT'         ,
              'TD',DECODE(l.lmode+l.request  ,
                          4,'PARSE '          ||
                            o.owner           ||
                            '.'               ||
                            o.object_name             ,
                          6,'DDL'              ,
                            l.lmode+l.request),
              'TM','DML '                     ||
                   o.owner                    ||
                   '.'                        ||
                   o.object_name                      ,
                   l.type                     )   type  ,
       DECODE(l.lmode+l.request              ,
              2   ,'RS'                        ,
              3   ,'RX'                        ,
              4   ,'S'                         ,
              5   ,'SRX'                       ,
              6   ,'X'                         ,
                   l.lmode+l.request         )   lmode ,
       DECODE(l.request                       ,
              0,NULL                           ,
                'WAIT'                         )   wait,
       vs.sql_text sql
from t_lock l, dba_objects o, t_session s
, v$sqltext_with_newlines vs
where l.id1 = o.object_id and owner != 'SYS'
and s.sid = l.sid
and s.sql_address = vs.address and s.sql_hash_value = vs.hash_value
union all 
select 
  s.program, s.osuser, s.process,
  substr(owner,1,7) owner, substr(object_type,1,8) type, substr(object_name,1,15) object,
  decode (l.lmode,1,'Null',
                  2,'Row-S',
                  3,'Row-X',
                  4,'Share',
                  5,'S/Row-X',
                  6,'Exclusive', 
		LTRIM(TO_CHAR(l.Lmode,'990'))) lmode,
       DECODE(l.type,
              'TX','TRANSACTION ROW-LEVEL'     ,
              'RT','REDO-LOG'                  ,
              'TS','TEMPORARY SEGMENT '        ,
              'TD','TABLE LOCK'                ,
              'TM','ROW LOCK'                  ,
                   l.type                     )   vlock,
       DECODE(l.type,
              'TX','DML LOCK'                  ,
              'RT','REDO LOG'                  ,
              'TS','TEMPORARY SEGMENT'         ,
              'TD',DECODE(l.lmode+l.request  ,
                          4,'PARSE '          ||
                            o.owner           ||
                            '.'               ||
                            o.object_name             ,
                          6,'DDL'              ,
                            l.lmode+l.request),
              'TM','DML '                     ||
                   o.owner                    ||
                   '.'                        ||
                   o.object_name                      ,
                   l.type                     )   type  ,
       DECODE(l.lmode+l.request              ,
              2   ,'RS'                        ,
              3   ,'RX'                        ,
              4   ,'S'                         ,
              5   ,'SRX'                       ,
              6   ,'X'                         ,
                   l.lmode+l.request         )   lmode ,
       DECODE(l.request                       ,
              0,NULL                           ,
                'WAIT'                         )   wait,
       vs.sql_text sql
from t_lock l, dba_objects o, t_session s
, v$sqltext_with_newlines vs
where l.id2 = o.object_id and owner != 'SYS'
and s.sid = l.sid
and s.sql_address = vs.address and s.sql_hash_value = vs.hash_value
/
