create user &&user
  identified by &&user
  default tablespace xs001
  temporary tablespace temp;
grant unlimited tablespace to &&user;
grant app_owner to &&user;
