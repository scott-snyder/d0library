$ FORTRAN D0$ERRMSG_UTIL$SOURCE:ERRMSG_TEST
$ LINK ERRMSG_TEST,D0$UTIL:DEB_UTIL4.OPT/OPT,'CERNP'
$ DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$ RUN ERRMSG_TEST
$ EXIT