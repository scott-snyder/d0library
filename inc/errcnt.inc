C-----------------------------------------------------
C-   Filename  ERRCNT.INC
C-   Purpose   Storage for the number of calls
C-             NENTRY is the number of logged messages
C-             NOTE: MAXSTR is defined in ERRMSG.PARAMS
C-   Created   18-DEC-1988    Jun-jang Jeng (MSU)
C-----------------------------------------------------
       INTEGER COUNT , NENTRY
       INTEGER MAXWRN, MAXLOG
       COMMON /ERRCNT/NENTRY,COUNT(MAXSTR),MAXLOG(MAXSTR),MAXWRN(MAXSTR)


