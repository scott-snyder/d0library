C---------------------------------------------------
C-   Filename  ERRCTL.INC
C-   Purpose   Storage of control parameters 
C-             NOTE:MAXSTR is defined in ERRMSG.PARAMS
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)   
C-   Updated   3-JAN-1989   James T. Linnemann
C---------------------------------------------------
      INTEGER LULOG, OVLCNT, LOGDFL, WRNDFL
      LOGICAL WARN , OVFL
      COMMON /ERRCTL/  WARN, OVFL, WRNDFL, LOGDFL , LULOG, OVLCNT
