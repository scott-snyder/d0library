C----------------------------------------------------------------------
C-
C-   Name     LKSRCP.INC
C-   Purpose  Permanent link area addresses of SRCP banks
C-   Created  27-SEP-1988   Harrison B. Prosper
C-   Updated   1-MAY-1990   Harrison B. Prosper  
C-   Updated  15-NOV-1993   Stan M. Krzywdzinski
C-                          Added ERRVAL and COMMON /LKSRCPC/ VARNAME
C-
C----------------------------------------------------------------------
C
C     MXSRCP     Maximum number of SRCP banks
C                that can be created (See SRCP.DEF)
C     ERRSRC     Error code for SRCP routines
C     NSRCP      Number of SRCP banks
C     ISRCP      Index of selected SRCP bank
C     LSRCP      Address of selected SRCP bank
C     KSRCP      Addresses of SRCP banks
C     ERRVAL     Error code for RCP input
C     VARNAME    Erroneous input variable name 
C
      INTEGER ERRSRC,ISRCP,NSRCP,LSRCP,KSRCP(MXSRCP)
      INTEGER ISTACK,KSTACK(MXSTACK)
      INTEGER ERRVAL
      CHARACTER*132 VARNAME
      COMMON /LKSRCP/ ERRSRC,NSRCP,ISRCP,KSRCP,ISTACK,KSTACK,ERRVAL
      COMMON /LKSRCPC/ VARNAME
