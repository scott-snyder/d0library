      LOGICAL FUNCTION D0DAD_ISDISK(FNAME)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Return .true. if FNAME refers to a file on
C-     a disk. (Used by D0DAD to bypass autodetection code for tape
C-     files.)  
C-
C-   VAX ONLY - others are dummies
C-
C-   CAVEAT:
C-     If FNAME refers to a tape file via a logical name, the logical
C-     name must either be defined in the process or job tables, or
C-     FNAME must contain a colon. That is, tapedrive:data.dst is OK 
C-     independently of wherer tapedrive is defined, but if FNAME='data'
C-     is defined as (eg) mka400:file1.dst, 'data' must be defined
C-     in either the process or job tables.
C-
C-   Inputs  : FNAME - input file name
C-   Outputs :
C-   Controls:
C-
C-   Created  28-Mar-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FNAME
C&IF VAXVMS
      INCLUDE '($DVIDEF)'
      INCLUDE '($DCDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($LNMDEF)'
      CHARACTER*(*) LOGTAB
      CHARACTER*128 DEVNAME
      PARAMETER(LOGTAB='LNM$PROCESS_TABLE')
      INTEGER*2 ILEN2
      INTEGER ISTAT,DEVTYPE,ILEN,IBUFF(4)
      INTEGER  LIB$GETDVI,SYS$TRNLNM,LENOCC,ICFILA
      EXTERNAL LIB$GETDVI,SYS$TRNLNM,LENOCC,ICFILA
C-----------------------------------------------------------------------
C
C  Default is not a disk file and initialize...
C
      D0DAD_ISDISK=.FALSE.
      DEVNAME=FNAME
C
C  Attempt to translate input filename as a logical name...
C
      ILEN=LENOCC(FNAME)
      IBUFF(1)=IOR(ISHFT(LNM$_STRING,16),LEN(DEVNAME))
      IBUFF(2)=%LOC(DEVNAME)
      IBUFF(3)=%LOC(ILEN2)
      IBUFF(4)=0
      ISTAT=SYS$TRNLNM(LNM$M_CASE_BLIND,LOGTAB,FNAME(1:ILEN),,IBUFF)
      IF( ISTAT.NE.SS$_NORMAL .AND. ISTAT.NE.SS$_NOLOGNAM) THEN
        WRITE(*,1001) ISTAT
 1001   FORMAT(' Logical name translation failed.  Error ',Z8.8)
      ENDIF
C
C  Get device portion of filename.  If none, must be a disk file...
C
      ILEN = ICFILA(':',DEVNAME,1,LENOCC(DEVNAME))
      IF( ILEN.GT.LENOCC(DEVNAME) ) THEN
        D0DAD_ISDISK=.TRUE.
        GOTO 999
      ENDIF
C
C  Lookup device information and decide if it's a disk...
C
      ISTAT = LIB$GETDVI(DVI$_DEVCLASS,0,DEVNAME(1:ILEN),DEVTYPE)
      IF(ISTAT.EQ.SS$_NORMAL.AND.DEVTYPE.EQ.DC$_DISK)D0DAD_ISDISK=.TRUE.
      IF(ISTAT.EQ.SS$_NONLOCAL ) D0DAD_ISDISK=.TRUE.
C
C&ELSE
C&      D0DAD_ISDISK=.TRUE.
C&ENDIF
  999 RETURN
      END
