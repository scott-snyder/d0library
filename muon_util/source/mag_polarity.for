C VAX/DEC CMS REPLACEMENT HISTORY, Element MAG_POLARITY.FOR
C *2     6-JUL-1993 19:37:04 MEENA "UPDATE FROM DARIEN :   MC FIXES"
C *1    12-MAY-1993 23:24:46 MEENA "Darien Wood: Get polarity from DBMON"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MAG_POLARITY.FOR
      SUBROUTINE MAG_POLARITY(IPOL_WAM,IPOL_SAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get torroid field polarity from DBMON datbase
C-      Warning: DBL3 access can trigger garbage collection.  Protect
C-      your pointers.
C-
C-   Inputs  : none (must be called in event loop)
C-   Outputs : IPOL_WAM = 1 for "forward", -1 for "reverse", 0 for "off"
C-             IPOL_SAM = 1 for "forward", -1 for "reverse", 0 for "off"
C-             IERR = 0 OK, 1 wamus failure, 2 samus failure, 3 both fail 
C-   Controls: 
C-
C-   Created  10-MAR-1993   Darien R. Wood, Lars 0. Rasmussen
C-   Revised  22-JUN-1993   DRW - don't try to access DBMON for Monte Carlo
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IPOL_WAM,IPOL_SAM
      REAL WAM_CUR_MIN,SAM_CUR_MIN
      INTEGER IERR,ISTAT_W,ISTAT_S,IRUN,IRUN_MIN,ITYP_REC
      CHARACTER*12    CLASS
      INTEGER         NDEV
      PARAMETER     ( NDEV = 5 )
      CHARACTER*17    MDEV( NDEV )
      REAL            MVAL( NDEV ), XHOU
      INTEGER         VTIM( 2 ),MAXCHN,IRET
C
      DATA CLASS /'DBM_MUO'/
      DATA MDEV  /'MUO_MAG_MN.CURR','MUO_MAG_MN.VOLT','MUO_MAG_SM.CURR',
     &            'MUO_MAG_MN.STAT', 'MUO_MAG_SM.STAT'/
      DATA WAM_CUR_MIN/2350./,SAM_CUR_MIN/900./
      DATA IRUN_MIN/62118/
C----------------------------------------------------------------------
      IPOL_WAM = 1
      IPOL_SAM = 1
      IERR = 3
      IF(LHEAD.GT.0) THEN
        VTIM(1) = IQ(LHEAD+4)     ! Vax system time, for that event.
        VTIM(2) = IQ(LHEAD+5)     ! From the event header
        IRUN = IQ(LHEAD+6)
        ITYP_REC = IQ(LHEAD+1)
C Monte Carlo should always be default polarity
        IF(ITYP_REC.GT.999) THEN
          IERR = 0
          GOTO 999
        ENDIF  
C
C read the DBMON database
        CALL DBMU_GETDM(CLASS,MDEV,NDEV,VTIM,1,' ',MVAL,XHOU,IRET)
C
C
        IF ( IRET .LT. 0 ) THEN
          CALL ERRMSG('Mag polarity NOT known','MAG_POLARITY',
     &     'Cannot attatch to DBMON$GLB -- no pol corr made','W')
        ELSE  
          IERR = 0
C polarity was logged only after run 62118.  Before that, polarity was
C always "forward"
          IF(IRUN.GT.IRUN_MIN) THEN    
            ISTAT_W = NINT(MVAL(4))
            ISTAT_S = NINT(MVAL(5))
            IF(.NOT.BTEST(ISTAT_W,4)) IPOL_WAM = -1
            IF(.NOT.BTEST(ISTAT_S,4)) IPOL_SAM = -1
          ENDIF  
C if magnet current was off, return polarity zero
          IF(MVAL(1).LT.WAM_CUR_MIN) THEN
            IPOL_WAM = 0
          ENDIF  
          IF(MVAL(3).LT.SAM_CUR_MIN) THEN
            IPOL_SAM = 0
          ENDIF  
        ENDIF
      ENDIF 
  999 RETURN
      END
