      SUBROUTINE VTX_DYNADJ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get voltage sagging corrections and envirnmental
C-               corrections for GAIN and DISTANCE-TIME corrections.  Apply
C-               these correctons directly to the VGNL and VDTM banks.
C-               
C-               Use VTX_LUMADJ for voltage sagging for RUN<LUM_STOP_RUN
C-               Use VTX_HVADJ   "     "      "      "   " >=    "
C-               Use VTX_ENVAHD for T,P corrections for RUN>ENV_START_RUN
C-      
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-DEC-1992   Ed Oltman
C-   Updated  15-JAN-1993   Ed Oltman  fix bug -- storing VGNL 
C-   Updated  25-MAR-1993   Ed Oltman  fix bug -- skip if  monte carlo
C-   Updated  27-MAR-1993   Ed Oltman  Modify error messages
C-   Updated   3-NOV-1993   Ed Oltman  FIX GAIN ADJUST BUG; ADD VTX_GETGFAC,
C-                                     VTX_GETDFAC ENTRIES
C-   Updated  16-NOV-1993   Ed Oltman  Save VGNL contents once per run rather
C-                                     then once -- get ready for run dep. corr.
C-   Updated  17-NOV-1993   Liang-Ping Chen update GSAVE only when VGNH, VGNL
C-                   are fresh from database (no environmental correction has
C-                   been applied, DYN_BIT of IC(LVGNH) not set)
C-   Updated  19-NOV-1993   Liang-Ping Chen Do nothing if it is called more than
C-                   once for the same event          
C-   Updated  12-FEB-1994   Ed Oltman   Add call to VTRPAR...
C-                                      Update VGNL, VTMW from VCAL bank..
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVCAL.LINK'
C I/O:  
      INTEGER LUM_STATUS,ENV_STATUS
c Locals:
      INTEGER LUM_STAT,ENV_STAT,KVGNL
      INTEGER LAYER,SECTOR,WIRE,WEND,NSEC(0:2),RUN,FEED
      INTEGER LEN,LUM_START_RUN,LUM_STOP_RUN,ENV_START_RUN,ERR,J
      INTEGER LAST_RUN, LAST_EVT, LVTXH
      INTEGER GZVGNH, DYN_BIT, EVENT,LVCAL,LAST_VCAL_TIME
      REAL    GAIN_ADJ(2,0:31,0:2),GFAC,DFAC,FACT
      REAL    GSAVE(0:1535),GFACTOR,DFACTOR
      LOGICAL FIRST,NO_CORRECTION
      LOGICAL DO_LUM,DO_ENV,DO_HV   ! Corrections SHOULD be applied
      LOGICAL NEW_LUM,NEW_ENV       ! Corrections ARE TO BE applied
      CHARACTER*80 TXT
      CHARACTER*1  SEVER
      LOGICAL APPLY_LUM,APPLY_ENV,APPLY_HV
c Externals:
      INTEGER GZVGNL,TRULEN,RUNNO,GZVCAL
      INTEGER EVONUM,GZVTXH
      LOGICAL VTRPAR
c Data:
      DATA FIRST/.TRUE./
      DATA NSEC/15,31,31/
      DATA LAST_RUN/-1/, LAST_EVT/-1/
      DATA LAST_VCAL_TIME/0/
      PARAMETER (DYN_BIT=0)
C----------------------------------------------------------------------
      LUM_STAT = 0
      ENV_STAT = 0
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('LUM_START_RUN',LUM_START_RUN,ERR)
        CALL EZGET('LUM_STOP_RUN',LUM_STOP_RUN,ERR)
        CALL EZGET('ENV_START_RUN',ENV_START_RUN,ERR)
        CALL EZGET('APPLY_LUM_CORR',APPLY_LUM,ERR)
        CALL EZGET('APPLY_ENV_CORR',APPLY_ENV,ERR)
        CALL EZGET('APPLY_HV_CORR' ,APPLY_HV ,ERR)
        CALL EZRSET
      ENDIF
C
C ****  If run has changed, call standard run setup routine
C
      RUN = RUNNO()
      IF (RUN .NE. LAST_RUN) THEN
        IF (.NOT. VTRPAR()) 
     &    CALL ERRMSG('VTRPAR failed','VTX_DYNADJ',' ','F')
      ENDIF
C
C ****  If MC, nothing to do..
C
      IF (IQ(LHEAD+1) .GT. 1000) THEN
        LAST_RUN = RUN
        ENV_STAT = 0
        LUM_STAT = 0
        GFAC = 1.
        DFAC = 1.
        GO TO 999
      ENDIF
C
C ****  IF VCAL EXISTS, NO NEED TO ACCESS DBL3 -- CHECK TO SEE IF VCAL IS
C ****  DIFFERENT FROM LAST ONE; IF IT IS, UPDATE THE SVTX BANKS
C
      LVCAL = GZVCAL(1)
      IF (LVCAL .GT. 0) THEN
        LAST_RUN = RUN
        IF (LAST_VCAL_TIME .EQ. IQ(LVCAL+5)) GO TO 999
        LAST_VCAL_TIME = IQ(LVCAL+5)
        CALL VCAL_TO_SVTX(ENV_STAT,LUM_STAT,GFAC,DFAC)
        LAST_RUN = RUNNO() 
        GO TO 999
      ENDIF
C
C ****  No VCAL bank --> do a DBL3 access if appropriate
C
      EVENT = EVONUM()
      IF (RUN .EQ. LAST_RUN.AND.EVENT.EQ.LAST_EVT) GOTO 900
      IF (RUN .NE. LAST_RUN) THEN
        DO_LUM = (RUN.GE.LUM_START_RUN) .AND. (RUN.LT.LUM_STOP_RUN)
     &          .AND. APPLY_LUM
        DO_HV  = (RUN.GE.LUM_STOP_RUN) .AND. APPLY_HV
        DO_ENV = RUN.GE.ENV_START_RUN .AND. APPLY_ENV
        NO_CORRECTION = .NOT. (DO_LUM .OR. DO_ENV .OR. DO_HV)
        SEVER = 'I'
        IF (DO_ENV) THEN
          TXT = ' VTX Environmental corrections to be applied'
        ELSE
          TXT = ' VTX Environmental corections NOT applied'
          SEVER = 'W'
        ENDIF
        LEN = TRULEN(TXT)
        CALL INTMSG(TXT(1:LEN))
        IF (SEVER(1:1) .EQ. 'I') THEN
          CALL ERRMSG('VTX P,T corr made','VTX_DYNADJ',TXT(1:LEN),SEVER)
        ELSE
          CALL ERRMSG('VTX P,T corr NOT made','VTX_DYNADJ',TXT(1:LEN),
     &      SEVER)
        ENDIF
        SEVER = 'I'
        IF (DO_LUM) THEN
          TXT = ' VTX HV correction made using I(LUM) parameterization'
        ELSEIF(DO_HV) THEN
          TXT = ' VTX HV correcton made using measured I,V from DBMON'
        ELSE
          TXT = ' VTX HV sagging correction NOT made'
          SEVER = 'W'
        ENDIF
        LEN = TRULEN(TXT)
        CALL INTMSG(TXT(1:LEN))
        IF (SEVER(1:1) .EQ. 'I') THEN
          CALL ERRMSG('VTX HV corr made','VTX_DYNADJ',TXT(1:LEN),SEVER)
        ELSE
          CALL ERRMSG('VTX HV corr NOT made','VTX_DYNADJ',TXT(1:LEN),
     &      SEVER)
        ENDIF
C
C ****  Save database values of VGNL gains
C
        LVGNH = GZVGNH()
        IF (.NOT. BTEST(IC(LVGNH),DYN_BIT) ) THEN ! fresh VGNL from database  
          IC(LVGNH) = IBSET(IC(LVGNH), DYN_BIT)      
          DO LAYER = 0,2
            KVGNL = GZVGNL(LAYER)
            DO SECTOR = 0,NSEC(LAYER)
              DO WIRE = 0,7
                DO WEND = 0,1
                  J = 16*SECTOR + 2*WIRE + WEND
                  GSAVE(512*LAYER+J) = C(KVGNL+5+3*41+J+1)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C
C ****  NORMAL ENTRY
C
      IF (NO_CORRECTION) GO TO 900
      IF (DO_LUM) THEN
        CALL VTX_LUMADJ(GAIN_ADJ,LUM_STAT,NEW_LUM)
      ELSEIF(DO_HV) THEN
        CALL VTX_HVADJ (GAIN_ADJ,LUM_STAT,NEW_LUM)
      ENDIF
      IF (DO_ENV) THEN
        CALL VTX_ENVADJ(GFAC,DFAC,ENV_STAT,NEW_ENV)
      ENDIF
C
C ****  If nothing has been updated, we're done.  Otherwise, replace the
C ****  gain contents of VGNL and/or replace scale contents of VDTM
C
      IF (.NOT. (NEW_LUM .OR. NEW_ENV) ) GO TO 900
      DO LAYER = 0,2
        KVGNL = GZVGNL(LAYER)
        DO SECTOR = 0,NSEC(LAYER)
          DO WEND = 0,1
            DO WIRE = 0,7
              FEED = 1
              IF ((WIRE .EQ. 0) .OR. (WIRE .EQ. 7)) FEED = 2
              FACT = GAIN_ADJ(FEED,SECTOR,LAYER)
              FACT = FACT*GFAC
              J = 16*SECTOR + 2*WIRE + WEND
              C(KVGNL+5+3*41+J+1) = GSAVE(512*LAYER+J)*FACT
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      IF (NEW_ENV) CALL VTX_ENVDTM(DFAC)  ! Update scale factors in VDTM banks
      CALL SVTX_TO_VCAL
  900 CONTINUE
C
C **** OK, Copy the VCAL hanging from SVTX to the event record..
C
      LVCAL = GZVCAL(1)
      IF (LVCAL .GT. 0) CALL MZDROP(IXCOM,LVCAL,' ')
      LVCAL = GZVCAL(0)
      LVTXH = GZVTXH()
      IF (LVTXH .EQ. 0) THEN
        CALL BKVTXH
        LVTXH = GZVTXH()
      ENDIF
      CALL MZCOPY(IDVSTP,LVCAL,IXMAIN,LVTXH,-IZVCAL_1,' ')
      IF (RUN .NE. LAST_RUN .OR. EVENT .NE. LAST_EVT)
     &          CALL VCAL_TO_SVTX(ENV_STAT,LUM_STAT,GFAC,DFAC)
      LAST_EVT = EVENT
      LAST_RUN = RUN
      GO TO 999
C
C ****  OTHER ENTRIES
C
      ENTRY VTX_DYNSTAT(ENV_STATUS,LUM_STATUS)
      ENV_STATUS = ENV_STAT
      LUM_STATUS = LUM_STAT
      GO TO 999
      ENTRY VTX_GETGFAC(GFACTOR)
      GFACTOR = GFAC
      GO TO 999
      ENTRY VTX_GETDFAC(DFACTOR)
      DFACTOR = DFAC
  999 RETURN
      END
