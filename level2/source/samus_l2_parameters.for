      SUBROUTINE SAMUS_L2_PARAMETERS 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOOL_PARAM Routine for SAMUS_L2 
C-
C-   Inputs  : NEWPAR : Number of parameter sets to read
C-   Outputs : None
C-   Controls: None
C-
C-   Created  15-SEP-1992 Oleg Eroshin
C-   Updated  24-OKT-1992 Alexei Volkov
C-   Updated  27-OKT-1992 Alexei Volkov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C      INCLUDE 'D0$INC:SAMUS_L2_PARAMS.INC'
      INCLUDE 'D0$LINKS:IZ2SSAM.LINK'
      INCLUDE 'D0$LINKS:IZSAMUS_L2_RCP.LINK'
c      INCLUDE 'SAMUS_L2_PARAMS.INC'
c      INCLUDE 'IZ2SSAM.LINK'
c      INCLUDE 'IZSAMUS_L2_RCP.LINK'
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK' 
      INCLUDE 'D0$LINKS:IZSSAM.LINK' 
      INCLUDE 'D0$LINKS:IZSTPC.LINK' 
C----------------------------------------------------------------------
      CHARACTER*80 ERROR_MES,OUT_MES
C----------------------------------------------------------------------
      INTEGER  IER
      INTEGER  LSL2H,GZSL2H
      INTEGER  LSTPC
      INTEGER  LOLD_SSAM,LNEW_SSAM
C
C----------------------------------------------------------------------
C- Read From The RCP File.
C----------------------------------------------------------------------
      LSL2H = GZSL2H()    
C
      IF(LSL2H.LE.0)                                  THEN
        ERROR_MES= 'COULD NOT FIND LSL2H. STOP.'
        CALL ERRMSG('SAMUS_L2','SAMUS_L2_PARAMETERS',ERROR_MES,'W')
        GOTO 1000
      END IF
C
      LSTPC     = LC(LSTPH - IZSTPC)
      LOLD_SSAM = LC(LSL2H -IZ2SSAM)
      LNEW_SSAM = LC(LSTPC - IZSSAM)
      CALL ZSHUNT(IXSTP,LOLD_SSAM,LSTPC,-IZSSAM,1)
C
      CALL EZPICK('SAMUS_UTIL_PARAM')
      CALL EZERR (IER)
      IF(IER.NE.0) THEN
        ERROR_MES=' YOU STRUCK OUT AT EZPICK SAMUS_UTIL_L2_PARAM.'
        GOTO 1000
      ENDIF
      CALL EZRSET
C
C      CALL EZPICK('SAMUS_L2_TRIGGER') ! WE DONT HAVE ANYTHING HERE YET
C      CALL EZERR (IER)
C      IF(IER.NE.0) THEN
C        ERROR_MES= 'COULD NOT FIND SAMUS_L2_TRIGGER_RCP'
C        GOTO 1000
C      ENDIF
C     CALL EZRSET
C
C      IF (NEWPAR.LE.0) GO TO 999   ! nothing more to do now
C
C      CALL EZPICK('SAMUS_L2')
C      CALL EZERR(IER)
C
C      IF(IER.NE.0) THEN        
C        ERROR_MES= 'COULD NOT FIND SAMUS_PARAMETERS_RCP'
C        GOTO 1000
C      ENDIF          
C
C      CALL EZGET('MIN_SAMUS_TRACKS',SAMUS_NUM,IER)
C      IF(IER.NE.0) THEN
C        ERROR_MES= 'COULD NOT FIND MIN_SAMUS_TRACKS'
C        GOTO 1000
C      ENDIF
C
C      CALL EZGET('MIN_SAMUS_PT',MIN_SAMUS_PT,IER)
C      IF(IER.NE.0) THEN
C        ERROR_MES= 'COULD NOT FIND MIN_SAMUS_PT'
C        GOTO 1000
C      ENDIF
C
C      CALL EZGET('NUMBER_OF_SETS',NUMBER_OF_SETS,IER)
C      IF(IER.NE.0) THEN
C        ERROR_MES= 'ERROR IN NUMBER OF SETS'
C        GOTO 1000
C      ENDIF
C
C     CALL EZRSET
C----------------------------------------------------------------------
  999 RETURN
C----------------------------------------------------------------------
 1000 CONTINUE
      WRITE(OUT_MES,1001)ERROR_MES,IER
      CALL ERRMSG('SAMUS_L2','SAMUS_L2_PARAMETERS',OUT_MES,'W')
 1001 FORMAT(A40,' IER ',I4)
C
      RETURN
      END
