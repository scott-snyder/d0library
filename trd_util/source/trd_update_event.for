      LOGICAL FUNCTION TRD_UPDATE_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads monitoring data in TROP bank once
C-                         per event
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-DEC-1992   Jean-Francois Glicenstein
C-   Updated  22-JUN-1993   JFG  Added error code in TROP
C-   Updated   2-FEB-1995   A. Zylberstejn   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL ERROR,FIRST,BYPASS_DBMON_ERROR
      INTEGER I,IER
      character*3 CAN_UPDT,HV_UPDT,c3
      INTEGER LTHIT,GZTHIT
      INTEGER LTROP
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('UPDATE_CANARY',i,IER)
        CALL UHTOC(I,3,CAN_UPDT,3)
        CALL EZGET('UPDATE_HV',i,IER)
        CALL UHTOC(I,3,HV_UPDT,3)
        CALL EZGET('BYPASS_DBMON_ERROR',BYPASS_DBMON_ERROR,IER)
        CALL EZRSET
      ENDIF
      TRD_UPDATE_EVENT = .TRUE.
      ERROR = .FALSE.
      LTHIT=GZTHIT()
      IF(LTHIT.NE.0)THEN
        IF(IQ(LTHIT+1).GT.3)GO TO 999 ! Get information from THIT
      END IF
      LTROP=LC(LTGEN-IZTROP)
      IF (LTROP.LE.0) THEN
        IF(BYPASS_DBMON_ERROR)THEN
          CALL INTMSG(' TRD_UPDATE_EVENT: no TROP bank   ')
          GO TO 999
        ELSE
          CALL ERRMSG('No TROP bank ','TRD_UPDATE_EVENT',
     &      ' No TROP bank','F')
          GO TO 999
        END IF
      ENDIF
C     Updates from DBMON
      IF (CAN_UPDT.EQ.'Y'.OR.CAN_UPDT.EQ.'YES') THEN
        CALL TRDCAN_UPDATE(ERROR)
      ENDIF
      IF (ERROR) THEN
        IF (.NOT.BYPASS_DBMON_ERROR) THEN
          C(LTROP+100) = 4.
          CALL INTMSG('TRD_UPDATE_EVENT: error in reading gas data')
          GOTO 999
        ELSE
          CALL INTMSG('TRD_UPDATE_EVENT: error in reading gas data')
        ENDIF
        TRD_UPDATE_EVENT = .FALSE.
      ENDIF
C     ERROR gets .TRUE. if anything goes wrong when reading the DBL3
C     databases
      IF (HV_UPDT.EQ.'Y'.OR.HV_UPDT.EQ.'YES') THEN
        CALL TRDHV_UPDATE(ERROR)
      ENDIF
      IF (ERROR) THEN
        IF (.NOT.BYPASS_DBMON_ERROR) THEN
          C(LTROP+100) = 5.
          CALL INTMSG('TRD_UPDATE_EVENT: error in reading HV data')
          GOTO 999
        ELSE
          CALL INTMSG('TRD_UPDATE_EVENT: error in reading HV data')
        ENDIF
        TRD_UPDATE_EVENT = .FALSE.
      ENDIF
  999 RETURN
      END
