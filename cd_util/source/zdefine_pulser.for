      SUBROUTINE ZDEFINE_PULSER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-NOV-1990   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZPULSER.INC'
C
      INTEGER I
      INTEGER CALL_STATUS,PFNUM
      INTEGER MAXITM,POSI,ILEN
      CHARACTER FILTYP*28,TOPS*22,ITEMS(16)*28, MSG*80
C
C----------------------------------------------------------------------
      IF (GOT_TARGET) THEN
        CALL DROP_RESOURCES(CALL_STATUS)
        IF (CALL_STATUS.NE.0) THEN
          WRITE(MSG,20)TARGET
          CALL INTMSG(MSG)
          GO TO 999
        ELSE
          WRITE(MSG,25)TARGET
          CALL INTMSG(MSG)
          GOT_TARGET = .FALSE.
        ENDIF
      ENDIF
C
      POSI = 0
      MAXITM = 18
      FILTYP = 'CFG:'//DECT//'_PLS*.OWNER'
      TOPS = ' Select Pulser '
      CALL FNDFIL(POSI,MAXITM,FILTYP,TOPS,ITEMS)
      IF ((POSI .GT. 0) .AND. (PFNUM() .EQ. 1))  THEN
        TARGET = ITEMS(POSI)
        CALL STR$TRIM(TARGET,TARGET,ILEN)
        CALL GET_PLS(TARGET,CALL_STATUS)
        IF (CALL_STATUS.EQ.0) THEN
          GOT_TARGET = .TRUE.
C ADD PULSER NAME TO LIST
          DO I = 1, NPULSE
            IF (PULSER(I).EQ.TARGET) GO TO 100
          END DO
          NPULSE = NPULSE + 1
          PULSER(NPULSE) = TARGET
  100     CONTINUE
        ELSE
          CALL INTMSG(' Error - Pulser ownership not acquired ')
          GOT_TARGET = .FALSE.
        ENDIF
      ENDIF
C
   20 FORMAT(' Unable to drop ownership of Pulser ',A12)
   25 FORMAT(' Pulser ',A12,' ownership dropped ')
C
  999 RETURN
      END
