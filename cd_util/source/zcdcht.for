      SUBROUTINE ZCDCHT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack part of outer sense wire hits and
C-                         their associated deley line hits for finding
C-                         vertex's Z position
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  27-FEB-1990   Qizhong Li-Demarteau
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INCLUDE 'D0$INC:CDPARA.INC/LIST'
      INCLUDE 'D0$INC:DDEBUG.INC/LIST'
      INCLUDE 'D0$INC:CDCCOS.INC/LIST'
      INCLUDE 'D0$INC:CDLOCA.INC'
C
      INTEGER NHITS, NHITS1, ERR, INDEX, IER
      INTEGER KPDSEC, KPDCDA
      INTEGER NPULSE(0:10), NPULS1(0:10)
      INTEGER MAXSEC
      INTEGER MXHTOT, MAXHIT
      PARAMETER( MAXHIT = 15 )
      PARAMETER( MXHTOT= 50 )
      REAL HITLST(LPULSE,MXHTOT),HITLS1(LPULSE,MXHTOT)
      LOGICAL EZERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZCDCHT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXSEC',MAXSEC,ERR)
        CALL EZRSET
      ENDIF
C
C ****  Initialise the bank structure
C
      CALL BKCDCH
C
      DO 10 SECTOR = 0, MAXSEC
        NHITS  = 0
        NHITS1 = 0
C
C    first look at the most inner sense wire and the delay line
C
        LAYER = 0
        DO 20 INDEX = 0, 2
          WIRE = INDEX
          IF (INDEX .NE. 0) WIRE = INDEX + 6
          IF( NHITS .GE. MXHTOT ) GOTO 10
          NPULSE(WIRE) = 0
          CALL CDPULS(NPULSE(WIRE) , HITLST(1,NHITS+1), MXHTOT-NHITS)
          IF (NPULSE(0) .GE. MAXHIT .OR. NPULSE(0) .LE. 0) GOTO 10
          IF (INDEX .EQ. 0) THEN
            LAYER = 2
            WIRE = 6
            NPULS1(WIRE) = 0
            CALL CDPULS(NPULS1(WIRE),HITLS1(1,NHITS1+1),MXHTOT-NHITS1)
            IF (NPULSE(0) .NE. NPULS1(6)) GOTO 10
C            IF (NPULSE(0) .NE. NPULS1(6) .AND. 
C     &         (NPULSE(0)+1) .NE. NPULS1(6)) GOTO 10
            NHITS1 = NHITS1 + NPULS1(WIRE)
            LAYER = 0
            WIRE = 0
          ENDIF
C
          NHITS = NHITS + NPULSE(WIRE)
  20    CONTINUE
        CALL BKDSEC(LAYER, SECTOR, NHITS, KPDSEC)
        CALL BKDCDA(LAYER, SECTOR, NHITS, KPDCDA)
C
        CALL ZFDCDA(HITLST, NPULSE)
        CALL ZFDSEC
        CALL CDGETZ
C
C    Then look at the most outer delay line in the layer 2 
C
        LAYER = 2
        DO 30 INDEX = 1, 2
          WIRE = INDEX + 8
          IF( NHITS1 .GE. MXHTOT ) GOTO 10
          CALL CDPULS(NPULS1(WIRE),HITLS1(1,NHITS1+1),MXHTOT-NHITS1)
          NHITS1 = NHITS1 + NPULS1(WIRE)
  30    CONTINUE
        CALL BKDSEC(LAYER, SECTOR, NHITS1, KPDSEC)
        CALL BKDCDA(LAYER, SECTOR, NHITS1, KPDCDA)
C
        CALL ZFDCDA(HITLS1, NPULS1)
        CALL ZFDSEC
        CALL CDGETZ
  10  CONTINUE
C
      CDCLNK(1) = 0
      LCDCH = 0
      CALL VZERO(LDLYR(0),4)
      CALL VZERO(LDSEC(0,0),32*4)
      CALL VZERO(LDCDA(0,0),32*4)
C
  999 RETURN
      END
