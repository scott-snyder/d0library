      SUBROUTINE MUGLOBAL(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Global fitting for muon tracks.
C-   Update MUON bank
C-
C-   Inputs  :
C-   Outputs : IERR   = 0  if fit successful
C-   Controls:
C-
C-   Created  22-JUN-1992   Daria Zieminska
C-   Modified 28-SEP-1995   Darien Wood, skip tracks with IFW2 bit 23 set
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR,NMUON,LMUON,GZMUON,IMUON,NWAMUS,NSAMUS,LMUOT,NASTUB
      INTEGER IFW2
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      CALL DHDIR('MURECO_RCP','HBOOK_DIRECTORY',IERR,' ')
C
      IF(IERR.NE.0) THEN
        CALL ERRMSG('MURECO','MURECO_HST',
     +     'ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF (FIRST) THEN
        FIRST=.FALSE.
      END IF
      CALL GTMTRH(NMUON)
      IF(NMUON .EQ. 0)GOTO 999
      DO 100 IMUON = 1,NMUON
        LMUON = GZMUON(IMUON)
        IF (LMUON.LE.0) GO TO 100
        LMUOT=LQ(LMUON-11)
        NWAMUS=IQ(LMUOT+1)
        NSAMUS=IQ(LMUOT+2)
        NASTUB=IQ(LMUOT+4)
        IFW2=IQ(LMUOT+5)
C don't use "unrefitable" tracks produced by MUFIX
        IF(.NOT.BTEST(IFW2,23)) THEN
          IF (NWAMUS.GT.0) THEN
            IF (NSAMUS.EQ.0) THEN
              CALL MCGLOBAL(LMUON,IERR)     ! central track
              IF(NASTUB.NE.5) CALL MFGLOBAL(LMUON,IERR) ! forward WAMUS track
            END IF
          ELSE IF(NSAMUS.GT.0) THEN
            IF(NASTUB.NE.5) CALL SAGLOBAL(LMUON,IERR)     ! Pure SAMUS
          ENDIF
        ENDIF
  100 CONTINUE
  999 RETURN
      END
