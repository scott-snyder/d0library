      SUBROUTINE FUNSUP(PED,EVDATA,Y)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : "Unzerosuppress" zero suppressed data
C-
C-   Inputs  : PED = value of pedestal
C-             EVDATA = FADC data
C-   Outputs : Y = expanded FADC data
C-   Controls:
C-
C-   Created  12-OCT-1990   Susan K. Blessing
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files 
C-   Updated  02-APR-1992   Susan K. Blessing   Check existance of 
C-    FTRAKS_RCP before reading.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER I,J,K
      INTEGER EVDATA(0:LFADC-1)
      INTEGER LENCLU,IPREAD,IFIR,IP
      INTEGER TMPADC
      INTEGER IER
      INTEGER MAP(0:255)
      INTEGER LRCP
C
      REAL PED
      REAL Y(0:LFADC-1)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZLOC('FTRAKS_RCP',LRCP)                        
        IF (LRCP.GT.0) THEN
          CALL EZPICK('FTRAKS_RCP')
          CALL EZGET('MAP(1)',MAP(0),IER)
          CALL EZRSET
        ELSE
          CALL EZPICK('CD_ELECTRONICS_RCP')
          CALL EZGET('MAP(1)',MAP(0),IER)
          CALL EZRSET
        END IF
        FIRST = .FALSE.
      END IF
C
      IF (EVDATA(0).LE.0) GOTO 29
      IPREAD=0
   29 LENCLU = EVDATA(IPREAD)
      IF (LENCLU.NE.0) THEN
        IFIR = EVDATA(IPREAD+1)
        IP = IPREAD + 1
        IPREAD = IPREAD+LENCLU+2
        DO 40 I = 1,LENCLU
          TMPADC = INT(FLOAT(EVDATA(IP+I))-PED)
          IF (TMPADC.GT.0) TMPADC = MAP(TMPADC)
          Y(IFIR+I) = FLOAT(TMPADC) + PED
   40   CONTINUE
        GOTO 29
      ENDIF
C-----------------------------------------------------------------------
  999 RETURN
      END
