      SUBROUTINE ZTRALL
C----------------------------------------------------------------------
C
C  Find all tracks in central detector.
C
C  Daria Zieminska March 1989
C-   Updated   2-NOV-1989   Serban Protopopescu 
C-   Updated   4-FEB-1991   Daria Zieminska   
C-   Set bit 12 in IQ(LZTRH) to indicate that full tracking has been done
C-   Skip if the bit has already been set
C-   Updated  28-MAR-1991   Susan K. Blessing   Add flags CDCON, FDCON,
C-    VTXON, TRDON to allow each sub]detector to be turned off.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'        
C
      INTEGER IER
      INTEGER RUN,ID,RUNSAV,IDSAV,FULL,GZZTRH,LZTRH
C
      LOGICAL FIRST
      LOGICAL CDCON,FDCON,VTXON
      LOGICAL VTRAKS,DTREVT,FTRAKS,OK
C
      SAVE RUNSAV,IDSAV
      SAVE FIRST,CDCON,FDCON,VTXON
C
      DATA RUNSAV,IDSAV/-1,-1/
      DATA FIRST/.TRUE./
      DATA CDCON,FDCON,VTXON/3*.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('ZTRAKS_RCP',IER)
        CALL EZGET('CDCON',CDCON,IER)
        CALL EZGET('FDCON',FDCON,IER)
        CALL EZGET('VTXON',VTXON,IER)
        FIRST = .FALSE.
        CALL EZRSET
      END IF
C
      CALL EVNTID(RUN,ID)
      IF(RUN.NE.RUNSAV.OR.ID.NE.IDSAV) THEN
        RUNSAV=RUN
        IDSAV=ID
        LZTRH=GZZTRH()
        IF (LZTRH.GT.0) THEN
          FULL=IBITS(IQ(LZTRH),12,1)
          IF (FULL.EQ.1) THEN 
            CALL ZTRHIS
            GO TO 999 ! full tracking done
          END IF
        END IF
C
        IF (VTXON) OK=VTRAKS() 
        IF (CDCON) OK=DTREVT() 
        IF (FDCON) OK=FTRAKS() 
C
        IF (VTXON.AND.CDCON) CALL CVTRAK
        IF (VTXON.AND.FDCON) CALL FVTRAK
C
        CALL ZTSOLO
C
        LZTRH=GZZTRH()
        IQ(LZTRH)=IBSET(IQ(LZTRH),12)
        CALL ZTRHIS
C
      ENDIF
C
  999 RETURN
      END
