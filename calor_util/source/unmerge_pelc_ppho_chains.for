      SUBROUTINE UNMERGE_PELC_PPHO_CHAINS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unmerge linear chains of PELCs and PPHOs 
C-                         to undo MERGE_PELC_PPHO_CHAINS
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   1-JUN-1995   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INTEGER LPPHO,GZPPHO,LPARH,GZPARH,LPELC,GZPELC
C----------------------------------------------------------------------
      LPARH = GZPARH()
      LPELC = GZPELC()

C- Copy the PELC/PPHO chain at LPELC to the usual PPHO location
      
      IF (LPELC.GT.0) CALL MZCOPY(IXCOM,LPELC,IXMAIN,LPARH,-IZPPHO,'L')

C- Now loop over both chains dropping PELCs from PPHO or vice versa

      LPELC = GZPELC()
      DO WHILE (LPELC.GT.0)
        IF (Q(LPELC-4).NE.4HPELC) CALL MZDROP(IXCOM,LPELC,'.')
        LPELC = LQ(LPELC)
      ENDDO

      LPPHO = GZPPHO()
      DO WHILE (LPPHO.GT.0)
        IF (Q(LPPHO-4).NE.4HPPHO) CALL MZDROP(IXCOM,LPPHO,'.')
        LPPHO = LQ(LPPHO)
      ENDDO

  999 RETURN
      END
