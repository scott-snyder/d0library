      SUBROUTINE UDST_FIX_PELC(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill computed quantities in PELC/PPHO banks
C-
C-   Inputs  : LPELC - pointer to PELC/PPHO bank
C-
C-   Created  18-NOV-1995   Ulrich Heintz   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ET,ETA,PHI
      INTEGER LPELC,LZTRK,LDTRK,LFDCT

      IF(LPELC.EQ.0)THEN
        CALL ERRMSG('LPELC=0','UDST_FIX_PELC','called with LPELC=0','W')
        GOTO 999
      ENDIF

      ET  = Q(LPELC+7)
      ETA = Q(LPELC+9)
      PHI = Q(LPELC+10)
C... fill kinematic quantities
      Q(LPELC+3) = ET*COS(PHI)
      Q(LPELC+4) = ET*SIN(PHI)
      Q(LPELC+5) = ET*SINH(ETA)
C... fill isolation fraction
      IF(Q(LPELC+17).GT.0)THEN
        Q(LPELC+32) = (Q(LPELC+16) - Q(LPELC+17))/Q(LPELC+17)
      ENDIF
C... fill dE/dx for CDC/FDC track
      LZTRK = LQ(LPELC-3)             ! Link to associated ZTRAK bank
      IF (LZTRK .GT. 0) THEN
        LDTRK = LQ(LZTRK-7)
        IF (LDTRK.NE.0) THEN
          Q(LPELC+46) = Q(LDTRK+20) ! Ionization in chamber
        ELSE
          LFDCT = LQ(LZTRK-8)
          IF (LFDCT.NE.0) THEN
            Q(LPELC+46) = Q(LFDCT+20)
          ENDIF
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
