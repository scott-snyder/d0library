      SUBROUTINE DISPLAY_QUANS(LPELC,CQUANS,CQUAN_NAMES,TQUANS
     &  ,TQUAN_NAMES,QUANS,NAMQUANS,NQUANS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the EM quantity to display
C-
C-   Inputs  : LPELC       - PELC/PPHO Bank address
C-             CQUANS      - Cluster related quantities
C-             CQUAN_NAMES - Cluster related quantities names
C-             TQUANS      - Tracking quantities (only for PELC bank)
C-             TQUAN_NAMES - Tracking quantities names(only for PELC bank)
C-
C-   Outputs : QUANS       - List of selected quantities to be displayed
C-             NAMQUANS    - Names of selected quantities to be displayed
C-             NQUANS      - No. of selected quantities to be displayed
C-   Controls:
C-
C-   Created  18-MAY-1993   Sailesh Chopra
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NQUANS,LPELC,LCASH,LCACL,N
      INTEGER LZTRK,LZFIT,LDTRK,LFDCT,LVTXT,LTRDT,LLYR
      REAL        CQUANS(50),TQUANS(50),QUANS(50)
      CHARACTER*8 CQUAN_NAMES(50),TQUAN_NAMES(50),NAMQUANS(50)
C----------------------------------------------------------------------
C-
      CALL VZERO(QUANS,50)
      LCACL = LQ(LPELC-2)
      LCASH = LQ(LCACL-2)
      IF (IQ(LPELC-4) .EQ. 4HPELC) THEN
        LZTRK = LQ(LPELC-3)
        LVTXT = LQ(LZTRK-6)
        LDTRK = LQ(LZTRK-7)
        LFDCT = LQ(LZTRK-8)
        LTRDT = LQ(LZTRK-9)
      ELSE
        LZTRK = 0
        LVTXT = 0
        LDTRK = 0
        LFDCT = 0
        LTRDT = 0
      ENDIF
C-
C--- Calorimeter Quantity
C-
      N = 1
      QUANS(N) = CQUANS(2)
      NAMQUANS(N) = CQUAN_NAMES(2)    !ECLUS
      N = N + 1
      QUANS(N) = CQUANS(3)
      NAMQUANS(N) = CQUAN_NAMES(3)    !ETCLUS
      N = N + 1
      QUANS(N) = CQUANS(17)
      NAMQUANS(N) = CQUAN_NAMES(17)   !CLUS ETA
      N = N + 1
      QUANS(N) = CQUANS(18)
      NAMQUANS(N) = CQUAN_NAMES(18)   !CLUS PHI
      N = N + 1
      QUANS(N) = CQUANS(19)
      NAMQUANS(N) = CQUAN_NAMES(19)   !CLUS THETA
      N = N + 1
      QUANS(N) = CQUANS(21)
      NAMQUANS(N) = CQUAN_NAMES(21)   !NCELLS
      N = N + 1
      QUANS(N) = CQUANS(13)
      NAMQUANS(N) = CQUAN_NAMES(13)   !ISO_EN ISO_CONE_SIZE=0.4
      N = N + 1
      QUANS(N) = CQUANS(4)
      NAMQUANS(N) = CQUAN_NAMES(4)    !CHISQ
C-
C--- Tracking Quantity
C-
      IF (LZTRK .LE. 0) GOTO 999
      N = N + 1
      QUANS(N) = TQUANS(1)
      NAMQUANS(N) = TQUAN_NAMES(1)    !NZTRAKS
      N = N + 1
      QUANS(N) = TQUANS(12)
      NAMQUANS(N) = TQUAN_NAMES(12)  !MATCHSIG
C-
      IF(LDTRK .GT. 0) THEN
        N = N + 1
        QUANS(N) = TQUANS(13)
        NAMQUANS(N) = TQUAN_NAMES(13)  !CDC MIP
      ELSEIF(LFDCT .GT. 0)THEN
        N = N + 1
        QUANS(N) = TQUANS(14)
        NAMQUANS(N) = TQUAN_NAMES(14)  !FDC MIP
      ENDIF
      IF(LVTXT .GT. 0) THEN
        N = N + 1
        QUANS(N) = TQUANS(15)
        NAMQUANS(N) = TQUAN_NAMES(15)  !VTX MIP
      ENDIF
      IF(LTRDT .GT. 0) THEN
        N = N + 1
        QUANS(N) = TQUANS(19)
        NAMQUANS(N) = TQUAN_NAMES(19)  !TRD MEAN
        N = N + 1
        QUANS(N) = TQUANS(16)
        NAMQUANS(N) = TQUAN_NAMES(16)  !TRD LIKLIHOOD
      ENDIF
C-
  999 NQUANS = N
      RETURN
      END
