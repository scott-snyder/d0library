      SUBROUTINE BKTRGR (LTRGR, TRGR_BANK_LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books the TRGR bank.
C-
C-      NOTE: There must already be a HEAD bank for the event.
C-
C-   Inputs  : TRGR_BANK_LENGTH : TRGR Zebra bank length.
C-
C-   Outputs : LTRGR : Zebra pointer to TRGR bank.
C-
C-   Controls: None.
C-
C-   Created  20-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  23-AUG-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-    - Now always creates an new TRGR bank at the head of a linear chain
C-      pointed to by LQ(LHEAD-IZTRGR). Older TRGR banks can be found by moving
C-      down the linear chain.
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER GZTRGR
      INTEGER GZHEAD
C
      INTEGER  LTRGR, TRGR_BANK_LENGTH
C
      LOGICAL  FIRST
      INTEGER  IOTRGR
C
      SAVE FIRST, IOTRGR
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
C     Defines the TRGR bank data format : only once
C     =============================================
C
      IF (FIRST) THEN
        CALL MZFORM ('TRGR','-I',IOTRGR)
        FIRST = .FALSE.
      ENDIF
C
C     TRGR bank booking
C     =================
C
      CALL MZBOOK (IXMAIN, LTRGR,LHEAD,-IZTRGR,'TRGR',0,0,
     +             TRGR_BANK_LENGTH,IOTRGR,-1)
      RETURN
C
      END
