      SUBROUTINE BKMDST(LMDST,NUM_WORDS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book Zebra bank MDST (microDST)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-AUG-1991   Andrew J. Milder
C-   Updated  11-OCT-1991   Andrew J. Milder  Bug fix- skip if bank exists
C-   Updated  28-SEP-1993   R. Astur - updated for new format
C-   Updated  17-NOV-1993   R. Astur - Add structural link for CAID
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZMDST.LINK/LIST'
      INTEGER LMDST,IXIO_MDST,NUM_WORDS,LANLS,GZANLS,GZMDST
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      LMDST = 0
      LMDST = GZMDST()
      IF (LMDST.LE.0) THEN
        IF (FIRST) THEN
          CALL MZFORM('MDST','25I-F',IXIO_MDST)
          FIRST = .FALSE.
        ENDIF
C
        LANLS = GZANLS()
        IF (LANLS.LE.0) THEN
          CALL BKANLS(LANLS)
        ENDIF
        CALL MZBOOK(IXMAIN,LMDST,LANLS,-IZMDST,'MDST',
     &    5,5,NUM_WORDS,IXIO_MDST,0)
      ENDIF
  999 RETURN
      END
