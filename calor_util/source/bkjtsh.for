      SUBROUTINE BKJTSH(LJETS,LJTSH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BKJTSH books the JTSH banks under the JETS banks.
C-              The routine returns LJTSH, the link to the JTSH bank under the
C-              JETS bank. BKJTSH does not try to book the supporting
C-              JETS bank if It does not exist.
C-
C-   Inputs  : 
C-     LJETS = address of supporting jet bank
C-   Output  :
C-     LJTSH = pointer to created bank
C-
C-   CREATED 06-JULY-1989 N.J. HADLEY
C-   Updated   3-MAY-1990   N.J. Hadley 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LJETS,LJTSH,GZJETS
      LOGICAL FIRST /.TRUE./
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJTSH.LINK/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER IDN,IFORM
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL MZFORM('JTSH','2I 7F 1I 10F',IFORM)
        LJETS = GZJETS()
        FIRST=.FALSE.
      END IF
C
C
      LJTSH=LQ(LJETS-IZJTSH)
      IF(LJTSH.GT.0)GO TO 999
C
C
      IDN=IQ(LJETS-5)
      CALL MZBOOK(IXMAIN,LJTSH,LJETS,-IZJTSH,'JTSH',0,0,20,IFORM,0)
      IQ(LJTSH-5)=IDN         !SET ID NUMBER
      IQ(LJTSH+1)=1           ! version #
  999 RETURN
      END
