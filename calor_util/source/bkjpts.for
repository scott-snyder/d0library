      SUBROUTINE BKJPTS(LJETS,NJPTS,LJPTS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BKJPTS books the JPTS banks under the JETS banks.
C-              The routine returns LJPT0, the link to the JPTS bank under the
C-              first JETS bank. BKJPTS does not try to book the supporting
C-              JETS banks if they do not exist.
C-
C-   Inputs  : 
C-     LJETS = address of supporting jet bank
C-     NJPTS = number of pointers
C-   Output  :
C-     LJPTS = pointer to created bank
C-
C-   Created  12-OCT-1988   Z. Wolf
C-   Updated  24-MAR-1989   Z. Wolf
C-   Modified 22-Apr-1989   S. Protopopescu
C-   MODIFIED 25-MAY-1989 N.J. HADLEY
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LJETS,NJPTS,LJPTS,GZCAPH
      LOGICAL FIRST /.TRUE./
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJPTS.LINK/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER IDN,IFORM
C----------------------------------------------------------------------
      LJPTS=LQ(LJETS-IZJPTS)
      IF(LJPTS.GT.0)GO TO 999
C
C
      IDN = IQ(LJETS-5)
      CALL MZBOOK(IXMAIN,LJPTS,LJETS,-IZJPTS,'JPTS',0,0,NJPTS+2,2,0)
      IQ(LJPTS-5) = IDN         !SET ID NUMBER
      IQ(LJPTS+1) = 1           ! version #
      IQ(LJPTS+2) = 0           ! Number of cells filled in FLJPTS
  999 RETURN
      END
