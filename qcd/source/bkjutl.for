      SUBROUTINE BKJUTL(LJUTL, IZOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : IZOFF = [I] Offset in mother bank from which to hang JUTL
C-   Outputs : LJUTL = [I] Pointer to new JUTL bank
C-   Controls: 
C-
C-   Created   5-DEC-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
c      INCLUDE 'D0$LINKS:IZJUTL.LINK'
      INTEGER IZOFF
      INTEGER LJUTL,GZANLS,LANLS,IOH,IVER,NR
      DATA IVER / 4 /         ! for version 1 - NR = 40
      DATA NR /20/
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C---  Initialize
C-
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('JUTL','1I 2B/ 1F',IOH)
      ENDIF
C
C  Book it under ANLS.
C
      LANLS = GZANLS()
      IF (LANLS.LE.0) THEN
        CALL BKANLS(LANLS)
      ENDIF
C
      CALL MZBOOK(IXMAIN,LJUTL,LANLS,-IZOFF,'JUTL',1,1,NR,IOH,0)
      IQ(LJUTL+1) = IVER
C
  999 RETURN
      END
