      SUBROUTINE BKGTRH(LGTRH)
C======================================================================
C
C   Purpose and Methods :  Books the /ZEBCOM/ event data structure down
C                          through the level of GTRH in preparation for
C                          adding TRD hits
C    OUTPUT: LGEANT--->LGHIT--->LGTRH ARE CREATED (IF NEEDED)
C-
C-   Created   6-NOV-1987   A. ZYLBERSTEJN
C-   Updated   8-DEC-1989   A. Zylberstejn  Change call to MZLINT
C-   Updated  28-DEC-1989   A. Zylberstejn  Put link as an argument
C-   Updated  11-MAY-1992   Alain PLUQUET  Add MZFORM
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C     INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INCLUDE 'D0$LINKS:IZGTRH.LINK/LIST'
      INTEGER GZGHIT,LGHIT,LGTRH,IOGTRH
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C ****  Initialize a temporary link area
C      IF(GTRHLN(1).EQ.0)
C     +   CALL MZLINT ( IXCOM,  '/GTRHLN/', GTRHLN, LGTRH,GTRHLN)
C--   GET LINK TO SUPPORTING ZTRH BANK
      LGHIT=GZGHIT()
C--   CHECK LGHIT
      IF(LGHIT.LE.0)CALL BKGHIT(LGHIT)
C ****  Book GTRH
      LGTRH = LQ ( LGHIT - IZGTRH )
      IF ( LGTRH .EQ. 0 ) THEN
        IF(FIRST)THEN
          CALL MZFORM('GTRH','-I',IOGTRH)
          FIRST=.FALSE.
        END IF
        CALL MZBOOK ( IXMAIN, LGTRH, LGHIT, -IZGTRH,
     &    'GTRH', 3, 3, 5, IOGTRH, 0 )
      ENDIF
  999 CONTINUE
      RETURN
      END
