      SUBROUTINE BKGTLY(LGTLY,ILAYT)
C======================================================================
C-
C-   Purpose and Methods :  Books the trd banks down to LAYER for
C-                           GTRH (under GHITS)
C-
C-
C-
C-
C-   Created   6-NOV-1987   A. ZYLBERSTEJN
C-   Updated  28-DEC-1989   A. Zylberstejn  :use GZTRH to get upper link
C-
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C      INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INTEGER  GZGTRH,LGTRH,LGTLY
      INTEGER ILAYT,NIO
C
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
C
C======================================================================
C
C  book GTRH if needed
      LGTRH=GZGTRH()
      IF(LGTRH.EQ.0)
     +      CALL BKGTRH(LGTRH) !book the ZBOOK structure down to GTRH
C
C  DEFINE THE DATA FORMAT
      IF ( FIRST ) THEN
        CALL MZFORM('GTLY','12F -I',NIO)
        FIRST = .FALSE.
      ENDIF
C ****  Book GTLYR(ILAYT)
      CALL MZBOOK ( IXMAIN, LGTLY, LGTRH, -ILAYT,
     &    'GTLY', 0, 0, 25, NIO, 0 )
C
  999 CONTINUE
      RETURN
      END
