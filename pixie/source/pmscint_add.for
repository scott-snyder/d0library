      SUBROUTINE PMSCINT_ADD(TOTMSCNT,SADD,NHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gives the ADD and # of scints hit
C-
C-   Inputs  : Total No. of scints TOTMSCNT
C-   Outputs : Address of scint and ith scint being hit
C-   Controls:
C-
C-   Created  25-FEB-1994   Vipin Bhatnagar
C-   Modified 26-MAY-1994   Vipin Bhatnagar
C-    Cleanup of code
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER TOTMSCNT,SADD(10),NHT
      INTEGER IFLAG,IMOUT,I,IADD
      INTEGER LMUHT, GZMUHT

      REAL    TOF,TXYZ,XYZ,DXYZ

C     DATA I/1/
C----------------------------------------------------------------------
      LMUHT = GZMUHT()
      TOTMSCNT = IQ(LMUHT + 9)
      I = 1
  100 CALL GTMSCT(I,IADD,IFLAG,IMOUT,TOF,TXYZ,XYZ,DXYZ)
      IF ( IADD.NE.0) THEN
        SADD(I) = IADD
        NHT     = I
        IF ( I.EQ.TOTMSCNT ) GOTO 999
        I       = I + 1
        GOTO 100
      ELSEIF ( I.EQ.1.AND.TOTMSCNT.EQ.1) THEN
        NHT     = 0
        GOTO 999
      ELSEIF ( I.EQ.TOTMSCNT) THEN
        GOTO 999
      ENDIF
C
  999 RETURN
      END
