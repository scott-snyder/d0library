      SUBROUTINE BKCDD4(LCDD4)
C======================================================================
C
C   Purpose and Methods :  Books the bank "CDD4" containing Flash ADC
C                          raw data for the TRD
C
C-  Inputs : None
C-  Output :
C-
C-   Created  12-JAN-1988   A. ZYLBERSTEJN
C-   Updated  15-DEC-1989   A. Zylberstejn  Transmit link through argument
C-   Updated  10-JAN-1990   A. Zylberstejn  Remove version number (done in
C-                                                                  CDD4FL)
C-   Updated  11-MAY-1992   Alain PLUQUET Add MZFORM
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$LINKS:IZCDD4.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C     INCLUDE 'D0$INC:TRDLNK.INC/LIST'
      INTEGER LCDD4,IOCDD4
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C======================================================================
      IF ( LHEAD .EQ. 0 ) THEN
        CALL INTMSG( ' PROBLEM_TRD : **** Error in BKCDD4 ')
        CALL INTMSG(' Header bank LHEAD not booked')
        GO TO 999
      ENDIF
C ****  Book CDD4
      LCDD4 = LQ ( LHEAD - IZCDD4 )
      IF(LCDD4.NE.0)GO TO 999
      IF(FIRST)THEN
        CALL MZFORM ('CDD4','-I',IOCDD4)
        FIRST=.FALSE.
      END IF
      CALL MZBOOK ( IXMAIN, LCDD4, LHEAD, -IZCDD4,
     &    'CDD4', 0, 0, 50000,IOCDD4, 0 )
  999 CONTINUE
      RETURN
      END
