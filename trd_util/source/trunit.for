      INTEGER FUNCTION TRUNIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define the output logical unit for TRD print out
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-JUN-1990   A. Zylberstejn
C-   Updated  19-MAR-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      LOGICAL FIRST
      INTEGER I,IER,USUNIT,UNIT_USER
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TRD_RCP')
        UNIT_USER=0
        CALL EZGET('LUDEBG',I,IER)
        IF(IER.EQ.0)UNIT_USER=I
        IF(UNIT_USER.LE.0)UNIT_USER=USUNIT()
        CALL EZRSET
        FIRST=.FALSE.
      END IF
      TRUNIT=UNIT_USER
  999 RETURN
      END
