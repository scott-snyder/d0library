      FUNCTION QCD_TRIGON()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if any requested QCD trigger bits are on
C-
C-   Returned value  : .TRUE. if any requested trigger bits are on
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   26-JAN-1993   Andrew G. Brandt
C-   Updated   01-NOV-1994   Andrew G. Brandt Mask is found by QCD_BIT_PASSED
C-                           remove JUTL_HEAD since unnecessary
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
C
      INTEGER I, DUMASK
      LOGICAL QCD_TRIGON,QCD_BIT_PASSED
C
C----------------------------------------------------------------------
C
C        Initialize
C
      QCD_TRIGON=.FALSE.
C
C  Loop over triggers to see if any are on
C
      DO I=1,NTRIG
        IF(QCD_BIT_PASSED(QCD_TRIG(I),DUMASK)) THEN
          QCD_TRIGON=.TRUE.
          GO TO 999
        END IF
      END DO
C
  999 RETURN
      END
