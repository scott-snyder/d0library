      SUBROUTINE SUPER_GSWT(IFFRD,ISAVE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decides whether an FFREAD switch
C-                         Can over-ride a Saved Switch. If it can
C-                         IFFRD is left as is. If not IFFRD is
C-                         replaced by ISAVE
C-
C-   Inputs  : IFFRD Switch specified in FFREAD cards.
C-             Same switch saved using GSAVE
C-   Outputs : IFFRD
C-   Controls:
C-
C-   Created  15-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IFFRD,ISAVE
      INTEGER IALLOW(0:4,0:4)
      DATA IALLOW/25*0/
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  IALLOW(IFFRD,ISAVE) = 0 IF IFFRD SWITCH CANNOT OVER-RIDE ISAVE SWITCH
C ****  =1 IF IT CAN.
C
        IALLOW(0,0) = 1
        IALLOW(4,0) = 1    !Permit Analysis even if Detector not there!
C
        IALLOW(1,1) = 1
        IALLOW(4,1) = 1
C
        IALLOW(1,2) = 1
        IALLOW(2,2) = 1
C
        IALLOW(1,3) = 1
        IALLOW(2,3) = 1
        IALLOW(3,3) = 1
        IALLOW(4,3) = 1
C
        IALLOW(1,4) = 1
        IALLOW(2,4) = 1
        IALLOW(3,4) = 1
        IALLOW(4,4) = 1
C
      ENDIF
      IF(IALLOW(IFFRD,ISAVE).EQ.0)THEN
        IFFRD = ISAVE
C
C ****   This IFFRD is not allowed. So gets replaced by ISAVE
C
      ENDIF
  999 RETURN
      END
