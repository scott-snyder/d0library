      SUBROUTINE INZCOM(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       Initialize ZEBCOM (event data Zebra common)
C-
C-   Inputs  : I = 1 data in division 1, otherwise in division 2
C-   Outputs : NONE
C-
C-   Created  28-OCT-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$XFRAME$SOURCE:d0map.inc'
      INTEGER I
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
C
C **** Initialize store in /ZEBCOM/ common (store 0)
C
        IXCOM=0
        CALL MZSTOR (IXCOM,'/ZEBCOM/','Q',FENCE,LHEAD,LREF(1),ZSTOR(1),
     &   ZSTOR(40000),ENDZS)
C
C **** Use division IXMAIN for event data
C
        IXMAIN=IXCOM+2
        IF(I.EQ.1) IXMAIN=IXCOM+1
C
C **** Create a division for run header (3rd division)
C
        CALL MZDIV(IXCOM,IXDVR,'RUN DIV',100,40000,'L')
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
