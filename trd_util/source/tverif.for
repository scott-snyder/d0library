      SUBROUTINE TVERIF(GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if same event comes back.Make the sum for the
C-   first FADC TRD channel and checks if it has changed copared to the
C-   previous event
C-
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created 9-MAY-1991   A. Zylberstejn
C-   Updated  20-SEP-1993   A. Zylberstejn  Updated for 512 wires in chamber 3
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:fadccn.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INTEGER LOUT,TRUNIT
      INTEGER I,ICH,IWI,CHA1,UBIT,TDATA(NMFADC+10)
      EQUIVALENCE ( TDATA(1),IWS(1)  )
      REAL SUM,SUM0,TRGTIM,VSUMI,VSUM
      LOGICAL FIRST,GOOD
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        SUM0=0
        IWI=0
      END IF
      GOOD=.TRUE.
      DO 12 ICH=1,6
        DO 10 I=1,nwire_per_layer(ich)
          CALL TCODER(CHA1,ICH-1,I-1,UBIT,2)
          CALL ZDEXPD(4,CHA1,TDATA)
          IF (TDATA(1).EQ.0)GO TO 10
          CALL VFLOAT(TDATA(3),WS(1001),TDATA(1)) ! transform integer into real
          SUM=VSUM(WS(1001),TDATA(1))
          IF(SUM.EQ.SUM0)GOOD=.FALSE.
          SUM0=SUM
          GO TO 999
   10   CONTINUE
   12 CONTINUE
  999 RETURN
      END
