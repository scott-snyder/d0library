      SUBROUTINE DIFWIRE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make plot of closest cells in different layers
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-MAY-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
C      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC/LIST'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC'
      INTEGER I,I1,I2,IER,J, K,CHA1,INFO,IERR,LVMAX,LOUT,TRUNIT
      INTEGER IZSTAR,NDD,IINF,ISUP,CHAMB,NDECFAD,NTOT,UBIT
      INTEGER NTOTEV,WI,WIRE
      INTEGER ICH,JCH,DIFF,DIFM,DIFM1,TCHNB
      REAL VMIN,VMAX,WG
      LOGICAL FIRST,CATHOD,DOCOR,DOPRINT
      DATA FIRST/.TRUE./
      DATA NTOTEV / 0 /
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
      END IF
      NTOT=0
      DO 120 ICH=1,2
        DO 80 I=1, NWIRE_PER_LAYER(ICH)
          IF(.NOT.TWCOD(TCHNB(I,ICH)))GO TO 80
          DIFM=4000
          DO 60 JCH=ICH+1,3
            DO 40 J=1, NWIRE_PER_LAYER(JCH)
              IF(.NOT.TWCOD(TCHNB(J,JCH)))GO TO 40
              K=J
              IF(JCH.EQ.3)K=(J-1)/2+1
              DIFF=I-K
              IF(IABS(DIFF).LT.IABS(DIFM))DIFM=DIFF
              IF(DIFM.EQ.0)GO TO 50
   40       CONTINUE
   50       CONTINUE
            IF(DIFM.NE.4000)
     +        CALL HF1(FIRSHT+598+ICH+JCH,FLOAT(DIFM),1.) ! diffv
   60     CONTINUE
   80   CONTINUE
  120 CONTINUE
C****
 1999 CONTINUE ! Exit the routine
 1120 FORMAT( ' CHAMBRE',I2, ' ANODES INF',I4,' ANODE SUP',I4,
     &      ' NB OF POINTERS',I3, ' CATH  INF',I4,' CATHODE SUP',
     &      I4,' NB OF POINTERS',I3)
 2009 CONTINUE
      RETURN
 4022 FORMAT(12I8)
      END
