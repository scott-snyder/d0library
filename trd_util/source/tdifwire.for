      SUBROUTINE TDIFWIRE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plots of the difference between the wire numbers in
C-   two consecutive cells of the TRD
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
c      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC/LIST'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC'
      INTEGER I,I1,I2,IER,J, K,CHA1,INFO,IERR,LVMAX,LOUT,TRUNIT
      INTEGER IZSTAR,NDD,IINF,ISUP,CHAMB,NDECFAD,NTOT,UBIT
      INTEGER NTOTEV,IKK1,IKK2,IBCL2,IBCLP,WI,WIRE,NW(3)
      INTEGER ICH,IMIN,IMOT,NG(3),DIFF,DIFM,DIFM1,tchnb
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
      DO 120 ICH=1,3
        NW(ICH)=0
        CHAMB=ICH
        DO 50 I=1, nwire_per_layer(ich)
          IF(.NOT.TWCOD(TCHNB(I,ICH)))GO TO 50
          NW(ICH)=NW(ICH)+1
          NTOT=NTOT+1
          IWS(NTOT)=I
          DIFM=1000
          IF(ICH.GE.2)THEN
            DO J=1,NW(1)
              DIFF=I-IWS(J)
              IF(IABS(DIFF).LT.IABS(DIFM))DIFM=DIFF
              IF(DIFM.EQ.0)GO TO 30
            END DO
   30       CONTINUE
            CALL HF1(FIRSHT+600+ICH-1,FLOAT(DIFM),1.) ! diff 2-1 and 3-1
            IF(ICH.EQ.3)THEN
              DIFM=1000
              DO J=1,NW(2)
                DIFF=I-IWS(J+NW(1))
                IF(IABS(DIFF).LT.IABS(DIFM))DIFM=DIFF
                IF(DIFM.EQ.0)GO TO 40
              END DO
   40         CONTINUE
              CALL HF1(FIRSHT+603,FLOAT(DIFM),1.) ! diff 3-2
            END IF
          END IF
   50   CONTINUE
  120 CONTINUE
C****
 1999 CONTINUE ! Exit the routine
 1120 FORMAT( ' CHAMBRE',I2, ' ANODES INF',I4,' ANODE SUP',I4,
     &      ' NB OF POINTERS',I3, ' CATH  INF',I4,' CATHODE SUP',
     &      I4,' NB OF POINTERS',I3)
 2009 CONTINUE
      RETURN
 4022 FORMAT(12I8)
 1000 FORMAT(' CHANNEL LENGTH',I4,' CHANNNEL ADDRESS',I8,
     +    ' NUMBER OF DATA WORDS ',I8)
 1010 FORMAT(' SUB-ADRESS',I3,' MODULE',I3,' CRATE',I2,' TRIPLET',I2,
     +' LAYER',I2,' WIRE ',I4)
 1011 FORMAT(' SUB-ADRESS',I3,' MODULE',I3,' CRATE',I2,' TRIPLET',I2,
     +' LAYER',I2,' STRIP',I4)
      END
