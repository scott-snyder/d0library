      SUBROUTINE THOTWIRE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hbook for TRD hot wires (not on a track)
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
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC/LIST'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC/LIST'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INTEGER IER,IFOIS,ITR,IUCOMP,UBIT
      INTEGER I,J, LOUT,TRUNIT
      INTEGER CHAMB, WI,WIRE ,ICH,tchnb
      LOGICAL DOPRINT,FIRST
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        IFOIS=0
        LOUT=TRUNIT()
        CALL EZPICK('trd_rcp')
        DOPRINT=SWTDBG.EQ.1
        CALL EZRSET
      END IF
****
      DO 120 ICH=1,3
        CHAMB=ICH
        DO 100 I=1, nwire_per_layer(ich)
          WI=I
          WIRE=I
          IF (CHAMB.EQ.3) THEN
            IF (WIRE.GE.48.AND.WIRE.LE.55) WI = WIRE + 8
            IF (WIRE.GE.56.AND.WIRE.LE.63) WI = WIRE - 8
          ENDIF
          IF(.NOT.TWCOD(TCHNB(WIRE,CHAMB)))GO TO 100
          DO 50 ITR=1,NGOODT  !loop on tracks
            IF(NBHWIR(CHAMB,ITR).GE.1)THEN
              IF(IUCOMP(I,WIRNBH(1,ICH,ITR),NBHWIR(CHAMB,ITR)).NE.0)
     &                                            GO TO 100
C            DO 20 IBCL2 = 1,NBHWIR(CHAMB,ITR)
C              WIRE = WIRNBH(IBCL2,ICH,ITR)
C              IF(I.EQ.WIRE)GO TO 100
C   20       CONTINUE
            END IF
   50     CONTINUE
          CALL HF1(FIRSHT+590+CHAMB,FLOAT(I),1.)
  100   CONTINUE
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
