      SUBROUTINE TSETWC_LEV2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set TWCOD(I,J) to .TRUE. if wire nb. I is coded
C-                          in layer J
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: TRD.RCP
C-
C-   Created  10-MAY-1991   A. Zylberstejn
C-   Updated  22-NOV-1991   A. Zylberstejn  Activate the cathodes
C-   Updated  28-JAN-1993   Alain PLUQUET   Change cathode_analysis switch
C-   Updated  29-JUN-1993   A. Zylberstejn  Updated for level2
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS/list'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC/list'
      INCLUDE 'D0$INC:TCNTRL.INC/list'
      INCLUDE 'D0$INC:TRDBGU.INC/list'
C*DC*      INCLUDE 'D0$INC:tr_info_hit.INC/list'
      INCLUDE 'D0$INC:trd_ped.INC/list'
      INCLUDE 'D0$INC:TRWCOD_512.INC/list'
      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER J,CHA1,DIFF,ICH,I,ICHM,IER,IMI,IWI,WIRE,UBIT,NW,NEVOLD
      INTEGER ISUPFLAT
      INTEGER LTHIT,GZTHIT,TDATA(NMFADC+10),TCHNB
      INTEGER I0,I1,LT,SOM,IMS,GZTRDH,LHITS,GZHITS,TWIRCOR
      INTEGER CHAN_ADR(NTOT_WIRE_TRD)
      REAL VMIN,VMAX,BINMIN,PEDES,VSUM
      INTEGER YFADC(NMFADC+10)
      EQUIVALENCE(YFADC(1),TDATA(3))
      INTEGER BINT0(2),BINTIM(2),TTOT,NTTOT,DT1,DT2
      INTEGER TIM0_TREDEP(2),TIM1_TREDEP(2),NTTOT_TREDEP,TTTOT_TREDEP
      INTEGER TIM0_ZDEXP(2),TIM1_ZDEXP(2),NTTOT_ZDEXP,TTTOT_ZDEXP
      LOGICAL PED_SUBSTRACT
      LOGICAL FIRST,TWIRHOT,FILL_HITS,SWAP,CATHODE_ANALYSIS
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL TRD_NWIRE_PER_LAYER
        FIRST=.FALSE.
        FILL_HITS=.FALSE.
        CALL EZPICK('TRD_lev2')
        PED_SUBSTRACT=.FALSE.
        CALL EZGET('PED_SUBSTRACT',PED_SUBSTRACT,IER)
        CALL EZRSET
        ICHM=6
        BINMIN=-10.
        NEVOLD=0
        J=0
        DO ICH=1,ICHM
          DO WIRE=1,NWIRE_PER_LAYER(ICHM)
            J=J+1
            PED_TRD(J)=0.
            IF(PED_SUBSTRACT)THEN
              CALL TRGPED('BID',WIRE,ICHM,PEDES,IER) ! Get pedestals
              IF(IER.NE.0)THEN
                CALL ERRMSG('CANT FIND TRD PEDESTALS','TSETWC',' ','W')
C              PED_TRD(J)=0.
              ELSE
                PED_TRD(J)=PEDES
              END IF
            END IF
          END DO
        END DO
      END IF
C--------------------------------------------------------------------------
C      CALL SYS$GETTIM(BINT0)
C-------------------------------------------------------------------------------

      IF(IQ(LHEAD+9).EQ.NEVOLD)GO TO 999 ! do not call several times for
C                                        ! same event
      NEVOLD=IQ(LHEAD+9)
      J=0
      DO  100 ICH =  1,  ICHM
        IMI=1000
        NW=0
        DO 10 IWI=1,NWIRE_PER_LAYER(ICH)
          IF(J.NE.0 .AND. .NOT.TWCOD(TCHNB(IWI,ICH)))GO TO 10
          J=J+1
          WIRE=IWI
C          CALL SYS$GETTIM(TIM0_ZDEXP)
          NTTOT_ZDEXP=NTTOT_ZDEXP+1
          CALL ZDEXPD_TRD(WIRE,ICH,TDATA)
C          CALL SYS$GETTIM(TIM1_ZDEXP)
C          TTTOT_ZDEXP=TTTOT_ZDEXP+TIM1_ZDEXP(1)-TIM0_ZDEXP(1)
          IF (TDATA(1).LE.0)GO TO 10
C          print*,' in tsetwc, ich,iwi',ich,iwi,' appel a tredep'
C          CALL SYS$GETTIM(TIM0_TREDEP)
          NTTOT_TREDEP=NTTOT_TREDEP+1
          CALL TREDEP_LEV2(IWI,ICH,YFADC)
C          CALL SYS$GETTIM(TIM1_TREDEP)
C          TTTOT_TREDEP=TTTOT_TREDEP+TIM1_TREDEP(1)-TIM0_TREDEP(1)
   10   CONTINUE
  100 CONTINUE
C      WRITE(LOUT,*)' temps moyen dans zdexpd',FLOAT(TTTOT_ZDEXP)/
C     &  FLOAT(NTTOT_ZDEXP)
C      WRITE(LOUT,*)' temps moyen dans tredep',FLOAT(TTTOT_TREDEP)/
C     &  FLOAT(NTTOT_TREDEP)
      CALL REDUCE_THIT_LEV2
  999 CONTINUE
      NTTOT=NTTOT+1
C      WRITE(LOUT,*)' dt1',DT1,' temps total',1.E-07*TTOT,' pour ',
C     &  NTTOT,' temps moyen(sec)',1.E-07*FLOAT(TTOT)/FLOAT(NTTOT)
C      WRITE(LOUT,*),' temps moyen par evt dans zdexpd',
C     +1.E-07*TTTOT_ZDEXP/FLOAT(NTTOT),
C     +' temps moyen par evt dans tredep',
C     +1.E-07*TTTOT_TREDEP/FLOAT(NTTOT)
      RETURN
      END
