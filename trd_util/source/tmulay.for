      SUBROUTINE TMULAY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute multiplicity in TRD cells
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-FEB-1990   a. Zylberstejn
C-   Updated  25-MAR-1990   J.Fr. Glicenstein  Remove all prints (!!)
C-   Updated  10-SEP-1990   A. Zylberstejn  Do the multiplicty analysis only
C-                                          for the selected track
C-   Updated  28-MAY-1991   A. Zylberstejn  :Remove call to GEANT routines
C-   Updated   2-FEB-1993   Alain PLUQUET  Add multiplicities per wire in TPRL
C-   Updated  11-FEB-1993   A. Zylberstejn
C-   Updated  10-MAR-1993   A. Zylberstejn
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTRDT,I,ICH,ID,ITR,IUCOMP,JTR,LOC,LTRDT,LZFIND,NSUM
      INTEGER ICAS,INUM,J,LGTRH,LL,K,LTPRL,M1,M2,LOUT,TRUNIT,NM
      INCLUDE 'D0$INC:tcntrl.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NMUL(6,NTOTTR),FLG(NTOTTR),CM(NBINFO,6,TTRAKG)
      LOGICAL DOPRINT,FIRST,TRD_DO_PRINT
      EQUIVALENCE(IWS,NMUL(1,1)),(IWS(601),FLG(1)),(CM(1,1,1),IWS(801))
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        LOUT=TRUNIT()
        FIRST=.FALSE.
      END IF
      DOPRINT=TRD_DO_PRINT()
      IF(NGOODT.LE.1)GO TO 999
      CALL VZERO(NMUL,6*NGOODT)
      IF(NGOODT.LE.1)GO TO 999
      IF(DOPRINT)THEN
        WRITE(LOUT,*)'DEBUG TMULAY for',NGOODT,' tracks'
        WRITE(LOUT,*)'------------'
        DO ICH=1,6
          WRITE(LOUT,*)' --LAYER---',ICH
          DO ITR=1,NGOODT
            IF(NBHWIR(ICH,ITR).NE.0)THEN
              WRITE(LOUT,*)' TRACK',ITR,' WIRES ',
     &          (WIRNBH(I,ICH,ITR)+ 10000*MULT_CELL(I,ICH,ITR),
     &          I= 1,NBHWIR(ICH,ITR))
            END IF
          END DO
        END DO
      END IF
C copy array MULT on array CM
      DO ITR=1,NGOODT
        DO ICH=1,6
          IF(NBHWIR(ICH,ITR).NE.0)
     +       CALL UCOPY (MULT_CELL(1,ICH,ITR),CM(1,ICH,ITR),
     &       NBHWIR(ICH,ITR))
        END DO
      END DO
      DO 300 ICH=1,6
        DO 200 ITR=1,NGOODT
          IF(NBHWIR(ICH,ITR).LE.0)GO TO 200
 2042     FORMAT(' track',I3,' layer ',I2,' hit wires',I4,'mult',
     &      I8)
          ICAS=0
          CALL VZERO(FLG,NGOODT)
          DO 150  I=1,NBHWIR(ICH,ITR) !Loop on hits of track ITR
C            IF(DOPRINT)
C     +        WRITE(LOUT,*) ' wire nb pour la trace 1',WIRNBH(I,ICH,
C     +        ITR),
C     &        ' mult_cell',MULT_CELL(I,ICH,ITR)
            NM=0
            DO 60 JTR=1,NGOODT    !Loop on tracks i
              IF(ITR.EQ.JTR)GO TO 60
              IF(NBHWIR(ICH,JTR).LE.0)GO TO 60
              INUM=IUCOMP(WIRNBH(I,ICH,ITR),WIRNBH(1,ICH,JTR),
     &            NBHWIR(ICH,JTR))
              IF(INUM.LE.0)GO TO 60
C              M1=MULT_CELL(I,ICH,ITR)
C              M2=MULT_CELL(INUM,ICH,JTR)
              NM=NM+MOD(CM(INUM,ICH,JTR),100)
C              CM(INUM,ICH,JTR)=M1+M2
              IF(FLG(JTR).EQ.0)THEN
                FLG(JTR)=1
                IF(NMUL(ICH,ITR).LT.9) NMUL(ICH,ITR)=NMUL(ICH,ITR)+1
              END IF
   60       CONTINUE
            CM(I,ICH,ITR)=NM*100+ CM(I,ICH,ITR)
  150     CONTINUE ! end of loop on wires of track ITR
  200   CONTINUE
        DO ITR=1,NGOODT
          DO I=1,NBHWIR(ICH,ITR)
            IF(CM(I,ICH,ITR).GT.1)THEN
              CM(I,ICH,ITR)=MOD( CM(I,ICH,ITR),100)+
     &                        CM(I,ICH,ITR)/100
            END IF
          END DO
        END DO
  300 CONTINUE
      IF(DOPRINT)THEN
        DO ICH=1,6
          WRITE(LOUT,*)' --LAYER---',ICH
          DO ITR=1,NGOODT
            WRITE(LOUT,*)' TRACK',ITR,' WIRES ',
     &          (WIRNBH(I,ICH,ITR)+ 10000*CM(I,ICH,ITR)
     &          +  1000*MULT_CELL(I,ICH,ITR),
     &          I= 1,NBHWIR(ICH,ITR)),' NMUL',NMUL(ICH,ITR),
     +          NMUL(1,ITR)+NMUL(2,ITR)*10+NMUL(3,ITR)*100+
     &          NMUL(4,ITR)*1000+NMUL(5,ITR)*10000+NMUL(6,ITR)*100000
          END DO
        END DO
      END IF
      LTRDT=GZTRDT()
      DO WHILE (LTRDT.NE.0)
C        NSUM=0
C      DO 400 ITR=1,NGOODT
C        LOC=LZFIND(IXCOM,LTRDT,ITR,-5)
C        IF(LOC.LE.0)GO TO 400
        ITR=IQ(LTRDT-5)
        NSUM=NMUL(1,ITR)+NMUL(2,ITR)*10+NMUL(3,ITR)*100+
     &       NMUL(4,ITR)*1000+NMUL(5,ITR)*10000+NMUL(6,ITR)*100000
        IQ(LTRDT+2)=NSUM
        DO ICH=1,3
          LTPRL=LQ(LTRDT-ICH)
          IF (LTPRL.GT.0) THEN
            IQ(LTPRL+17)=0
            DO I=1,MIN0(4,NBHWIR(ICH,ITR))
              CALL SBYT(CM(I,ICH,ITR),IQ(LTPRL+17),1+8*(I-1),8)
              IF(DOPRINT)WRITE(LOUT,*)' LAYER',ICH,' WIRE',
     &          WIRNBH(I,ICH,ITR), ' MULT',CM(I,ICH,ITR)
            ENDDO
          ENDIF
        ENDDO
        LTRDT=LQ(LTRDT)
      END DO
  999 CONTINUE
      RETURN
      END
