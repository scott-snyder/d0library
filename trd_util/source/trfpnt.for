      SUBROUTINE  TRFPNT(ITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Fill bank LTLYR (under BKTRDH-under RECO)
C-   Compute pointers in CDD4 bank for the TRD fadc
C-
C-   Inputs  : Quantitites in common /TRINTR/
C-            ANOINF(1,I)= min. anode number for track I
C-                  (2,I)= max.  ""     ""
C-            CATINF(1,I)= min  cathode "     "    "   "
C-                  (2,I)= max.   "     "
C-            NGOODT     = nb. of "good" tracks
C-
C-   Outputs : Quantities in common /TRTOBN/
C-            TLPINF(K,J,I)= pointer in bank LTLYR for K th hit wire
C-     for track I in layer J, j=1-->3 for anodes ,4-->6 for cathodes
C-
C-   Controls:
C-
C-   Created  26-APR-1989   A. Zylberstejn
C-   Updated   8-DEC-1989   A. Zylberstejn  Modify the call to MZLINT
C-   Updated  19-JAN-1990   J.Fr. Glicenstein  : Call to ZDEXPD
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADCCN.INC/LIST'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:TRDBGU.INC/LIST'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRINTR.INC/LIST'
      INCLUDE 'D0$INC:TRTOBN.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER IREG0,ITR,UBIT,USUNIT
      INTEGER I,J, K,CHA1
      INTEGER IZSTAR,NDD,IINF,ISUP,LAYER,NTOTWR
      INTEGER NTOTEV,LOUT,IKK1,IKK2,IBCL2,IBCLP,IBCLD,NWC(256),NW
      INTEGER ICH,TDATA(NMFADC+10)
      REAL FTDATA(NMFADC+10)
      EQUIVALENCE(FTDATA(1),TDATA(1))
      LOGICAL FIRST,CATHOD
      DATA FIRST/.TRUE./
      DATA NTOTEV / 0 /
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=LUDEBG
      END IF
****
      CALL VZERO(NBPNT(1,ITR),6)
      NTOTWR = 256
      IF ((SWTDBG.EQ.1).AND.(TNEVDG.GT.0).AND.(TNTRDG.LE.ITR)) THEN
        IF(ITR.EQ.1)THEN
          DO 12 ICH=1,6
            NW=0
            DO 10 I=1, nwire_per_layer(ich)
C             write(ludebg,*)'before TCODER nw,nwc',nw,(nwc(k),k=1,nw)
              CALL TCODER(CHA1,ICH-1,I-1,UBIT,2)
C             write(ludebg,*)'after TCODER nw,nwc',nw,(nwc(k),k=1,nw)
              CALL ZDEXPD(4,CHA1,TDATA)
C             write(ludebg,*)'After ZDEXPD nw,nwc',nw,(nwc(k),k=1,nw)
              IF (TDATA(1).EQ.0)GO TO 10
              NW=NW+1
              NWC(NW)=I
   10       CONTINUE
            WRITE(LUDEBG,*)' layer',ICH,' nb. de voies codes',NW
            IF(NW.LE.0)GO TO 12
            WRITE(LUDEBG,'(10i4)')(NWC(I),I=1,NW)
   12     CONTINUE
        END IF
      END IF
****
      DO 60 LAYER = 1,6 ! Loop on layers
        IF(LAYER.LE.3)THEN  !Anodes
          CATHOD=.FALSE.
          ICH = LAYER
          IINF=ANOINF(ICH,ITR)
          ISUP=ANOSUP(ICH,ITR)
        ELSE                 !Cathodes
          ICH=LAYER-3
          CATHOD=.TRUE.
          IINF=CATINF(ICH,ITR)
          ISUP=CATSUP(ICH,ITR)
        END IF
        IREG0=0
        IF(IABS(IINF-ISUP).GT.10)IREG0=1
        IF (IREG0.EQ.0) THEN
          IKK1 = MIN0(IINF,ISUP)
          IKK2 = MAX0(IINF,ISUP)
        ELSE
          IKK1 = max0(iinf,isup)
          IKK2 = min0(iinf,isup)+ntotwr
        ENDIF
        NTOTWR = nwire_per_layer(layer)
        DO 20 IBCL2 = IKK1,IKK2
          IBCLP = LAYER
          IBCLD = IBCL2
          if(ibcld.gt.ntotwr)ibcld=ibcld-ntotwr
          CALL TCODER(CHA1,IBCLP-1,IBCLD-1,UBIT,2)
          CALL ZDEXPD(4,CHA1,TDATA)
          IF (TDATA(1).EQ.0) GOTO 20       ! Wire not coded
          CALL BKTLYR(IZSTAR,ICH)
          NBPNT(LAYER,ITR)=NBPNT(LAYER,ITR)+1
          TLPINF(NBPNT(LAYER,ITR),LAYER,ITR)=IZSTAR
          IF ((SWTDBG.EQ.1).AND.(TNEVDG.GT.0).AND.(TNTRDG.LE.ITR))
     +  WRITE(LUDEBG,*)' LAYER,ITR,NBPNT',LAYER,ITR,
     +  NBPNT(LAYER,ITR),' coded channel',IBCLD
          NDD=IQ(IZSTAR-1)
          IQ(IZSTAR+NDD)=IBCLD+1000*LAYER
          IQ(IZSTAR+NDD-1)=ITR
          CALL UCOPY(FTDATA(3),IQ(IZSTAR+1),NFADC)
   20   CONTINUE
   60 CONTINUE
      NTOTEV = NTOTEV + 1
 1082 CONTINUE
 1100 CONTINUE
 1280 CONTINUE
 1999 CONTINUE
      IF ((SWTDBG.EQ.1).AND.(TNEVDG.GT.0).AND.(TNTRDG.LE.ITR)) THEN
      DO 2009 ICH=1,3
        WRITE(LUDEBG,*)' chambre',ICH
        WRITE(LUDEBG,*)' anodes inf',ANOINF(ICH,ITR),' anode sup',
     &      ANOSUP(ICH,ITR),' nb of pointers',NBPNT(ICH,ITR)
        WRITE(LUDEBG,*)' chambre',ICH+3
        WRITE(LUDEBG,*)' cath  inf',CATINF(ICH,ITR),' cathode sup',
     &      CATSUP(ICH,ITR),' nb of pointers',NBPNT(ICH+3,ITR)
 2009 CONTINUE
      ENDIF
      RETURN
 4022 FORMAT(12I8)
 1000 FORMAT(' CHANNEL LENGTH',I4,' CHANNNEL ADDRESS',I8,
     +    ' NUMBER OF DATA WORDS ',I8)
 1010 FORMAT(' SUB-ADRESS',I3,' MODULE',I3,' CRATE',I2,' TRIPLET',I2,
     +' LAYER',I2,' WIRE ',I4)
 1011 FORMAT(' SUB-ADRESS',I3,' MODULE',I3,' CRATE',I2,' TRIPLET',I2,
     +' LAYER',I2,' STRIP',I4)
      END
