      SUBROUTINE TREDEP_LEV2 (WIRE,LAYER,YFADC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyse energy deposit for all the coded
C-                         TRD wires. Start to fill bank THIT
C-
C-   Inputs  :  Wire=wire number
C-              LAYER =layer number
C-              Yfadc=FADC
C-              NDAT= Number of FADC bins
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAY-1991   A. Zylberstejn
C-   Updated  26-JUL-1991   A. Zylberstejn  : Create and fill bank THIT
C-   Updated  29-JUN-1993   A. Zylberstejn  Updated for level2
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUREC.INC'
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:trd_ped.INC'
      INTEGER CHA1,LAYER,ISUPFLAT,I,IERR,IWI,WIRE,UBIT,NW,NEVOLD
C     INTEGER LOUT,TRUNIT
      INTEGER ICL,II,IR,ND,NDAT,JBYT,JJ,NDRAW,IAUX(4)
      INTEGER I0,I1,JR,LT,SOM,IMS,GZTRDH,SPED,TMINC,VERSION
      INTEGER GZTHIT,LSH,LTHIT,NHIT,NTOT,TCHNB
      INTEGER CLUS_PER_WORD,ENTOT,NB1,NB2,NBIT_WIRE,NBIT_CLUS,EMAXC
      INTEGER IER,YFADC(NMFADC+10),IFOIS
      LOGICAL FIRST,DOPRINT,DO_DRAW,DO_HISTO,DOCLUS
      LOGICAL PED_SUBSTRACT,DO_CLUSTER
      DATA FIRST/.TRUE./
      DATA IFOIS/0/
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
C        LOUT=TRUNIT()
        NDAT=NMFADC
        DOPRINT=.FALSE.
        NEVOLD=-20
        NDRAW=0
        VERSION=1
        IF(NWIRE_PER_LAYER(3).EQ.512)VERSION=2
        NBIT_WIRE=7+VERSION
        NB1=NBIT_WIRE+2
        NB2=NB1+2
        CLUS_PER_WORD=3
        NBIT_CLUS=32/CLUS_PER_WORD
        EMAXC=2**NBIT_CLUS-1
        CALL EZPICK('TRD_LEV2')
        PED_SUBSTRACT=.FALSE.
        CALL EZGET('PED_SUBSTRACT',PED_SUBSTRACT,IER)
        DO_CLUSTER=.TRUE.
        CALL EZGET('DO_CLUSTER',DO_CLUSTER,IER)
        CALL EZRSET
        SPED=0
      END IF
      IFOIS=IFOIS+1
      DOPRINT=.FALSE.
      NEVOLD=IQ(LHEAD+9)
      IF(LTHIT.EQ.0)THEN
        NHIT=0
        CALL BKTHIT(LTHIT)   !Book bank LTHIT
        IQ(LTHIT+1) = VERSION        ! Bank version
        IF(LTHIT.LE.0)THEN
          CALL ERRMSG('Cant book bank THIT ','TREDEP',' ','W')
          GO TO 999
        END IF
        NTOT=2
      END IF
      NW=0
      SOM=0
      LTHIT=GZTHIT()
      IF(LTHIT.LE.0)THEN
        CALL ERRMSG('Cant find THIT ','TREDEP',' ','W')
        GO TO 999
      END IF
      NHIT=NHIT+1
      IQ(LTHIT+2)=NHIT
      LSH=LTHIT+NTOT
      ENTOT=0.
      DO I=1,128
        ENTOT=ENTOT+YFADC(I)
      END DO
      NCREC=0
      IF(DO_CLUSTER)
     +CALL CLUSTF_LEV2(WIRE,LAYER,YFADC,NDAT) ! Reconstruct clusters
      CHA1=LAYER
      IF(LAYER.GT.3)THEN
        CHA1=LAYER-3
        IQ(LSH+1)=1         !Encode anode/cathode
      END IF
      CALL SBYT(WIRE-1,IQ(LSH+1), 2,NBIT_WIRE) !Encode wire number (-1)
      CALL SBYT(CHA1-1,IQ(LSH+1),NB1,2)        !Encode layer number (-1)
      CALL SBYT(NCREC ,IQ(LSH+1),NB2,3)        !Encode nb. of clusters
      II=IQ(LSH+1)
      IF(PED_SUBSTRACT)
     +     SPED=128*PED_TRD(TCHNB(WIRE,LAYER))+.5   ! Sum of peds
      IQ(LSH+2)= ENTOT-SPED+.5               !Etot peds non subtract.
      NTOT=NTOT+2
      IF(NCREC.NE.0)THEN
        IF (NTOT+NCREC+2.GE.IQ(LTHIT-1))THEN!Is there is enough room?
C        print*,' avant First push of THIT,ntot+ncrec,nd',
C     &    NTOT+ncrec,IQ(LTHIT-1),' ncrec',ncrec
C          IF(DOPRINT)WRITE(LOUT,*)' First push of THIT,ntot+ncrec,nd',
C     &      NTOT,IQ(LTHIT-1)
          CALL MZPUSH(IXCOM,LTHIT,0,1000,' ')
C        print*,' apres push of THIT,ntot,nd',
C     &    NTOT,IQ(LTHIT-1),' ncrec',ncrec
        END IF
        JJ=3
        JR=-1
        DO IR =  1, NCREC  !       Encode clusters position and energy
C          IF(DOPRINT)write(lout,*)' in TREDEP, cluster',IR,' position',
C     &      IBCEN(IR),' energy',ECLR(IR)
C          print*,' avant sbyt dans tredep,ir,jr,jj',ir,jr,jj
C
C  old encoding
C          JR=JR+1
C          IF(JR.GE.CLUS_PER_WORD) THEN
C            JR=0
C            JJ=JJ+2
C          END IF
C          CALL SBYT(IBCEN(IR),IQ(LSH+JJ),JR*8+1,8)!position
C          II= MIN0(EMAXC,INT(ECLR(IR)+.5))
C          CALL SBYT(II,IQ(LSH+JJ+1),JR*NBIT_CLUS+1,8)     ! energy
C          JJ=JJ+1
C
C  New encoding
C  ------------
C          packed word: bit 1-->13  E clust
C                           14,15   Spares
C                           16-->22 left position of clust. (7 bits)
C                           23-->29 right ""      ""  ""
          II=ECLR(IR)+.5
          CALL SBYT(II,IQ(LSH+JJ),1,13)     ! Energy
          CALL SBYT( IBLFT(IR),IQ(LSH+JJ),16,7)! left side of clust
          CALL SBYT(IBRGHT(IR),IQ(LSH+JJ),23,7)! right side of clust
          NTOT=NTOT+1
        END DO
C        IF(DOPRINT)
C     +       write(lout,*)'After decoding of the cluster banks',
C     &        (JBYT(IQ(LSH+3),(IR-1)*8+1,8),FLOAT(JBYT(IQ(LSH+4),
C     &      (IR-1)*8+1,8)),IR=1,MIN0(4,NCREC))
      END IF
C      PRINT*,'ncrec',NCREC,'ntot',NTOT,' nd',IQ(LTHIT-1)
C***
   10 CONTINUE
      GO TO 999
      ENTRY REDUCE_THIT_LEV2
      LTHIT=GZTHIT()
      IF(LTHIT.LE.0)GO TO 999
C
C      IF(DOPRINT)
C     +  WRITE(LOUT,*)' In TREDEP before final push ,ntot',
C     &  NTOT,'nd',IQ(LTHIT-1)
      IF (NTOT.LT.IQ(LTHIT-1))
     +  CALL MZPUSH(IXCOM,LTHIT,0,NTOT-IQ(LTHIT-1),'R')
C      lthit=gzthit()
C        CALL DZSHOW(' In tredep apres push',IXMAIN,Lthit,'SLV',
C     &    0,-1,0,-1)
      NTOT=0
  999 CONTINUE
C      print*,' exit tredep'
      RETURN
      END
