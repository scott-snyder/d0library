      SUBROUTINE TRDPAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Try to associate 3 TRD layers
C-
C-   Inputs  : Table of coded wires in common block TRWCOD_512
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAY-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INTEGER I,ICH,ID1,ID2,ID3,J,K,NTOT,NGD(3,128),NTRIP,NW(3)
      INTEGER II,IS,ICI,ICS,ISTART,IUCOMP,IW1,IW2,IW3
      INTEGER JI,JS,IAUX(800),MCH(3),IW3I,IW3S,IW3C
      INTEGER LOUT,TRUNIT,TCHNB
      EQUIVALENCE (IAUX,IWS(1001)),(NGD(1,1),IWS(2001))
      LOGICAL DOPRINT
C----------------------------------------------------------------------
      LOUT=TRUNIT()
      DOPRINT=SWTDBG.EQ.1 .AND. TNEVDG.GT.0 .AND. LOUT.NE.0
C  Put coded wires in a continuous array
      NTOT=0
      DO 10 ICH =  1,  3
        NW(ICH)=0
        DO I=1, NWIRE_PER_LAYER(ICH)
          IF(TWCOD(TCHNB(I,ICH)))THEN
            NTOT=NTOT+1
            NW(ICH)=NW(ICH)+1
            IWS(NTOT)=I
            IAUX(NTOT)=0
          END IF
        END DO
   10 CONTINUE
      NTRIP=0
      IF(NW(1)*NW(2)*NW(3).LE.0)GO TO 400
C      IF(DOPRINT)THEN
C        WRITE(LOUT,*)' In TRDPAT, fils codes layer 1',NW(1)
C        WRITE(LOUT,'(20i4)')(IWS(I),I=1,NW(1))
C        WRITE(LOUT,*)' In TRDPAT, fils codes layer 2',NW(2)
C        WRITE(LOUT,'(20i4)')(IWS(I+NW(1)),I=1,NW(2))
C        WRITE(LOUT,*)' In TRDPAT, fils codes layer 3',NW(3)
C        WRITE(LOUT,'(20i4)')(IWS(I+NW(1)+NW(2)),I=1,NW(3))
C      END IF
C  First look for aligned triplets
      DO 40 I=1,NW(1)
        ID2=IUCOMP(IWS(I),IWS(1+NW(1)),NW(2))
        IF(ID2.EQ.0)GO TO 40
        ID3=IUCOMP(IWS(I),IWS(1+NW(1)+NW(2)),NW(3))
        IF(ID3.EQ.0)GO TO 40
        IW1=ID3
        NTRIP=NTRIP+1
        NGD(1,NTRIP)=IWS(I)
        NGD(2,NTRIP)=IWS(I)
        NGD(3,NTRIP)=IWS(I)
C        IF(DOPRINT)
C     +   WRITE(LOUT,*)' triplet num.',NTRIP,' fil  commun',NGD(1,NTRIP),
C     &    'id1,id2,id3',ID1,ID2,ID3
C  Set IAUX to 1 for wire and adjacent ones
        IAUX(I)=1
        IAUX(ID2+NW(1))=1
        IAUX(ID3+NW(1)+NW(2))=1
        IF(I.NE.  1 .AND. IWS(I-1).EQ.IW1-1)IAUX(I-1)=1
        IF(I.NE.NWIRE_PER_LAYER(ICH) .AND. IWS(I+1).EQ.IW1+1)IAUX(I+1)=1
        IF(ID2.NE.  1.AND.IWS(ID2+NW(1)-1).EQ.IW1-1)IAUX(ID2+NW(1)-1)=1
        IF(ID2.NE.NWIRE_PER_LAYER(ICH).AND.IWS(ID2+NW(1)+1).EQ.IW1+1)
     &    IAUX(ID2+NW(1)+1)=1
        IF(ID3.NE.  1 .AND.  IWS(ID3+NW(1)+NW(2)-1).EQ.IW1-1)
     &                               IAUX(ID3+NW(1)+NW(2)-1)=1
        IF(ID3.NE.NWIRE_PER_LAYER(ICH) .AND.  IWS(ID3+NW(1)+NW(2)+1).EQ.
     &    IW1+1)
     &                               IAUX(ID3+NW(1)+NW(2)+1)=1
   40 CONTINUE
C  discard wires already used (compact array)
C      write(lout,*)' ntrip alignes',ntrip
      IF(NTRIP.LE.0)GO TO 49
      ISTART=0
      NTOT=0
      DO 48 ICH=1,3
        MCH(ICH)=0
        DO 46 I=1,NW(ICH)
C          IF(DOPRINT)
C     +     WRITE(LOUT,*)' dans compactage, layer',ICH,' i+istart',I+
C     +     ISTART,' fil',IWS(I+ISTART),' iaux',IAUX(I+ISTART)
          IF(IAUX(I+ISTART).NE.0)GO TO 46
          NTOT=NTOT+1
          IWS(NTOT)=IWS(I+ISTART)
          MCH(ICH)=MCH(ICH)+1
   46   CONTINUE
        ISTART=ISTART+NW(ICH)
        NW(ICH)=MCH(ICH)
   48 CONTINUE
C      IF(DOPRINT)THEN
C        WRITE(LOUT,*)' apres compactage'
C        WRITE(LOUT,*)' In TRDPAT, fils codes layer 1 ',NW(1)
C        WRITE(LOUT,'(20i4)')(IWS(I),I=1,NW(1))
C        WRITE(LOUT,*)' In TRDPAT, fils codes layer 2',NW(2)
C        WRITE(LOUT,'(20i4)')(IWS(I+NW(1)),I=1,NW(2))
C        WRITE(LOUT,*)' In TRDPAT, fils codes layer 3',NW(3)
C        WRITE(LOUT,'(20i4)')(IWS(I+NW(1)+NW(2)),I=1,NW(3))
C      END IF
   49 CONTINUE
C  look for configurations where one wire is displaced by 1 cell
      IF(NW(1)*NW(2)*NW(3).LE.0)GO TO 400
      DO 100 I=1,NW(1)
        IW1=IWS(I)
        II=IWS(I)-1
        IF(II.LE.0)II=NWIRE_PER_LAYER(ICH)
        IS=IWS(I)+1
        IF(IS.GE.257)IS=1
        DO 80 J=1,NW(2)
          IW2=IWS(J+NW(1))
          IF(IABS(IW2-IW1).GT.1)GO TO 80
          JI=IW2-1
          IF(JI.LE.0)JI=NWIRE_PER_LAYER(ICH)
          JS=IW2+1
          IF(JS.GE.257)JS=1
          IW3I=IUCOMP(JI ,IWS(1+NW(1)+NW(2)),NW(3))
          IW3S=IUCOMP(JS ,IWS(1+NW(1)+NW(2)),NW(3))
          IW3C=IUCOMP(IW2,IWS(1+NW(1)+NW(2)),NW(3))
C          IF(DOPRINT .AND. IW3I+IW3S+IW3C.LE.0)THEN
C            WRITE(LOUT,*)' i,iw1',I,IW1,' ii,is',II,IS
C            WRITE(LOUT,*)' j,iw2',J,IW2,' ji,js',JI,JS
C            WRITE(LOUT,*)' iw3i,iw3s,iw3c',IW3I,IW3S,IW3C
C          END IF
          IF(IW2.EQ.IW1)THEN! layer2=layer 1
C  Layer2=layer1=layer3-1 ou layer2=layer1=layer3+1
            IF(IW3I+IW3S.LE.0)GO TO 80
            IW3=0
            IF(IW3I.NE.0)IW3=IWS(IW3I+NW(1)+NW(2))
C            IF(DOPRINT
C     +   .AND.IW3.NE.0)WRITE(LOUT,*)' Good triplet 1',IW1,IW2,IW3
            IF(IW3S.NE.0)IW3=IWS(IW3S+NW(1)+NW(2))
C            IF(DOPRINT
C     +   .AND. IW3S.NE.0) WRITE(LOUT,*)' Good triplet 1',IW1,IW2,IW3
          ELSE
C layer2 =layer-1 or layer2 =layer+1
            IF(IW3I+IW3S+IW3C.LE.0)GO TO 80
            IW3=0
            IF(IW3I.NE.0)IW3=IWS(IW3I+NW(1)+NW(2))
C            IF(DOPRINT
C     +   .AND. IW3I.NE.0) WRITE(LOUT,*)' Good triplet 2',IW1,IW2,IW3
            IF(IW3S.NE.0)IW3=IWS(IW3S+NW(1)+NW(2))
C            IF(DOPRINT
C     +   .AND. IW3S.NE.0) WRITE(LOUT,*)' Good triplet 2',IW1,IW2,IW3
            IF(IW3C.NE.0)IW3=IWS(IW3C+NW(1)+NW(2))
C            IF(DOPRINT
C     +   .AND. IW3C.NE.0) WRITE(LOUT,*)' Good triplet 2',IW1,IW2,IW3
          END IF
          NTRIP=NTRIP+1
          NGD(1,NTRIP)=IW1
          NGD(2,NTRIP)=IW2
          NGD(3,NTRIP)=IW3
   80   CONTINUE
  100 CONTINUE
  400 CONTINUE
      IF(DOPRINT)THEN
        WRITE(LOUT,*)' In  trdpat, ntrip',NTRIP
        IF(NTRIP.NE.0)THEN
          DO 410 I =  1,  NTRIP
            WRITE(LOUT,'(4i4)')I,NGD(1,I),NGD(2,I),NGD(3,I)
  410     CONTINUE
        END IF
      END IF
  999 RETURN
      END
