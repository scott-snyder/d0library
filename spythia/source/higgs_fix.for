      SUBROUTINE HIGGS_FIX
      implicit none
      include 'D0$SPYTHIA$INC:LUDAT1.INC'
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:LUDAT3.INC'
      include 'D0$SPYTHIA$INC:LUDAT4.INC'
      include 'D0$SPYTHIA$INC:PYPARS.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
      include 'D0$SPYTHIA$INC:SPYTHIA.INC'
C.....
      integer I,J,J1,J2,I1,I2
      integer IHCHECK
      integer KC,LKNT,IDLAM(100,3),IDLAM0(100,3),LKNT0
      real XLAM(0:100),XLAM0(0:100),XALL
C.....
C.....PUT IN A NEW SUBROUTINE
C.....NOW, 'FIX UP' higgs information
      DO I=1,3
       PMAS(72,I)=PMAS(25,I)
       MDCY(72,I)=MDCY(25,I)
      ENDDO
      CHAF(72)=SCHAF(72)
      DO J1=1,3
       KCHG(72,J1) = KCHG(25,j1)
      ENDDO
      MDCY(72,2)=MDCY(71,2)+MDCY(71,3)
      DO J1=0,100
       XLAM0(J1)=0.0
      ENDDO
      LKNT0=MDCY(72,3)
      DO J1=1,MDCY(72,3)
       J2=MDCY(72,2)+J1-1
       I1=MDCY(25,2)+J1-1
       XLAM0(J1)=BRAT(I1)*PMAS(72,2)
       XLAM0(0)=XLAM0(0)+XLAM0(J1)
       MDME(J2,1)=MDME(I1,1)
       MDME(J2,2)=MDME(I1,2)
       DO I2=1,3
        IDLAM0(J1,I2)=IHCHECK(KFDP(I1,I2))
       ENDDO
      ENDDO
      LKNT=0
      call hextra(72,XLAM,IDLAM,LKNT)
      IF(LKNT.ne.0) THEN
       DO I1=1,LKNT
        XLAM0(LKNT0+I1)=XLAM(I1)
        XLAM0(0)=XLAM0(0)+XLAM(I1)
        DO I2=1,3
         IDLAM0(LKNT0+I1,I2)=IDLAM(I1,I2)
        ENDDO
       ENDDO
      ENDIF
      MDCY(72,3)=LKNT+LKNT0
      DO J1=1,MDCY(72,3)
       J2=MDCY(72,2)+J1-1
       IF(J1.GT.LKNT0) THEN
        MDME(J2,1)=1
        MDME(J2,2)=0
       ENDIF
       BRAT(J2) = XLAM0(J1)/XLAM0(0)
       DO I1=1,3
        KFDP(J2,I1)=IDLAM0(J1,I1)
       ENDDO
      ENDDO
C
      PMAS(72,2)=XLAM0(0)
      PMAS(72,3)=10.*XLAM0(0)
C
      DO I=1,3
       PMAS(73,I)=PMAS(35,I)
       MDCY(73,I)=MDCY(35,I)
      ENDDO
      CHAF(73)=SCHAF(73)
      DO J1=1,3
       KCHG(73,J1) = KCHG(35,j1)
      ENDDO
      MDCY(73,2)=MDCY(72,2)+MDCY(72,3)
      DO J1=0,100
       XLAM0(J1)=0.0
      ENDDO
      LKNT0=MDCY(73,3)
      DO J1=1,MDCY(73,3)
       J2=MDCY(73,2)+J1-1
       I1=MDCY(35,2)+J1-1
       XLAM0(J1)=BRAT(I1)*PMAS(73,2)
       XLAM0(0)=XLAM0(0)+XLAM0(J1)
       MDME(J2,1)=MDME(I1,1)
       MDME(J2,2)=MDME(I1,2)
       DO I2=1,3
        IDLAM0(J1,I2)=ihcheck(KFDP(I1,I2))
       ENDDO
      ENDDO
      LKNT=0
      call hextra(73,XLAM,IDLAM,LKNT)
      IF(LKNT.ne.0) THEN
       DO I1=1,LKNT
        XLAM0(LKNT0+I1)=XLAM(I1)
        XLAM0(0)=XLAM0(0)+XLAM(I1)
        DO I2=1,3
         IDLAM0(LKNT0+I1,I2)=IDLAM(I1,I2)
        ENDDO
       ENDDO
      ENDIF
      MDCY(73,3)=LKNT+LKNT0
      DO J1=1,MDCY(73,3)
       J2=MDCY(73,2)+J1-1
       IF(J1.GT.LKNT0) THEN
        MDME(J2,1)=1
        MDME(J2,2)=0
       ENDIF
       BRAT(J2) = XLAM0(J1)/XLAM0(0)
       DO I1=1,3
        KFDP(J2,I1)=IDLAM0(J1,I1)
       ENDDO
      ENDDO
      PMAS(73,2)=XLAM0(0)
      PMAS(73,3)=10.*XLAM0(0)
C
      DO I=1,3
       PMAS(74,I)=PMAS(36,I)
       MDCY(74,I)=MDCY(36,I)
      ENDDO
      CHAF(74)=SCHAF(74)
      DO J1=1,3
       KCHG(74,J1) = KCHG(36,j1)
      ENDDO
      MDCY(74,2)=MDCY(73,2)+MDCY(73,3)
      DO J1=0,100
       XLAM0(J1)=0.0
      ENDDO
      LKNT0=MDCY(74,3)
      DO J1=1,MDCY(74,3)
       J2=MDCY(74,2)+J1-1
       I1=MDCY(36,2)+J1-1
       XLAM0(J1)=BRAT(I1)*PMAS(74,2)
       XLAM0(0)=XLAM0(0)+XLAM0(J1)
       MDME(J2,1)=MDME(I1,1)
       MDME(J2,2)=MDME(I1,2)
       DO I2=1,3
        IDLAM0(J1,I2)=ihcheck(KFDP(I1,I2))
       ENDDO
      ENDDO
      LKNT=0
      call hextra(74,XLAM,IDLAM,LKNT)
      IF(LKNT.ne.0) THEN
       DO I1=1,LKNT
        XLAM0(LKNT0+I1)=XLAM(I1)
        XLAM0(0)=XLAM0(0)+XLAM(I1)
        DO I2=1,3
         IDLAM0(LKNT0+I1,I2)=IDLAM(I1,I2)
        ENDDO
       ENDDO
      ENDIF
      MDCY(74,3)=LKNT+LKNT0
      DO J1=1,MDCY(74,3)
       J2=MDCY(74,2)+J1-1
       IF(J1.GT.LKNT0) THEN
        MDME(J2,1)=1
        MDME(J2,2)=0
       ENDIF
       BRAT(J2) = XLAM0(J1)/XLAM0(0)
       DO I1=1,3
        KFDP(J2,I1)=IDLAM0(J1,I1)
       ENDDO
      ENDDO
      PMAS(74,2)=XLAM0(0)
      PMAS(74,3)=10.*XLAM0(0)
C
      DO I=1,3
       PMAS(75,I)=PMAS(37,I)
       MDCY(75,I)=MDCY(37,I)
      ENDDO
      CHAF(75)=SCHAF(75)
      DO J1=1,3
       KCHG(75,J1) = KCHG(37,j1)
      ENDDO
      MDCY(75,2)=MDCY(74,2)+MDCY(74,3)
      DO J1=0,100
       XLAM0(J1)=0.0
      ENDDO
      LKNT0=0
      LKNT=0
      call hextra(75,XLAM,IDLAM,LKNT)
      IF(LKNT.ne.0) THEN
       DO I1=1,LKNT
        XLAM0(LKNT0+I1)=XLAM(I1)
        XLAM0(0)=XLAM0(0)+XLAM(I1)
        DO I2=1,3
         IDLAM0(LKNT0+I1,I2)=IDLAM(I1,I2)
        ENDDO
       ENDDO
      ENDIF
      MDCY(75,3)=LKNT+LKNT0
      DO J1=1,MDCY(75,3)
       J2=MDCY(75,2)+J1-1
       IF(J1.GT.LKNT0) THEN
        MDME(J2,1)=1
        MDME(J2,2)=0
       ENDIF
       BRAT(J2) = XLAM0(J1)/XLAM0(0)
       DO I1=1,3
        KFDP(J2,I1)=IDLAM0(J1,I1)
       ENDDO
      ENDDO
      PMAS(75,2)=XLAM0(0)
      PMAS(75,3)=10.*XLAM0(0)
C.....This must be done earlier
C      CALL TOP_TO_STOP
C.....Finally, make the old Higgses point to the new Higgses
      CALL OLD_TO_NEW
      RETURN
      END
