      SUBROUTINE TSUM_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     make and fill TSUM bank (list of trigger names)
C-
C-   Created  20-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LTSUM,GZTSUM,LHSUM,GZHSUM
      INTEGER RUN,NTRIG,TRIGBIT(32),NFILT,FILTBIT(128),IER
      INTEGER RUNNO,I,POINT,NFIX,NR
      CHARACTER*32 TRIGNAME(32),FILTNAME(128)
C----------------------------------------------------------------------
C
      IF(GZTSUM().GT.0) GOTO 999   ! bank already exists
      LHSUM=GZHSUM()
      IF(LHSUM.LE.0) CALL BKHSUM(LHSUM)
      IF (LHSUM .LE. 0) THEN
        CALL ERRMSG('Cannot book','BKTSUM','Cannot book HSUM','E')
        GO TO 999
      END IF
C
      CALL TSUM_NAMES
     &  (NTRIG,TRIGBIT,TRIGNAME,NFILT,FILTBIT,FILTNAME)
C
C        create and fill bank only if there are triggers present
      IF(NTRIG+NFILT.GT.0) THEN  
        CALL BKTSUM(NTRIG,NFILT,LTSUM)
C
        NFIX=IQ(LTSUM+2)
        NR=IQ(LTSUM+3)
        POINT=LTSUM+NFIX
C
        DO I=1,NTRIG
          IQ(POINT+1)=TRIGBIT(I)
          CALL UCTOH(TRIGNAME(I),IQ(POINT+2),8,32)
          POINT=POINT+NR
        ENDDO
C
        DO I=1,NFILT
          IQ(POINT+1)=FILTBIT(I)
          CALL UCTOH(FILTNAME(I),IQ(POINT+2),8,32)
          POINT=POINT+NR
        ENDDO
      ENDIF
  999 RETURN
      END
