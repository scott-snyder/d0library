      SUBROUTINE CTTR_SMEAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Smear contents of CTTR by trigger tower resolution function
C-
C-   Created  19-AUG-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCTTR,GZCTTR,NR,NCH,LDATA,I
      REAL    E,ET,R1
C----------------------------------------------------------------------
C
      LCTTR=GZCTTR()
      IF(LCTTR.LE.0) RETURN
      IF(IQ(LCTTR+4).GT.1) GOTO 999 ! smearing already done
      NR=IQ(LCTTR+2)
      NCH=(IQ(LCTTR-1)-4)/NR
      IQ(LCTTR+4)=IQ(LCTTR+4)+2
      DO I=1,NCH
        LDATA=LCTTR+NR*(I-1)
        E=Q(LDATA+5)
        ET=Q(LDATA+6)
C          resolution function is for now 4% constant term
        CALL NORRAN(R1)
        Q(LDATA+5)=E+.04*R1*E
        Q(LDATA+6)=ET+.04*R1*ET
      ENDDO
  999 RETURN
      END
