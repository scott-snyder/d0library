      SUBROUTINE CTTR_ANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-         histograms for trigger towers using CTTR
C-         Example subroutine
C-         histograms are in directory CTTR
C-
C-   Created  22-AUG-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCTTR,GZCTTR,PNTR,NEM,NTOT,NSUM,NR,I,ICH
      REAL    ET,ETMAX(2),ANEM,ANSUM
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL HCDIR('//PAWC',' ')    ! go to top directory
        CALL HMDIR('CTTR','S')      ! create CTTR directory
        CALL HBOOK1(1,'no. of em towers',50,0.,50.,0.)
        CALL HBOOK1(2,'no. of summed towers',50,0.,50.,0.)
        CALL HBOOK1(3,'Et of em towers',50,0.,200.,0.)
        CALL HBOOK1(4,'Et of tot towers',50,0.,200.,0.)
        CALL HBOOK1(5,'max. Et of em towers',50,0.,200.,0.)
        CALL HBOOK1(6,'max. Et of summed towers',50,0.,200.,0.)
      ENDIF
C
      CALL HCDIR('//PAWC/CTTR',' ')  ! go to CTTR directory
      LCTTR=GZCTTR()
      IF(LCTTR.LE.0) GOTO 999
      NR=IQ(LCTTR+2)
      NEM=IQ(LCTTR+3)/1000        !no. of em towers
      NSUM=MOD(IQ(LCTTR+3),1000)  ! no. of summed towers
      NTOT=NSUM+NEM
      ANEM=NEM
      ANSUM=NSUM
      CALL HFILL(1,ANEM,0,1.)
      CALL HFILL(2,ANSUM,0,1.)
      ETMAX(1)=0
      ETMAX(2)=0
      PNTR=LCTTR
C
C             loop over all towers
      DO I=1,NTOT
        ET=Q(PNTR+6)
        ICH=IQ(PNTR+11)
        CALL HFILL(ICH+2,ET,0,1.)
        IF(ET.GT.ETMAX(ICH)) ETMAX(ICH)=ET
        PNTR=PNTR+NR
      ENDDO
C
      CALL HFILL(5,ETMAX(1),0,1.)
      CALL HFILL(6,ETMAX(2),0,1.)
  999 RETURN
      END
