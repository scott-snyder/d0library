      SUBROUTINE DELDIG(HITIN,IORDER,NHIN,HITOUT,NHOUT)             
C------------------------------------------------------------------
C  delay line hits are merged if they overlap
C  Input:                  
C         NHIN       = # input hits 
C         HITIN(3,NHIN)
C         HITIN(1,I) = time
C         HITIN(2,I) = pulse hight
C         HITIN(3,I) = pulse width           
C   Output:
C         NHOUT      = # output hits
C         HITOUT(1:3,NHOUT)
C 
C   K.NG, D.Z. APR.,1987
C-------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NHIN,IORDER(*),NHOUT,INEXT,IORD,NHMAX
      PARAMETER (NHMAX=50)   
      REAL HITIN(3,*),HITOUT(3,NHMAX),OVRLAP,DELTA,THRESH
      DATA DELTA,THRESH/0.,0./
      NHOUT=0
      DO 100 INEXT=1,NHIN
        IORD=IORDER(INEXT)
C
C  If pulse is below threshold, ignore it.
        IF(HITIN(2,IORD).LT.THRESH) GO TO 100
C
C  If pulse overlaps previous pulse, merge them.
        IF (NHOUT.GT.0) THEN
          OVRLAP=HITOUT(1,NHOUT)+DELTA-HITIN(1,IORD)
          IF (OVRLAP.GE.0) THEN                        
            HITOUT(2,NHOUT)=HITOUT(2,NHOUT)+HITIN(2,IORD)                  
            HITOUT(3,NHOUT)=HITOUT(3,NHOUT)+HITIN(3,IORD)-OVRLAP
          END IF
        END IF
        NHOUT=NHOUT+1
        HITOUT(1,NHOUT)=HITIN(1,IORD)
        HITOUT(2,NHOUT)=HITIN(2,IORD)
        HITOUT(3,NHOUT)=HITIN(3,IORD)
  100 CONTINUE
      RETURN
      END
