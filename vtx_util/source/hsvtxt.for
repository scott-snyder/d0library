       SUBROUTINE HSVTXT(ITRACK) 
C----------------------------------------------------------------------
C
C  Fill Histograms for VTX track ITRACK. 
C  Histograms are booked in routine VTRINI.  
C
C        ID            histogrammed quantity     
C
C     IDVTX+1      normalized chi_squared for VTX tracks in x-y view  
C                  (y2, PDG '86 p.53) 
C     IDVTX+2      normalized chi_squared for VTX tracks in r-z view 
C     IDVTX+3      Number of hits on VTX tracks 
C
C  23-JUN-1987   Daria Zieminska
C-   Updated  23-OCT-1991   A. Mayorov check # of z hits - protect from crashing
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDVTX,VHISID,ICALL 
      REAL CHIXY,CHIRZ 
      INTEGER ITRACK 
      INTEGER IQTRAK(21)
      REAL QTRAK(21),CONT(21),QHSEC(4,24),QHZLA(3,6),QHIT(18)
      EQUIVALENCE (IQTRAK,QTRAK)
      DATA ICALL/0/
C------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        IDVTX=VHISID()
        ICALL=1
      END IF
      CALL GTVTXT(ITRACK,CONT,QHSEC,QHZLA)
      CALL UCOPY(CONT,QTRAK,21)
      IF (IQTRAK(2).LE.2) GO TO 1000
      CHIXY=SQRT(2.*QTRAK(12))-SQRT(2.*(FLOAT(IQTRAK(2))-2.)-1.)
      CALL HFILL(IDVTX+1,CHIXY,1.)
      CALL HFILL(IDVTX+2,CHIRZ,1.)
      IF (IQTRAK(5).LE.2) GO TO 1000
      CHIRZ=SQRT(2.*QTRAK(13))-SQRT(2.*(FLOAT(IQTRAK(5))-2.)-1.)
      CALL HFILL(IDVTX+3,FLOAT(IQTRAK(2)),1.)
 1000 CONTINUE 
      RETURN
      END       
