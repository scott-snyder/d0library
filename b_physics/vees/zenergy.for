      FUNCTION ZENERGY(KZTRK,ITRACK,IER)
C------------------------------------------------------------------
C 
C  Calorimeter energy for a central track ZTRK, given 
C  the pointer to the track bank or the track number. 
C
C  Inputs: KZTRK, ITRACK: pointer to track bank and track number
C          (only one of them has to be provided)
C 
C  Output: 
C          IER  = 0: OK;  
C                 1: CAL_ECELL returns error or IDET=0
C                 2: EEM/ETOT > ELMAX
C 
C  Daria Zieminska 4-NOV-1991
C-   Updated  17-MAR-1992   Daria Zieminska  use a point on the track 
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER ITRACK,ICALL,IDET,IER,PRUNIT,USUNIT
      INTEGER LVTX,LCDC,LFDC,IFDC
      INTEGER KZTRK,LZTRK,GZZTRK,NC,LVERT 
      REAL PHI,THETA,ETA,VXYZ(3)
      REAL ZENERGY,EEM,ETOT,ET,ELTOTOT,ELMAX
      DATA ICALL/0/
      SAVE ICALL 
      IF (ICALL.EQ.0) THEN
        PRUNIT=USUNIT()
        ICALL=1
        NC=1 
        ELMAX=0.9
      END IF
      IER=1
      IF (KZTRK.GT.0) THEN
        LZTRK=KZTRK
      ELSE IF (ITRACK.GT.0) THEN
        LZTRK=GZZTRK(ITRACK)       ! Location of the track bank
      END IF
      IF (LZTRK.LE.0) THEN
        GO TO 1000 ! Bank doesn't exist 
      END IF
C
C  Find parameters of track in r-phi
C
      LVTX=LQ(LZTRK-6)          ! Ref. link to VTX track
      LCDC=LQ(LZTRK-7)          ! Ref. link to CDC track
      LFDC=LQ(LZTRK-8)          ! Ref. link to FDC track
C
C  Find track angles and a point
C
      IF(LCDC.GT.0) THEN
        PHI=Q(LCDC+6)
        THETA=Q(LCDC+9)
        VXYZ(1)=Q(LCDC+7)
        VXYZ(2)=Q(LCDC+8)
        VXYZ(3)=Q(LCDC+11)
      ELSE IF(LFDC.GT.0) THEN
        PHI=Q(LFDC+6)
        THETA=Q(LFDC+22)
        VXYZ(1)=Q(LFDC+4)
        VXYZ(2)=Q(LFDC+5)
        IFDC=IQ(LFDC-5)
        CALL FGETZ0(IFDC,VXYZ(3))
      ELSE IF (LVTX.GT.0) THEN  
        PHI=Q(LVTX+6)
        THETA=Q(LVTX+9)
        VXYZ(1)=Q(LVTX+7)
        VXYZ(2)=Q(LVTX+8)
        VXYZ(3)=Q(LVTX+11)-Q(LVTX+10)/TAN(THETA)
      ELSE 
        GO TO 1000
      END IF
      IF (ABS(THETA).LT.0.001) GO TO 1000
      ETA=-ALOG(TAN(THETA/2.))
      LVERT=LQ(LZTRK-2)
      IF (LVERT.GT.0) THEN  ! use the point of track origin if available
        VXYZ(1)=Q(LVERT+3)
        VXYZ(2)=Q(LVERT+4)
        VXYZ(3)=Q(LVERT+5)
      END IF
      CALL CAL_ECELL(VXYZ,ETA,PHI,NC,EEM,ETOT,ET,IER)
      IF (ETOT.LT.0.001) THEN
        ZENERGY=ETOT
      ELSE
        ELTOTOT=EEM/ETOT
        IF (ELTOTOT.GT.ELMAX) THEN
          IER=2 
        END IF
      END IF
      ZENERGY=ETOT
 1000 RETURN
      END       
