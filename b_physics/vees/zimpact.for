      FUNCTION ZIMPACT(KZTRK,ITRACK,IDET)
C------------------------------------------------------------------
C 
C  Returns impact parameter (in cm) for a central track ZTRK, given 
C  the pointer to the track bank or the track number. 
C  Uses the information from the VTX chamber if available, otherwise 
C  takes track parameters found in the CDC or FDC chambers.
C
C  Inputs: KZTRK, ITRACK: pointer to track bank and track number
C          (only one of them has to be provided)
C 
C  Output: IDET 
C          IDET = 1: VTX, 2: CDC, 3: FDC, 0: track doesn't exist 
C 
C  Daria Zieminska 6-JUN-1991
C-   Updated   ??-1993      Gene Alvarez use VXY_BEAM 
C-   Updated   2-NOV-1993   Daria Zieminska correct error
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER ITRACK,ICALL,IDET,RUN,ID,STATUS
      INTEGER LVTX,LCDC,LFDC 
      INTEGER KZTRK,LZTRK,GZZTRK 
      REAL ZIMPACT,IMPACT,X_BEAM,Y_BEAM,DX,DY
      REAL PHI,PHIG,XG,YG,RG
      DATA ICALL/0/
      SAVE ICALL 
      IF (ICALL.EQ.0) THEN
        ICALL=1
      END IF
      IDET=0
      IF (KZTRK.GT.0) THEN
        LZTRK=KZTRK
      ELSE IF (ITRACK.GT.0) THEN
        LZTRK=GZZTRK(ITRACK)       ! Location of the track bank
      END IF
      IF (LZTRK.LE.0) THEN
        IDET=1
        GO TO 1000 ! Bank doesn't exist 
      END IF
      CALL EVNTID(RUN,ID)
      CALL VXY_BEAM(RUN,X_BEAM,DX,Y_BEAM,DY,STATUS)
C
C  Find parameters of track in r-phi
C
      LVTX=LQ(LZTRK-6)          ! Ref. link to VTX track
      LCDC=LQ(LZTRK-7)          ! Ref. link to CDC track
      LFDC=LQ(LZTRK-8)          ! Ref. link to FDC track
      IF (LVTX.GT.0) THEN  ! use the VTX component if available
        PHI=Q(LVTX+6)
        XG=Q(LVTX+7)
        YG=Q(LVTX+8)
        IDET=1
      ELSE IF(LCDC.GT.0) THEN
        PHI=Q(LCDC+6)
        XG=Q(LCDC+7)
        YG=Q(LCDC+8)
        IDET=2
      ELSE IF(LFDC.GT.0) THEN
        PHI=Q(LFDC+6)
        XG=Q(LFDC+4)
        YG=Q(LFDC+5)
        IDET=3
      ELSE 
        IDET=0
        GO TO 1000
      END IF
      PHIG=ATAN2(YG,XG)
      IF(PHIG.LT.0) PHIG=PHIG+TWOPI
      RG=SQRT( (XG - X_BEAM)**2 + (YG - Y_BEAM)**2)
C      IMPACT=ABS(RG*SIN(PHIG-PHI))
      IMPACT=ABS((YG-Y_BEAM)*COS(PHI)-(XG-X_BEAM)*SIN(PHI))
      ZIMPACT=IMPACT
 1000 RETURN
      END       
