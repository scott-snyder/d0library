      SUBROUTINE UDST_CDTK_ERRORS_PACK(LDTRK,PKWORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns a packed word containing some information
C-                         for a CDC track. Packing algorithm is hardcoded
c                          to ensure that it is duplicated by the unpack
c                          routine UDST_CDTK_ERRORS_UNPACK
C-
C-   Inputs   : LDTRK: DTRK bank pointer
C-   Outputs  : PKWORD: packed 32 bit integer containing following information:
c   bits  1-8 : coded chi squared probability in XY
c   bits 9-16 : coded chi squared probability in RZ
c  bits 17-24 : coded error on XY impact parameter of track w.r.t. beam axis
c  bits 25-32 : coded error on Z position of track at beam axis
C-
C-   Created  3-oct-1995   Ashutosh V. Kotwal
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER*4 PKWORD
      REAL    X0, Y0, BEAMX, BEAMY,IMPROB,DZPROB
      REAL    R0DPHI,DBEAMX,DBEAMY,DTHETA,DZ0,DZ,DZ0DTH,SINTHE,COSTHE
      REAL    DIMPAC,CHI2XY,CHI2RZ,XYPROB,RZPROB,THETA,R0
      INTEGER LDTRK,IER,NXY,NRZ,NRUN,IXYPRB,IRZPRB,IIMPRB,IDZPRB
      REAL PROB
      EXTERNAL PROB

C----------------------------------------------------------------------
C
c get run number
      NRUN   = IQ(LHEAD+6)

c extract beam position in x,y and errors for this run
      CALL VXY_BEAM(NRUN,BEAMX,DBEAMX,BEAMY,DBEAMY,IER)

c check that the DTRK bank exists
      IF (LDTRK .GT. 0) THEN
C get some info for this CDC track 
          THETA  =  Q(LDTRK+9)
          X0     =  Q(LDTRK + 7)
          Y0     =  Q(LDTRK + 8)
          NXY    = IQ(LDTRK + 2)
          NRZ    = IQ(LDTRK + 5)
          CHI2XY =  Q(LDTRK + 12)
          CHI2RZ =  Q(LDTRK + 13)
          R0DPHI =  Q(LDTRK + 17)
          DTHETA =  Q(LDTRK + 18)
          DZ0    =  Q(LDTRK + 19)
          DZ0DTH =  Q(LDTRK + 22)

          SINTHE = SIN(THETA)
          COSTHE = COS(THETA)
          R0 = SQRT((X0-BEAMX)**2 + (Y0-BEAMY)**2)

c error on XY impact parameter
          DIMPAC = SQRT( 
     &    ( DTHETA* ( COSTHE*(BEAMX-X0)+SINTHE*(BEAMY-Y0) ) )**2 +
     &    (DBEAMX*SINTHE)**2 + (DBEAMY*COSTHE)**2 + R0DPHI**2 )

c error on Z at beam
          IF (ABS(SINTHE).LT.0.000001) SINTHE = 0.000001
          DZ = SQRT (
     &    DZ0**2 + (R0*DTHETA/(SINTHE**2))**2 + 
     &    2*R0*DZ0DTH/(SINTHE**2) )

c compute chi2 probability in XY fit
          IF (NXY.GT.2) THEN
             XYPROB = PROB(CHI2XY,NXY-2)
          ELSE
             XYPROB = 0.0
          ENDIF

c compute chi2 probability in RZ fit
          IF (NRZ.GT.2) THEN
             RZPROB = PROB(CHI2RZ,NRZ-2)
          ELSE
             RZPROB = 0.0
          ENDIF

c now codify outputs into compact form

c chi2 probability ranges between 0 and 1
c convert to integer between 0 and 255
          XYPROB = XYPROB*255.0
          RZPROB = RZPROB*255.0

          IXYPRB = NINT(XYPROB)
          IRZPRB = NINT(RZPROB)
        
          IF (IXYPRB.LT.0)   IXYPRB = 0
          IF (IXYPRB.GT.255) IXYPRB = 255
          IF (IRZPRB.LT.0)   IRZPRB = 0
          IF (IRZPRB.GT.255) IRZPRB = 255

c errors range between 0 and infty
c convert to range between 0,1 by using chi2 probability function
c use scale to normalize errors

c 0.5 cm scale for XY impact parameter error
          DIMPAC = DIMPAC / 0.5
c convert to probability
          IMPROB = PROB(DIMPAC,1)
c convert to integer 0-255
          IMPROB = IMPROB*255.0
          IIMPRB = NINT(IMPROB)

          IF (IIMPRB.LT.0) IIMPRB = 0
          IF (IIMPRB.GT.255) IIMPRB = 255
            
c 3 cm scale for Z position error
          DZ = DZ / 3.0
c convert to probability
          DZPROB = PROB(DZ,1)
c convert to integer 0-255
          DZPROB = DZPROB*255.0
          IDZPRB = NINT(DZPROB)
          
          IF (IDZPRB.LT.0) IDZPRB = 0
          IF (IDZPRB.GT.255) IDZPRB = 255

c now stuff into 32 bit integer
          CALL SBYT(IXYPRB,PKWORD, 1,8)          
          CALL SBYT(IRZPRB,PKWORD, 9,8)          
          CALL SBYT(IIMPRB,PKWORD,17,8)          
          CALL SBYT(IDZPRB,PKWORD,25,8)          

      ELSE
c if bank does not exist, return zero
          PKWORD = 0

      ENDIF

  999 RETURN
      END






