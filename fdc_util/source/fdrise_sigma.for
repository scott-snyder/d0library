      SUBROUTINE FDRISE_SIGMA( IFIRST,IPEAK,ITAIL,B,
     &                   RISETIME,FALLTIME,WIDTH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate rise time, fall time, width
C-      of pulse from FADC differences array.
C-
C-   Inputs  : IFIRST - beginning of hit
C-             IPEAK -  top of hit
C-             ITAIL -  end of hit
C-             B - data (FADC bin differences)
C
C-   Outputs : RISETIME - sigma of rising edge.
C-             FALLTIME - sigma of falling edge.
C-             WIDTH - difference betw. rising edge and falling edge.
C-
C-   Created   3-JAN-1993   Robert E. Avery   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
C
      INTEGER IFIRST,ITAIL,IPEAK
      REAL B(*)
      REAL RISETIME,FALLTIME,WIDTH
C
      INTEGER I
      REAL    AVR,AVF,SIGMA 
      REAL    SM,SMX,SMX2 
C
C----------------------------------------------------------------------
C
C ****  get the sigma of the rise time 
C
      SM   = 0.0
      SMX  = 0.0
      SMX2 = 0.0
      SIGMA = 0.0
      DO I = IFIRST, IPEAK
        IF ( B(I).GT.0. ) THEN
          SM   = SM  + B(I) 
          SMX  = SMX + B(I)*FLOAT(I)
          SMX2 = SMX2 + B(I)*FLOAT(I)*FLOAT(I)
        ENDIF
      ENDDO
      IF ( SM.GT.1 ) THEN
        AVR = SMX/SM
        SIGMA = SMX2 - SM * AVR**2
        IF ( SIGMA .GE. 0 ) THEN
          SIGMA = SQRT( SIGMA / (SM -1) )
        ENDIF
      ENDIF
      RISETIME = SIGMA * NBPBIN 
C
C ****  get the sigma of the fall time 
C
      SM   = 0.0
      SMX  = 0.0
      SMX2 = 0.0
      SIGMA = 0.0
      DO I = IPEAK,ITAIL
        IF ( B(I).LT.0. ) THEN
          SM   = SM  + ABS(B(I)) 
          SMX  = SMX + ABS(B(I))*FLOAT(I)
          SMX2 = SMX2 + ABS(B(I))*FLOAT(I)*FLOAT(I)
        ENDIF
      ENDDO
      IF ( SM.GT.1 ) THEN
        AVF = SMX/SM
        SIGMA = SMX2 - SM * AVF**2
        IF ( SIGMA .GE. 0 ) THEN
          SIGMA = SQRT( SIGMA / (SM -1) )
        ENDIF
      ENDIF
      FALLTIME = SIGMA * NBPBIN 
C
      WIDTH = (AVF - AVR) * NBPBIN 
C
C----------------------------------------------------------------------------
  999 RETURN
      END
