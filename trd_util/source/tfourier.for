      SUBROUTINE TFOURIER(I1,WSS,NMOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates fast Fourier transform of signal
C-                         Extracts amplitude and phase.
C-   Inputs  : I1 determines the histogram where the Fourier transform 
C-             is stored.
C-             The signal is tranmitted thru WSS.
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-MAY-1991   JFG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I1,NMOT,NUMTRIG,ITIMING
      INTEGER ID,IBC,IER
      CHARACTER*4 TIMING
      EQUIVALENCE (TIMING,ITIMING)
      LOGICAL FIRST
      REAL WSS(*),WSA(1000),WSR(1000),WSI(1000),WSP(1000)
      INCLUDE 'D0$INC:HTFIRS.INC'
      DATA FIRST /.TRUE./
      CALL HCDIR('//PAWC/TRH',' ')  !GO TO TRD_ON DIRECTORY
      IF (FIRST) THEN
       CALL EZPICK('TRDHIT_RCP')
       CALL EZGET('NUM_PHYS_SWIT',NUMTRIG,IER)
       CALL EZGET('FOURIER_PHASE',ITIMING,IER)
       CALL EZRSET
       FIRST = .FALSE.
       ID = HTFIRS + 1000 * (NUMTRIG+1)
      ENDIF
      CALL VZERO(WSI,NMOT)
      CALL UCOPY(WSS,WSR,NMOT)
      CALL CFT(WSR,WSI,NMOT,NMOT,NMOT,1)
C     Gets module squared
      CALL VMUL(WSR,WSR,WSA,NMOT)
      CALL VMUL(WSI,WSI,WSP,NMOT)
      CALL VADD(WSA,WSP,WSA,NMOT)
      WSA(1) = 0                        ! Suppress the "continuous" component
      CALL HPAK(100,WSA)
      CALL HOPERA(I1,'+',100,I1,1.,1.)
      IF (TIMING.EQ.'Y') THEN
       DO 10 IBC = 1,NMOT
        IF (WSR(IBC).NE.0..AND.WSI(IBC).NE.0.) THEN
         WSP(IBC) = ATAN2(WSR(IBC),WSI(IBC))
         CALL HFILL(ID,FLOAT(IBC),WSP(IBC),WSA(IBC))
        ENDIF
 10    CONTINUE
      ENDIF
  999 RETURN
      END
