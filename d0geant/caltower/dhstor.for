      SUBROUTINE DHSTOR(ITRACK,IETA,IPHI,LAYER,DESTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sums Calorimeter Energies into /CALTRK/
C-
C-   Inputs  : ITRACK   = TRACK NUMBER
C-             IETA   = ETA INDEX (-37 -> +37)
C-             IPHI   = PHI INDEX (1-64) STARTING FROM X AXIS GOING TO +Y
C-                       Some don't exist at high eta (summed over several)
C-             LAYER  = LAYER # (Physics System)
C-             DESTEP = ENERGY LOST IN STEP
C-
C-   Outputs : /CALTRK/
C-   Controls:
C-
C-   Created  22-MAY-1986   Alan M. Jonckheere
C-   Updated  27-APR-1987   Alan M. Jonckheere
C-                      Changed Data length to hold Blazey's Massless Gaps
C-                      and  White's  Scintillators
C-   Updated  31-JAN-1989   Alan M. Jonckheere  Rewritten to use Physics
C-                      Indices for IETA/IPHI/Layer
C-   Updated  24-APR-1989   Alan M. Jonckheere  Reverse EWRK indices for speed
C-   Updated  19-SEP-1989   Chip Stewart  Split DESTEP into EM & HADRONIC sums 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  INPUT VARIABLES
      INTEGER ITRACK,IETA,IPHI,LAYER
      REAL DESTEP
      INCLUDE 'D0$INC:CALTRK.INC/LIST'  ! Working storage
      INCLUDE 'D0$INC:GCKINE.INC/LIST'  ! geant track description
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'  ! Geant Unit numbers
      INCLUDE 'D0$INC:D0LOG.INC/LIST'   ! Logical Flags
C
C----------------------------------------------------------------------
C
C ****  Energy totals
      IF ( LAYER.GT.17 ) THEN           ! Dead Material
        DEDTOT = DEDTOT + DESTEP
      ELSEIF ( LAYER.EQ.9 ) THEN        ! ICD's
        SCNTOT = SCNTOT + DESTEP
      ELSE
        CALTOT = CALTOT + DESTEP
      ENDIF
C
      IF ( IABS(IETA).LE.37 ) THEN

        EWRK(LAYER,IPHI,IETA) =
     +       EWRK(LAYER,IPHI,IETA) + DESTEP
        EETA(IETA) = EETA(IETA) + DESTEP
        IF ( SCAL(2).EQ.2 .AND. ITRTYP.EQ.4 ) EWRK_H(LAYER,IPHI,IETA) =
     +       EWRK_H(LAYER,IPHI,IETA) + DESTEP
      ELSE
C ****  ERROR IN IETA index
        WRITE (LOUT,*) ' IETA OUT OF RANGE, LAYER: ',LAYER,IETA
      ENDIF
C
C PRINT ?
      IF (DTRK.EQ.1) THEN
        WRITE(LOUT,10) ITRACK,IETA,IPHI,LAYER,DESTEP
   10   FORMAT(' ITRACK/IETA/IPHI/LAYER:',4I4,' DESTEP:',F10.5)
      ENDIF
  999 RETURN
      END
