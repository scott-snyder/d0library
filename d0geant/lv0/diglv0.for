      LOGICAL FUNCTION DIGLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Digitize the LV0 Detector
C-        Fetches from SRCP_LV0 bank and loops through LV0 detector sets :
C-              'IUSET_LV0_TILES+' ... LV0 scintillator tiles at +Z
C-              'IUSET_LV0_SUPPORT+' ..Support structure at +Z
C-              'IUSET_LV0_TILES-' ... LV0 scintillator tiles at -Z
C-              'IUSET_LV0_SUPPORT-' ..Support structure at +Z
C-              'IUSET_LV0_PMT+'   ... LV0 PMT at +Z
C-              'IUSET_LV0_PMT-'   ... LV0 PMT at -Z
C-         Sets up STOLV0 to do the actual fetching hits from GEANT and then
C-         storing of LV0 data into into ZEBRA bank GLVH hangin under GHIT.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated  24-FEB-1989   Chip Stewart, Harrison B. Prosper
C-                          No longer a dummy routine
C-   Updated  28-JUL-1989   A.M.Jonckheere  - Made into PBD function
C-   Updated  20-MAR-1992   Freedy Nang  - Add PMT simulation
C-   Updated   9-DEC-1992   Freedy Nang  - Extensive modification of old code
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0PARAM.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INTEGER LTRGR, I
C
C----------------------------------------------------------------------
C
      DIGLV0 = .TRUE.
      IF ( DLV0.LT.3 ) GOTO 999
      CALL VZERO(DATA_WORD,123)
C
C ****  Pack HEADER bank
C
      CALL L0_PACK_HEADER
C
C ****  Pack results into a big ADC card
C
      CALL L0_PACK_ADC
C
C ****  Pack results into VERTEX board
C
      CALL L0_PACK_VERTEX
C
C ****  Pack TRAILER bank
C
      CALL L0_PACK_TRAILER
C
C ****  FILL IN TRGR BANK
C
      CALL BKTRGR(LTRGR,123)
      DO I = 1 , 123
        IQ(LTRGR+I)=DATA_WORD(I)
      ENDDO
      CALL VZERO_i (INFO,240)
C----------------------------------------------------------------------
  999 RETURN
      END
