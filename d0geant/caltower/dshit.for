      SUBROUTINE DSHIT(ITRA,TILEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Enter a hit into A.White's ICD Scintillators
C-
C-   Inputs  : ITRA =  Geant track number
C-             TILEL = Light output by tile
C-   Outputs :
C-   Controls:
C-
C-   Created   1-MAY-1987 Alan M. Jonckheere
C-   Updated  16-FEB-1989   Alan M. Jonckheere  Change to new Layer
C-                              corresponding to the PHYSICS Index
C-   Updated   9-AUG-1989   Alan M. Jonckheere  Store Light output, rather than
C-                                      track count. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C INPUT VARIABLES
      INTEGER ITRA
      REAL    TILEL
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
      INTEGER IETA,IPHI,LAYER
C
      INTEGER NAMSV(18),ISCIN
      DATA NAMSV/4hD06+,4hD07+,4hD08+,4hD09+,4hD10+,4hD11+,
     &           4hD12+,4hD13+,4hD14+,
     &           4hD06-,4hD07-,4hD08-,4hD09-,4hD10-,4hD11-,
     &           4hD12-,4hD13-,4hD14-/
C
      INTEGER IUCOMP
C
C----------------------------------------------------------------------
C
      ISCIN = IUCOMP(IHDET,NAMSV,18) - 1
      IF ( ISCIN.LT.0 ) THEN
        WRITE (LOUT,*) ' SCINTILLATOR IHDET not in NAME list'
        GO TO 999
      ENDIF
      IETA = MOD(ISCIN,9) + 6
      IF ( ISCIN.GT.9 ) IETA = -IETA
      IPHI = NUMBV(1)
      LAYER = 9
      CALL DHSTOR(ITRA,IETA,IPHI,LAYER,TILEL)
C
  999 RETURN
      END
