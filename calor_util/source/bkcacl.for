      SUBROUTINE BKCACL(LCACL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CACL to hang from CAPH
C-                         Modelled after BKCAPH
C-
C-   Inputs  : None
C-   Outputs : Link of Booked CACL Bank
C-   Controls: None
C-
C-   Created  18-APR-1989   Rajendran Raja
C-   Updated  27-MAR-1990   Norman Graf, Harrison B. Prosper 
C-      Add 3 more words to CACL bank (E, ET, spare)
C-      E and ET are the TOTAL energy and ET for EM+Hadronic layers.
C-   Updated  15-DEC-1992   Norman A. Graf  version 2 
C-      Add more isolation cone energies, some redundant with PELC/PPHO
C-      but useful for re-running from DST.
C-   Updated   6-MAR-1994   Meenakshi Narain  increase version 5
C-                        (words 32 and 33 are now cone 0.7 cone E/Et) 
C-   Updated  28-SEP-1994   Meenakshi Narain  increase version to 6 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C--   I/O
      CHARACTER*4 PATH
      INTEGER LCACL
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCACL.LINK/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LZLOC
      INTEGER IPATH
      INTEGER LPATH,IXIO,LCAPH,GZCAPH
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCACL = 0
      IF(FIRST)THEN
C
C We assume that when booking CACL banks either the Path is 'RECO'
C 'GEAN' or 'FAKE'. After Booking a 'RECO' type CACL, this routine does
C not permit one to book another which is 'GEAN' in the same program and
C vice versa
C
        FIRST = .FALSE.
C
C--   CHECK PATH
        CALL PATHGT(PATH)
        IF ( PATH.NE.'RECO' .AND. PATH.NE.'GEAN' 
     &    .AND. PATH.NE.'FAKE') THEN
          CALL ERRMSG('CALORIMETER','BKCACL',
     &      'INCORRECT PATH SUPPLIED','F')
          RETURN
        ENDIF
C
C--   PATH (AND BANK) ID NUMBER
        IF ( PATH.EQ.'RECO' ) IPATH = 0
        IF ( PATH.EQ.'GEAN' ) IPATH = 100
        IF ( PATH.EQ.'FAKE' ) IPATH = 200
C
        CALL MZFORM('CACL','3I -F',IXIO)        ! Describe Bank format
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING CAPH BANK
C
      LCAPH = GZCAPH()
      IF ( LCAPH.LE.0 ) CALL BKCAPH(LCAPH)
      IF (LCAPH.LE.0 ) THEN
        CALL ERRMSG('CALORIMETER','BKCACL',
     &      'CANNOT ESTABLISH LCAPH','F')
        RETURN
      ENDIF
C
      CALL MZBOOK(IXMAIN,LCACL,LCAPH,-IZCACL,'CACL',6,2,33,IXIO,0)
C
C--   FILL IN BANK ID
      IQ(LCACL-5) = IPATH
C
C--   FILL IN BANK VERSION NUMBER
      IQ(LCACL+1) = 6

  999 RETURN
      END
