      SUBROUTINE BKCACH(LCACL,NDATA,LCACH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books the Bank CACH to hang off the Bank CACL
C-                         Bank contains pointers to CAEP for the
C-                         Cluster in CACL
C-
C-   Inputs  : LCACL = Address of the CACL bank from which to hang CACH
C-             NDATA = number of data words
C-   Outputs : LCACH = Address of the created CACH bank
C-   Controls: None
C-
C-   Created  19-APR-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C--   I/O
      CHARACTER*4 PATH
      INTEGER LCACH
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCACH.LINK/LIST'
C--   INTERNAL VARIABLES
      INTEGER LZLOC
      INTEGER IPATH
      INTEGER LPATH,IXIO,LCACL,NDATA
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
      LCACH = 0
      IF(FIRST)THEN
C
C We assume that when booking CACH banks either the Path is 'RECO'
C or it is GEAN. After Booking a 'RECO' type CACH, this routine does
C not permit one to book another which is 'GEAN' in the same program and
C vice versa
C
        FIRST = .FALSE.
C
        CALL MZFORM('CACH','-I',IXIO)        ! Describe Bank format
C
C--   CHECK PATH
        CALL PATHGT(PATH)
        IF ( PATH.NE.'RECO' .AND. PATH.NE.'GEAN' 
     &    .AND.PATH.NE.'FAKE') THEN
          CALL ERRMSG('CALORIMETER','BKCACH',
     &      'INCORRECT PATH SUPPLIED','F')
          RETURN
        ENDIF
C
C--   PATH (AND BANK) ID NUMBER
        IF ( PATH.EQ.'RECO' ) IPATH = 0
        IF ( PATH.EQ.'GEAN' ) IPATH = 100
        IF ( PATH.EQ.'FAKE' ) IPATH = 200
      ENDIF
C
      CALL MZBOOK(IXMAIN,LCACH,LCACL,-IZCACH,'CACH',0,0,NDATA,IXIO,0)
C
C--   FILL IN BANK ID
      IQ(LCACH-5) = IPATH
C
C--   FILL IN BANK VERSION NUMBER
      IQ(LCACH+1) = 1
C
  999 RETURN
      END
