      LOGICAL FUNCTION MUFIX_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Muon reconstruction sub-package,
C-                         'MUFIX'
C-
C-   Returned value  : 
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: 
C-
C-   Created   20-MAY-1991   Shahriar Abachi    : Created from MURECO_INI
C-   DH 12/91  move calib init to MUOPAR
C    DH 1/92 ADD CALL TO SAMUS
C     UPDATED  31-oct-1993 D. Wood : add GEANT geometry initialization
C                                    to allow DST refitting
C     UPDATED  21-jun-1995 RE Hall : Change name from MUONLY_INI to 
C              MUFIX_INI may diverge from muonly from this date forward
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE,GEOFIL
      PARAMETER ( RCPFILE = 'MURECO_RCP' )
      PARAMETER ( GEOFIL  = 'GEANT_GEOMETRY' )
      CHARACTER*64 FILN
      LOGICAL OK,SAMRECO_INI
      INTEGER I,IER,IDX,LEN
C
      MUFIX_INI=.TRUE. 
      I=2
      CALL INZCOM(I)            !Initialize Zebra data banks
      CALL INZSTP               !Initialize Zebra STP banks
      CALL MUON_BOOK_FLAGS      !Book logical flags for application
      CALL INRCP (RCPFILE,IER)
      IF(IER.NE.0) THEN
         CALL ERRMSG('Error return from S/R INRCP','MUFIX_INI'
     +            ,'stop processing.','F')
         MUFIX_INI = .FALSE.
         GO TO 999
      ENDIF
C
C  set RCP bank to 'MUON_RCP'.
C=============================
C
      CALL EZPICK('MURECO_RCP')
C
C - GET GEANT GEOMETRY  (SA)
C
      IDX = 1
      CALL EZGETS (GEOFIL,IDX,FILN,LEN,IER)
      IF (IER.NE.0) THEN
         GO TO 999
      ENDIF
      CALL MGET_GEANT(FILN)
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
C
C  User initialization.
C  ====================
C
      CALL MUUSER_INIT
      OK=SAMRECO_INI()
C
  999 RETURN
      END
