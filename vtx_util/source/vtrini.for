      LOGICAL FUNCTION VTRINI
C--------------------------------------------------------------
C
C  Initialization routine for VTRAKS package.
C  Read control parameters.
C
C  Daria Zieminska May 1988; modified June 1989.
C  Updated 23-AUG-1991 P. Grudberg - Book and set flag to indicate VTRAKS_RCP
C                                    has been read.
C-   Updated   4-NOV-1991   Peter M. Grudberg  Add perm link area, clean up 
C-   Updated   9-NOV-1992   Peter M. Grudberg  Add call to INRCPE 
C
C--------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL VTRDDF, OK, FIRST
      INTEGER IER, LRCP
      CHARACTER*(*) RCPFIL
      PARAMETER( RCPFIL = 'VTRAKS_RCP' )   ! Logical name of control file
      DATA FIRST / .TRUE. /
C---------------------------------------------------------------------------
      VTRINI = .TRUE.
      IF ( .NOT. FIRST ) GO TO 999
      FIRST = .FALSE.
C
      CALL VTPLNK                ! Create permanent link area for hit banks
C
C ****  Check for existence of VTRAKS_RCP.  If it's not there, read it in.
C
      CALL EZLOC(RCPFIL,LRCP)
      IF ( LRCP .LE. 0 ) THEN
        CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
        OK = IER .EQ. 0
        IF ( .NOT. OK ) THEN
          CALL ERRMSG('INRCP ERROR','VTRINI',
     &    'Attempt to read VTRAKS_RCP failed','F')
        ELSE
C
C ****  Book and set flag to indicate VTRAKS_RCP has been read
C
          CALL FLGSET('VTRAKS_RCP',.TRUE.)
          CALL FLGERR(IER)
          IF ( IER .NE. 0 ) THEN        ! Flag not booked
            CALL FLGBK('VTRAKS_RCP',1)
            CALL FLGSET('VTRAKS_RCP',.TRUE.)
          ENDIF
        ENDIF
      ELSE
        OK = .TRUE.
      ENDIF
C
C ****  Read in VTRAKS edit rcpfile VTRAKS_RCPE
C
      CALL INRCPE('VTRAKS_RCPE',IER)
      IF ( IER .EQ. 0 ) THEN
        CALL ERRMSG('VTRAKS_RCPE used','VTRINI',
     &    'Default VTRAKS_RCP modified','W')
      ENDIF
C
      OK=VTRDDF().AND.OK       !  Get list of banks to dump
      CALL VTRDRP(IER)
      IF(IER.NE.0) OK=.FALSE.
C
      VTRINI=OK 
 999  CONTINUE
      RETURN
      END
