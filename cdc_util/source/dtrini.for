      FUNCTION DTRINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in CDC control file and initialize ZEBSTP.
C-                         The control file is given the name `DTRAKS_RCP'. 
C-                         The logical name:
C-                                           DTRAKS_RCP
C-                         should be DEFINEd to be the name of the
C-                         required SRCP control file.
C-
C-                         NOTE: The control bank can be accessed at any
C-                         time with a call to EZPICK ('DTRAKS_RCP'). 
C-                         Necessary if more than one SRCP-type bank
C-                         exists in /ZEBSTP/.
C-
C-   Inputs  : None
C-   Returned value: TRUE if initialization successful
C-
C-   Created  21-JUN-1989 Qizhong Li-Demarteau  
C-   Updated   8-NOV-1989 Qizhong Li-Demarteau add link area initialization
C-   Updated  12-APR-1990 Qizhong Li-Demarteau make sure it is called 
C-                                             once only
C-   Updated   1-AUG-1991 Qizhong Li-Demarteau  book flag for reading RCP 
C-   Updated  15-NOV-1993   Susan K. Blessing   Add call to INRCPE.
C- 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
C
      LOGICAL DTRINI, DTRDDF, OK, FIRST
      INTEGER IER, LRCP
      CHARACTER*(*) RCPFIL 
      PARAMETER( RCPFIL = 'DTRAKS_RCP' )   ! Logical name of RCP file
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      DTRINI = .TRUE.
      IF (.NOT.FIRST) RETURN
      FIRST = .FALSE.
C
C ****  Read RCP file into the bank
C
      CALL EZLOC(RCPFIL,LRCP)
      OK = LRCP .GT. 0
      IF (.NOT. OK) THEN
        CALL INRCP(RCPFIL,IER)
        OK = IER .EQ. 0
        IF (.NOT. OK) CALL ERRMSG('DTRAKS','DTRINI',
     &    'Reading DTRAKS_RCP failed','F')
      ENDIF
C
C  book and set a flag for reading DTRAKS_RCP
C
      CALL FLGBK('DTRAKS_RCP',1)
      IF (OK) CALL FLGSET('DTRAKS_RCP',.TRUE.)
C
      OK = DTRDDF() .AND. OK       !  DTRDDF gets list of banks to dump
      CALL DTRDRP(IER)
      IF (IER .NE. 0) OK = .FALSE.
      DTRINI = OK          
C
C ****  Read in DTRAKS edit rcpfile DTRAKS_RCPE
C
      CALL INRCPE('DTRAKS_RCPE',IER)
      IF ( IER .EQ. 0 ) THEN
        CALL ERRMSG('DTRAKS_RCPE used','DTRINI',
     &    'Default DTRAKS_RCP modified','W')
      ENDIF
C
C ****  Initialise the LINK area
C
      CALL MZLINT(IXCOM, '/CDCLNK/', CDCLNK, LCDD2, CDCLNK)
      CALL MZLINT(IXCOM, '/CDLTRK/', CDLTRK, LDTSG(3), CDLTRK)      
C
  999 RETURN
      END
