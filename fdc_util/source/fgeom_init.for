      FUNCTION FGEOM_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform all initialization of FDC geometry
C-      banks. (Called from PFINIT and FTRPAR).
C-
C-   Returned value  : False if unable to find STP file.
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  10-OCT-1991   Robert E. Avery
C-   Updated   1-FEB-1993   Robert E. Avery  MC defined as RUNTYPE <= 0,
C-                              not necessarily =0 (for special cases). 
C-   Updated  16-FEB-1993   Robert E. Avery  Allow ALIGN_LEVEL = 2. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FGEOM_INIT
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFALH.LINK'
C
      INTEGER LKFALH,LALIGN,LZFIND
      INTEGER IALIGN,RUNTYPE
      INTEGER IDX,LENGTH,IER
C
      LOGICAL SHIFT_FDC
      LOGICAL FIRST
C
      CHARACTER*26 FILNAM
      CHARACTER*(*) GEOFIL
      PARAMETER(GEOFIL = 'FDC_STPFILE' )
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        IDX=1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('RUNTYPE',RUNTYPE,IER)
        IF(RUNTYPE.LE.0) IDX=2
        CALL EZGETS(GEOFIL,IDX,FILNAM,LENGTH,IER)
        CALL EZGET_i('ALIGN_LEVEL',IALIGN,IER)
        CALL EZGET('SHIFT_FDC',SHIFT_FDC,IER)
        CALL EZRSET
C
C  Read in FDC STP file
C
        IF ( LSTPH .LE. 0 ) CALL INZSTP
        CALL INTMSG(' Reading FDC Static Parameter File '//FILNAM)
        CALL FDISTP(FILNAM, IER)
        IF(IER.NE.0) THEN
          FGEOM_INIT = .FALSE.
          CALL INTMSG(' FGEOM_INIT: can not open file '//FILNAM)
          CALL ERRMSG('FTRAKS','FGEOM_INIT',
     &    'FGEOM_INIT can not read STP file, abort run','F')
        ELSE
          FGEOM_INIT = .TRUE.
          CALL FZERO_FDCPRM
        ENDIF
C
C  Select desired alignment bank.
C
        IF(IALIGN.GT.0) THEN
          LKFALH=LC(LSFDC-IZFALH)
          IF(LKFALH.LE. 5) THEN
            CALL ERRMSG('FTRAKS','FGEOM_INIT',
     &                'Alignment banks not present','W')
            GOTO 999
          ENDIF
          LALIGN=LZFIND(IXSTP,LKFALH,IALIGN,-5)
          IF(LALIGN.LE. 5) THEN
            CALL ERRMSG('FTRAKS','FGEOM_INIT',
     &                'Requested alignment banks not present','W')
            GOTO 999
          ENDIF
          CALL ZSHUNT(IXSTP,LALIGN,LSFDC,-IZFALH,0)
          CALL FZERO_FDCPRM
          IF ( IALIGN .EQ. 1 ) THEN
            CALL INTMSG
     &        (' FDC-Switched to D0/Survey Alignment constants.')
          ELSEIF ( IALIGN .EQ. 2 ) THEN
            CALL INTMSG
     &        (' FDC-Alignment constants based on survey + data.')
          ELSE
            CALL INTMSG
     &        (' FDC-Switched to unexpected Alignment constants.')
          ENDIF
        ENDIF
C
C  Perform z-shift (if requested).
C
        IF ( SHIFT_FDC ) THEN
          CALL FDC_SHIFT
        ENDIF
C
      ELSE
        FGEOM_INIT = .TRUE.
      ENDIF
  999 RETURN
      END
