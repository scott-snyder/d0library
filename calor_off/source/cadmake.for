      FUNCTION CADMAKE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Remake CAD from CAEP, if found, so that raw analysis
C-              can be done from STA tapes (within limitations of zero
C-              suppression)
C-
C-   Returned value  : .TRUE. in all cases
C-   Inputs  : possibly a CAEP bank
C-   Outputs : a CAD bank if possible
C-   Controls: none
C-
C-   Created  13-AUG-1991   James T. Linnemann
C-   Updated  21-APR-1992   Chip Stewart  - CADMAKE_RCP control 
C-   Updated  25-NOV-1992   Chip Stewart, James T. Linnemann 
C-   Updated   2-NOV-1994   Meenakshi Narain - Make CAEP  from CAEQ if CAEP not
C-                                             present
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL CADMAKE
      REAL ECUT
      CHARACTER*4 OLD_PATH,PATH
      INTEGER GZCAD1,GZCAD2,GZCAEP,LOC,IER,LCAD,GZCAEQ
      LOGICAL FIRST,DEL_CAD
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC('CADMAKE_RCP',LOC)
        IF (LOC.EQ.0) CALL INRCP('CADMAKE_RCP',IER)
        CALL EZPICK('CADMAKE_RCP')
        CALL EZGET('ECUT_ZERO_SUPRESS',ECUT,IER)
        CALL EZGETS('PATH',1,PATH,LOC,IER)   ! D0,TB,MIX,PLT
        IF ((PATH.EQ.'RECO').AND.(ECUT.LE.0)) THEN
          CALL ERRMSG('CAD_ZERO_SUP','CADMAKE',
     &      'SET ECUT_ZERO_SUPRESS 1.E-37','I')
          ECUT = 1.0E-37    ! compress out channels not in CAEP and correct len
        ENDIF
        CALL EZGET_l('DELETE_EXISTING_CAD',DEL_CAD,IER)
        CALL EZRSET
        CALL PATHGT(OLD_PATH)
        CALL PATHST(PATH)
C
C ****  check CAD bank in first event
C
        IF ( GZCAEP().LE.0) THEN
          IF (GZCAEQ().LE.0) THEN
            CALL ERRMSG('CAEQ NOT THERE','CADMAKE',
     &        'CAEQ and CAEP not found','W')
          ELSE
            CALL ERRMSG('USE CAEQ ','CADMAKE',
     &        'Make CAEP from CAEQ ','W')
            CALL CAEQ_TO_CAEP ! Make CAEP with CAEQ
          ENDIF
        ENDIF
        IF ((((GZCAD1().LE.0).AND.(GZCAD2().LE.0))
     &    .OR.(DEL_CAD)) 
     &    .AND.( GZCAEP().LE.0)) THEN
C
C ****  check CAEP path in first event
C
          CALL ERRMSG('CAEP NOT HERE','CADMAKE',PATH,'W')
          IF(PATH(1:4).NE.OLD_PATH(1:4)) THEN
            CALL PATHST(OLD_PATH)
            IF ( GZCAEP().LE.0) THEN
              CALL ERRMSG('CAEP NOT HERE EITHER','CADMAKE',OLD_PATH,'W')
              GOTO 99
            ELSE
              CALL ERRMSG('CAEP HERE','CADMAKE',OLD_PATH,'W')
              PATH = OLD_PATH
            END IF
          END IF
        END IF
      END IF
C
C...rebuild if both CAD banks missing
C
      IF ((GZCAD1().GT.0).OR.(GZCAD2().GT.0)) THEN
        IF(DEL_CAD) THEN
          LCAD = GZCAD1 ()
          IF(LCAD.GT.0)CALL MZDROP(IXMAIN,LCAD,' ')
          LCAD = GZCAD2 ()
          IF(LCAD.GT.0)CALL MZDROP(IXMAIN,LCAD,' ')
          CALL ERRMAX('DELETE_EXISTING_CAD',0,1)
          CALL ERRMSG('DELETE_EXISTING_CAD','CADMAKE',
     &     'CAD remade from CAEP','I')
        ELSE
          CALL ERRMAX('CAD THERE',0,1)
          CALL ERRMSG('CAD THERE','CADMAKE',
     &     'CAD not remade since it was already there','W')
          GOTO 99
        END IF
      ENDIF
C
C ****  need CAEP to build CAD banks from
C
      CALL PATHGT(OLD_PATH)
      CALL PATHST(PATH)
      IF ( GZCAEP().LE.0) THEN
        IF (GZCAEQ().GT.0) THEN
          CALL CAEQ_TO_CAEP ! Make CAEP with CAEQ
        ENDIF
      ENDIF
      IF ( GZCAEP().LE.0) THEN
        CALL ERRMSG('CAEP NOT HERE','CADMAKE',PATH,'W')
        GOTO 99
      END IF
      CALL FLCAD(ECUT)
  99  CADMAKE = .TRUE.
      CALL PATHST(OLD_PATH)
  999 RETURN
      END
