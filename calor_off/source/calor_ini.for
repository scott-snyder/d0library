      FUNCTION CALOR_INI ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in calorimeter control file and calori-
C-                         meter geometry. The control file is given the
C-                         name `CALEVT_RCP'. The logical name:
C-
C-                            CALEVT_RCP
C-
C-                         should be DEFINEd to be the name of the
C-                         required SRCP control file. 
C-
C-   Returned value: TRUE if initialization successful
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-   Package : CALOR
C-
C-   Created  12-OCT-1989   Harrison B. Prosper
C-   Updated  19-SEP-1990   Chip Stewart   - ADDED CAD_TABLE
C-   Updated   3-FEB-1992   Chip Stewart, Harrison Prosper
C-             Dropped GEANT SRCP banks
C-   Updated  18-SEP-1992   Chip Stewart - CALL CALZED,CALRAD to INIT CLINPH 
C-   Updated  07-JUL-1993   Stu Fuess    - minor bug fix for drop of SRCP_ECAL
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CALOR_INI,CALDDF
      LOGICAL OK,FIRST
      INTEGER IER,NBANKS,IDX
      REAL    R1,R2,R3,R4,R5  !DUMMY aguments to CALRAD,CALZED
      CHARACTER*(*) RCPFIL,GEOFIL
      CHARACTER*80  FILNAM
C
      PARAMETER( IDX    = 1 )                   ! Index of name in array
      PARAMETER( RCPFIL = 'CALEVT_RCP' )        ! Logical name of control file
      PARAMETER( GEOFIL = 'GEOMETRY_FILE' )     ! Array containing geo-filename
C
      INTEGER LENF
      INTEGER GZCGEH,GZCRST,GZCECL,GZCUCL,LCRST,LCECL,LCUCL
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$LINKS:IZCRST.LINK'
      INCLUDE 'D0$LINKS:IZCECL.LINK'
      INCLUDE 'D0$LINKS:IZCUCL.LINK'
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF( .NOT. FIRST ) GOTO 999  ! make sure CALOR_INI is executed only once
C
      FIRST = .FALSE.
C
C ****  Initialize /ZEBSTP/
C
      CALL INZSTP
C
C ****  Declare Calorimeter Link area in /ZLINKC/
C
      CALL CZLINI
C
C ****  Declare STP general Link area in /STP_ZLINKA/
C
      CALL STP_INZLNK
C
C ****  Read control file into an SRCP bank
C
      CALL INRCP (RCPFIL,IER)
      OK = IER .EQ. 0
C
      IF ( OK ) THEN
C
C ****  Get name of calorimeter geometry bank
C
        CALL EZPICK ( RCPFIL )
        CALL EZGETS (GEOFIL,IDX,FILNAM,LENF,IER)
        CALL EZRSET
        OK = IER .EQ. 0
C
C ****  Read in calorimeter geometry banks
C
        IF ( OK ) THEN
          CALL CAISTP (FILNAM(1:LENF),IER)
          OK = IER .EQ. 0
          IF(IER.NE.0) CALL ERRMSG('CAISTP FAILS','CALOR_INI',
     &      'READ '//FILNAM(1:LENF),'W')
        ELSE
          CALL ERRMSG('NO '//GEOFIL//' IN '//RCPFIL,'CALOR_INI',
     &      'CALOR PACKAGE FAILS','W')
        ENDIF
C
C ****  DROP GEANT STRUCTURES
C
        CALL CALZED(20,1,R1,R2,R3,R4,R5,IER) ! save part of that are used
        CALL CALRAD(1,1,R1,R2,R3,R4,IER)     ! in CLINPH
C
        CALL EZLOC('SRCP_REST',LCRST)
        IF(LCRST.GT.0) CALL EZDROP('SRCP_REST')  ! DROP SRCP BANK
        CALL EZLOC('SRCP_UCAL',LCUCL)
        IF(LCUCL.GT.0) CALL EZDROP('SRCP_UCAL')  ! DROP SRCP BANK
C * following line altered 07-Jul-1993
        CALL EZLOC('SRCP_ECAL',LCECL)
        IF(LCECL.GT.0) CALL EZDROP('SRCP_ECAL')  ! DROP SRCP BANK
        LCGEH = GZCGEH ()
        LCRST = LC(LCGEH-IZCRST)
        IF(LCRST.GT.0) CALL MZDROP(IXSTP,LCRST,' ')  ! DROP CRCP IF EXISTS
        LCGEH = GZCGEH ()
        LCECL = LC(LCGEH-IZCECL)
        IF(LCECL.GT.0) CALL MZDROP(IXSTP,LCECL,' ')  ! DROP CRCP IF EXISTS
        LCGEH = GZCGEH ()
        LCUCL = LC(LCGEH-IZCUCL)
        IF(LCUCL.GT.0) CALL MZDROP(IXSTP,LCUCL,' ')  ! DROP CRCP IF EXISTS

      ENDIF
C
C ****  Get list of banks to dump
C
      OK = CALDDF() .AND. OK
      CALL CALDRP(IER)
      OK = (IER .EQ. 0 ) .AND. OK
C
      PTZFLG  = .TRUE. ! if true PTCAEP array is set to 0
      CALOR_INI = OK
C
  999 RETURN
      END
