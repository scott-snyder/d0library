C----------------------------------------------------------------------
      LOGICAL FUNCTION ZCDINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization of the ZCD package
C-                         The control file is given the name `ZCD_RCP'. 
C-                         The logical name:
C-                                           ZCD_RCP
C-                         should be DEFINEd to be the name of the
C-                         required SRCP control file.
C-
C-   Returned value  : .True.  - success
C-                     .False. - otherwise
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-MAR-1993   Gregory L. Landsberg
C-   Updated  16-OCT-1995   Freddie Landry  renamed to ZCDINI from T0DINI
C-                                          replace t0d with ZCD everywhere
C----------------------------------------------------------------------
      IMPLICIT          NONE
      INCLUDE          'D0$INC:ZEBCOM.INC'
      INCLUDE          'D0$INC:ZCDLN1.INC'
      INCLUDE          'D0$INC:ZCDLN2.INC'
      INCLUDE          'D0$INC:ZCDLN3.INC'
      INCLUDE          'D0$INC:ZCDREC.INC'
      LOGICAL           LFirst, EZERR
      INTEGER           IERR, LRCP
      CHARACTER*(*)     RCPFIL 
      PARAMETER       ( RCPFIL = 'ZCD_RCP' )   ! Logical name of RCP file
C
      DATA              LFirst/.True./
C----------------------------------------------------------------------
      ZCDINI = .True.
      IF ( .NOT. LFirst) RETURN
      LFirst = .False.
C
C ****  Read RCP file into the bank
C
      CALL EZLOC(RCPFIL,LRCP)
      IF (LRCP .LE. 0) THEN
        CALL INRCP(RCPFIL,IERR)
        IF (IERR .NE. 0) 
     &    CALL ERRMSG('ZCD','ZCDINI', 'Reading ZCD_RCP failed','F')
      END IF
C ****  Read in configuration parameters from the ZCD_RCP
C
      CALL EZPICK('ZCD_RCP')
      IF ( EZERR(IERR) )
     &    CALL ERRMSG('ZCD','ZCDINI', 'Picking ZCD_RCP bank failed','F')
      L_ZCD_REC  = .False.
      L_ZCD_ZTRK = .False.
      N_ZCD_EVT = 1
      CALL EZGET('ZCDREC',L_ZCD_REC,IERR)
      CALL EZGET('ZCDFUL',L_ZCD_FUL,IERR)
      CALL EZGET('ZCDEVT',N_ZCD_EVT,IERR)
      CALL EZGET('ZCDZTRK',L_ZCD_ZTRK,IERR)
      IF ( (.NOT. L_ZCD_REC) .OR. (N_ZCD_EVT .EQ. 0) ) THEN
        L_ZCD_REC = .False.
        CALL ERRMSG('ZCD','ZCDINI','ZCD package is switched off','I')
        Go To 999
      END IF
C
C ****  Initializing histogramming for the ZCD
C
      L_Hist = .False.
      CALL EZGET('ZCDHIS',L_Hist,IERR)
      IF (L_Hist) THEN
        CALL DHDIR('ZCD_RCP','HBOOK_DIRECTORY',IERR,' ')
        IF (IERR .NE. 0) 
     &    CALL ERRMSG('ZCD','ZCDINI','Can''t set HBOOK directory','W')
        CALL HBook1(1,'Number of DTRAKS stored$',10,-0.5,9.5,0.)
        CALL HBook1(2,'Number of ZDRW banks stored$',21,-0.5,20.5,0.)
        CALL HBook1(3,'Position of DTRAKS in fiber$',60,-2.5,27.5,0.)
      END IF
C
C ****  Initialize temporary link areas and deactivate them
C
      CALL MZLINT(IXCOM,'/ZCDLN1/',ZCDLNT1,LDTRK,ZCDLNT1)
      CALL MZLINT(IXCOM,'/ZCDLN2/',ZCDLNT2,LZDRW,ZCDLNT2)
      CALL MZLINT(IXCOM,'/ZCDLN3/',ZCDLNT3,LZTRK,ZCDLNT3)
      ZCDLNT1(1) = 0    ! Deactivates first  temporary link area
      ZCDLNT2(1) = 0    ! Deactivates second temporary link area
      ZCDLNT3(1) = 0    ! Deactivates third  temporary link area
C----------------------------------------------------------------------
  999 CALL EZRSET
      RETURN
      END
