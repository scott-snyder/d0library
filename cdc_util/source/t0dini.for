C----------------------------------------------------------------------
      LOGICAL FUNCTION T0DINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization of the T0D package
C-                         The control file is given the name `T0D_RCP'.
C-                         The logical name:
C-                                           T0D_RCP
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
C-   Updated  16-NOV-1995   Norman A. Graf   Now call ZCDINI as well
C-
C----------------------------------------------------------------------
      IMPLICIT          NONE
      INCLUDE          'D0$INC:ZEBCOM.INC'
      INCLUDE          'D0$INC:FLAGS.INC'
      INCLUDE          'D0$INC:T0DLNT1.INC'
      INCLUDE          'D0$INC:T0DLNT2.INC'
      INCLUDE          'D0$INC:T0DLNT3.INC'
      INCLUDE          'D0$INC:T0DPOS.INC'
      INCLUDE          'D0$INC:T0DREC.INC'
      LOGICAL           LFIRST, EZERR
      LOGICAL           OK,ZCDINI
      INTEGER           IERR, LRCP
      CHARACTER*(*)     RCPFIL
      PARAMETER       ( RCPFIL = 'T0D_RCP' )   ! Logical name of RCP file
C
      DATA              LFIRST/.TRUE./
C----------------------------------------------------------------------
      T0DINI = .TRUE.
      IF ( .NOT. LFIRST) RETURN
      LFIRST = .FALSE.
C
C ****  Read RCP file into the bank
C
      CALL EZLOC(RCPFIL,LRCP)
      IF (LRCP .LE. 0) THEN
        CALL INRCP(RCPFIL,IERR)
        IF (IERR .NE. 0)
     &    CALL ERRMSG('T0D','T0DINI', 'Reading T0D_RCP failed','F')
      END IF
C
C ****  Book and set a flag for reading DTRAKS_RCP
C
      CALL FLGBK('T0D_RCP',1)
      IF (ERRFLG .NE. 0) THEN
        CALL ERRMSG('T0D','T0DINI', 'Booking T0D_RCP flag failed','W')
      ELSE
        CALL FLGSET('T0D_RCP',.TRUE.)
        IF (ERRFLG .NE. 0)
     &    CALL ERRMSG('T0D','T0DINI', 'Setting T0D_RCP flag failed','W')
      END IF
C
C ****  Read in configuration parameters from the T0D_RCP
C
      CALL EZPICK('T0D_RCP')
      IF ( EZERR(IERR) )
     &    CALL ERRMSG('T0D','T0DINI', 'Picking T0D_RCP bank failed','F')
      L_T0D_REC  = .FALSE.
      L_T0D_ZTRK = .FALSE.
      N_T0D_EVT = 1
      CALL EZGET('T0DREC',L_T0D_REC,IERR)
      CALL EZGET('T0DFUL',L_T0D_FUL,IERR)
      CALL EZGET('T0DEVT',N_T0D_EVT,IERR)
      CALL EZGET('T0DZTRK',L_T0D_ZTRK,IERR)
      IF ( (.NOT. L_T0D_REC) .OR. (N_T0D_EVT .EQ. 0) ) THEN
        L_T0D_REC = .FALSE.
        CALL ERRMSG('T0D','T0DINI','T0D package is switched off','I')
        GO TO 999
      END IF
C
C ****  Filling the common block /T0POS/ with T0D position relative to CDC
C
      CALL EZGET('T0DXR',XR,IERR)
      CALL EZGET('T0DYR',YR,IERR)
      CALL EZGET('T0DPHIR',PHIR,IERR)
C
      RR   = DSQRT(DBLE(XR)**2+DBLE(YR)**2)
      PSIR = DATAN(DBLE(YR)/DBLE(XR))
      TGR  = DTAN(DBLE(PHIR))
C
C ****  Initializing histogramming for the T0D
C
      L_HIST = .FALSE.
      CALL EZGET('T0DHIS',L_HIST,IERR)
      IF (L_HIST) THEN
        CALL DHDIR('T0D_RCP','HBOOK_DIRECTORY',IERR,' ')
        IF (IERR .NE. 0)
     &    CALL ERRMSG('T0D','T0DINI','Can''t set HBOOK directory','W')
        CALL HBOOK1(1,'Number of DTRAKS stored$',10,-0.5,9.5,0.)
        CALL HBOOK1(2,'Number of T0RW banks stored$',21,-0.5,20.5,0.)
        CALL HBOOK1(3,'Position of DTRAKS in fiber$',60,-2.5,27.5,0.)
      END IF
C
C ****  Initialize temporary link areas and deactivate them
C
      CALL MZLINT(IXCOM,'/T0DLNT1/',T0DLNT1,LDTRK,T0DLNT1)
      CALL MZLINT(IXCOM,'/T0DLNT2/',T0DLNT2,LT0RW,T0DLNT2)
      CALL MZLINT(IXCOM,'/T0DLNT3/',T0DLNT3,LZTRK,T0DLNT3)
      T0DLNT1(1) = 0    ! Deactivates first  temporary link area
      T0DLNT2(1) = 0    ! Deactivates second temporary link area
      T0DLNT3(1) = 0    ! Deactivates third  temporary link area
C----------------------------------------------------------------------
  999 CALL EZRSET
C
      OK = ZCDINI()
      IF(.NOT.OK) 
     &  CALL ERRMSG('T0D','T0DINI','Problem initializing ZCD','W')
C
      RETURN
      END
