      SUBROUTINE D0MAIN
C
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main control routine for D0Geant
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  ??-???-????   Steve Linn
C-   Updated  15-FEB-1991   K. Wyatt Merritt  Eliminate HBOOK3 setup
C-                            Incorporate initialization of ALL Zebra
C-                            stores
C-   Updated  22-FEB-1991   K. Wyatt Merritt  Attempt to make a control
C-                            routine for V3.13
C-   Updated   1-APR-1992   K. Wyatt Merritt  Now includes GRUN call to
C-                            make a batch run without graphics (Diesburg),
C-                            and does not open a metafile if IWK <= 0:
C-                            requires concomitant changes to batch 
C-                            command files!!
C-
C----------------------------------------------------------------------
C&IF VAXVMS,SIUNIX,ULTRIX,IBMAIX,SUNOS,ALFOSF
C********************************************************************
C  VAX 780 VERSION
C  D0 MAIN ROUTINE ... FOR BATCH OR INTERACTIVE COMPUTING
C******************************************************************
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCFLAG.INC'
      INCLUDE 'D0$INC:GCTIME.INC'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER NWKUIP
      INTEGER IWK
      INTEGER ISTAT
C
      COMMON/CWK/IWK
      EXTERNAL IGTERM
      EXTERNAL UGLAST
      PARAMETER (NWKUIP=30000)
C----------------------------------------------------------------------
C
C ***  These calls initialize the GEANT Zebra structure and the GEANT
C ***  constants.
C
      CALL INZGCB                       ! Init ZEBRA, GCBANK
      CALL GINIT                        ! Init GEANT constants
      CALL GZINIT                       ! Init GEANT Zebra structures
C
C ***  This call initializes the Zebra store for HBOOK4
C
      CALL INPAWC                       ! Init PAWC
C
C ***  These calls initialize the D0-specific Zebra structure
C
      CALL ZEBINI                       ! Init D0 Zebra structure ZEBCOM
      CALL INZSTP                       ! Init D0 Zebra structure ZEBSTP
      CALL INZLNK                       ! Init D0 Link Area ZLINKA
      CALL UGINIT
C
C *** Check for interactive or batch running
C
      IF(LBAT)  THEN
        CALL GRUN
        CALL UGLAST
        RETURN
      ELSE
C
C ***  Initialize Command Processor (KUIP)
C
        CALL KUINIT(NWKUIP)

        CALL KUEXIT(UGLAST)
        CALL KUTERM(IGTERM)
C
C ***  Initialize Graphics (HIGZ with GKS-GRAL and HPLOT) if IWK > 0
C
        IWK = 211
        CALL KUPROI(
     &    ' GKS WKSTATION TYPE{<CR>=211; <=0 ==>NO GRAPHICS} ',IWK)
        IF (IWK .GT. 0) THEN
          CALL IGINIT(0)
          CALL HPLINT(IWK)
          CALL IGSA(0)
          CALL GDINIT
        ENDIF
        CALL HERMES(6)
C
C ***  Define Menus and Commands
C
        CALL GINTRI
C
C ***  Keep starting time
C
        CALL TIMEST(99999.)
        CALL TIMEX(TIMINT)
C
        CALL KUEXEC('EXEC GLOGON')
        CALL KUEXEC('SET/PROMPT ''GEANT >''')
C
***   PRINT *,'=========> GEANT now ready , You can type your commands'
        IEVENT=0
C
C ***  Give control to the Command Processor
C
        CALL KUWHAT
      ENDIF
C
C
C **********************************************************************
C
C&ENDIF
C&IF VAXELN
C&C********************************************************************
C&C  FARM VERSION
C&C  D0 MAIN ROUTINE - BATCH ONLY
C&C******************************************************************
C&      IMPLICIT NONE
C&      INCLUDE 'D0$PARAMS:NBLANK.DEF'
C&C
C&      LOGICAL LNKINC
C&C
C&      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C&      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
C&      INCLUDE 'D0$INC:GCTIME.INC/LIST'
C&      INCLUDE 'D0$INC:D0LOG.INC/LIST'       ! RUN TIME SWITCHES
C&C
C&      INTEGER NWKUIP
C&      INTEGER IWK
C&      INTEGER ISTAT
C
C&      COMMON/CWK/IWK
C&      EXTERNAL IGTERM
C&      EXTERNAL UGLAST
C&      PARAMETER (NWKUIP=30000)
C&C
C&C------ FARM specific
C&      CHARACTER*6 MICNAM
C&C
C&      CHARACTER*80 FILCOM,GTEXT,HOSDIR,MAIDIR
C&      CHARACTER*1 DUM1,DUM2,DUM3
C&      INTEGER TRULEN,I
C&C
C&      DATA LNKINC/.TRUE./,LOUT/6/
C&C
C&      LBAT = .TRUE.
C&C------ Initialize FARM part
C&C
C&C     Get name of directory on host passed as program argument and the name of
C&C     the command file to be used. Also use staggered start with a 2 min.
C&C     stagger time.
C&C
C&      CALL FARMINI(HOSDIR,.TRUE.,2,2,MAIDIR,GTEXT,DUM1,DUM2,DUM3)
C&      IF(HOSDIR.NE.' ') THEN
C&        HOSDIR=HOSDIR(1:TRULEN(HOSDIR))//MAIDIR
C&      ELSE
C&        HOSDIR=MAIDIR
C&      ENDIF
C&C
C&C     Get the name of the node
C&C
C&      CALL GETNAM(MICNAM)
C&C
C&C     Set up command file name
C&C
C&      FILCOM=HOSDIR(1:TRULEN(HOSDIR))//GTEXT
C&C
C&C     Open command file
C&C
C&      OPEN(UNIT=5,ERR=100,FILE=FILCOM,STATUS='OLD',
C&     1       FORM='FORMATTED',READONLY,SHARED,IOSTAT=I)
C&C
C&C     Open LOG file
C&C
C&      FILCOM=HOSDIR(1:TRULEN(HOSDIR))//MICNAM(1:TRULEN(MICNAM))//'.LOG'
C&      OPEN(UNIT=6,FILE=FILCOM,STATUS='NEW')
C&C
C&C     Open INIT file
C&C
C&      FILCOM=HOSDIR(1:TRULEN(HOSDIR))//'GSAVE.DAT'
C&      OPEN(UNIT=1,FILE=FILCOM,ERR=100,IOSTAT=I,STATUS='OLD',READONLY,
C&     *     SHARED,FORM='UNFORMATTED')
C&C
C&C----------END FARM specific initialization
C&C
C&      CALL INZGCB                     ! Init GCBANK
C&      CALL GINIT
C&      CALL GZINIT
C&
C&      CALL INPAWC
C&
C&      CALL ZEBINI
C&      CALL INZSTP
C&      CALL INZLNK
C&
C&      CALL UGINIT
C&C
C&      CLOSE(5)
C&      CLOSE(1)
C&C
C&      CALL GRUN
C&
C&      CALL UGLAST
C&C
C&      GOTO 101
C&  100 WRITE(LOUT,1035)FILCOM,I        ! Error in file opening
C& 1035 FORMAT(/' Unable to open or close file --> ',A80/10X,
C&     *        'IOSTAT=',I12/)
C&  101 CONTINUE
C&      CALL EXIT(1)        ! Terminate MORE GRACEFUL THAN STOP
C&ENDIF
C&IF ETA10
C&C********************************************************************
C&C  CYBER205 VERSION
C&C  D0 MAIN ROUTINE - BATCH ONLY
C&C******************************************************************
C&      INCLUDE 'D0$PARAMS:NBLANK.DEF'
C&C
C&      INCLUDE 'D0$INC:GVECT.INC/LIST'
C&C
C&      INTEGER NWKUIP
C&      INTEGER IWK
C&      INTEGER ISTAT
C
C&      COMMON/CWK/IWK
C&      EXTERNAL IGTERM
C&      EXTERNAL UGLAST
C&      PARAMETER (NWKUIP=30000)
C&C
C&      LOGICAL LNKINC
C&C
C----------------------------------------------------------------------
C&      DATA LNKINC/.TRUE./
C----------------------------------------------------------------------
C&C
C&      OPEN(UNIT=1,FILE='GEAIN',STATUS='OLD',FORM='UNFORMATTED')
C&      OPEN(UNIT=2,FILE='GEAOUT',STATUS='NEW',FORM='UNFORMATTED')
C&C
C&      CALL INZGCB                     ! Init GCBANK
C&      CALL GINIT
C&      CALL GZINIT
C&
C&      CALL INPAWC
C&
C&      CALL ZEBINI
C&      CALL INZSTP
C&      CALL INZLNK
C&
C&      CALL UGINIT
C&C
C&      CALL VINIT
C&C
C&      CALL GRUN
C&
C&      CALL UGLAST
C&C
C&ENDIF
C&IF UFFARM
C&C********************************************************************
C&C  VAX UofFlorida Farm VERSION
C&C******************************************************************
C&C
C&      IMPLICIT NONE
C&C
C&      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C&      INCLUDE 'D0$PARAMS:NBLANK.DEF'
C&C
C&      INTEGER NWKUIP
C&      INTEGER IWK
C&      INTEGER ISTAT
C
C&      COMMON/CWK/IWK
C&      EXTERNAL IGTERM
C&      EXTERNAL UGLAST
C&      PARAMETER (NWKUIP=30000)
C&C
C&C
C----------------------------------------------------------------------
C&      CALL MAP_INIT                                             ! Multi
C&      CALL MAP_CONNECT_SUP                                      ! Multi
C&C
C&      CALL INZGCB                     ! Init GCBANK
C&      CALL GINIT
C&      CALL GZINIT
C&
C&      CALL INPAWC
C&
C&      CALL ZEBINI
C&      CALL INZSTP
C&      CALL INZLNK
C&
C&      CALL UGINIT
C&C
C&      CLOSE(5)
C&      CLOSE(1)
C&C
C&      CALL GRUN
C&
C&      CALL UGLAST
C&C
C&ENDIF
      END
