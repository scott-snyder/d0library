      PROGRAM DO_HBOOK_TEST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test program to illustrate the use of the
C-   routines:
C-
C-      DO_HBOOK_OPEN
C-      DHDIR_DECLARE_FILE
C-      DO_HBOOK
C-      DO_HBOOK_CLOSE
C-      DHDIR
C-      DHSETDIR
C-      DHSHOW
C-      DHDUMP
C-
C-   Created  11-NOV-1991   Harrison B. Prosper
C-   Updated  17-JUN-1992   Harrison B. Prosper
C-      Add some more goodies
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,K,STATUS,ID,NEVENTS,NID
      INTEGER MAXDIM, MAXID,USERID
      PARAMETER( MAXDIM =  5)
      PARAMETER( MAXID  = 10)
      PARAMETER( USERID =  1)
      REAL    X(MAXDIM),Y,Z,RNDM
C
C ****  Histogram IDs
C
      INTEGER A_LA_MODE(MAXID),COBBLER(MAXID),AND_CREAM(MAXID),
     &  HBOOK(MAXID)
C----------------------------------------------------------------------
C
C ********************************************
C ****  Setup ZEBRA
C ********************************************
C
      CALL INZBRA
C
C ****  Setup /ZEBSTP/
C
      CALL INZSTP
C
C ****  Setup HBOOK
C
      CALL INPAWC
C
C ********************************************
C ****  Read RCP file
C ********************************************
C
      CALL INRCP('DO_HBOOK_TEST_RCP',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Cannot open DO_HBOOK_TEST_RCP'
      ENDIF
C
      CALL EZPICK('DO_HBOOK_TEST_RCP')
      CALL EZGET('NUMBER_OF_EVENTS',NEVENTS,STATUS)
      IF ( NEVENTS .LE. 0 ) THEN
        NEVENTS = 1000
      ENDIF
C
C ********************************************
C ****  Open HBOOK files
C ********************************************
C
      CALL INTMSG('  ')
      CALL INTMSG(' Open HBOOK files')
      CALL DO_HBOOK_OPEN('HBOOK_OPEN',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Cannot open HBOOK files'
      ENDIF
      CALL EZRSET
C
C ********************************************
C ****  Create 1st set of directories and book histograms/ntuples.
C ****  Get the directory path from the RCP bank.
C
C ****  The paths will be:
C ****
C ****  //PAWC/PIE/A_LA_MODE
C ****  //APPLE/PIE/A_LA_MODE
C
C ****  Note: If only one file has been opened it is NOT necessary to call
C ****  DHDIR_DECLARE_FILE explicitly.
C
      CALL DHDIR_DECLARE_FILE('APPLE')
      CALL DHDIR('DO_HBOOK_TEST_RCP','APPLE_DIRECTORY',STATUS,' ')
C
C ****  Show current directories in memory and RZ
C
      CALL DHSHOW
      CALL DHSHOWRZ
C
      CALL INTMSG('  ')
      CALL INTMSG(' DO_HBOOK using APPLE_HISTOGRAMS1')
C
      CALL EZPICK('DO_HBOOK_TEST_RCP')
      CALL DO_HBOOK('APPLE_HISTOGRAMS1')
C
C ****  Get Histogram/Ntuple IDs
C
      CALL DO_HBOOK_GET_IDS(MAXID,A_LA_MODE,NID)
      CALL EZRSET
C
C ****  Create 2nd set of directories and book histograms.
C ****  Specify the path explicitly. The path will be //PAWC/COBBLER.
C ****  //APPLE/COBBLER will be created automatically in the RZ-file.
C
      CALL DHDIR_DECLARE_FILE('APPLE')
      CALL DHSETDIR('//PAWC/COBBLER',STATUS)
      CALL DHSHOW
      CALL DHSHOWRZ
C
      CALL INTMSG('  ')
      CALL INTMSG(' DO_HBOOK using APPLE_HISTOGRAMS2')
C
      CALL EZPICK('DO_HBOOK_TEST_RCP')
      CALL DO_HBOOK('APPLE_HISTOGRAMS2')
      CALL DO_HBOOK_GET_IDS(MAXID,COBBLER,NID)
      CALL EZRSET
C
C ********************************************
C ****  Set directory to PAWC for third file
C ********************************************
C
      CALL DHDIR_DECLARE_FILE('HBOOK')
      CALL DHSETDIR('//PAWC',STATUS)
      CALL DHSHOW
      CALL DHSHOWRZ
C
      CALL INTMSG('  ')
      CALL INTMSG(' DO_HBOOK using HBOOK_HISTOGRAMS')
C
      CALL EZPICK('DO_HBOOK_TEST_RCP')
      CALL DO_HBOOK('HBOOK_HISTOGRAMS')
      CALL DO_HBOOK_GET_IDS(MAXID,HBOOK,NID)
      CALL EZRSET
C
C ********************************************
C ****  Associate next set of histograms/ntuples with the file
C ****  identified by the top directory PEACHES.
C ********************************************
C
      CALL DHDIR_DECLARE_FILE('PEACHES')
      CALL DHDIR('DO_HBOOK_TEST_RCP','PEACHES_DIRECTORY',STATUS,' ')
      CALL DHSHOW
      CALL DHSHOWRZ
C
C ****  Book histograms
C
      CALL INTMSG('  ')
      CALL INTMSG(' DO_HBOOK using PEACHES_HISTOGRAMS')
C
      CALL EZPICK('DO_HBOOK_TEST_RCP')
      CALL DO_HBOOK('PEACHES_HISTOGRAMS')
      CALL DO_HBOOK_GET_IDS(MAXID,AND_CREAM,NID)
      CALL EZRSET
C
C ****  Test Building of other directories
C
      CALL DHSETDIR('AND',STATUS)
      CALL DHSHOW
      CALL DHSHOWRZ
C
      CALL DHSETDIR('ORANGES',STATUS)
      CALL DHSHOW
      CALL DHSHOWRZ
C
C ****  Now cancel automatic creation/setting of RZ-directories
C
      CALL DHDIR_DECLARE_FILE(' ')
C
C ****  Test flip between RZ-directories
C
      CALL DHSETDIR('//APPLE/PIE/A_LA_MODE',STATUS)
      CALL DHSHOWRZ
C
      CALL DHSETDIR('//PEACHES/AND/CREAM',STATUS)
      CALL DHSHOWRZ
C
      CALL DHSETDIR('//APPLE/COBBLER',STATUS)
      CALL DHSHOWRZ
C
      CALL DHSETDIR('//HBOOK',STATUS)
      CALL DHSHOWRZ
C
      CALL DHSETDIR('//PEACHES/AND/CREAM',STATUS)
      CALL DHSHOWRZ
C
      CALL DHSETDIR('//HBOOK',STATUS)
      CALL DHSHOWRZ
C
      CALL DHSETDIR('//APPLE/PIE',STATUS)
      CALL DHSHOWRZ
C
C ********************************************
C ****  Do FILLING in //PAWC/PIE/A_LA_MODE
C ********************************************
C
      CALL INTMSG('  ')
      CALL INTMSG(' FILLING: Set to ')
      CALL DHDIR_DECLARE_FILE('APPLE')
      CALL DHDIR('DO_HBOOK_TEST_RCP','APPLE_DIRECTORY',STATUS,' ')
      CALL DHSHOW
      CALL DHSHOWRZ
C
      DO I = 1, NEVENTS
C
        Y = I*I
        DO J =  1, MAXDIM
          X(J) = Y
        ENDDO
        CALL DO_HFN(' ',A_LA_MODE(1),X)
C
        Z = 100.0*RNDM(I)
        CALL DO_HF1(A_LA_MODE(2),Z,1.0)
      ENDDO
C
C ********************************************
C ****  Do FILLING in /COBBLER
C ********************************************
C
      CALL INTMSG('  ')
      CALL INTMSG(' FILLING: Set to ')
      CALL DHDIR_DECLARE_FILE('APPLE')
      CALL DHSETDIR('//PAWC/COBBLER',STATUS)
      CALL DHSHOW
      CALL DHSHOWRZ
C
      DO I = 1, NEVENTS/2
C
        Z = 1000.0*RNDM(I)
        CALL DO_HF1(COBBLER(1),Z,1.0)
C
        Z = I/100.0
        Y = 1000.0*(SIN(Z))**2
        DO J =  1, MAXDIM
          X(J) = Y
        ENDDO
        CALL DO_HFN(' ',COBBLER(2),X)
      ENDDO
C
      CALL INTMSG('  ')
      CALL INTMSG(' FILLING: Set to ')
      CALL DHDIR_DECLARE_FILE('PEACHES')
      CALL DHDIR('DO_HBOOK_TEST_RCP','PEACHES_DIRECTORY',STATUS,' ')
      CALL DHSHOW
      CALL DHSHOWRZ
C
      DO I = 1, NEVENTS/2
        Y = I*I
        Z = RNDM(I)
C
        Z = FLOAT(INT(Y*Z))
        CALL DO_HF1(AND_CREAM(1),Z,1.0)
C
        DO J =  1, MAXDIM
          X(J) = Y*J
        ENDDO
        CALL DO_HFN('PEACHES',AND_CREAM(2),X)
C
        DO J =  1, MAXDIM
          X(J) = Y/J
        ENDDO
        CALL DO_HFN('PEACHES',AND_CREAM(3),X)
      ENDDO
C
C ********************************************
C ****  Do FILLING in //HBOOK
C ********************************************
C
      CALL INTMSG('  ')
      CALL INTMSG(' FILLING: Set to ')
      CALL DHDIR_DECLARE_FILE('HBOOK')
      CALL DHSETDIR('//PAWC',STATUS)
      CALL DHSHOW
      CALL DHSHOWRZ
C
      DO I = 1, NEVENTS
C
        Z = 100.0*((I-1)**2)*(NEVENTS-I)**2
        DO J =  1, MAXDIM
          X(J) = J*J*Z
        ENDDO
        CALL DO_HFN('HBOOK',HBOOK(1),X)
C
        CALL DO_HF1(HBOOK(2),Z,1.0)
      ENDDO
C
C ****  Close HBOOK files; but maintain the directory structure
C
      CALL INTMSG('  ')
      CALL INTMSG('  BEFORE CLOSE OF APPLE and HBOOK')
      CALL DHDUMP
C
      CALL INTMSG('  ')
      CALL INTMSG(' List contents of PAWC')
      CALL HLDIR('//PAWC','T')
C
C
C ****  Close files and delete histograms/ntuples from all
C ****  sub-directories of //PAWC, but NOT from //PAWC itself.
C
      CALL NTUPLE_CLOSE_AND_DELETE('APPLE',STATUS)
C
C ****  Close another file
C
C ****  NOTE: NTUPLE_FILE_OPEN automatically declares the file to
C ****        DHDIR, while NTUPLE_CLOSE etc., un-declares the file.
C
      CALL NTUPLE_CLOSE('HBOOK',STATUS)
C
      CALL INTMSG('  ')
      CALL INTMSG('  AFTER CLOSE OF APPLE and HBOOK')
      CALL DHDUMP
C
      CALL INTMSG('  ')
      CALL INTMSG(' List contents of PAWC')
      CALL HLDIR('//PAWC','T')
C
C ****  Now delete, explicitly, histograms/ntuples in //PAWC (=> //HBOOK)
C
      CALL INTMSG('  ')
      CALL INTMSG(' Delete stuff from //PAWC')
      CALL DHSETDIR('//PAWC',STATUS)
      CALL HDELET(0)
C
      CALL INTMSG('  ')
      CALL INTMSG(' List contents of PAWC')
      CALL HLDIR('//PAWC','T')
C
C ****  Re-open files and list some info about ntuples
C
      CALL NTUPLE_FILE_OPEN(USERID,.FALSE.,'APPLE',0,'APPLE',STATUS)
      IF ( STATUS .NE. 0 ) GOTO 999
C
      CALL HCDIR('//PAWC/COBBLER',' ')
      CALL RZCDIR('//APPLE/COBBLER',' ')
      CALL NTUPLE_LIST_INFO(6,STATUS)
C
      CALL HCDIR('//PAWC/PIE/A_LA_MODE',' ')
      CALL RZCDIR('//APPLE/PIE/A_LA_MODE',' ')
      CALL NTUPLE_LIST_INFO(6,STATUS)
C
      CALL NTUPLE_FILE_OPEN(USERID,.FALSE.,'HBOOK',0,'HBOOK',STATUS)
      IF ( STATUS .NE. 0 ) GOTO 999
C
      CALL HCDIR('//PAWC',' ')
      CALL RZCDIR('//HBOOK',' ')
      CALL NTUPLE_LIST_INFO(6,STATUS)
C
C ****  Close all files
C
  999 CONTINUE
      CALL DO_HBOOK_CLOSE
      END
