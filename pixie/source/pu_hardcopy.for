      SUBROUTINE PU_HARDCOPY(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets the hardcopy device so when the display is
C-   redrawn it will do it in the hardcopy device (2) creating a file using the
C-   printer default name.  The Screen device is truned off.
C-
C-   Inputs  : None
C-
C-   Outputs : IER  [I]: Flag to indicate state of the operation:
C-                       IER = 0, Okay
C-                       IER = -1 There was no hardcopy driver set
C-
C-   Created  11-JUL-1990   Lupe Rosas Howell
C-   Updated   1-OCT-1991   Lupe Howell   IER flag added to avoid the redrawn 
C-             of the previous view if a hardcopy driver was not set
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI
C-   Updated  14-AUG-1992   Lupe Howell  If superimpose mode is on print using
C-             the retained segments
C-   Updated  23-MAR-1993   Lupe Howell  Saving current devices and set it back
C-   Updated  20-JUL-1993   Lupe Howell  Naming the postscript out file using
C-            run, event numbers, and view name.
C-   Updated  20-AUG-1993   Lupe Howell  Fixing name for hardcopy 
C-   Updated   4-OCT-1993   Lupe Howell  Fixing name for hardcopy .PS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
C
      INCLUDE 'D0$INC:PXCOMK.INC/LIST'
C
      INTEGER PRINT_DEV, SCRE_DEV,CUR_DEV,QPRINT,STATUS,LIB$FIND_FILE
      INTEGER LIB$FIND_FILE_END,ISTAT,CONTXT,RUNNO,EVONUM,LEN,INTEGER
      INTEGER LIB$RENAME_FILE,R,E,I,J,K,L,M,N,NM,O,P,Q,II,JJ,NN
      LOGICAL PRINFL, FLGSET, FLGVAL,ODD,ACTIVE
      CHARACTER*3 DRIVER
      CHARACTER*80 MESS,DATAFILE,NEW_FILENAME,RUNO,EVNTNO,VIEW_NAME
      CHARACTER*1 TOKEN(4)
      DATA TOKEN/'+','&','/',' '/

      CHARACTER*(*) FILENAME
      PARAMETER( FILENAME = 'HARDCOPY.PIXIE' )
      CHARACTER*(*) POST_FILENAME
      PARAMETER( POST_FILENAME ='POSTS.DAT'  )
      DATA PRINT_DEV/2/
      DATA SCRE_DEV/1/
      SAVE CUR_DEV,DRIVER
C----------------------------------------------------------------------
C
C ****  Defining the ODD function
C
      ODD(NM) = 2*INT (NM/2).NE.NM
C
C ****  Getting the hardcopy driver name used and checking a
C ****  printer driver was assgined
C
        IER = 0
        CALL D0HDRV( PRINT_DEV, DRIVER )
        IF( DRIVER .EQ. '   ' .OR. DRIVER .EQ. 'DRV' ) THEN
          MESS = ' Hardcopy can not be done without a driver '//
     &               'for DI3000 device 2'
          CALL INTMSG(MESS)
          IER = -1
          GOTO 999
        ENDIF
C
C ****  Displaying message to the user
C
        CALL INTMSG( ' Hardcopy Processing..')
C
C ****  Do hardcopy by redrawing the display using the hardcopy device
C ****  if we are not in SUPERIMPOSE mode
C
      IF ( .NOT. FLGVAL('SUPERIMPOSE') ) THEN
C
C ****  Turning off the CURRENT device (most cases screen device)
C
        CUR_DEV = IDEV
        CALL JDEVOF(CUR_DEV)
C
C ****  Initializing the hardcopy device if NOT 
C ****  in BATCH mode 
C
        IDEV = PRINT_DEV
        IF ( .NOT. FLGVAL('BATCH') ) THEN
          CALL JDINIT( PRINT_DEV )
        ENDIF
C
C ****  Selecting the hardcopy device
C
        CALL JDEVON( PRINT_DEV )                ! Selecting display dev
C
C ****  Turning ON the Hardcopy Flag
C
        CALL FLGSET('HARDCOPY',.TRUE.)

      ELSE
C
C ****  If the superimpose mode is on do the hardcopy
C ****  will be done using retained segements
C
        CALL RETAIN_PUHEAD
        CALL JFSOPN(3,PRINT_DEV,0,FILENAME)
        CALL JDINIT(PRINT_DEV)
        CALL JDEVON(PRINT_DEV,-1.0,1.0,-1.0,1.0)
        CALL JSAALL(PRINT_DEV,1)
        CALL JSAALL(PRINT_DEV,0)
        CALL JDEVOF(PRINT_DEV)
        CALL JDEND(PRINT_DEV)
      ENDIF
C
  999 RETURN
      ENTRY PURESET_DEVICE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Turns off the hardcopy device and turn ON the screen
C-   device after a hardcopy have been made.   The hardcopy will
C-   be send automatically to a defined printer using an HBOOK routine. The
C-   printer name has to be assigned
C-   Ex. ASSIGN PRINTER_NAME SYS$PRINT.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  28-JUN-1990   Lupe Howell
C-
C----------------------------------------------------------------------
C
C ****  Do hardcopy by redrawing the display using the hardcopy device
C ****  if we are not in SUPERIMPOSE mode
C
      IF ( .NOT. FLGVAL('SUPERIMPOSE') ) THEN
C
C ****  Turning off printer device
C
        CALL JDEVOF( PRINT_DEV )
        IF ( CUR_DEV .NE. PRINT_DEV ) THEN
          CALL JDEND(  PRINT_DEV )
        ENDIF
C
C ****  Printing hardcopy on a laser printer
C
        CALL D0HLAS
C
C ****  Turning ON the device that was ON before print
C ****  usually the Screen Device and setting the device for the color table
C
        CALL JDEVON(CUR_DEV)
        IDEV = CUR_DEV
C
C ****  Turning off the HARDCOPY flag
C
        CALL FLGSET('HARDCOPY',.FALSE.)
      ELSE
        STATUS = QPRINT(FILENAME,'SYS$PRINT','DEFAULT',.TRUE.)
        CALL INTMSG(' Hardcopy sent to SYS$PRINT')
      ENDIF
C
C ****  If using PST driver search for hardcopy file named POSTS.DAT
C ****  to rename it to a more meaningfull name.
C
      IF ( DRIVER .EQ.  'PST') THEN
        CONTXT = 0
        ISTAT = LIB$FIND_FILE(POST_FILENAME,DATAFILE,CONTXT)
C
C ****  If the file is found rename it using run number,
C ****  event number and view name
C
        IF ( ODD(ISTAT) )THEN
          R = RUNNO()
          E = EVONUM()
C
C ****  Get run and event number
C
          CALL PXITOC(R,10,RUNO)
          CALL PXITOC(E,10,EVNTNO)
          CALL WORD(RUNO,I,J,N)
          CALL WORD(EVNTNO,K,L,M)
C
C ****  Get title of the view
C
          CALL GET_TITLE(VIEW_NAME)
          CALL SWORDS(VIEW_NAME,O,P,Q)
          NEW_FILENAME = 
     &    'PIX_'//RUNO(I:J)//'_'//EVNTNO(K:L)//'_'
     &    //VIEW_NAME(O:P)//'.PS'
C
C ****  Search for the special charcter "+"or "&" in the file
C ****  name if found remove it from the name
C ****  Also, search for blank characters within the name of 
C ****  the file if found they are removed and replaced 
C ****  with a '_' 
C
          CALL SWORDS(NEW_FILENAME,II,JJ,LEN)
          DO NN = 1, 4
            II = INDEX(NEW_FILENAME,TOKEN(NN))
            ACTIVE = ( II .GT. 0 ) .AND. ( II .LE. LEN )
            DO WHILE ( ACTIVE )
              IF ( TOKEN(NN) .EQ. ' ' ) THEN
                NEW_FILENAME = NEW_FILENAME(1:II-1)
     &            //'_'//NEW_FILENAME(II+1:LEN)
              ELSE
                NEW_FILENAME = NEW_FILENAME(1:II-1)
     &            //NEW_FILENAME(II+1:LEN)
                LEN = LEN - 1
              ENDIF
              II = INDEX(NEW_FILENAME,TOKEN(NN))
              ACTIVE = ( II .GT. 0 ) .AND. ( II .LE. LEN )
            ENDDO
          ENDDO
          CALL INTMSG(' NEW_FILE_NAME='//NEW_FILENAME)
          CALL INTMSG(' POST_FILE_NAME='//POST_FILENAME)
C
C ****  Renaming file
C
          ISTAT = LIB$RENAME_FILE(POST_FILENAME,NEW_FILENAME)
          IF ( ODD(ISTAT) ) 
     &      CALL INTMSG
     &        (' Renaming postscript file to '//NEW_FILENAME )
        ENDIF
        ISTAT  = LIB$FIND_FILE_END(CONTXT)
      ENDIF
      END
