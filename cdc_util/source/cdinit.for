      SUBROUTINE CDINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read initialisation file, and defines all
C-               parameters and constants needed for CDC processing
C-
C-   Inputs  : file
C-   Outputs : 
C-
C-   Created  11-AUG-1987   Olivier Callot
C-   Updated  14-AUG-1988   Qizhong Li-Demarteau  add a choice to select
C-                          a exist run STP file for a new run
C-   Updated  14-MAR-1989   Qizhong Li-Demarteau  use SRCP for the run
C-                          control parameters
C-   Updated   2-OCT-1989   Qizhong Li-Demarteau  use STP file from D0LIBRARY
C-                                                for D0GEANT data 
C-   Updated  16-JAN-1990   Qizhong Li-Demarteau  pick up default STP file 
C-                                                name from .RCP file 
C-   Updated  28-DEC-1990   Qizhong Li-Demarteau  STP banks can be updated
C-                                                by the values from DBL3 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  16-JUL-1991   Qizhong Li-Demarteau  not read default STP file
C-                                                again for multi-runs 
C-   Updated  16-DEC-1992   Domenico Pizzuto      Flag enabeling use of 150um
C-                                                sense wire staggering in STP
C-   Updated  20-DEC-1992   Qizhong Li-Demarteau  handling DBL3 errors 
C-   Updated   4-MAR-1994   Qizhong Li-Demarteau  removed DBL3 reading 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
C
      INTEGER IERR, NUMRUN, OLDRUN, IER, LENGTH, RUNSAV
      LOGICAL EZERROR
      LOGICAL OK, MCDATA, FIRST
      LOGICAL READ_DFLSTP, MDFSTG
      CHARACTER*26 FILNAM, DFLSTP
C
      SAVE RUNSAV, FIRST, READ_DFLSTP
      DATA MCDATA/.FALSE./
      DATA FIRST/.TRUE./
      DATA RUNSAV/-1/
C----------------------------------------------------------------------
C
      IF (IQ(LHEAD+6).EQ.RUNSAV) GOTO 999
      RUNSAV = IQ(LHEAD+6)
      IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
      NUMRUN = 0
C
C  read default STP file name from DTRAKS_RCP file
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDINIT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGETS('CDC_STPFILE',1,DFLSTP,LENGTH,IER)
        CALL EZGET('READ_DFLSTP',READ_DFLSTP,IER)
        CALL EZGET('MDFSTG',MDFSTG,IER)
        CALL EZRSET
C
C read debug control parameters from SRCP bank and dump RCP if requested
C
        CALL DRDDBG(OK)
        IF (.NOT. OK) 
     &   CALL INTMSG(' Debug control parameters for CDC are not found')
      ELSE
C
C  handle multi-run case
C
        IF (MCDATA) GOTO 999
        IF (READ_DFLSTP) GOTO 200
      ENDIF
C
C   For Monte Carlo data, use the STP file from D0LIBRARY as the default
C   STP file
C   For non-MC data, can also use the STP file from D0LIBRARY as the
C   default STP file if the flag READ_DFLSTP file in DTRAKS.RCP is true,
C   otherwise a run dependent STP file STP_nnnnnnn.DAT will be needed.
C
      IF (MCDATA .OR. READ_DFLSTP) THEN
        CALL INTMSG(' Read Static Parameter File '//DFLSTP)
        CALL CDISTP(DFLSTP, IERR)
        IF (IERR .EQ. 0) GOTO 200
        GOTO 301
      ENDIF
C
C ****  The run dependent part for non-MonteCarlo data
C
      IF (LHEAD .GT. 0) NUMRUN = IQ(LHEAD+6)
   55 CONTINUE
      WRITE( FILNAM, 1900 ) NUMRUN
 1900 FORMAT('D0$CDC_GEO:STP_',I7.7,'.DAT')
      CALL INTMSG(' Read Static Parameter File '//FILNAM)
      CALL CDISTP( FILNAM, IERR )
      IF( IERR .EQ. 0 ) GOTO 200
C
C ****  Error, the requested file was not found...
C
  300 CALL INTMSG(' Can not access Initialisation file '//FILNAM)
      IF( NUMRUN .NE. 0 ) THEN
        OLDRUN = 0
        CALL GETPAR(1,' Give run number for CDC_STPFILE to be used'//
     &      ' (default is 0) >','I',OLDRUN)
        NUMRUN = OLDRUN
        GOTO 55
      ENDIF
 301  CALL INTMSG(' We can''t read the CDC constants set. Abort')
      CALL EXIT
C
C ****  all has been read out, print if needed
C
  200 CONTINUE
C
C Modify STP sense wire staggering geometry for CDC for real data only.
      IF (MDFSTG.AND.(.NOT.MCDATA)) THEN
       CALL DMDSTG
      END IF
C
      IF( LVLDBG(1) .NE. 0 ) THEN
        IF (LHEAD .GT. 0) NUMRUN = IQ(LHEAD+6)
        WRITE( LUNDBG, 4000 ) NUMRUN
 4000   FORMAT('0',25('-'),' We used the following parameters for run',
     &          I10,1X,25('-'))
        CALL CDSAVE(LUNDBG)
        CALL INTMSG(' Initialisation parameters written on debug file')
      ENDIF
  999 RETURN
      END
