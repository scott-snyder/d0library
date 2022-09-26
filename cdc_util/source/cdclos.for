      SUBROUTINE CDCLOS( LUNIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : On User Summary, and if CDSURV is on,
C-             save a new STP file. The new values are computed in CDALGN when
C-             needed.  CDSAVE is used to print the new values.
C-
C-   Inputs  : LUNIT [I] : Unit for print-out
C-   Outputs : on LUNIT, and a new STP file on D0$CDC_GEO
C-
C-   Created  12-AUG-1987   Olivier Callot
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  use SRCP
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  clean up and added
C-                                                EZRSET and EZERROR
C-   Updated   3-MAR-1992   Qizhong Li-Demarteau  use D0OPEN 
C-   Updated  16-DEC-1992   Domenico Pizzuto   write new STP if drift velocity
C-                                             alignment (CDVALN) is TRUE   
C-   Updated   2-SEP-1994   Stefano Lami       added switch WRTSTP to write out
C-                                             new STPFILE
C-   Updated  21-DEC-1994   Stefano Lami       fixed problem with STPFILE
C-                                             name length 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LUNIT, RUNNO
      INTEGER CDSURV, ERR, NUMRUN
      INTEGER LUNOUT
      PARAMETER( LUNOUT= 80)
      INTEGER IER
      LOGICAL EZERROR, OK, CDVALN, WRTSTP
      CHARACTER*60  FLNAME
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','CDCLOS',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET_i('CDSURV',CDSURV,ERR)
      CALL EZGET_l('CDVALN',CDVALN,ERR)
      CALL EZGET('WRTSTP',WRTSTP,ERR)
      CALL EZRSET
      IF( CDSURV .NE. 0 .OR. (CDVALN.AND.WRTSTP)) THEN
        WRITE( LUNIT, 1100 )
 1100   FORMAT('0',30('-'),' new set of parameters to run CDC D0USER',
     &             30('-'))
        CALL CDSAVE(LUNIT)
C
C ****  Save on a new initialisation file
C
        NUMRUN = RUNNO()
        WRITE( FLNAME, 1200 ) NUMRUN
 1200   FORMAT('D0$STP$ROOT:[CDC]STP_',I7.7,'.DAT')
        CALL D0OPEN(LUNOUT,FLNAME,'OU',OK)
        IF (.NOT. OK) THEN
          CALL ERRMSG('DTRAKS','CDCLOS',
     &    'Unable to open output file for new set of STP','W')
          GOTO 999
        ENDIF
        CALL FZFILE( LUNOUT, 0, 'O' )
        CALL FZOUT ( LUNOUT, IDVSTP, LSCDC, 1, ' ', 1, 0, 0 )
        CLOSE( LUNOUT )
        CALL INTMSG(' New set of CDC constants on '//FLNAME)
      ENDIF
C
  999 RETURN
      END
