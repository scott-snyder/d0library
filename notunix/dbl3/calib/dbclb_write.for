      SUBROUTINE DBCLB_WRITE ( PATH, DECT, CALTYPE, EXT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write a ZEBRA file extracted from DBL3
C-
C-   Inputs  : PATH     DBL3 path name
C-             CALTYPE  calibration type ( 'pedestal' etc)
C-             DECT     3 letter detector name
C-             EXT      file name extension
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-FEB-1990   J.Green  ( extracted from DBCLB_PROCESS )
C-   Updated  24-FEB-1990   J.Guida  ( Updated for Hist assem )
C-   Updated  16-JAN-1990   Jan Guida, Srini Rajajopalan   Modify the way
C-                                      histograms are written, eliminate some
C-                                      of the 0's in the filename
C-   Updated  29-MAY-1992   S. ABACHI  Start filename with detector name.
C-   Updated  30-MAY-1992   S. ABACHI  In case of .HST extension one extra
C-                                     character added to determine caltype,
C-                                     such as .HSTP to determine histogram
C-                                     for Peds.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE      'D0$PARAMS:CALIB.DEF'
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$INC:LKCALIB.INC'
      INCLUDE      'D0$INC:DBSTP.INC'
      CHARACTER*(*)  PATH, DECT,CALTYPE,EXT
      INTEGER       NRUN, CRATE         ! input key information
      INTEGER       PEDRUN
      INTEGER       LBANK               ! STP address of bank
      INTEGER       LUN, ERR, IOS, CLEN
      INTEGER       LIB$DELETE_FILE, ISTAT
      EXTERNAL      LIB$DELETE_FILE
      CHARACTER*24  OUTFIL,HSTFIL       ! output file name
      CHARACTER*80  MSG                 ! output string
      EQUIVALENCE  (CALIB_LNK(1),LBANK)
C----------------------------------------------------------------------
C
      CALL GETPAR(1,' Enter input run number > ','I',NRUN)
      IF ( DECT .EQ. 'MUO' ) THEN
        CALL GETPAR(1,' Enter input module number >','I',CRATE)
      ELSE
        CALL GETPAR(1,' Enter input crate number >','I',CRATE)
      ENDIF
      IF (CRATE.GT.MAXCRT) THEN
        CALL INTMSG(' Invalid Crate Number ')
        GO TO 999
      ENDIF
      IF (NRUN.LE.1 .OR. NRUN.GT.999999990) NRUN = 999999990
      CALL DBCLB_FETCH(PATH,NRUN,CRATE)
      IF (LBANK.EQ.0) THEN
        CALL INTMSG(' Error in finding requested data ')
        WRITE(MSG,14)CALTYPE,NRUN,CRATE
   14   FORMAT(' Requested Data for Type = ',A8,' Run = ', I9.9,
     &         ' Crate = ',I3.3)
        CALL INTMSG(MSG)
        GO TO 999
      ENDIF
      PEDRUN = IC(LBANK+6)
      IF (PEDRUN.LT.1000000) THEN
        WRITE(OUTFIL,50)DECT,PEDRUN,CRATE,EXT
   50   FORMAT(A2,'_',I6.6,'_',I3.3,A5)
      ELSEIF (PEDRUN.LT.10000000) THEN
        WRITE(OUTFIL,51)DECT,PEDRUN,CRATE,EXT
   51   FORMAT(A2,'_',I7,'_',I3.3,A5)
      ELSEIF (PEDRUN.LT.100000000) THEN
        WRITE(OUTFIL,52)DECT,PEDRUN,CRATE,EXT
   52   FORMAT(A2,'_',I8,'_',I3.3,A5)
      ELSEIF (PEDRUN.LT.1000000000) THEN
        WRITE(OUTFIL,53)DECT,PEDRUN,CRATE,EXT
   53   FORMAT(A2,'_',I9,'_',I3.3,A5)
      ENDIF
      CALL GTUNIT(12,LUN,ERR)
      IF (EXT(1:4).EQ.'.HST') THEN
        CALL STR$TRIM(OUTFIL,OUTFIL,CLEN)
        OUTFIL(CLEN+1:CLEN+1) = CALTYPE(1:1)
        HSTFIL = OUTFIL
        OUTFIL = 'TEMP.HST'
      ENDIF
      OPEN (UNIT=LUN,FILE=OUTFIL,STATUS='NEW',FORM='UNFORMATTED',
     &     IOSTAT=IOS)
      IF(IOS.NE.0)THEN
        MSG = 'Error opening file '//OUTFIL
        CALL INTMSG(MSG)
        GO TO 999
      ENDIF
      CALL FZFILE(LUN,0,'O')
      CALL FZLOGL(LUN,-2)
      CALL FZOUT(LUN,IDVSTP,LBANK,1,' ',1,0,0)
      CALL FZENDO(LUN,'T')
      CLOSE(LUN)
      CALL RLUNIT(12,LUN,ERR)
      MSG = ' '//OUTFIL//' written'
      CALL INTMSG(MSG)
C
      IF (EXT.EQ.'.HST') CALL HCONVERT('TEMP.HST',HSTFIL)
      ISTAT = LIB$DELETE_FILE('TEMP.HST')
C
 999  RETURN
      END
