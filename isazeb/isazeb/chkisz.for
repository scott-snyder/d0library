      PROGRAM CHKISZ
C-------------------------------------------------------------
C-
C-  Check output file of ISAZEB
C-
C-     SDP January,1985
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*80 FILIS
      INTEGER PRUNIT
      INTEGER NDUMP,IEVT,IUH
C
C  File to be checked
      PRINT 1
    1 FORMAT(/'  DATA FILE TO BE CHECKED')
      READ 2,FILISA
    2 FORMAT(A)
      ISUNIT=1
C
C  Initialize ZEBRA
      CALL MZEBRA(0)
      CALL ISAZEB('I')
C
C  Read beginning record
      CALL FZIN(ISUNIT,IXMAIN,LHEAD,1,' ',0,0)
      CALL UCTOH('ISAJ',IUH,4,4)
      IF(IQ(LHEAD+2).NE.IUH) THEN   ! not an ISAJET file
        PRINT 3,IQ(LHEAD+2),IQ(LHEAD+3)
    3   FORMAT('  IT IS NOT AN ISAJET FILE, HEADERS ARE ',2A4
     $  , /,'  JOB TERMINATED')
        STOP
      ENDIF                 
C
      PRUNIT=2
      PRINT 4
    4 FORMAT(' Name of file for printout?')
      READ 2,FILIS
      OPEN(UNIT=PRUNIT,FILE=FILIS,STATUS='NEW',FORM='FORMATTED')
C        dump beginning record
      CALL PRHEAD(PRUNIT,0,0,'BEGIN',0)
      IF(IQ(LHEAD+1).EQ.1001) CALL PRISAB(PRUNIT,0,0,0,0)
      PRINT 5
    5 FORMAT(' How many events do you wish to dump?')
      READ *,NDUMP
      IEVT=0
      CALL MZWIPE(0)
C
C  Read events
   10 CALL FZIN(ISUNIT,IXMAIN,LHEAD,1,' ',0,0)
      IF(IQ(LHEAD+1).EQ.1001) CALL PRISAB(PRUNIT,0,0,0,0) ! begin record
      IF(IQ(LHEAD+1).EQ.1002) CALL PRISAF(PRUNIT,0,0,0,0) ! end record
      IF(IQ(LHEAD+1).GT.1002) THEN         ! this is an event record
        CALL PRHEAD(PRUNIT,0,0,'EVENT',0)  ! dump event to unit PRUNIT
        CALL PRTEVZ(PRUNIT)            
        CALL MZWIPE(0)
        IEVT=IEVT+1
        IF(IEVT.GT.NDUMP) STOP
        GOTO 10
      ENDIF
      STOP
      END    
