      PROGRAM ISZHST
C----------------------------------------------------------------
C-
C-  Sample program to histogram ISAJET events
C-  written in ZEBRA format with trivial calorimetry.
C-  User must provide subroutines ISZUSR and ISZUSD
C-  start with ISZUSR example provided in D0$ISAZEB$SOURCE
C-
C-     SDP May,1986
C-     DH 8/87 Allow read of multiple files
C-   Updated  22-MAR-2004   sss - compile with g77
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ITAPES.INC'
      CHARACTER*80 FILIS
      INTEGER IUH,NT,NF,IOS
C
      ITLIS=2
      PRINT 4
    4 FORMAT(' Name of file for printout?')
      READ 2,FILIS
      OPEN(UNIT=ITLIS,FILE=FILIS,STATUS='NEW',FORM='FORMATTED')
      CALL MZEBRA(0)
C
C  File to be checked
      NT=0
      PRINT 1
    1 FORMAT(/'  Data file with ISAJET events?')
      READ 2,FILISA
    2 FORMAT(A)
100   CONTINUE
      NF=0
      ISUNIT=1
C
C  Initialize ZEBRA
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
C  Read events
   10 CALL EVTIN(ISUNIT,IOS)
      IF(IOS.EQ.0) THEN   ! this is an event record
C
        NT=NT+1
        NF=NF+1
        CALL ISZUSR(ITLIS)            ! User provided subroutine
C                                     ! see example in ISAZEB
        GOTO 10
      ENDIF
      PRINT 41,NF,NT
 41   FORMAT(' Number on run =, ',I6,'  number total = ',I7)
      NT=0
      NF=0
      IF(IOS.LT.3) GOTO 10
      PRINT 5
   5  FORMAT(' Next file (STOP to stop)')
      READ 2,FILISA
      IF(FILISA.NE.'STOP') GO TO 100
C
      CALL ISZUSD(ITLIS)              ! user provided subroutine
C                                     ! for end of job (entry point
C                                     ! in ISZUSR example)
      STOP
      END    
