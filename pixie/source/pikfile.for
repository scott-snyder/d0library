      SUBROUTINE PIKFILE(FILENAME,LFN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Constructing file name for GOTO EVENT case
C-
C-   Inputs  :
C-   Outputs :Returns the constructed file name w/o extension
C-   Controls:
C-
C-   Created  29-JAN-1993   Vipin Bhatnagar
C-   Modified 17-MAY-1993   Vipin Bhatnagar
C-    Showing the default stream name in GETPAR
C-   Updated  20-MAY-1993   Lupe Howell SGI machine block and error from user
C-                          checking
C-   Updated  2-JULY-1993   Vipin Bhatnagar Cleaned up redundent code
C-   Modified 6-OCT-1994    Vipin Bhatnagar Constructed_filename for GOTO
C-      EVENT case compatible with "changed" PICK filename
C-   Modified 8-OCT-1994    Vipin Bhatnagar Take care of RUN >= 100000
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXCOMK.INC'
C
C
      INTEGER IST,NST,LST
      INTEGER IRN,NRN,LRN,IEV,NEV,LEV
      INTEGER IFN,NFN,LFN
      INTEGER LL,MM,OO
      INTEGER TRULEN,NEXT4
C
      LOGICAL OUT
C
      CHARACTER*132 FILENAME,STRINGFN
      CHARACTER*60  PRMPTS
      CHARACTER*24  EVTNOC,RUNNOC,STREAM,STRINGR
      CHARACTER*24  EXT1,EXT2,STRINGE,STRINGS,STREAM2
      CHARACTER*24  EXT3,EXT4
C
      DATA EXT1,EXT2,EXT3/'_','_','.*'/
      DATA EXT4/'0'/
C----------------------------------------------------------------------
C
      OUT = .FALSE.
C
      PRMPTS = ' Enter Stream Name['//STREAM1(1:3)//']>'
      CALL GETPAR (1,PRMPTS,'C',STREAM2)
C
C*** Getting run no. & evt. no. from PXCOMK.INC
C
      WRITE(UNIT=STRINGR,FMT='(I8)') RUNCNT
      WRITE(UNIT=STRINGE,FMT='(I8)') EVTCNT
      IF ( EVTCNT .EQ. 0 ) THEN
        OUT = .TRUE.
        GOTO 100
      ENDIF
      IF (STREAM2(1:3).NE.'   ') THEN
        CALL UPCASE (STREAM2,STREAM1)
        STREAM  = STREAM1
      ELSE
        STREAM  = STREAM1(1:3)
      ENDIF
      STRINGS = STREAM
      CALL SWORDS(STRINGS,IST,NST,LST)
      STREAM  = STREAM(1:LST)//EXT1
      STRINGS = STREAM
      CALL SWORDS(STRINGS,IST,NST,LST)
C
C*** Changing integer to character and adding extensions
C
      CALL SWORDS(EXT4,LL,MM,OO)
C
      CALL PXITOC(RUNCNT,8,RUNNOC)
      CALL PXITOC(EVTCNT,8,EVTNOC)
      STRINGR = RUNNOC
      CALL WORD(STRINGR,IRN,NRN,LRN)
      RUNNOC  = RUNNOC(IRN:NRN)//EXT2
      IF (LRN.EQ.6) GOTO 600                   ! for RUN=>100000
      RUNNOC  = EXT4(1:OO)//RUNNOC(1:TRULEN(RUNNOC))
  600 STRINGR = RUNNOC
      CALL SWORDS(STRINGR,IRN,NRN,LRN)
      STRINGE = EVTNOC
      CALL WORD(STRINGE,IEV,NEV,LEV)
      EVTNOC  =EVTNOC(IEV:NEV)//EXT3
C
C *** finding # of zeros to add before the evt.no.
C
      NEXT4   = 7 - LEV
      IF (NEXT4 .EQ. 2) THEN
        EXT4=EXT4(1:OO)//EXT4(1:OO)
      ELSEIF (NEXT4 .EQ. 3) THEN
        EXT4=EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)
      ELSEIF (NEXT4 .EQ. 4) THEN
        EXT4=EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)
      ELSEIF (NEXT4 .EQ. 5) THEN
        EXT4=EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)
      ELSEIF (NEXT4 .EQ. 6) THEN
        EXT4=EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)//EXT4(1:OO)
     &    //EXT4(1:OO)
      ENDIF
C
      CALL SWORDS(EXT4,LL,MM,OO)
      EVTNOC = EXT4(1:OO)//EVTNOC(1:TRULEN(EVTNOC))
      STRINGE = EVTNOC
      CALL SWORDS(STRINGE,IEV,NEV,LEV)
C
C *** Constructing file name
C
      FILENAME=STREAM(1:LST)//RUNNOC(1:LRN)//EVTNOC(1:LEV)
C&IF SIUNIX
C&      CALL CUTOL(FILENAME)
C&ENDIF
      STRINGFN=FILENAME
      CALL SWORDS(STRINGFN,IFN,NFN,LFN)
C
C *** EXT4 is used now setting it back to '0' again
C
      EXT4 = '0'
C
C *** Return a blank if no run number or event
C *** number entered
C
  100 IF ( OUT ) THEN
        FILENAME= ' '
        LFN = 0
      ENDIF
  999 RETURN
      END
