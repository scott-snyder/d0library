      SUBROUTINE BKFGNS(LSUP,ND,LBANK,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create Forward Drift Chamber Gains bank 
C-
C-   Inputs  : LSUP = Address of the supporting bank
C-             ND = length of the created bank
C-   Output  : LBANK = address of the created bank
C-   Control : IFL = 0, Book FGNS with FGNH as supporting bank
C-             IFL = 1, Book FGNS as next bank in linear structure
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFGNS.LINK'
C----------------------------------------------------------------------
      CHARACTER*8 STIME
C
      INTEGER LBANK,LSUP,IFL
      INTEGER NL,NS,ND,NIO
      INTEGER JDATE,JTIME
      INTEGER I,J,K
      INTEGER LORUN,HIRUN,RUNNO
C
      DATA LORUN,HIRUN,RUNNO /0,9999,0/
      DATA NL,NS /2,2/
C     NL = Number of Links
C     NS = Number of Structural Links
C----------------------------------------------------------------------
C
      IF (LSUP.EQ.0) THEN
        LBANK = 0                       ! Supporting Bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZFORM('FGNS','30I -F',NIO)
      IF (IFL.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZFGNS,'FGNS',NL,NS,ND,NIO,0)
      ELSE
        CALL MZBOOK(IDVSTP,LBANK,LSUP,0,'FGNS',NL,NS,ND,NIO,0)
      ENDIF
C
C ****  Get date and time 
C
      CALL IDATE(I,J,K)
      JDATE = K + J*100 + I*10000
      CALL TIME(STIME)
      READ(UNIT=STIME,FMT='(I2,2(1X,I2))') I,J,K
      JTIME = K + J*100 + I*10000
C
      IC(LBANK+4)=LORUN                 ! Lowest valid run number
      IC(LBANK+5)=HIRUN                 ! Highest valid run number
      IC(LBANK+6)=RUNNO                 ! Current run number
      IC(LBANK+7)=JDATE                 ! Bank generation Date
      IC(LBANK+8)=JTIME                 ! Bank generation Time
C
C
  999 RETURN
      END      
