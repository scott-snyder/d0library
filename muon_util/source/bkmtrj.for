      SUBROUTINE BKMTRJ(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MTRJ'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-      NDAT    I   number of data words
C-
C-   Outputs : 
C-      LADD    I   address of bank, MTRJ.
C-
C-   Controls: 
C-
C-   Created  22-MAR-1990   KUNORI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMTRJ.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER LZLAST
      EXTERNAL LZLAST
C  -- initialize data...
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMTRJ  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('MTRJ',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=0                        ! NL (total number of links)
         MMBK(3)=0                        ! NS (number of struct. links)
CC         ND=0
CC         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MTRJ','5I-F',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
          CALL ERRMSG('MUON','BKMTRJ',
     +    'CANNOT BOOK MTRJ, SUPPORT BANK ADDRESS IS 0.','F')
          GO TO 999
      ELSE
         LSUP1=LSUP
      ENDIF
C
      IF(NDAT.EQ.0) THEN
          CALL ERRMSG('MUON','BKMTRJ',
     +    'CANNOT BOOK MTRJ, NUMBER OF DATA WORDS IS 0.','F')
          GO TO 999
      ELSE
         MMBK(4)=NDAT
      ENDIF
C
C  -- Book bank...
C
      LADDR=LZLAST(IXCOM,LQ(LSUP1-IZMTRJ))
      IF(LADDR.EQ.0) THEN
         CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMTRJ,MMBK,0)
C        -- reset bank number to 1...
         IQ(LADDR-5)=1
      ELSE
         CALL MZLIFT(IXMAIN,LADDR,LADDR,0,MMBK,0)
      ENDIF
C
      IQ(LADDR+1)=1      ! version number
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKMTRJ  ',IREF)
      ENDIF
C
  999 RETURN
      END
