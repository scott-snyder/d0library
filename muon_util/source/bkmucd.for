      SUBROUTINE BKMUCD(NH,IMUON,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MUCD'.
C-
C-   Inputs  :
C-      NH      I   Number of cd hits
C-      IMUON   I   Pointer to address of support bank.
C-
C-   Outputs :
C-      LADD    I   address of bank, MUCD.
C-
C-   Controls:
C-
C-   Created  22-MAR-1990   KUNORI
C-   Modified 08-OCT-1990   S. ABACHI
C-   Modified 11-NOV-1991   S. ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMUCD.LINK'
C  -- variable in arguments...
      INTEGER IMUON,LADDR,NH
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER LZLAST
      EXTERNAL LZLAST
C  -- initialize data...
      DATA FIRST, MMBK/.TRUE., 0, 0, 0, 0, 0/
C----------------------------------------------------------------------
C
        IDUM=0            ! for dummy argument.
        ND = 19 + 7 * NH
C
      IF(FIRST) THEN
        CALL UCTOH('MUCD',MMBK(1),4,4)   ! IDH (hollerith bank name)
        MMBK(2)=1                        ! NL (total number of links)
        MMBK(3)=0                        ! NS (number of struct. links)
        MMBK(4)=ND                       ! ND (number of data words)
        CALL MZFORM('MUCD','5I13F1I/6F1I',MMBK(5))  ! NIO (bank format)
        FIRST=.FALSE.
      ENDIF
C
      MMBK(4)=ND
      LADDR=0
C
      IF(LRLINK(IMUON).EQ.0) THEN
        CALL ERRMSG('MUON','BKMUCD',
     +    'CANNOT BOOK MUCD, SUPPORT BANK ADDRESS IS 0.','F')
        GO TO 999
      ELSE
        LSUP1=LRLINK(IMUON)
      ENDIF
C
C  -- Book bank...
C
      LADDR=LZLAST(IXCOM,LQ(LSUP1-IZMUCD))
      IF(LADDR.EQ.0) THEN
        CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMUCD,MMBK,0)
C        -- reset bank number to 1...
        IQ(LADDR-5)=1
      ELSE
        CALL MZLIFT(IXMAIN,LADDR,LADDR,0,MMBK,0)
      ENDIF
C
      IQ(LADDR+1)=2      ! version number
C
  999 RETURN
      END
