      SUBROUTINE BKMSCT( LSUP, NDAT, LADDR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK BANK 'MSCT'
C-
C-   Inputs  :
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH
C-                    is taken.
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-                    this should be non-zero for this bank
C-   Outputs :
C-      LADD    I   address of bank, MSCT.
C-
C-   Controls:  NONE
C-
C-   Created   3-MAR-1992   Atsushi Taketani
C-   MF  10-93 Change default to 7040 data words (22 mods * 16 scin * 20)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMSCT.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER GZMUHT
      EXTERNAL GZMUHT
C  -- initialize data...
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMSCT  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('MSCT',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=0                        ! NL (total number of links)
         MMBK(3)=0                        ! NS (number of struct. links)
         ND=7040
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MSCT','/1I 1B 4I 14F',MMBK(5))  ! NIO 
         FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
         LSUP1=GZMUHT(IDUM)
         IF(LSUP1.EQ.0) THEN
            CALL BKMUHT(LSUP1)
         ENDIF
      ELSE
         LSUP1=LSUP
      ENDIF
C
      IF(NDAT.EQ.0) THEN
         MMBK(4)=ND
      ELSE
         MMBK(4)=NDAT
      ENDIF
C
C  -- Book bank...
C
      CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMSCT,MMBK,0)
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF)
         CALL RRLINK('BKMSCT  ',IREF)
      ENDIF
C
  999 RETURN
      END
