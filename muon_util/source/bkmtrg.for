      SUBROUTINE BKMTRG(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MTRG'.
C-
C-   Inputs  :
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH
C-                    is taken.
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-   Outputs :
C-      LADDR   I   address of bank, MTRG.
C-
C-   Controls:
C-
C-   Created  			2/26/92  d. fein 
C-   Upated for run 1b (v2.0)   1/15/93  kj
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMTRG.LINK'
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
      SAVE FIRST,IDUM,ND,MMBK
C----------------------------------------------------------------------
C
C     -- save support bank address...
      IF(LSUP.NE.0) THEN
        CALL GRLINK('BKMTRG  ',IREF)
        LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
        IDUM=0            ! for dummy argument.
        CALL UCTOH('MTRG',MMBK(1),4,4)   ! IDH (hollerith bank name)
        MMBK(2)=1                        ! NL (total number of links)
        MMBK(3)=1                        ! NS (number of struct. links)
        ND=445                           ! number of words in bank 
        MMBK(4)=ND                       ! ND (number of words in bank)
        CALL MZFORM('MTRG','-B',MMBK(5))  ! NIO (bank format)
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
      CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMTRG,MMBK,0)
C
C     -- restore support bank address...
      IF(LSUP.NE.0) THEN
        LSUP=LRLINK(IREF)
        CALL RRLINK('BKMTRG  ',IREF)
      ENDIF
C
  999 RETURN
      END
