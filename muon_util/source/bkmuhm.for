      SUBROUTINE BKMUHM(LSUP,MODID,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MUHM'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      MODID   I   module id number used as link number from MUHT
C-                    this should be non-zero for this bank
C-
C-   Outputs : 
C-      LADDR   I   address of bank, MUHM.
C-
C-   Controls: 
C-
C-   Created  12-JAN-1994  M. Fortner   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C  -- variable in arguments...
      INTEGER LSUP,MODID,LADDR
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF,NMOD
      LOGICAL FIRST
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
C  -- external...
      INTEGER GZMUHT
      EXTERNAL GZMUHT
C  -- initialize data...
      DATA NMOD/460/
      DATA FIRST/.TRUE./
      SAVE FIRST,IDUM,ND,MMBK
C----------------------------------------------------------------------
C
C     -- save support bank address...
C
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMUHM  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('MUHM',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=0                        ! NL (total number of links)
         MMBK(3)=0                        ! NS (number of struct. links)
         ND=22
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MUHM','12I 10B',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
         LSUP1=GZMUHT(IDUM)
         IF(LSUP1.EQ.0) THEN
            CALL BKMUHT(0,0,LSUP1)
         ENDIF
      ELSE
         LSUP1 = LSUP
      ENDIF
C
C  -- Book bank...
C
      IF(MODID.GE.10.AND.MODID.LE.NMOD) THEN
         CALL MZLIFT(IXMAIN,LADDR,LSUP1,-MODID,MMBK,0)
      ENDIF
C
C     -- restore support bank address...
C
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKMUHM  ',IREF)
      ENDIF
C
  999 RETURN
      END
