      SUBROUTINE BKMOTR(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MOTR'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-                    this should be non-zero for this bank
C-   Outputs : 
C-      LADD    I   address of bank, MOTR.
C-
C-   Controls: 
C-
C-   Created  1/15/93   kj
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMOTR.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...       
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
C  -- external...
      INTEGER GZMTRG
      EXTERNAL GZMTRG
C  -- initialize data...
      SAVE FIRST,IDUM,ND,MMBK
      DATA FIRST/.TRUE./ 
C----------------------------------------------------------------------
C
C     -- save support bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMOTR  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('MOTR',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=0                        ! NL (total number of links)
         MMBK(3)=0                        ! NS (number of struct. links)
         ND=8
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MOTR','-B',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
C                                       
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
         LSUP1=GZMTRG(IDUM)
         IF(LSUP1.EQ.0) THEN
            CALL BKMTRG(0,0,LSUP1)
         ENDIF
      ELSE
         LSUP1 = LSUP
      ENDIF
C                       
      IF(NDAT.EQ.0) THEN
        MMBK(4) = 128*5*8
      ELSE
        MMBK(4) = NDAT
      ENDIF
C
C  -- Book bank...
C
      CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMOTR,MMBK,0)
C
C     -- restore support bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKMOTR  ',IREF)
      ENDIF
C
  999 RETURN
      END
