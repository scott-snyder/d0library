      SUBROUTINE BKMUOT(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MUOT'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-
C-   Outputs : 
C-      LADD    I   address of bank, MUOT.
C-
C-   Controls: 
C-
C-   Created  22-MAR-1990   KUNORI
C-  dh 9/90 kill version number
C   DH 1/91 ADD SAMUS
C   AT 2/94 add scinti
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER GZMTRH
      EXTERNAL GZMTRH
      INTEGER LZLAST
      EXTERNAL LZLAST
C  -- initialize data...
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMUOT  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('MUOT',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=3                        ! NL (total number of links)
         MMBK(3)=3                        ! NS (number of struct. links)
         ND=27
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MUOT','7I-F',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
         LSUP1=GZMTRH(IDUM)
         IF(LSUP1.EQ.0) THEN
            CALL BKMTRH(0,0,LSUP1)
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
      LADDR=LZLAST(IXCOM,LQ(LSUP1-IZMUOT))
      IF(LADDR.EQ.0) THEN
         CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMUOT,MMBK,0)
C        -- reset bank number to 1...
         IQ(LADDR-5)=1
      ELSE
         CALL MZLIFT(IXMAIN,LADDR,LADDR,0,MMBK,0)
      ENDIF
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKMUOT  ',IREF)
      ENDIF
C
  999 RETURN
      END
