      SUBROUTINE BKMUHT(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MUHT'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-
C-   Outputs : 
C-      LADD    I   address of bank, MUHT.
C-
C-   Controls: 
C-
C-   Created  12-SEP-1990   KUNORI
C-     DH add two links for SAMUS
C-   Updated  11-JUN-1991   Daria Zieminska: don't book if it already exists
C    Dh 2/92 add 2 links for MTRG and MSCT
C    MF 8/93 add link for MUHP
C-   MF 1/94 add links for MUHM banks
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMUHT.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR,GZMUHT,LMUHT
C  -- local variables...
      INTEGER LSUP1,NMOD,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER GZHITS
      EXTERNAL GZHITS
C  -- initialize data...
      DATA FIRST/.TRUE./
      DATA NMOD/460/
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMUHT  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM = 0                         ! for dummy argument.
         CALL UCTOH('MUHT',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2) = NMOD                   ! NL (total number of links)
         MMBK(3) = NMOD                   ! NS (number of struct. links)
         ND = 10+NMOD
         MMBK(4) = ND                     ! ND (number of data words)
         CALL MZFORM('MUHT','-I',MMBK(5)) ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
         LSUP1=GZHITS()
         IF(LSUP1.EQ.0) THEN
            CALL BKHITS(LSUP1)
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
      LMUHT=GZMUHT(IDUM)
      IF (LMUHT.EQ.0) CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMUHT,MMBK,0)
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKMUHT  ',IREF)
      ENDIF
C
  999 RETURN
      END
