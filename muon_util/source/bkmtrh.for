C+
      SUBROUTINE BKMTRH(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MTRH'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-
C-   Outputs : 
C-      LADD    I   address of bank, MTRH.
C-
C-   Controls: 
C-
C-   Created  22-MAR-1990   KUNORI
C-   Updated  12-JUN-1991   Daria Zieminska: don't book if already booked
C-   Updated  11-SEP-1992   Alexander Efimov   NL,NS = 3
C-   Updated  8-SEP-1994   M. Fortner  Extend length to 15
C-   
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMTRH.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR,LMTRH,GZMTRH
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER GZPROC
      EXTERNAL GZPROC
C  -- initialize data...
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMTRH  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('MTRH',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=3                        ! NL (total number of links)
         MMBK(3)=3                        ! NS (number of struct. links)
         ND=15
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MTRH','13I 2F',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
         LSUP1=GZPROC(IDUM)
         IF(LSUP1.EQ.0) THEN
            CALL BKPROC(LSUP1)
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
      LMTRH=GZMTRH()
      IF (LMTRH.EQ.0) CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMTRH,MMBK,0)
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKMTRH  ',IREF)
      ENDIF
C
  999 RETURN
      END
