      SUBROUTINE BKMUHP(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MUHP'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-                    this should be non-zero for this bank
C-   Outputs : 
C-      LADD    I   address of bank, MUHP.
C-
C-   Controls: 
C-
C-   Created  20-JUL-1993  M. Fortner   
C-
C-   Modified: 1/95 MF  Increase booked size for pure SAMUS MC events
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMUHP.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF,LMUD1,MUD1MX,ITMP
      LOGICAL FIRST
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
C  -- external...
      INTEGER GZMUHT,GZMUD1
      EXTERNAL GZMUHT,GZMUD1
C  -- initialize data...
      DATA FIRST/.TRUE./,MUD1MX/40000/
      SAVE FIRST,IDUM,ND,MMBK
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKMUHP  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('MUHP',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=0                        ! NL (total number of links)
         MMBK(3)=0                        ! NS (number of struct. links)
         ND=5
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MUHP','-I',MMBK(5))  ! NIO (bank format)
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
      IF(NDAT.EQ.0) THEN
         LMUD1 = GZMUD1(IDUM)
         IF (LMUD1.NE.0) THEN
           ITMP = IQ(LMUD1-1)
           IF (IDUM.GT.MUD1MX) THEN
             MESSID = 'BKMUHP: MUD1 too large'
  210        WRITE(MESSAG,215) ITMP
  215        FORMAT('Bank size = ',I12,' ')
             CALLER='BKMUHP'
             CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
             CALL MUHTFL(98,-1,ITMP)
             RETURN
           ELSE
             MMBK(4) = ITMP*5/3
           ENDIF
         ELSE
           MMBK(4) = ND
         ENDIF
      ELSE
         MMBK(4) = NDAT
      ENDIF
C
C  -- Book bank...
C
      CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMUHP,MMBK,0)
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKMUHP  ',IREF)
      ENDIF
C
  999 RETURN
      END
