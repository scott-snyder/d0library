      SUBROUTINE BKSAHH(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book zebra bank 'SAHH'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-
C-   Outputs : 
C-      LADD    I   address of bank, SAHH.
C-
C-   Controls: 
C-
C-   Created  12-MAR-1991   O. Eroshin
C-   Modified  5-SEP-1991   M. Fortner remove bank filling
C-    dh 10/91 change mzpush
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSAHH.LINK'
      INCLUDE 'D0$LINKS:IZMUHT.LINK'
C  --
      INTEGER    N_STATION,N_PLANE
      PARAMETER (N_STATION = 6, N_PLANE = 3)
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,LMUD1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER  GZSAHH,GZMUHT,GZMUD1,GZHITS
      EXTERNAL GZSAHH,GZMUHT,GZMUD1,GZHITS
C  -- initialize data...
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
         IDUM    = 0                       ! for dummy argument.
         CALL UCTOH('SAHH',MMBK(1),4,4)    ! IDH (hollerith bank name)
         ND      = N_STATION*N_PLANE+N_STATION
         MMBK(2) = ND+1                    ! NL (total number of links)
         MMBK(3) = ND+1                    ! NS (number of struct. links)
         MMBK(4) = ND                      ! ND (number of data words)
         CALL MZFORM('SAHH','-I',MMBK(5))  ! NIO (bank format)
         FIRST   = .FALSE.
      ENDIF
C
      LADDR      = 0
C
      IF(LSUP.EQ.0) THEN
        LSUP1    = GZHITS(IDUM)
        IF (LSUP1.NE.0) LSUP1 = LQ(LSUP1-IZMUHT)
        IF(LSUP1.EQ.0) THEN
          CALL BKMUHT(0,0,LSUP1)
        ENDIF
      ELSE
         LSUP1   = LSUP
      ENDIF
C
      IF(NDAT.EQ.0) THEN
         MMBK(4) = ND
      ELSE
         MMBK(4) = NDAT
      ENDIF
C
C  -- Book bank...
C
      IF (IQ(LSUP1-2).LT.IZSAHH) THEN
        CALL MZPUSH(IXCOM,LSUP1,IZSAHH-IQ(LSUP1-2),0,' ')
      END IF
      CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZSAHH,MMBK,0)
C
  999 RETURN
      END
