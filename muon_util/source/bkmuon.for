      SUBROUTINE BKMUON(LSUP,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'MUON'.
C-
C-   Inputs  :
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH
C-                    is taken.
C-   Outputs :
C-      LADDR    I   address of bank, MUON.
C-
C-   Controls:
C-
C-   Created  22-MAR-1990   KUNORI
C-   Modified 08-OCT-1990   S. ABACHI
C-   Modified 15-NOV-1990   S. ABACHI
C-   Modified 03-JUL-1991   S. ABACHI   Link to ZTRK added
C-   modified 03-DEC-1992   S. Kunori   incresed to 58 words from 55.
C-   Modified 15-NOV-1990   S. ABACHI   Vertex number added 58-->59 words
C-    DH 2/22/93 add reference link
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC/LIST'
      INCLUDE 'D0$LINKS:IZMUON.LINK/LIST'
C  -- variable in arguments...
      INTEGER LSUP,LADDR
C  -- local variables...
      INTEGER LSUP1,MMBK(5),IDUM
      LOGICAL FIRST
C  -- external...
      INTEGER GZMTRH,ISUP
      INTEGER LZLAST, NDAT
      EXTERNAL LZLAST
C  -- initialize data...
      SAVE NDAT, MMBK
      DATA FIRST, NDAT, MMBK/.TRUE., 63, 0 ,0, 0, 0, 0/
      INTEGER murefit,ier
C----------------------------------------------------------------------
C
      IDUM=0            ! for dummy argument.
C
      IF(LSUP .GT. 0) THEN
        CALL GRLINK('BKMUON', ISUP)
        LRLINK(ISUP) = LSUP
      ENDIF
C
      IF(FIRST) THEN
        CALL EZPICK('MURECO_RCP')
        CALL EZGET('MUREFIT',MUREFIT,IER)
        CALL EZRSET
        CALL UCTOH('MUON',MMBK(1),4,4)   ! IDH (hollerith bank name)
        MMBK(2)=14                       ! NL (total number of links)
        IF (MUREFIT.GE.2) THEN
          MMBK(3)=11                ! NS (number of struct. links)
        ELSE
          MMBK(3)=10                
        END IF
        MMBK(4)=NDAT                       ! ND (number of data words)
        CALL MZFORM('MUON','10I 52F-I',MMBK(5))  ! NIO (bank format)
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
C  -- Book bank...
C
      LADDR=LZLAST(IXCOM,LQ(LSUP1-IZMUON))
      IF(LADDR.EQ.0) THEN
        CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMUON,MMBK,0)
        IQ(LADDR-5)=1
      ELSE
        CALL MZLIFT(IXMAIN,LADDR,LADDR,0,MMBK,0)
      ENDIF
C
      IQ(LADDR+1)=2      ! version number
C
      IF(LSUP .GT. 0) THEN
        LSUP = LRLINK(ISUP)
        CALL RRLINK('BKMUON', ISUP)
      ENDIF
C
  999 RETURN
      END
