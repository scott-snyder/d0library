      SUBROUTINE BKMRFT(LSUP,LREFF,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book muon refitting bank
C-
C-   Inputs  : LSUP = address of support bank
C-             NDAT = number of data works to book.  If NDAT=0,
C-                    default is taken
C-
C-   Outputs : LADDR = address of new bank, MRFT
C-
C-   Created  17-JAN-1994   Darien R. Wood
C-  modified  21-JUN-1995   RE Hall; add struc link for muot
C-                          MMBK(3)=4
C-   Updated   3-SEP-1995   Andrei Mayorov, supp. bank changed to MTRH, add
C-                                          refference from MUON
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMRFT.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IREF,IDUM,LMRFT,LREFF,ISUP
      LOGICAL FIRST
C  -- external functions
      INTEGER GZMUON,GZMTRH,LZLAST
      EXTERNAL GZMUON,GZMTRH,LZLAST
C  -- initialize data...
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      CALL GRLINK('BKMRFT1',IREF)
      LRLINK(IREF)=LREFF
      IF(LSUP.NE.0) THEN
        CALL GRLINK('BKMRFT  ',ISUP)
        LRLINK(ISUP)=LSUP
      ENDIF
C
      IF(FIRST) THEN
        IDUM=0            ! for dummy argument.
        CALL UCTOH('MRFT',MMBK(1),4,4)   ! IDH (hollerith bank name)
        MMBK(2)=4                        ! NL (total number of links)
        MMBK(3)=4                        ! NS (number of struct. links)
        ND=5
        MMBK(4)=ND                       ! ND (number of data words)
        CALL MZFORM('MRFT','-I',MMBK(5))  ! NIO (bank format)
        FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(NDAT.EQ.0) THEN
        MMBK(4)=ND
      ELSE
        MMBK(4)=NDAT
      ENDIF
C
      IF(LSUP.EQ.0) THEN
        LSUP1=GZMTRH(0)
      ELSE
        LSUP1 = LSUP
      ENDIF
      LMRFT=LQ(LSUP1-IZMRFT)
      IF (LMRFT.EQ.0)THEN
        CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZMRFT,MMBK,0)
      ELSE
        LSUP1=LZLAST(IXMAIN,LMRFT)
        IF(LSUP1.EQ.0) LSUP1=LMRFT
        CALL MZLIFT(IXMAIN,LADDR,LSUP1,0,MMBK,0)
      ENDIF
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
        LSUP=LRLINK(ISUP)
        CALL RRLINK('BKMRFT  ',ISUP)
      ENDIF
      LREFF=LRLINK(IREF)
      CALL RRLINK('BKMRFT1',IREF)
      LQ(LREFF-14)=LADDR
C
  999 RETURN
      END
