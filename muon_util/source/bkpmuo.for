      SUBROUTINE BKPMUO(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'PMUO'.
C-
C-   Inputs  : 
C-      LSUP    I   address of support bank.
C-                    if 0, default support bank for current PATH 
C-                    is taken. 
C-      NDAT    I   number of data words
C-                    if 0, default number of data words is taken.
C-
C-   Outputs : 
C-      LADD    I   address of bank, PMUO.
C-
C-   Controls: 
C-
C-   Created  22-MAR-1990   KUNORI
C-   Modified 29-JAN-1991   S. ABACHI   Isolation added, Format corrected
C-   Updated  03-JUL-1991   S. ABACHI   Link to ZTRK added
C-   Updated  03-DEC-1991   S. ABACHI   new items, version 2
C-            14-OCT-1992   S. Kunori   incresed to 100 data words.
C-    DH 2/22/93    add 2 reference links
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC/LIST'
      INCLUDE 'D0$LINKS:IZPMUO.LINK/LIST'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER LSUP1,ND,MMBK(5),IDUM,IREF
      LOGICAL FIRST
C  -- external...
      INTEGER GZPARH
      EXTERNAL GZPARH
      INTEGER LZLAST
      EXTERNAL LZLAST
C  -- initialize data...
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C     -- save supprot bank address...
      IF(LSUP.NE.0) THEN
         CALL GRLINK('BKPMUO  ',IREF)
         LRLINK(IREF)=LSUP
      ENDIF
C
      IF(FIRST) THEN
         IDUM=0            ! for dummy argument.
         CALL UCTOH('PMUO',MMBK(1),4,4)   ! IDH (hollerith bank name)
         MMBK(2)=7                        ! NL (total number of links)
         MMBK(3)=1                        ! NS (number of struct. links)
         ND=100
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('PMUO','9I 34F 8I 2F 2I -F',MMBK(5))  ! NIO (bank format)
         FIRST=.FALSE.
      ENDIF
C
      LADDR=0
C
      IF(LSUP.EQ.0) THEN
         LSUP1=GZPARH()
         IF(LSUP1.EQ.0) THEN
            CALL BKPARH(LSUP1)
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
      LADDR=LZLAST(IXCOM,LQ(LSUP1-IZPMUO))
      IF(LADDR.EQ.0) THEN
         CALL MZLIFT(IXMAIN,LADDR,LSUP1,-IZPMUO,MMBK,0)
C        -- reset bank number to 1...
         IQ(LADDR-5)=1
      ELSE
         CALL MZLIFT(IXMAIN,LADDR,LADDR,0,MMBK,0)
      ENDIF
C
      IQ(LADDR+1)=2      ! version number
C
C     -- restore supprot bank address...
      IF(LSUP.NE.0) THEN
         LSUP=LRLINK(IREF) 
         CALL RRLINK('BKPMUO  ',IREF)
      ENDIF
C
  999 RETURN
      END
