      SUBROUTINE GUTREV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : USER ROUTINE TO CONTROL TRACKING OF ONE EVENT
C-                         CALLED BY GRUN
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  ??            ??
C-   Updated  27-JUL-1988   A.M.Jonckheere
C-   Updated   5-JUN-1989   Harrison B. Prosper  :Added hook LUTREV
C-   Updated  20-SEP-1989   Alan M. Jonckheere   :Create noread BRR record here
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCLIST.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBIO.INC'
      INCLUDE 'D0$INC:ZEBIOG.INC'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
C DEFAULT VALUES
C      DATA IRDREC/0/,IWRREC/0/,IRDCHR/'I'/,IWRCHR/'O'/
C
      INTEGER IUCOMP,CHKBRR
      INTEGER HITS,DIGI,JXYZ
      DATA HITS/4hHITS/,DIGI/4hDIGI/,JXYZ/4hJXYZ/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER CHISAJ,CHBEG,CHGEAN,CHEVNT,NIO,NIOE,I
      INTEGER BINTIM(2),LISAE,ISTATS
C&IF SIUNIX,LINUX
C&      EXTERNAL TIME
C&ENDIF
C
C----------------------------------------------------------------------
C
C ****  Start timer
      IF ( DTRK.NE.2 .AND. PD0.GT.0 )
     &  CALL GTIMER('IO and setup time')
C
      IF ( FIRST ) THEN
        CALL UCTOH('GEAN',CHGEAN,4,4)
        CALL UCTOH('EVNT',CHEVNT,4,4)
        CALL UCTOH('ISAJ',CHISAJ,4,4)
        CALL UCTOH('BGN ',CHBEG,4,4)
        CALL MZFORM('HEAD','1I 2H 19I',NIO)
        CALL MZFORM('ISAE','10I 6F 2D',NIOE)
        CALL TIME(BINTIM)
C
C create Begin Run record even if not reading or writting them
        IF(LHEAD.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LHEAD,LHEAD,1,'HEAD',18,18,22,NIO,0)
          IQ(LHEAD+1)=1001
          IQ(LHEAD+2)=CHISAJ
          IQ(LHEAD+3)=CHBEG
          IQ(LHEAD+4) = BINTIM(1)
          IQ(LHEAD+5) = BINTIM(2)
          IQ(LHEAD+6) = IDRUN
          IQ(LHEAD+7) = 0
          IQ(LHEAD+8) = IDRUN
          IQ(LHEAD+9) = 0
          IQ(LHEAD+12) = IDRUN
C --initialize for I/O...
          IQ(LHEAD+2)=CHGEAN
          ISTATS = CHKBRR()
        ENDIF
        FIRST = .FALSE.
      ELSE
C ****  Reset GTIMER if not first call
        IF ( DTRK.NE.2 .AND. PD0.GT.0 )
     &    CALL RGTIMER('START TRACKING EVENT')
      ENDIF
C
      IF ( LHEAD.LE.0 ) THEN
C ****  create /ZEBCOM/ header bank
C ****  BOOK HEAD
        CALL MZBOOK(IXMAIN,LHEAD,LHEAD,1,'HEAD',18,18,22,NIO,0)
        CALL TIME(BINTIM)
        IQ(LHEAD+1) = 1005
        IQ(LHEAD+2) = CHGEAN
        IQ(LHEAD+3) = CHEVNT
        IQ(LHEAD+4) = BINTIM(1)
        IQ(LHEAD+5) = BINTIM(2)
        IQ(LHEAD+6) = IDRUN
        IQ(LHEAD+7) = IDEVT
        IQ(LHEAD+8) = IDRUN
        IQ(LHEAD+9) = IDEVT
        IQ(LHEAD+12) = IDRUN
C
C ****  BOOK ISAE
        CALL MZBOOK(IXMAIN,LISAE,LHEAD,-IZISAE,'ISAE',7,7,18,NIOE,0)
        IQ(LISAE+3) = IDEVT
        IQ(LISAE+4) = 10
C
      ENDIF
C
C     SKIP TRACKING/HITS IF HITS ARE READ FROM TAPE
C
      IF(NGET.EQ.0) GO TO 10
      IF( (IUCOMP(HITS,LGET,NGET).NE.0) .OR.
     &    (IUCOMP(DIGI,LGET,NGET).NE.0) .OR.
     &    (IUCOMP(JXYZ,LGET,NGET).NE.0) ) GO TO 999
   10 CONTINUE
C
C ************************
C ****  USER HOOK LUTREV
C ************************
      CALL LUTREV
C
      CALL GTREVE
  999 RETURN
      END
