      SUBROUTINE BKSTRD(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create Bank STRD under either STPO, STPC or STPN 
C-                         decided by call to CPATHG. Use CPATHS to specify
C-                         whether to hang from 'STPO' or 'STPN'
C-
C-   Inputs  : None
C-   Outputs : LBANK = address of the created bank
C-                   = 0 if an error has occured.
C-   Controls: none
C-   NL = Number of Links
C-   NS = Number of Structural Links
C-   ND = Number of data words in bank STRD
C-
C-   Created  29-MAY-1989   Srini Rajagopalan
C-   Updated  13-DEC-1989   SR, Made Version Number = Integer. Set NIO=2
C-   Updated  01-AUG-1990   SR, Bug on LSTRD corrected.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
C
      INTEGER LBANK,LSTP
      INTEGER ND,NL,NS,NIO
C
      CHARACTER*4 PATH
      CHARACTER*80 MESAGE
C
      DATA NL,NS,ND,NIO /5,5,10,2/
C----------------------------------------------------------------------
C
      IF (LSTPH.EQ.0) THEN
        LBANK = 0
        GO TO 999
      ENDIF
C
      CALL CPATHG(PATH)                 ! get path name
      IF (PATH.EQ.'STPO') THEN
        LSTP = LC(LSTPH-IZSTPO)
      ELSE IF (PATH.EQ.'STPC') THEN
        LSTP = LC(LSTPH-IZSTPC)
      ELSE IF (PATH.EQ.'STPN') THEN
        LSTP = LC(LSTPH-IZSTPN)
      ELSE
        WRITE(MESAGE,10)PATH
        CALL ERRMSG('PATH','BKSTRD',MESAGE,'E')
      ENDIF
C
      IF (LSTP.EQ.0) THEN
        LBANK = 0                       ! top level bank does not exist
        GO TO 999
      ENDIF
C
C  Call to MZBOOK - Book STRD bank
C
      LSTRD = LC(LSTP - IZSTRD)
      IF (LSTRD.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LSTRD,LSTP,-IZSTRD,'STRD',NL,NS,ND,NIO,0)
      ENDIF
      LBANK=LSTRD
C
      IC(LBANK+10) = 1                   ! Version Number
C
   10 FORMAT(' PATH name ',A4,' is not valid ')
C
  999 RETURN
      END      
