      SUBROUTINE BKSLV0(TREE,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create Bank SLV0 under TREE
C-
C-   Inputs  : TREE  = 'STPN','STPC','STPO'
C-   Outputs : LBANK = address of the created bank
C-                   = 0 if an error has occured.
C-   Controls: none
C-   NL = Number of Links
C-   NS = Number of Structural Links
C-   ND = Number of data words in bank SLV0
C-
C-   Created  10-DEC-1991   H.Xu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSLV0.LINK'
C
      INTEGER LBANK,LSTP
      INTEGER ND,NL,NS,NIO
C
      CHARACTER*4 TREE
C
      DATA NL,NS,ND/5,5,8/
C----------------------------------------------------------------------
C
      IF (LSTPH.EQ.0) THEN
        LBANK = 0
        GO TO 999
      ENDIF
C


      IF (TREE.EQ.'STPO') THEN
        LSTP = LC(LSTPH-IZSTPO)
      ELSE IF (TREE.EQ.'STPC') THEN
        LSTP = LC(LSTPH-IZSTPC)
      ELSE IF (TREE.EQ.'STPN') THEN
        LSTP = LC(LSTPH-IZSTPN)
      ENDIF
C
      IF (LSTP.EQ.0) THEN
        LBANK = 0                       ! top level bank does not exist
        GO TO 999
      ENDIF
C
C  Call to MZBOOK - Book SLV0 bank
C
      LSLV0 = LC(LSTP-IZSLV0)
      IF (LSLV0.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LSLV0,LSTP,-IZSLV0,'SLV0',NL,NS,ND,2,0)
      ENDIF
      LBANK=LSLV0
C
C
C
  999 RETURN
      END      
