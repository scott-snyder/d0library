      SUBROUTINE BKLDTH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create Bank LDTH under SLV0
C-
C-   Inputs  : NONE
C-   Outputs : LBANK = address of the created bank
C-                   = 0 if an error has occured.
C-   Controls: none
C-   NL = Number of Links
C-   NS = Number of Structural Links
C-   ND = Number of data words in bank LDTH
C-
C-   Created  10-DEC-1991   H.Xu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSLV0.LINK'
C
      INTEGER LBANK,IZLDTH/5/
      INTEGER ND,NL,NS,NIO
C
      DATA NL,NS,ND,NIO/1,1,8,2/
C----------------------------------------------------------------------
C
      IF (LSLV0.EQ.0) THEN
        LBANK = 0                            ! SLV0 do not exist
        GO TO 999
      ENDIF
C

C
C  Call to MZBOOK - Book LDTH bank
C
      LLV(1) = LC(LSLV0-IZLDTH)
      IF (LLV(1).EQ.0) THEN
        CALL MZBOOK(IDVSTP,LLV(1),LSLV0,-IZLDTH,'LDTH',
     *              NL,NS,ND,NIO,0)
      ENDIF
      LBANK=LLV(1)
C
C
C
  999 RETURN
      END      
