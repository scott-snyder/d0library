      SUBROUTINE BKLPDH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create Bank LPDH under SLV0
C-
C-   Inputs  : NONE
C-   Outputs : LBANK = address of the created bank
C-                   = 0 if an error has occured.
C-   Controls: none
C-   NL = Number of Links
C-   NS = Number of Structural Links
C-   ND = Number of data words in bank LPDH
C-
C-   Created  10-DEC-1991   H.Xu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C      INCLUDE 'D0$LINKS:IZLPDH.LINK'
      INCLUDE 'D0$LINKS:IZSLV0.LINK'
C
      INTEGER LBANK,IZLPDH/1/
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
C  Call to MZBOOK - Book LPDH bank
C
      LLPDH = LC(LSLV0-IZLPDH)
      IF (LLPDH.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LLPDH,LSLV0,-IZLPDH,'LPDH',NL,NS,ND,NIO,0)
      ENDIF
      LBANK=LLPDH
C
C
C
  999 RETURN
      END      
