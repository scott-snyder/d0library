C----------------------------------------------------------------------
      SUBROUTINE DBMCTOH (STR,ARR,NS)
C----------------------------------------------------------------------
C- Just to copy strings to hollerith and virsa versa, in some kind of
C- machine independent way. 4 character pr. word is hardcoded. This is 
C- mainly for the DBMON CLASS keys (8:10) which, by mistage, is declared 
C- as integers. Maximum of 132 bytes can be converted.
C- 
C- DBMCTOH (STR,ARR,NS)  input:  STR   String to be converted.
C-                               NS    Number of character to be converted.
C-                       output: ARR   Hollorith array.
C- 
C- DBMHTOC (STR,ARR,NS)  input:  ARR   Hollorith array to be converted.
C-                               NS    Number of character to be converted
C-                       output: STR   String.
C-
C-   Created  16-OCT-1992   Lars Rasmussen
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER NS
      CHARACTER*(*) STR
      INTEGER ARR(*)
      INTEGER I,J
      CHARACTER*132 CHL1,CHL2
C----------------------------------------------------------------------
      CHL1 = STR
      DO I = 1,LEN(CHL1)/4
         J = 4*(I-1)
         CHL2(J+BYTE1:J+BYTE1) = CHL1(J+1:J+1)
         CHL2(J+BYTE2:J+BYTE2) = CHL1(J+2:J+2)
         CHL2(J+BYTE3:J+BYTE3) = CHL1(J+3:J+3)
         CHL2(J+BYTE4:J+BYTE4) = CHL1(J+4:J+4)
      END DO
      CALL UCTOH (CHL2,ARR,4,NS)
      RETURN
C----------------------------------------------------------------------
      ENTRY DBMHTOC (STR,ARR,NS)
C----------------------------------------------------------------------
      CALL UHTOC (ARR,4,CHL1,NS)
      DO I = 1,LEN(CHL1)/4
         J = 4*(I-1)
         CHL2(J+BYTE1:J+BYTE1) = CHL1(J+1:J+1)
         CHL2(J+BYTE2:J+BYTE2) = CHL1(J+2:J+2)
         CHL2(J+BYTE3:J+BYTE3) = CHL1(J+3:J+3)
         CHL2(J+BYTE4:J+BYTE4) = CHL1(J+4:J+4)
      END DO
      STR = CHL2
      RETURN
C
      END
