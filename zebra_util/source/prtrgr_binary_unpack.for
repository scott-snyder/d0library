      CHARACTER*35 FUNCTION PRTRGR_BINARY_UNPACK(ADDRESS, DBLOCK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate a string of 0's and 1's from a series of
C-   Level 1 Datablock words. This string includes imbedded spaces for
C-   readability. EX: FEF0FEF0 => '00001111 01111111 00001111 01111111'
C-
C-   Returned value  : The generated string.
C-   Inputs  : ADDRESS  The address within the Level 1 Datablock. 
C-                      See D0 Note 967.
C-             DBLOCK   The Level 1 Datablock Array.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER JBIT
      EXTERNAL JBIT
C
      INTEGER ADDRESS
      INTEGER DBLOCK(1:*)
C
      INTEGER POSITION
      INTEGER BIT
      INTEGER VALUE
      INTEGER ISTAT
C
      INTEGER LONGWORD_LENGTH, BYTE_LENGTH
      PARAMETER (BYTE_LENGTH = 8, LONGWORD_LENGTH = 4*BYTE_LENGTH)
C
      PRTRGR_BINARY_UNPACK = ' '
      CALL PRTRGR_FIRST_BYTE_DECODING(4, ADDRESS, DBLOCK, VALUE)
      WRITE (PRTRGR_BINARY_UNPACK,100,IOSTAT=ISTAT) 
     &            (JBIT(VALUE, BIT), BIT = 1, LONGWORD_LENGTH)
  100 FORMAT(8I1, 3(' ', 8I1))
C----------------------------------------------------------------------
  999 RETURN
      END
