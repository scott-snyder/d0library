      SUBROUTINE BKHMTR(NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank HMTR. NAME is the name assigned
C-   to the current HMATRIX. HMTR is created stand-alone and there can be
C-   more than one such bank in memory. Use HMATRIX_SET to select a given 
C-   Hmatrix and HMATRIX_RESET to go back to the previously selected one.
C-
C-   Inputs  : NAME     [C*]    Name identifying Hmatrix (32-chars. max.)
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-DEC-1990 14:17:30.58  Rajendran Raja
C-   Updated  22-JAN-1991   Harrison B. Prosper  
C-      Add Hmatrix name 
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER IXIO
      INTEGER NL,NS,ND
      PARAMETER( NL = 12 )
      PARAMETER( NS = 12 )
      PARAMETER( ND = 16 )
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:NHMATRIX.INC'
C
      CHARACTER*32 GIVEN_NAME
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('HMTR','2I 6F 8H',IXIO)        ! Describe Bank format
      ENDIF
C
C ****  Create stand-alone bank
C
      CALL MZBOOK(IDVSTP,LHMTR,0,2,'HMTR',NL,NS,ND,IXIO,0)
C
C ****  Insert Hmatrix name into bank
C
      GIVEN_NAME = NAME(1:LEN(NAME))
      CALL UPCASE(GIVEN_NAME,GIVEN_NAME)
      CALL DCTOH(32,GIVEN_NAME,IC(LHMTR+9))
C
C ****  Declare named Hmatrix to Hmatrix package
C
      CALL HMATRIX_NAME(GIVEN_NAME,LHMTR)
C
  999 RETURN
      END
