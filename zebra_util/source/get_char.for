      SUBROUTINE GET_CHAR(LOC,DCHAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS A 8 LETTER CHARACTER VARIABLE
C-                          BEGINNING AT LOCATION LOC
C-                          IN STORE /ZEBCOM/
C-
C-   Inputs  : LOC
C-   Outputs : DNUM 
C-   Controls: 
C-
C-   Created  22-DEC-1990   Rajendran Raja
C-   Modified Original DGET/DSET for Character variable  Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LOC
      CHARACTER*8 DCHAR
      CHARACTER*8 DTEMP
      INTEGER     TEMP(2)
      EQUIVALENCE (TEMP,DTEMP)
C----------------------------------------------------------------------
      TEMP(1) = IQ(LOC)
      TEMP(2) = IQ(LOC+ 1)
      DCHAR = DTEMP
      RETURN
C
      ENTRY SET_CHAR(LOC,DCHAR)
C
C ****  SETS A CHARACTER*8 VARIABLE AT LOCATION LOC IN STORE /ZEBCOM/
C
      DTEMP = DCHAR
      IQ(LOC) = TEMP(1)
      IQ(LOC + 1) = TEMP(2)
  999 RETURN
      END
