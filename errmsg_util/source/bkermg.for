      SUBROUTINE BKERMG(LENSTR,LERMG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books error message bank ERMG
C-
C-   Inputs  : LENSTR -  Length of string in words
C-   Outputs : LERMG  -  Pointer to newly created ERMG bank
C-   Controls:
C-
C-   Created  11-APR-1992   Andrew J. Milder
C-   Updated  18-APR-1992   James T. Linnemann  books latest ermsg at front of
C-                                                linear chain 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZERMG.LINK'
      INTEGER LERMG,LHSTR,GZHSTR,NDATA,IOH,LINK,LENSTR,NFIX
      LOGICAL first
      SAVE FIRST
      DATA first / .TRUE. /
C----------------------------------------------------------------------
      NFIX = 5  !be sure to change MZFORM if you change this
      IF ( FIRST ) THEN
        CALL MZFORM('ERMG','5I -H',IOH)
        FIRST = .FALSE.
      END IF
      LINK = GZHSTR()
      IF (LINK .EQ. 0 ) CALL BKHSTR(LINK)
      NDATA = NFIX + LENSTR
      CALL MZBOOK(IXMAIN,LERMG,LINK,-IZERMG,'ERMG',1,1,NDATA,IOH,0)
      IQ(LERMG+1) = 1             ! Bank version number
      IQ(LERMG+2) = NFIX             ! length of fixed part of bank
C
  999 RETURN
      END
