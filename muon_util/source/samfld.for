      SUBROUTINE SAMFLD ( NMAG, VECLOC, FIELD )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate value of magnetic field in the point
C-                         with local coordinartes VECLOC in SAMUS magnet
C-                         number NMAG 
C-
C- THIS PROGRAM NOW (TEMPORARY !!!) DO NOT USE MAGNETIC MAPS
C-
C-   Inputs  : NMAG - SAMUS magnet number
C-             VECLOC - local coordinates of the point in the magnet
C-   Outputs : FIELD - vector of magnetic field in the local system
C-   Controls: 
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-   Modified  3-JAN-1992   Susumu Igarashi  Change the sign of field
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMAG
      REAL    VECLOC(3),FIELD(3)
      REAL    RAD,FIELD_MAX
      DATA FIELD_MAX/20.0/
C----------------------------------------------------------------------
C
C ****  Calculate field
C
      RAD=SQRT(VECLOC(1)**2+VECLOC(2)**2)
      FIELD(1)=-FIELD_MAX * VECLOC(2)/RAD
      FIELD(2)= FIELD_MAX * VECLOC(1)/RAD
      FIELD(3)= 0.0
C
  999 RETURN
      END
