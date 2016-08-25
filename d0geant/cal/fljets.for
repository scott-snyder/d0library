      SUBROUTINE FLJETS(LJET0,JETN,CELPOS,ECELL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FLJETS fills the JETS banks
C-
C-   Inputs  : LJET0     link to the first JETS bank
C-             JETN      jet number
C-             CELPOS(3) vector from the interaction vertex to the cell
C-             ECELL     energy deposited in the cell
C-   Outputs : None
C-   Controls:
C-
C-   Created  13-OCT-1988   Z. Wolf
C-   Updated  20-JAN-1989   Z. Wolf
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER LJET0
      INTEGER JETN
      REAL CELPOS(3)
      REAL ECELL
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C--   GEANT UNITS
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LJETS
      INTEGER I
      REAL MAGN
      REAL UVECT(3)

      integer ihjets/4HJETS/
C----------------------------------------------------------------------
C
C--   CHECK LJET0
      IF(LJET0.LE.0)THEN
        WRITE(LOUT,*)'FLJETS--> PROBLEM WITH LJET0'
        GOTO 999
      END IF
C
C--   GET LINK TO JETS BANK
      LJETS=LJET0
      IF(JETN.EQ.0)GO TO 10
      DO I=1,JETN
        LJETS=LQ(LJETS)
        IF(LJETS.LE.0)THEN
          WRITE(LOUT,*)'FLJETS--> PROBLEM WITH LJETS'
          GOTO 999
        END IF
      END DO
   10 CONTINUE
C
C--   CHECK LINK
      IF(IQ(LJETS-4).NE.ihJETS)THEN
        WRITE(LOUT,*)'FLJETS--> PROBLEM WITH LJETS'
        GOTO 999
      END IF
      IF(IQ(LJETS-5).NE.JETN)THEN
        WRITE(LOUT,*)'FLJETS--> BANK ID NOT JETN'
        GO TO 999
      END IF
C
C--   UNIT VECTOR FROM INTERACTION POINT TO CELL
      MAGN=SQRT(CELPOS(1)**2+CELPOS(2)**2+CELPOS(3)**2)
      IF(MAGN.EQ.0.)GO TO 999
      UVECT(1)=CELPOS(1)/MAGN
      UVECT(2)=CELPOS(2)/MAGN
      UVECT(3)=CELPOS(3)/MAGN
C
C--   ADD CELL'S ENERGY VECTOR TO TOTAL
      Q(LJETS+2)=Q(LJETS+2)+ECELL*UVECT(1)
      Q(LJETS+3)=Q(LJETS+3)+ECELL*UVECT(2)
      Q(LJETS+4)=Q(LJETS+4)+ECELL*UVECT(3)
C
C--   ADD CELL'S SCALAR ENERGY TO TOTAL
      Q(LJETS+5)=Q(LJETS+5)+ECELL
C
  999 RETURN
      END
