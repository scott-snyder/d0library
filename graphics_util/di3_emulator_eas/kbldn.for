      SUBROUTINE KBLDN(INUM,STR)
      IMPLICIT NONE
      INTEGER BASE, JP1, JP2, JP3
      PARAMETER (BASE=37,JP1=BASE,JP2=JP1*BASE,JP3=JP2*BASE)
      INTEGER INUM, TEMP, IS0, IS1, IS2, IS3, STRL
      CHARACTER*(*) STR
      CHARACTER*(BASE) VALS
      DATA VALS/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$'/

      TEMP = INUM
      IS0  = TEMP / JP3
      TEMP = TEMP - (IS0 * JP3)
      IS1  = (TEMP / JP2) + 1
      TEMP = TEMP - ((IS1 - 1) * JP2)
      IS2  = (TEMP / JP1) + 1
      IS3  = TEMP - ((IS2 - 1) * JP1) + 1
      STR  = VALS(IS1:IS1)//VALS(IS2:IS2)//VALS(IS3:IS3)
      IF (INUM .GE. JP3) STR = VALS(IS0+1:IS0+1)//STR
      RETURN
      END
