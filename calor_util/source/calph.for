      SUBROUTINE CALPH(IDATA,IPH)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Convert pulse height of raw calorimeter data
C                         word to a signed 32-bit integer.
C
C   Inputs  : IDATA    raw calorimeter data word from CADx
C   Outputs : IPH      pulse height from data word (signed 32-bit integer)
C
C   Created  25-OCT-1988   Wyatt Merritt
C   Updated  17-Mar-1992   Herbert Greenlee
C      Fix byte order.  Remove machine block.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDATA,IPH
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER NEWWRD
      INTEGER*2 HLFWRD(2)

      EQUIVALENCE (HLFWRD,NEWWRD)

C---------------------------------------------------------------------
      NEWWRD = IDATA

      IPH=HLFWRD(WORD1)

C
  999 RETURN
      END      
