      SUBROUTINE EZ_GET_NEXT_ELEMENT(
     &  IPOINT,LOCAL,ITYPE,PARNAME,IVAL,CVAL,LVAL,ITYP,REM,VALPOINT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given pointer (IPOINT) return the Name,
C-   value, type and remark inside the given array and return the pointer to
C-   the next element within that array. The arrays LOCAL(i) and ITYPE(i)
C-   contain the values and types, respectively as stored in the RCP
C-   bank. NOTE: the type is stored in the upper word of ITYPE(i).
C-
C-   Inputs  : IPOINT  [I ]:    Pointer within LOCAL(i), ITYPE(i)
C-             LOCAL [I(*)]:    Array of values from RCP-bank
C-             ITYPE [I(*)]:    Array of types from RCP-bank
C-
C-   Outputs : IPOINT   [I ]:   Pointer to the next element in the array
C-             PARNAME  [C*]:   Parameter name within array
C-             IVAL     [I ]:   Parameter value (if real, logical or integer)
C-             CVAL     [C*]:   Parameter value (if character)
C-             LVAL     [I ]:   Length of the parameter value CVAL
C-             ITYP     [I ]:   Parameter type
C-             REM   [C*(*)]:   Parameter remark
C-             VALPOINT [I ]:   Pointer to the value of the element in the
C-                              array 
C-   Controls:
C-
C-   Created   9-APR-1991   Lupe Howell, Harry B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPOINT,IVAL,ITYP,VALPOINT
      INTEGER LOCAL(*)
      INTEGER ITYPE(*)
      INTEGER LVAL
      CHARACTER*(*) PARNAME
      CHARACTER*(*) CVAL
      CHARACTER*(*) REM
C
      CHARACTER*32 NAME
C
      INTEGER LENGTH
      INTEGER EZZSHFT
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
      CALL EZGETC2 (LOCAL,ITYPE,IPOINT,NAME,LENGTH) ! Get data
      PARNAME = NAME(1:LENGTH)
      ITYP  = EZZSHFT(ITYPE(IPOINT),-NBITS)
C
C ****  Store the pointer to the value of the parameter if SET
C
      VALPOINT = IPOINT
C
C ****  Check TYPE of value (could be a character string greather than size 4)
C
      IF( ITYP .GT.VTCHR ) THEN
        IVAL = LOCAL(IPOINT)
        CALL EZGETC2 (LOCAL,ITYPE,IPOINT,NAME,LENGTH) ! Get data
        LVAL = LENGTH
        CVAL = NAME(1:LENGTH)
      ELSE
        IVAL = LOCAL(IPOINT)
        IPOINT = IPOINT + 1       ! Move to start of remark
      ENDIF
C
C ****  Get remark
C
      CALL EZGETC2 (LOCAL,ITYPE,IPOINT,NAME,LENGTH) ! Get data
      REM  = NAME(1:LENGTH)
  999 RETURN
      END
