C---------------------------------------------------------------
C  Extracted and emulated Cernlib routines used
C  in D0FLAVOR.FOR and DECODE_CMD_LINE.FOR
C---------------------------------------------------------------
C
      FUNCTION LENOCC (CHV)
C
C CERN PROGLIB# M507    LENOCC          .VERSION KERNFOR  4.21  890323
C ORIG. March 85, re-write 21/02/89, JZ
C
C-    Find last non-blank character in CHV
C
      CHARACTER    CHV*(*)
C
      N = LEN(CHV)
C
      DO 17  JJ= N,1,-1
      IF (CHV(JJ:JJ).NE.' ') GO TO 99
   17 CONTINUE
      JJ = 0
C
   99 LENOCC = JJ
      RETURN
      END
C
      FUNCTION INDEXC(STR,SSTR)
C
C CERN PROGLIB# M433    INDEXC          .VERSION KERNFOR  4.14  860211
C ORIG. 26/03/86 M.GOOSSENS/DD
C
C-    Find the leftmost position where substring SSTR does not match
C-    string STR scanning forward
C
      CHARACTER*(*) STR,SSTR
C
      LENS   = LEN(STR)
      LENSS  = LEN(SSTR)
C
      DO 10 I=1,LENS-LENSS+1
          IF (STR(I:I+LENSS-1).NE.SSTR) THEN
              INDEXC = I
                                         GO TO 999
          ENDIF
   10 CONTINUE
      INDEXC = 0
C
  999 END
C
      CHARACTER*(*) FUNCTION SPACES(STR,NSPACE)
C
C CERN PROGLIB# M433    SPACES          .VERSION KERNFOR  4.14  860211
C ORIG.  6/05/86 M.GOOSSENS/DD
C
C-    The function value SPACES returns the character string STR with
C-    leading blanks removed and each occurence of one or more blanks
C-    replaced by NSPACE blanks inside the string STR
C
      CHARACTER*(*) STR
C
      LENSPA = LEN(SPACES)
      SPACES = ' '
      IF (NSPACE.LT.0) NSPACE = 0
      IBLANK = 1
      ISPACE = 1
  100 INONBL = INDEXC(STR(IBLANK:),' ')
      IF (INONBL.EQ.0) THEN
          SPACES(ISPACE:) = STR(IBLANK:)
                                                    GO TO 999
      ENDIF
      INONBL = INONBL + IBLANK - 1
      IBLANK = INDEX(STR(INONBL:),' ')
      IF (IBLANK.EQ.0) THEN
          SPACES(ISPACE:) = STR(INONBL:)
                                                    GO TO 999
      ENDIF
      IBLANK = IBLANK + INONBL - 1
      SPACES(ISPACE:) = STR(INONBL:IBLANK-1)
      ISPACE = ISPACE + IBLANK - INONBL + NSPACE
      IF (ISPACE.LE.LENSPA)                         GO TO 100
  999 END
C
      SUBROUTINE CLTOU (STRNG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Emulate Cernlib routine CLTOU which converts
C-                         lower case strings to upper case.
C-
C-   Inputs  : STRNG      Input string.
C-   Outputs : STRNG      Output string
C-   Controls: None
C-
C-   Created   6-DEC-1991   Herbert Greenlee
C-   Modified 29-JUL-1992   Richard Mueller - Changed str_upcase function from
C-                                            unix lib to be subroutine cltou
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      CHARACTER*(*) STRNG
C----------------------------------------------------------------------
      DO 10 I=1,LEN(STRNG)
 4       IF(LGE(STRNG(I:I),'a') .AND. LLE(STRNG(I:I), 'z'))
     &        STRNG(I:I) = 
     &        CHAR(ICHAR(STRNG(I:I)) + ICHAR('A') - ICHAR('a'))
 10   CONTINUE
  999 RETURN
      END
C
      SUBROUTINE CUTOL (STRNG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Emulate Cernlib routine CUTOL which converts
C-                         upper case strings to lower case.
C-
C-   Inputs  : STRNG      Input string.
C-   Outputs : STRNG      Output string
C-   Controls: None
C-
C-   Created   6-DEC-1991   Herbert Greenlee
C-   Modified 29-JUL-1992   Richard Mueller - Changed str_upcase function from
C-                                            unix lib to be subroutine cutol
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      CHARACTER*(*) STRNG
C----------------------------------------------------------------------
      DO 10 I=1,LEN(STRNG)
 4       IF(LGE(STRNG(I:I),'A') .AND. LLE(STRNG(I:I), 'Z'))
     &        STRNG(I:I) = 
     &        CHAR(ICHAR(STRNG(I:I)) + ICHAR('a') - ICHAR('A'))
 10   CONTINUE
  999 RETURN
      END
