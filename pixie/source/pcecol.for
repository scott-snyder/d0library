      SUBROUTINE PCECOL(ENER, EMIN, COLRNGE, ICOL, CCOL, NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine will determine the color that will be
C-   use in the cal cells using the energy boundries.  It also determine the
C-   labels per range that will displayed in the legend.   
C-
C-   Inputs  : ENER    - Energy
C-             EMIN    - Minimu energy alloud
C-             COLRNGE - Energy range to distribute the colors
C-             NUM     - Number of items in the legend 
C-             
C-   Outputs : ICOL    - Color  given to the cell
C-             CCOL    - Three letter character defining the color given
C-             COLORS  - Array with the colors for the legend (PCELAB)
C-             LABELS  - Array with the labels for the legend (PCELAB)
C-
C-   Updated  14-MAY-1992   Nobu. Oshima 
C-                        Add one more format for fine color range.
C-   Updated  20-JAN-1992   Nobu. Oshima 
C-                        Change the format for CMINRNG and CMAXRNG
C-   Updated  11-OCT-1991   Nobu. Oshima
C-                        Change the label of last line in PCELAB.
C-   Created  27-FEB-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ENER, EMIN, COLRNGE, MINRNG, MAXRNG, RANGE(6), SVCRNG
      INTEGER ICOL, COLORS(*), NUM, J, COL(5)
      CHARACTER*(*)LABELS(*)
      CHARACTER*(*)CCOL
      CHARACTER*3 ELAB, ELAB2
      CHARACTER*5 CMINRNG, CMAXRNG, CENER, CEMIN, CX
      DATA ELAB /'<E<'/
      DATA ELAB2/'<E '/
      DATA COL/6, 7, 9, 14, 13/  ! Blue, Cyan, Green, Magenta, Red
C----------------------------------------------------------------------
      SVCRNG   = COLRNGE
      RANGE(1) = EMIN 
      RANGE(2) = EMIN + COLRNGE
      DO 100 J=3, NUM
        RANGE(J) = EMIN + COLRNGE * (J-1)
  100 CONTINUE
      RANGE(6) = 99.
      IF ( ENER .LE. RANGE(2) ) THEN
        ICOL = COL(1)  ! Blue
        CCOL = 'BLU'
      ELSEIF ( ENER .LE. RANGE(3) ) THEN
        ICOL = COL(2)  ! Cyan
        CCOL = 'CYA'
      ELSEIF ( ENER .LE. RANGE(4) ) THEN
        ICOL = COL(3)  ! Green
        CCOL = 'GRE'
      ELSEIF ( ENER .LE. RANGE(5) ) THEN
        ICOL = COL(4)  ! Magenta
        CCOL = 'MAG'
      ELSE
        ICOL = COL(5)  ! RED
        CCOL = 'RED'
      ENDIF
      RETURN
C ***
C ***  Entry PCELAB  Setting Colors and Labels for legend
C *** 
      ENTRY PCELAB(COLORS,LABELS)
      DO 150 J = 1, 5 
        MINRNG = RANGE(J)
        MAXRNG = RANGE(J+1)
        IF (SVCRNG.GE.1. .AND. RANGE(1).GE.1.) THEN
          WRITE(CX,130) MINRNG 
        ELSE
          WRITE(CX,135) MINRNG 
        ENDIF
        READ(CX,140) CMINRNG 
C-
        IF (SVCRNG.GE.1. .AND. RANGE(1).GE.1.) THEN
          WRITE(CX,130) MAXRNG 
        ELSE
          WRITE(CX,135) MAXRNG 
        ENDIF
        READ(CX,140)CMAXRNG
        COLORS(J) = COL(J)
        IF ( J .EQ. 5) THEN
          LABELS(J) = CMINRNG//ELAB2
        ELSE
          LABELS(J) = CMINRNG//ELAB//CMAXRNG 
        ENDIF
  150 CONTINUE
C-
  130 FORMAT(F5.0)
  135 FORMAT(F4.1)
  140 FORMAT(A5)
C-
      END
