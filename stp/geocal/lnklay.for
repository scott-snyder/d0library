      SUBROUTINE LNKLAY(LCLGA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CONNECT CLGA LINKS TO CLAY BANK FOR 2ND
C-                         THROUGH 16TH MODULE AS REFERENCE LINKS.
C-                         THE FIRST MODULE IS ALREADY LINKED AS A
C-                         STRUCTURAL LINK.
C-
C-   Inputs  : LCLGA -     POINTER TO 1ST CLGA BANK
C-   Outputs : NONE
C-   Zebra Banks Modified: CLGA
C-
C-   Created  31-MAY-1988   Stephen Kahn, Esq.
C-   Revised  16-MAR-1989   Stephen Kahn  -- match NAME
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INTEGER LCLGA, L, NAME
C
      L = LC(LCLGA)
      NAME = IC(LCLGA + IGNAM)
  100 IF ( L .EQ. 0 ) GO TO 999
      IF (IC(L + IGNAM) .NE. NAME ) GO TO 150
      IF (LC(L-IZCLAY) .EQ.0 ) THEN     ! not set -- set it to 1st
C                                       ! module pointer
         LC(L - IZCLAY) = LC(LCLGA - IZCLAY)
      ELSE                              ! Error situation
         WRITE (5,145) L, LCLGA, LC(L-IZCLAY)
  145    FORMAT(' LNKLAY -- MODULE ALREADY HAS POINTER ',3I7)
      END IF
  150 L = LC(L)
      GO TO 100                         ! end of loop
C----------------------------------------------------------------------
  999 RETURN
      END
