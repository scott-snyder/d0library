      SUBROUTINE PLOT_ISA_GCAH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PLOTS ISAJET TRACK AND ASSOCIATED GCAH TRACKS
C-                         AND CALORIMETER CELLS
C-
C-   Inputs  : LISP1, LGCAH
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAY-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INTEGER LISAE,LISV1,LISP1,LJET
      EQUIVALENCE (CSTLNK(31),LISAE),(CSTLNK(32),LISV1)
      EQUIVALENCE (CSTLNK(33),LISP1),(CSTLNK(34),LJET)
C
      LOGICAL NOMORE
      INTEGER GZISAE,GZISV1
      LOGICAL FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL PUINIT
      ENDIF
C
      ENTRY PC3DCL
      CALL PLOT_ISA_TRACK
      CALL PUOPEN
      LGCAH = LQ(LISP1-5)           ! GCAH LINK.
      DO WHILE (LGCAH.NE.0)
        CALL PLOT_GCAH            ! PLOT GCAH TRACK AND HITS
        LGCAH = LQ(LGCAH-1)
      ENDDO
      CALL JRCLOS
C-
C--- If device had not a rotation capability, just return...(Nobu. 140290)
      CALL PU3ROT
C-
  999 RETURN
      END
