      SUBROUTINE PFGET(OUTSTR)
C-   Purpose and Methods : Get current PF-key labels from bottom line of
C-                         display
C-   Outputs : OUTSTR: Array of PF-key labels
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
      CHARACTER*(*) OUTSTR(4)
      DO 10 I=1,4
        OUTSTR(I)=PFSTR(I)
   10 CONTINUE
      RETURN
      END
