      SUBROUTINE D0HCMP_GET_SUBDIRS(LSUBDIRS,SSUBDIRS,NLSUB,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : obtain names of directories specified and no of
C-                         levels
C-                         ONE is level 1
C-                         ONE/TWO is level 2 etc..., so
C-                         if subdirs=ONE/TWO the output will be:
C-                         ONE
C-                         ONE/TWO
C-                         2
C-
C-   Inputs  : LSUBDIRS string specifying the directory
C-   Outputs : SSUBDIRS split subdirectories
C-             NLSUB number of subdir levels
C-             IER error code 0 = OK
C-
C-   Controls: none
C-
C-   Created  24-MAR-1992   Krzysztof L. Genser
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:D0HCMP.DEF/LIST'
      CHARACTER*255 LSUBDIRS,SSUBDIRS(MAXDIRL)
      INTEGER NLSUB

      INTEGER LENSUBD
      INTEGER LSLASH,ISLASH

C----------------------------------------------------------------------
      IER = 0

      LENSUBD = LENOCC(LSUBDIRS)

      LSLASH = 0
      NLSUB = 0

 1001 CONTINUE

      ISLASH=INDEX(LSUBDIRS(LSLASH+1:LENSUBD), '/')
      NLSUB = NLSUB + 1

      IF ( ISLASH.EQ.0 ) THEN
        SSUBDIRS(NLSUB) = LSUBDIRS(1:LENSUBD)
        RETURN
      ELSE
        SSUBDIRS(NLSUB) = LSUBDIRS(1:ISLASH-1)
        LSLASH = ISLASH
        GOTO 1001
      ENDIF

      END
