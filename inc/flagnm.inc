C----------------------------------------------------------------------
C-   Filename  FLAGNM.INC
C-   Purpose   Storage for flag names; see FLAG facility.
C-             NOTE: MAXFLG is defined in FLAGS.INC.
C-   Created   9-JUL-1988   Harrison B. Prosper
C----------------------------------------------------------------------
      INTEGER MAXCHR
      PARAMETER( MAXCHR = 16 )
      CHARACTER*16 NAMFLG(MAXFLG)
      COMMON /FLAGNM/ NAMFLG
