      FUNCTION OFFTIM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns D0 Standard time in seconds from a
C-                         base of 1 Jan 1990 = 0.
C-                         VAX specific.
C-
C-   Inputs  : None
C-   Outputs : 32 bit integer with number of seconds
C-   Controls: None
C-
C-   Created  28-JUN-1989   Jason McCampbell (MSU)
C-   Updated  10-DEC-1991   Herbert Greenlee
C-       UNIX compatible version.
C-   Updated  14-APR-1992   Jan S. Hoftun  ELN version 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER OFFTIM
      INTEGER*4 ITIM, IVAX(2)
      INTEGER ISTAT
C&IF VAXVMS
      INCLUDE '($SSDEF)'
      INCLUDE '($SYSSRVNAM)'
C&ELSEIF VAXELN
C&      INCLUDE 'ELN$:FORTRAN_DEFS.FOR'
C&ELSE
C&      INTEGER SYS$GETTIM
C&ENDIF
C----------------------------------------------------------------------
      IVAX(1)=0                       ! Clear time before calling
      IVAX(2)=0                       ! SYS$GETTIM.
C&IF VAXELN
C&      CALL KER$GET_TIME(ISTAT,IVAX)
C&ELSE
      ISTAT = SYS$GETTIM(IVAX)        ! Get current VAX time in 64 bit
C&ENDIF
C
C     Call the routine to shift to 32-bit notation and subtract the
C     base time for the current time.
      CALL VAXOFT(IVAX, ITIM)
C
      OFFTIM=ITIM
  999 RETURN
      END
