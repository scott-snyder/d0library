      SUBROUTINE HMATRIX_FILL(NBUF,BUFFER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the QUAN bank with event related
C-                         Quantities. The order of the quantities in the
C-                         Quan bank should be as specified in the
C-                         Hmatrix_rcp file. 1st the VISIBLE quantities(i.e
C-                         measurables) followed by INVISIBLE quantities
C-                         (i.e. quantities you want to predict in the data
C-                         but you know what they are in a Monte Carlo model.)
C-                         
C-   Entry Point: HMATRIX_FILL(Nbuf,Buffer)                      
C-
C-   Inputs  :
C-   Outputs : 
C-   Controls:
C-
C-   Created  22-DEC-1990   Rajendran Raja
C-   Updated  17-SEP-1991   joey thompson, Harrison B. Prosper
C-      Make a clean interface between user package and hmatrix package.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NBUF
      REAL    BUFFER(*)
C----------------------------------------------------------------------
      INTEGER NBUFFER
      INTEGER MAXBUF
      PARAMETER( MAXBUF = VIS_MAX + INVIS_MAX )
      INTEGER I
C----------------------------------------------------------------------
C
C ****  Fill current QUAN bank with data from internal buffer
C
      NBUFFER = MIN(NBUF,MAXBUF)
      IF ( NBUFFER .GT. 0 ) THEN
        CALL UCOPY(BUFFER,C(LQUAN+1),NBUFFER)
      ENDIF
C
  999 RETURN
      END
