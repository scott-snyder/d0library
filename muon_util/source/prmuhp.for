      SUBROUTINE PRMUHP(PRUNIT,LMUHP,NMUHP,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out MUHP; pointers to MUD1 hits
C-
C-   Inputs  :  PRUNIT - Unit number for printout
C-              LMUHP - Bank address
C-              NMUHP - Bank number
C-              CFL - Flag to control print out (dummy)
C-              IFL - How much to print
C-
C-   Outputs :  None
C-
C-   Created :  1-JAN-95  M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N_MUHP
      PARAMETER (N_MUHP=5)
      INTEGER PRUNIT,LMUHP,NMUHP,IFL,LFLAG,LHIT
      CHARACTER CFL*(*)
      INTEGER LZLOC,NHIT,I,J,K,GZMUHP
      EXTERNAL GZMUHP
C----------------------------------------------------------------------
      LFLAG=LMUHP
      IF(LFLAG.EQ.0) THEN
        LFLAG=GZMUHP(0)
        IF(LFLAG.EQ.0) THEN
          WRITE(PRUNIT,100)
 100      FORMAT(' NO MUHP BANK')
          RETURN
        ENDIF
      ENDIF
C
      WRITE(PRUNIT,101)
101   FORMAT('0',10X,' BANK MUHP: MUON RAW HIT POINTERS '//
     A '    MUHP    CELL    LATCH     LOCATION      NEXT'/
     B '    ENTRY   NUMBER  BITS    EVEN    ODD     HIT'/)
C
      NHIT = IQ(LFLAG-1)/N_MUHP
      K = LFLAG
      DO I = 1,NHIT
        WRITE(PRUNIT,102) I,(IQ(K+J),J=1,5)
 102    FORMAT(1X,6(I7,1X))
        K = K + N_MUHP
      ENDDO
C
      RETURN
      END
