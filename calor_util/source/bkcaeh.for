      SUBROUTINE BKCAEH(NCH,LCAEH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-       NCH = number of channels
C-   Outputs : 
C-     LCAEH = pointer to created bank
C-
C-   Created  25-APR-1989   Serban D. Protopopescu
C-   Updated   6-APR-1993   Stan M. Krzywdzinski
C-     Increased size by 1 to accommodate sig**2(Et):
C-       version no. = 2
C-       NR = 13
C-   Updated  17-MAY-1993   Stan M. Krzywdzinski
C-     Increased size by 4 to accommodate the remaining elements of error
C-     matrix: sig**2(Ez), <dExdEy>, <dExdEz>, <dEydEz>
C-       version no. = 3
C        NR = 17
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCH,LCAEH
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEH.LINK'
      INTEGER NALOC,NR,IOH
      INTEGER LCAHT,GZCAHT
      LOGICAL FIRST
      SAVE FIRST,IOH
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('CAEH','3I/8F4I5F',IOH)
        FIRST=.FALSE.
      ENDIF
C
      LCAHT=GZCAHT()
      NR=17               ! repetition number
      NALOC=NCH*NR+3
      CALL MZBOOK(IXMAIN,LCAEH,LCAHT,-IZCAEH,
     $                    'CAEH',1,1,NALOC,IOH,-1)
      IQ(LCAEH+1)=3   ! version no.  
      IQ(LCAEH+2)=NR  ! repetition no.
      IQ(LCAEH+3)=NCH ! number of channels
  999 RETURN
      END
