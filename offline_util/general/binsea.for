      SUBROUTINE BINSEA(IPATRN,LENARR,IARRAY,NSTEP,NOFF,JPATRN,JINDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do Binary Search of larray(lenarr) for word Iaddr
C-                              Look at every nstep words
C-
C-   Inputs  : IPATRN   = Pattern word to look for
C-             LENARR   = Length of search array
C-             IARRAY   = Array to search
C-             NSTEP    = Check every NSTEP words
C-             NOFF     = Offset from 0 for word to check
C-   Outputs : JPATRN   = Closest match to IPATRN
C-             JINDX    = Index into array of JPATRN
C-   Controls:
C-
C-   Created  30-NOV-1988   A.M.Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPATRN,LENARR,IARRAY(LENARR),NSTEP,NOFF
      INTEGER JPATRN,JINDX
      INTEGER JFIRST,JLAST
      INTEGER KINDX
C----------------------------------------------------------------------
      JFIRST = 0
      JLAST = INT(LENARR/NSTEP) - 1
  100 KINDX = INT((JLAST-JFIRST)/2) + JFIRST
      JINDX = KINDX*NSTEP
      JPATRN = IARRAY(JINDX+NOFF+1)
C
C ****  DONE?
      IF ( JFIRST.EQ.JLAST ) GOTO 999
C
      IF ( JPATRN-IPATRN ) 200,300,400
C
C ****  JPATRN .LT. IPATRN
  200 JFIRST = KINDX + 1
      GOTO 100
C
C ****  JPATRN .EQ. IPATRN
  300 GOTO 999
C
C ****  JPATRN .GT. IPATRN
  400 JLAST = KINDX
      GOTO 100
C
  999 RETURN
      END
