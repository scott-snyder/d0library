      SUBROUTINE GET_QUARK_CONTENT(ID,J,K,L)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find quark content of a Particle, given its
C-                         ISAJET ID
C-
C-   Inputs  :
C-              ID      [I]     - Particle ID
C-   Outputs :
C-              J,K,L   [I]     - ID's of constituent quarks
C-   Controls:
C-
C-   Created   4-NOV-1994   Stan M. Krzywdzinski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER ID,IABSID,J,K,L
C----------------------------------------------------------------------
C
   10 CONTINUE
CCC      WRITE(*,'('' Type Particle ID: '',$)')
CCC      READ (*,'(I)') ID
CCC      WRITE(*,'(1X)')
C
      IABSID = IABS(ID)
      IF     (                           (IABSID .LT.    10)) THEN
        J = IABSID
        K = 0
        L = 0
      ELSEIF ((IABSID .GE.     10) .AND. (IABSID .LT.   100)) THEN
        J = 0
        K = 0
        L = 0
      ELSEIF ((IABSID .GE.    100) .AND. (IABSID .LT.  1000)) THEN
        J = IABSID/100
        K = MOD(IABSID/10,10)
        L = 0
      ELSEIF ((IABSID .GE.   1000) .AND. (IABSID .LT. 10000)) THEN
        J = IABSID/1000
        K = MOD(IABSID/100,10)
        L = MOD(IABSID/10,10)
      ELSE
        J = 0
        K = 0
        L = 0
      ENDIF
C
CCC      WRITE(*,'(1X, ''J, K, L = '', 3I5)') J,K,L
C
      GO TO 10
C
  999 RETURN
      END
