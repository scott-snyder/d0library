      SUBROUTINE GFVER1(IV,VERT,TOFG,NTBEAM,NTTARG,NTGEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extracts information from vertex stack of
C-                         GEANT 3.14
C-
C-   Inputs  : IV         Vertex # to read from stack
C-   Outputs : VERT(3)    X,Y,Z of vertex
C-             TOFG       Time of flight to reach vertex
C-             NTBEAM     Track # in JKINE of beam track into vertex
C-             NTTARG     Track # in JKINE of target track (if any) at vertex
C-             NTGEN      # of tracks generated at this vertex ( A separate
C-                        utility GFTVER is provide to return the track #
C-                        in JKINE of each of these tracks.  Since this number
C-                        can be arbitrarily large, the track numbers cannot
C-                        be supplied with an array here.)
C-   Controls:
C-
C-   Created  19-DEC-1991   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCMAIL.INC'
      INCLUDE 'D0$INC:GCNUM.INC'
C
      INTEGER IV,NTBEAM,NTTARG,NTGEN
      INTEGER JV,I
C
      REAL VERT(3),TOFG
C
C----------------------------------------------------------------------
      IF (IV.LE.0 .OR. IV.GT.NVERTX) THEN
        WRITE (CHMAIL,1000) IV
        CALL GMAIL(0,0)
        RETURN
      ENDIF
C
      IF (JVERTX .EQ. 0) THEN
        WRITE (CHMAIL,1001)
        RETURN
      ENDIF
C
      JV = LQ(JVERTX - IV)
      DO 10 I = 1 , 3
        VERT(I) = Q(JV + I)
   10 CONTINUE
      TOFG = Q(JV + 4)
      NTBEAM = INT(Q(JV + 5))
      NTTARG = INT(Q(JV + 6))
      NTGEN  = INT(Q(JV + 7))
C
  999 RETURN
C
 1000 FORMAT(' GFVER1: Vertex requested does not exist. IV = ',I10)
 1001 FORMAT(' GFVER1: No vertex bank!')
      END
