      SUBROUTINE GFTVER(IV,IT,ITRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For the IT'th track associated with vertex
C-                         IV, return the track # ITRA in the JKINE bank.
C-
C-   Inputs  : IV         Vertex #
C-             IT         Track # in generated tracks list
C-   Outputs : ITRA       Track # in JKINE
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
      INTEGER IV,IT,ITRA
      INTEGER JV,NTGEN
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
      NTGEN  = INT(Q(JV + 7))
      IF (IT.LE.0 .OR. IT.GT.NTGEN) THEN
        WRITE (CHMAIL,1002) IT
        CALL GMAIL(0,0)
        RETURN
      ENDIF
      ITRA = INT(Q(JV + 7 + IT))
C
  999 RETURN
C
 1000 FORMAT(' GFTVER: Vertex requested does not exist. IV = ',I10)
 1001 FORMAT(' GFTVER: No vertex bank!')
 1002 FORMAT(' GFTVER: Track # not associated with vertex')
      END
