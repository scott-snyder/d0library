      INTEGER FUNCTION GZGMUH (ITRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to Zebra Bank GMUH # ITRA
C-
C-   Returned value  : GZGMUH = link to zebra bank
C-   Inputs  : ITRA = Track number & Bank number of GMUH bank
C-   Outputs :
C-   Controls:
C-
C-   Created  9-APR-1993   Jasbir Singh , Chip Stewart (based on GZGCAH)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ITRA
C
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! Main ZEBRA store
      INCLUDE 'D0$LINKS:IZGMUH.LINK'

      INTEGER LZFIND,LZLAST             ! Zebra utilities
      INTEGER LGHIT,GZGHIT
      INTEGER LGMUH,LGMUH0,LGMUHL
C----------------------------------------------------------------------
C
      GZGMUH = -1                       ! Flag as being past last bank
      LGHIT = GZGHIT()
      IF ( LGHIT.LE.0 ) GO TO 999
C
      LGMUH0 = LQ(LGHIT-IZGMUH)
      IF ( ITRA.LE.0 ) THEN
        GZGMUH = LGMUH0
        GOTO 999
      ELSE IF ( LGMUH0.GT.0 ) THEN
        LGMUH  = LZFIND(IXCOM,LGMUH0,ITRA,-5)
        IF ( LGMUH.LE.0 ) THEN          ! Bank not there, Why?
          CALL ZSORTI(IXCOM,LGMUH0,-5)  ! Sort on bank number
          LGMUHL = LZLAST(IXCOM,LGMUH0) ! Go to last bank
          IF ( IQ(LGMUHL-5).LT.ITRA ) THEN
            LGMUH = -1                  ! Flag as past highest bank number
          ENDIF
        ENDIF
        GZGMUH = LGMUH
      ENDIF
C
  999 RETURN
      END
