      INTEGER FUNCTION GZGCAH(ITRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to Zebra Bank GCAH # ITRA
C-
C-   Returned value  : GZGCAH = link to zebra bank
C-   Inputs  : ITRA = Track number & Bank number of GCAH bank
C-   Outputs :
C-   Controls:
C-
C-   Created  10-FEB-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ITRA
C
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! Main ZEBRA store
      INCLUDE 'D0$LINKS:IZGCAH.LINK'
C
      INTEGER LZFIND,LZLAST             ! Zebra utilities
      INTEGER LGHIT,GZGHIT
      INTEGER LGCAH,LGCAH0,LGCAHL
C----------------------------------------------------------------------
C
      GZGCAH = -1                       ! Flag as being past last bank
      LGHIT = GZGHIT()
      IF ( LGHIT.LE.0 ) GO TO 999
C
      LGCAH0 = LQ(LGHIT-IZGCAH)
      IF ( LGCAH0.GT.0 ) THEN
        LGCAH  = LZFIND(IXCOM,LGCAH0,ITRA,-5)
        IF ( LGCAH.LE.0 ) THEN          ! Bank not there, Why?
          CALL ZSORTI(IXCOM,LGCAH0,-5)  ! Sort on bank number
          LGCAHL = LZLAST(IXCOM,LGCAH0) ! Go to last bank
          IF ( IQ(LGCAHL-5).LT.ITRA ) THEN
            LGCAH = -1                  ! Flag as past highest bank number
          ENDIF
        ENDIF
        GZGCAH = LGCAH
      ENDIF
C
  999 RETURN
      END
