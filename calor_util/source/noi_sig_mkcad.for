      SUBROUTINE NOI_SIG_MKCAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create signal event cad banks if they
C-                         do not exist, and deletes signal CAEP
C-                         banks if they exist.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-FEB-1992   Allen I. Mincer
C-   Updated  27-DEC-1993   Ian Adam  - Remove code which drops CAEP
C-    and only make signal CAD banks if DROP_SIG_CAD is false
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZCAD2.LINK/LIST'
      INTEGER LCAEP,GZCAEP,NRP,NCH,PT_CAEP,IETA,IPHI,ILYR
      INTEGER I
      REAL ENERGY
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C ***
C *** TEST if old CAD banks exist, if no, create new ones
C ***
      IF (.NOT.DROP_SIG_CAD) THEN

        IF ( LQ(LHEAD-IZCAD1).LE.0 .OR.
     &           LQ(LHEAD-IZCAD2).LE.0 ) THEN
          LCAEP=GZCAEP()
          IF (LCAEP.LE.0)THEN
            CALL ERRMSG('NOISY','NOI_SIG_MKCAD',
     &        ' NEITHER SIGNAL CAD NOR CAEP BANKS EXIST','W')
          ELSE
            IF(FIRST)THEN
C            CALL ERRMSG('NOISY','NOI_SIG_MKCAD',
C     &       ' CREATING SIGNAL CAD BANKS FROM CAEP BANKS','W')
              FIRST=.FALSE.
            ENDIF
            CALL NOI_CELL_ZERO
            NRP=IQ(LCAEP+2)
            NCH=IQ(LCAEP+3)
            DO I=1,NCH
              PT_CAEP=LCAEP+(I-1)*NRP
              CALL CAEP_INDICES(IQ(PT_CAEP+4),IETA,IPHI,ILYR) ! unpack address
              ENERGY=Q(PT_CAEP+5)
              CELL_EN(IETA,IPHI,ILYR)=
     &            CELL_EN(IETA,IPHI,ILYR)+ENERGY
            ENDDO
            CALL NOI_MKCAD(0,0)
            CALL NOI_CELL_ZERO
          ENDIF
        ENDIF

      ENDIF

  999 RETURN
      END
