      REAL FUNCTION GM_EXAMPLE(SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Example of a GM COMIS routine.
C-
C-   Returned value  :
C-   Inputs  :
C-
C-   Created   5-NOV-1992   Harrison B. Prosper
C-   Updated   3-JAN-1994   Harrison B. Prosper
C-    Use GMPAW.INC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL SWITCH
C----------------------------------------------------------------------
      INCLUDE 'GM$PAW:GMPAW.INC'
C----------------------------------------------------------------------
      INTEGER I, J
      LOGICAL FIRST
      DATA FIRST  /.TRUE./
C----------------------------------------------------------------------
      GM_EXAMPLE = 1.0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        PRINT*,' GM_EXAMPLE: Started'
      ENDIF
C
C *********************************************************************
C ****  READ OBJECTS INTO BUFFERS
C *********************************************************************
C
      IOBJECT = IFIX(OBJECT)
C
      IF     ( IOBJECT .EQ. ID_VERT ) THEN
        NVERT  = ICOUNT
        NPHOT  = 0
        NELEC  = 0
        NMUON  = 0
        NTAU   = 0
        NJET   = 0
        NNU    = 0
        CALL UCOPY(PX,VERTEX(1,INUMBER),NSIZ)
C
      ELSEIF ( IOBJECT .EQ. ID_PHOT ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NPHOT   = ICOUNT
        CALL UCOPY(PX,PHOTON(1,INUMBER),NSIZ)
C
      ELSEIF ( IOBJECT .EQ. ID_ELEC ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NELEC   = ICOUNT
        CALL UCOPY(PX,ELECTRON(1,INUMBER),NSIZ)
C
      ELSEIF ( IOBJECT .EQ. ID_MUON ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NMUON   = ICOUNT
        CALL UCOPY(PX,MUON(1,INUMBER),NSIZ)
C
      ELSEIF ( IOBJECT .EQ. ID_TAU  ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NTAU    = ICOUNT
        CALL UCOPY(PX,TAU(1,INUMBER),NSIZ)
C
      ELSEIF ( IOBJECT .EQ. ID_JET  ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NJET    = ICOUNT
        CALL UCOPY(PX,JET(1,INUMBER),NSIZ)
C
      ELSEIF ( IOBJECT .EQ. ID_NU   ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NNU     = ICOUNT
        CALL UCOPY(PX,NU(1,INUMBER),NSIZ)
C
      ENDIF
      IF ( IOBJECT .NE. ID_END  ) THEN
        RETURN
      ENDIF
C
C *********************************************************************
C ****  DO ANALYSIS HERE
C *********************************************************************
C
      IRUN   = IFIX(RUN)
      IEVENT = IFIX(EVENT)
      PRINT*,' ----< EVENT >---- ',IRUN, IEVENT
C
      PRINT*,'    VERTICES'
      DO J = 1, NVERT
        PRINT*, J,(VERTEX(I,J),I=1,4)
      ENDDO
C
      IF ( NPHOT .GT. 0 ) THEN
        PRINT*,'    PHOTONS'
        DO J = 1, NPHOT
          PRINT*, J,(PHOTON(I,J),I=1,4)
        ENDDO
      ENDIF
C
      IF ( NELEC .GT. 0 ) THEN
        PRINT*,'    ELECTRONS'
        DO J = 1, NELEC
          PRINT*, J,(ELECTRON(I,J),I=1,4)
        ENDDO
      ENDIF
C
      IF ( NJET .GT. 0 ) THEN
        PRINT*,'    JETS'
        DO J = 1, NJET
          PRINT*, J,(JET(I,J),I=1,4)
        ENDDO
      ENDIF
C
      IF ( NNU .GT. 0 ) THEN
        PRINT*,'    MISSING_ET'
        DO J = 1, NNU
          PRINT*, J,(NU(I,J),I=1,4)
        ENDDO
      ENDIF
      PRINT*,'  '
C
      GM_EXAMPLE = 1.0
      END
