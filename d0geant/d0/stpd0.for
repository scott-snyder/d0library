      LOGICAL FUNCTION STPD0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-                           Store the new tracks created in this step,
C-                           either in JKINE if they are in the Central
C-                           Detector, or in the stack.
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-JUN-1989   Harrison B. Prosper
C-   Code taken from pre-program-builder version of GUSTEP.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CENMV.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCKING.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCVOLU.INC/LIST'
C
      REAL    RXY
C
      INTEGER NUBUF, MVERT, NTRAC, I, K, TYPE
      PARAMETER(NUBUF=7)
      REAL UBUF(NUBUF)
C
      REAL    VERT1(3),PVERT1(4),BUF(NUBUF)
      INTEGER IPART1,NVERT1,NUBUFF
C
      INTEGER IPNT,ISKP
      DATA IPNT/0/,ISKP/4/
C----------------------------------------------------------------------
      STPD0 = .TRUE.
C
C ****  Store secondaries tracks created inside MCEN into JKINE.
C ****  Also, store end Vertex in JVERT if the current track has lost its
C ****  identity even has no secondaries.
C
      IF ( SSEC.GE.0 ) THEN
        IF ( (( ISTOP.GT.0 .AND. ISTAK.LE.0 ) .OR. NGKINE.GT.0 ) ) THEN
C
C ****  In MCEN + Beam Pipe?
          RXY = SQRT(VECT(1)**2 + VECT(2)**2)
          IF ( RXY.LE.MCEN(2) .AND. ABS(VECT(3)).LE.MCEN(3) ) THEN
C
C ****  Create a vertex even if no secondaries
            UBUF(1) = FLOAT(ISTOP)
            CALL GSVERT(VECT,ITRA,0,UBUF,1,MVERT)
C
C ****  The particle ITRA has lost its identity at the vertex MVERT:
C ****  Overwrite the user words of the current track ITRA
            IF ( ISTOP.GT.0 ) THEN
              CALL ISUBFN(ITRA,1,FLOAT(MVERT))
              IF ( NGKINE.EQ.0 ) THEN
                NGKINE = 1                         ! enter dummy track
                CALL UZERO(GKIN,1,4)
                GKIN(5,1) = 4                      ! make it a neutrino
                KCASE = 4hSTOP
              ENDIF
            ENDIF
C
C ****  Get UBUF (BUF here) for parent track
            CALL GFKINE(ITRA,VERT1,PVERT1,IPART1,NVERT1,BUF,NUBUFF)
C
C ****  Store secondary tracks in JKINE
            UBUF(1) = 0                 ! End vertext # - none yet
            UBUF(2) = ITRA              ! Parent track #
            CALL GET_TYPE_INTERACT(KCASE,TYPE)
            UBUF(3) = TYPE              ! Interaction type
C ****  zero values here signal tracks created in Geant!!!
            UBUF(4) = 0.                ! Starting Vertex #
            UBUF(5) = 0.                ! Current Track #
C ****
            UBUF(6) = 0.
            IF ( NUBUFF.GE.6 ) UBUF(6) = BUF(6) ! Parent Parton number
            UBUF(7) = 0.
            IF ( NUBUFF.GE.7 ) UBUF(7) = BUF(7) ! Parent Jet number
C
            DO I = 1, NGKINE
              CALL GSKINE(GKIN(1,I),IFIX(GKIN(5,I)),MVERT,UBUF,NUBUF,
     &        NTRAC)
            ENDDO
C
            NGKINE = 0
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
