      SUBROUTINE STORE_MCEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-                           Store the new tracks created in this step
C-                           in both stacks, if the current position is
C-                           inside the MCEN volume as defined in CENMV.INC
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-DEC-1991   K. Wyatt Merritt
C-   Updated  25-JAN-1993   K. Wyatt Merritt  Does not store secondaries
C-                                            created by electrons or 
C-                                            positrons when SHWG=3 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CENMV.INC/LIST'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCKING.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
C
      REAL    RXY
C
      INTEGER NUBUF, MVERT, NTRAC, I, K, TYPE
      PARAMETER(NUBUF=7)
      REAL UBUF(NUBUF)
C
      REAL    VERT1(3),PVERT1(4),BUF(NUBUF)
      INTEGER IPART1,NVERT1,NUBUFF
      INTEGER ITRASTO
C
      INTEGER IPNT,ISKP
      DATA IPNT/0/,ISKP/4/
C----------------------------------------------------------------------
C
C ****  Store secondaries tracks created inside MCEN into JKINE.
C ****  Also, store end Vertex in JVERT if the current track has lost its
C ****  identity even if it has no secondaries.
C
      IF (RAJA_SHLB .AND. (IPART.EQ.2 .OR. IPART.EQ.3)) RETURN
      IF (ISTOP.GT.0 .OR. NGKINE.GT.0 ) THEN ! Stops or creates secondaries
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
            DO 10 I =  1,  NUBUF
              BUF(I) = -1.
   10       CONTINUE
            BUF(1) = FLOAT(MVERT)
            CALL ISUBFN(ITRA,NUBUF,BUF)
C            IF ( NGKINE.EQ.0 ) THEN
C              NGKINE = 1                         ! enter dummy track
C              CALL UZERO(GKIN,1,4)
C              GKIN(5,1) = 4                      ! make it a neutrino
C              KCASE = 'STOP'
C            ENDIF
          ENDIF
C
C ****  Get UBUF (BUF here) for parent track
          CALL GFKINE(ITRA,VERT1,PVERT1,IPART1,NVERT1,BUF,NUBUFF)
C
C ****  Store secondary tracks in JKINE and JSTAK
          UBUF(1) = 0
          UBUF(2) = ITRA
          CALL GET_TYPE_INTERACT(KCASE,TYPE)
          UBUF(3) = TYPE
          UBUF(4) = 0.
          UBUF(5) = 0.
          IF ( NUBUFF.GE.6 ) THEN             ! Parent Parton number
            UBUF(6) = BUF(6)
          ELSE
            UBUF(6) = 0.
          ENDIF
          IF ( NUBUFF.GE.7 ) THEN             ! Parent Jet number
            UBUF(7) = BUF(7)
          ELSE
            UBUF(7) = 0.
          ENDIF
          ITRASTO =ITRA
          DO I = 1, NGKINE
            CALL GSKINE(GKIN(1,I),IFIX(GKIN(5,I)),MVERT,UBUF,NUBUF,
     &        NTRAC)
            IFLGK(I) = 2   !Record in JSTAK as a primary track with 
            ITRA = NTRAC   ! track # NTRAC
            CALL GSKING(I) !
            IFLGK(I) = 0   !Rezero this flag word because GEANT doesn't.
          ENDDO
          ITRA = ITRASTO
C         WRITE (40,1000)
C1000     FORMAT(///,' CALL SNAPSHOT FROM STORE_MCEN')
C         CALL SNAPSHOT(40)
          NGKINE = 0
        ENDIF
      ENDIF
C
  999 RETURN
      END
