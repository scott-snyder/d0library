      SUBROUTINE GOKING
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-            Stores kinematics of NEWLY GENERATED shower particle in
C-            stack bank.
C-            If Electron or Photon below cut transform to Geantino
C-         ==>Called by :  STPCAL
C-
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   ?-???-1986   Steve Linn
C-   Updated   8-MAR-1989   Chip Stewart   MADE TOFOLD REAL NUMBER
C-   Updated  23-AUG-1991   K. Wyatt Merritt  Make 3.14 compatible -
C-                          argument list and functionality of GSSTAK
C-                          have changed. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCKING.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
C
      LOGICAL EM,STORE
C
      INTEGER I,J
      INTEGER IPASTO
C
      REAL TOFSTO,VSTO(3),PSTO(3),E,P(4)
C-------------------------------------------------------------------
C
C ****   Store original contents of /GCKINE/
C
      IPASTO = IPART
      TOFSTO = TOFG
      DO 5 I = 1 , 3
        VSTO(I) = VERT(I)
        PSTO(I) = PVERT(I)
    5 CONTINUE
C
C ****   If the current track is EM and has fallen below HIECUT, stop
C ****   it and add a geantino of the same energy instead.
C
      IF     ( GETOT.LT.LOECUT .AND.
     &       IPART.LE.3 .AND.
     &         ISTOP.EQ.0 ) THEN
        DESTEP = DESTEP + GETOT
        ISTOP = 2
      ELSEIF ( GETOT.LT.HIECUT .AND.
     &       IPART.LE.3 .AND.
     &         ISTOP.EQ.0 ) THEN
        DO 10 I = 1 , 3
          VERT(I) = VECT(I)
          PVERT(I) = VECT(I+3)*VECT(7)
          P(I)=PVERT(I)
   10   CONTINUE
        P(4)=GETOT
        CALL ROTINO(P(1),DCUSER(5))
        IPART = 48
        ISTOP = 1
        CALL GSSTAK(0)
      ENDIF
C
C
C ****   Loop over secondaries generated at this step. Store all
C ****   non-EM tracks. For EM tracks, select which ones to store, 
C ****   according to LOECUT and HIECUT. changing the particle to type to
C ****   48 (geantino) for those below HIECUT which will be parametrized,
C ****   and dumping the energy of those below LOECUT at this step.
C ****   Then zero NGKINE to disable any later calls to track storage
C ****   routines.
C
      DO 100 I = 1 , NGKINE
        STORE = .TRUE.
        IPART = NINT(GKIN(5,I))
        EM = IPART.LE.3
        IF ( EM ) THEN
          E = GKIN(4,I)
          IF (E .LT. LOECUT) THEN
            STORE = .FALSE.
            DESTEP = DESTEP + E
          ELSE IF (E .LT. HIECUT) THEN
            IPART = 48
            CALL ROTINO(GKIN(1,I),DCUSER(5))
          ENDIF
        ENDIF
        IF (STORE) THEN
          TOFG = TOFSTO + TOFD(I)
          DO 50 J = 1 , 3
            VERT(J) = VECT(J)
            PVERT(J) = GKIN(J,I)
   50     CONTINUE
          CALL GSSTAK(0)
        ENDIF
  100 CONTINUE
      NGKINE = 0.
C
C ****   Restore the parameters of the track currently being followed.
C
      TOFG = TOFSTO
      IPART = IPASTO
      DO 105 I = 1 , 3
        VERT(I) = VSTO(I)
        PVERT(I) = PSTO(I)
  105 CONTINUE
C
  999 RETURN
      END
