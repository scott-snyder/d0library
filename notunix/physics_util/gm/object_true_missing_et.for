      SUBROUTINE OBJECT_TRUE_MISSING_ET(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object TRUE-MISSING-ET
C-
C-   Inputs  : IDX      [I]   Object Number:
C-                                          1 - neutrinos only
C-                                          2 - neutrinos and muons
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C-              ARRAY(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-   Controls:
C-
C-   Created   4-FEB-1993   Stan M. Krzywdzinski
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints 
C-   Updated   4-NOV-1993   Stan M. Krzywdzinski
C-     Better suppression of objects in NOBJ_TRUE_MISSING_ET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ISAJET_CODES.DEF'
C----------------------------------------------------------------------
      INTEGER I, II, NN
      INTEGER NUMBER, NTOTAL
      INTEGER LBANK, GZISAE
      INTEGER IDD, IDDABS, ISAJ_ID, CTRL
      REAL X(3), P(4), P_MISS_ISA(4), PT_MISS_ISA, THETA
C
      INTEGER IBUFFER(100)
      REAL    BUFFER(100)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM  =   9 )
      INTEGER JPX, JPY, JPZ, JE, JPT, JETA, JPHI, JDETA, JQUAL, JBASE
      PARAMETER( JPX = 1,
     &           JPY = JPX + 1,
     &           JPZ = JPY + 1,
     &           JE  = JPZ + 1,
     &           JPT = JE  + 1,
     &           JETA  = JPT   + 1,
     &           JPHI  = JETA  + 1,
     &           JDETA = JPHI  + 1,
     &           JQUAL = JDETA + 1,
     &           JBASE = JQUAL)
C----------------------------------------------------------------------
      SAVE NUMBER, NTOTAL
C----------------------------------------------------------------------
C
C ****  OBJECT: MISSING ET FROM ISAJET, I.E. TRUE MISSING ET
C
      II = MIN(IDX,NUMBER)
      NN = MIN(NMAX,NTOTAL)
      CALL VZERO(BUFFER,NN)
C
      IF ( II .GT. 0 ) THEN
C
C ****  FIXED PART
C
        CALL VZERO(P_MISS_ISA,4)
        CTRL = 1
        DO WHILE (CTRL .GT. 0)
          CALL GET_NEXT_PARTICLE(IDD,ISAJ_ID,X,P,CTRL)
          IDDABS = IABS(IDD)
          IF ( ( IDDABS .EQ. NUE  )               .OR.
     &         ( IDDABS .EQ. NUMU )               .OR.
     &         ( IDDABS .EQ. NUTAU)               .OR.
     &         ((IDDABS .EQ. MUON ) .AND. (II .EQ. 2)) ) THEN
            DO I = 1, 3
              P_MISS_ISA(I) = P_MISS_ISA(I) + P(I)
            ENDDO
          ENDIF
        ENDDO
        PT_MISS_ISA = P_MISS_ISA(1)**2+P_MISS_ISA(2)**2
        P_MISS_ISA(4) = SQRT(PT_MISS_ISA+P_MISS_ISA(3)**2)
        PT_MISS_ISA = SQRT(PT_MISS_ISA)
C
        CALL UCOPY(P_MISS_ISA(1),BUFFER(JPX),4)
        BUFFER(JPT) = PT_MISS_ISA
C
        CALL ETOETA(P_MISS_ISA,BUFFER(JPHI),THETA,BUFFER(JETA))
C
C ****  OTHER
C
      ENDIF
      CALL UCOPY(BUFFER,ARRAY,NN)
      RETURN
C
      ENTRY NOBJ_TRUE_MISSING_ET(NOBJS,NSIZE)
      NUMBER = 2
      CTRL = 1
      CALL VZERO(P,4)
      CALL GET_NEXT_PARTICLE(IDD,ISAJ_ID,X,P,CTRL)
      IF ( (CTRL .LE. 0) .AND. (P(4) .LE. 0.) ) THEN
        NUMBER = 0
      ENDIF
C
      NTOTAL= MINNUM
      NOBJS = NUMBER
      NSIZE = NTOTAL
C
      RETURN
      END
