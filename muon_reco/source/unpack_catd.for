      SUBROUTINE UNPACK_CATD(ETA,PHI,CHAR,ARRAY)
C----------------------------------------------------------------------
C-   Purpose: Unpack CATD bank.
C-
C-   Author:  C. R. Murphy
C-   Created:  6 Jun 1993
C-   Revised: 22 Jul 1993 Gene Álvarez - to calculate energy about a
C-                        given cone size.
C-             3 Dec 1993 Modified to conform with D0 standards.
C-   Updated  17-DEC-1993   Ian Adam  - add check on NTWRS<MXTWRS
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF/LIST'

      INTEGER ITYPE,EMPNT,HADPNT,MUPNT,PNT,IERR
      INTEGER I,J,LCATD,GZCATD,JBYT
      INTEGER MXTWRS,NTWRS,NMUTWRS,NEMTWRS,NHADTWRS
      PARAMETER (MXTWRS = 500)
      INTEGER TETA(MXTWRS),TPHI(MXTWRS)
      REAL    TE(MXTWRS),ESUM,ETA,PHI,TWORD,ET(MXTWRS),ETSUM,THETA
      REAL    ETMINHAD,ETMINEM,CENETA,DELETA,CENPHI,DELPHI
      REAL    DETA,DPHI,DR,ARRAY(10,2)
      CHARACTER*2 CHAR
C----------------------------------------------------------------------

      ESUM  = -1.
      ETSUM = -1.

      LCATD = GZCATD()
      IF (LCATD .LE. 0) RETURN

      ETMINEM  = IQ(LCATD + 5)
      ETMINHAD = IQ(LCATD + 6)

      EMPNT    = IQ(LCATD + 2)
      HADPNT   = IQ(LCATD + 3)
      MUPNT    = IQ(LCATD + 4)
      NEMTWRS  = IQ(LCATD + EMPNT)
      NHADTWRS = IQ(LCATD + HADPNT)
      NMUTWRS  = IQ(LCATD + MUPNT)

      IF (CHAR .EQ.'MU') ITYPE = 1
      IF (CHAR .EQ.'EM') ITYPE = 2
      IF (CHAR .EQ.'HA') ITYPE = 3
      GOTO (10,20,30) ITYPE

   10 IF (NMUTWRS .LE. 0) RETURN
      PNT   = LCATD + MUPNT
      NTWRS = NMUTWRS
      GOTO 40

   20 IF (NEMTWRS .LE. 0) RETURN
      PNT   = LCATD + EMPNT
      NTWRS = NEMTWRS
      GOTO 40

   30 IF (NHADTWRS .LE. 0) RETURN
      PNT   = LCATD + HADPNT
      NTWRS = NHADTWRS
      GOTO 40

   40 CONTINUE

      ESUM  = 0.
      ETSUM = 0.
      CALL VZERO(TE,MXTWRS)
      CALL VZERO(ET,MXTWRS)
      CALL VZERO(TETA,MXTWRS)
      CALL VZERO(TPHI,MXTWRS)
      CALL VZERO(ARRAY,20)

      IF (NTWRS.LE.MXTWRS) THEN
        DO 1 I = 1, NTWRS

          TWORD   = Q(PNT + I)
          TETA(I) = JBYT(TWORD,1,7)
          TPHI(I) = JBYT(TWORD,8,7)
          TE(I)   = FLOAT(JBYT(TWORD,20,13))*0.1

          IF (TETA(I) .LE. NETAL) THEN
            TETA(I) = TETA(I) - NETAL - 1
          ELSE
            TETA(I) = TETA(I) - NETAL
          ENDIF

          CALL CALETA(TETA(I),CENETA,DELETA,IERR)
          IF (CENETA .NE. 0) THEN
            THETA = 2.*ATAN(EXP(-CENETA))
          ELSE
            THETA = PI/2.
          ENDIF
          ET(I) = TE(I)*SIN(THETA)
          CALL CALPHI(TPHI(I),TETA(I),CENPHI,DELPHI,IERR)
          IF (IERR .NE. 0) GOTO 1

          DETA = ABS(ETA - CENETA)
          DPHI = ABS(PHI - CENPHI)
          IF (DPHI .GT. PI) DPHI = TWOPI - DPHI
          DR = SQRT(DETA**2 + DPHI**2)

          DO 2 J = 1, 10
            IF ( (DR .GT. (FLOAT(J) - 1.)/10.) .AND.
     &         (DR .LE. FLOAT(J)/10.) ) THEN
              ARRAY(J,1) = ARRAY(J,1) + TE(I)
              ARRAY(J,2) = ARRAY(J,2) + ET(I)
            ENDIF
    2     CONTINUE

    1   CONTINUE

      ELSE
        CALL ERRMSG('CATD','UNPACK_CATD','TOO MANY TOWERS','W')
      ENDIF

      RETURN
      END
