      SUBROUTINE CNEIGH_CATE
     &  (TYPE,DOENERGY,ETMIN,DELETA,DELPHI,TOWER,IMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a CATE tower I, works out
C-                         the CATE index of the cell with Maximum
C-                         ENERGY/ET in a neighboring Grid. Towers with
C-                         ENERGY/ET < ETMIN are ignored.
C-                         This routine can be used by ANY package.
C-
C-                         NOTE: For MOD(IETA) > 32 2 * DELPHI is used
C-                         to account for the reduced resolution in phi
C-                         for those values of IETA.
C-
C-   Inputs  : TYPE     [I]     1 --> Em tower, 2 --> Em + Hadronic tower
C-             DOENERGY [L]     TRUE then use tower energy, otherwise Et.
C-             ETMIN    [R]     Minimum energy/Et 
C-             DELETA   [I]     IETA +/- offset
C-             DELPHI   [I]     IPHI +/- offset
C-             TOWER    [I]     CATE tower number
C-
C-   Outputs : IMAX     [I]     Tower with Maximum E or ET
C-   Controls:
C-
C-   Created  31-JUL-1989   Rajendran Raja
C-   Updated   3-OCT-1989   Harrison B. Prosper
C-   Copy of CNEIGH2
C-   Updated  13-OCT-1989   Harrison B. Prosper
C-   Add Etmin argument
C-   Updated  17-OCT-1989   Boaz Klima
C-   Ignore towers already connected
C-   Updated 16-SEP-1991    Allen I. Mincer  allow towers with E<0
C-   Updated   9-AUG-1993   Chip Stewart
C-      ADD SWITCH FOR PREVIOUS CLUSTERING CHECK (ETMIN<0)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER TYPE
      LOGICAL DOENERGY
      REAL    ETMIN
      INTEGER DELETA
      INTEGER DELPHI
      INTEGER TOWER
      INTEGER IMAX
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CLUPAR.INC'
C
      INTEGER IER
      INTEGER I,J,IHADR,CATW,CATI,CATJ
      INTEGER ETAI,PHII,ETAJ,PHIJ,IETA,IPHI
      REAL    EMAX,ETOWI,ETOWJ
      REAL    ETAIM,ETAJM
      INTEGER DELPHI1
C
      INTEGER CLASSI,CLASSJ
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CATW(J) = (J-1)*NREP+LCATE        ! FUNCTION TO GIVE CATE OFFSET.
C----------------------------------------------------------------------
C
      CATI = CATW(TOWER)                ! Base pointer into CATE bank
      CLASSI = IQ(CATI+15)              ! Class number of tower I
      ETAI = IQ(CATI+12)
      ETAIM=ETAI-SIGN(0.5,FLOAT(ETAI))  ! Solve the displaced zero
      PHII = IQ(CATI+13)
      EMAX = -1.E8
      IMAX = 0
C
      DO 100 IETA = -DELETA,DELETA
        ETAJM = ETAIM +IETA
        ETAJ  = ETAJM+SIGN(0.5,ETAJM)   ! This gives n.neighbor across 0
        IF(IABS(ETAJ).GT.NETAL)GO TO 100
C
        DELPHI1 = DELPHI                ! Change delphi with resolution
        IF(IABS(ETAJ).GE.33) DELPHI1 = 2*DELPHI
C
        DO 200 IPHI = -DELPHI1,DELPHI1
          IF(IETA.EQ.0.AND.IPHI.EQ.0)GO TO 200        ! SKIP OVER ITSELF
          PHIJ = PHII + IPHI
          IF(PHIJ.LE.0)PHIJ=NPHIL                     ! wrap-around
          IF(PHIJ.GT.NPHIL)PHIJ=PHIJ-NPHIL          ! fix for phi>65
C
          J = PTCATE(ETAJ,PHIJ,TYPE)    ! Get tower number of neighboring
          IF(J.EQ.0)GO TO 200           ! tower
C
          CATJ = CATW(J)                ! Base pointer to tower J
          CLASSJ = IQ(CATJ+15)          ! Class number of tower J
C
C ****  IGNORING TOWERS WHICH ARE ALREADY CONNECTED
C
          IF ((ETMIN.GT.0).AND.( CLASSJ.EQ.CLASSI )) GOTO 200
C
          ETOWJ = Q(CATJ+7)
          IF(.NOT.DOENERGY)ETOWJ = Q(CATJ+8)          ! ET IF NOT ENERGY
          IF(ETOWJ.GT.EMAX)THEN
            EMAX = ETOWJ
            IMAX = J                    ! NEIGHBOR WITH MAXIMUM ENERGY
          ENDIF
  200   CONTINUE
C
  100 CONTINUE
C
C ****  Apply Minimum Et/Energy cut
C
      IF ( EMAX .LT. ABS(ETMIN) ) THEN
        IMAX = 0
      ENDIF
C
  999 RETURN
      END
