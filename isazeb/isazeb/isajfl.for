      SUBROUTINE ISAJFL
C-------------------------------------------------------------------
C-
C-  Fill ISAJ Zebra banks (primary partons)
C-
C-  INPUT:
C-
C-  OUTPUT:
C-  NJT  = number of partons
C-
C-      SDP JAN.,1986
C-
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KEYS.INC'
      INCLUDE 'D0$INC:XKEYS.INC'
      INCLUDE 'D0$INC:PJETS.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISALNK.INC'
      INTEGER LISAE
      INTEGER I,IOISAJ,LISAJ
      REAL PHI1,THETA,ETA
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(NJET.NE.0) THEN
C
        IF(KEYS(3))  THEN  ! special section for Drell-Yan
          CALL BKISAJ(LISAJ)
          PQREF(1)=LISAJ
          PQREF(2)=LISAJ
          IQ(LISAJ+1)=IDENTW !    particle id
          CALL UCOPY(QWJET,Q(LISAJ+2),5)   ! momenta and masses
          CALL ISPETA(QWJET,THETA,PHI1,ETA)   ! calculate phi,theta and eta
          Q(LISAJ+7)=PHI1
          Q(LISAJ+8)=THETA
          Q(LISAJ+9)=ETA
        ENDIF
C
        DO 200 I=1,NJET       ! loop over primary jets
        IF((.NOT.KEYS(3).OR.I.GT.2).AND.      ! only 3rd jet for Drell-Yan
     $     (PJETS(4,I).GT.0.01))         THEN ! and check jet energy 
          CALL BKISAJ(LISAJ)
          PQREF(I)=LISAJ
          IQ(LISAJ+1)=IDJETS(I)     !    particle id
          CALL UCOPY(PJETS(1,I),Q(LISAJ+2),5) ! momenta and mass
C   calculate phi,theta and eta
          CALL ISPETA(PJETS(1,I),THETA,PHI1,ETA)   
          Q(LISAJ+7)=PHI1
          Q(LISAJ+8)=THETA
          Q(LISAJ+9)=ETA
C
         ENDIF
  200    CONTINUE
C
      ENDIF
      RETURN
      END
