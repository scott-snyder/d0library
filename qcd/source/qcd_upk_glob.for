      FUNCTION QCD_UPK_GLOB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get the GLOB information and fill
C-                         QCD_GLOB common
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   15-FEB-1993   Andrew G. Brandt
C-   Updated   27-Sep-1993   Andrew G. Brandt add GTGLOB_2
C-   Updated   12-Nov-1993   Andrew G. Brandt protect GTGLOB_2
C-   Updated   01-Feb-1994   Andrew G. Brandt GTGLOB_2 if MDST>=3
C-   Updated   24-MAR-1994   Andrew G. Brandt FILL HOT_E HOT_ET
C-   Updated   01-NOV-1994   Andrew G. Brandt GLOB1 and GLOB2 for CW
C-   Updated   25-MAR-1996   Andrew G. Brandt GTGLOB_2_QCD
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_GLOB.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      LOGICAL QCD_UPK_GLOB
      INTEGER IVERS,IQUAL,NTRK,NCEL,IER
      INTEGER MRB,MAGPOL(2),L1ANOR(8)
      REAL ETSUMS(3),ESUMS(9),T29
      REAL EH,ETH
      INTEGER I
C
      QCD_UPK_GLOB = .TRUE.
C
C  Initialize
C
      DO I=1,3
        ETGLOB(I)   =-999.
      END DO
      DO I=1,8
        EGLOB(I)   =-999.
      END DO
      ETOT=-999.
      NCDCTK=-1
      NCELCC=-1
      MRBITS=-1
      TIME29=-999.
C
      CALL GTGLOB(IVERS,IQUAL,NTRK,NCEL,ETSUMS,ESUMS,IER)
      IF(IER.NE.0) RETURN
C
C: Fill common
C
      DO I=1,3
        ETGLOB(I)   =ETSUMS(I)
      END DO
C
C: First E word is ETOT, load other 8 into EGLOB
C
      ETOT=ESUMS(1)
      DO I=2,9
        EGLOB(I-1)    =ESUMS(I)
      END DO
      NCDCTK=NTRK
      NCELCC=NCEL
C
C: Add new GLOB words for version 3 of GLOB and MDST
C
      IF(IVERS.GE.3.AND.JUVERS.GE.3) THEN
        IF(UPATH.EQ.'RECO'.AND.IVERS.EQ.3) THEN
          IVERS=4
          CALL CALC_HOT_ET(EH,ETH)
C
C: Overwrite last 2 EGLOB words
C
          EGLOB(7)=EH
          EGLOB(8)=ETH
        END IF
        CALL GTGLOB_2_QCD(T29,MRB,MAGPOL,L1ANOR,IER)
        IF(IER.NE.0) GO TO 999
        MRBITS=MRB
        TIME29=T29
      END IF
C
  999 RETURN
      END
