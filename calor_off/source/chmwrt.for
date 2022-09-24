      SUBROUTINE CHMWRT(IWRT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Writes out H-matrix
C-
C-   Inputs  : IWRT = 1 WRITE OUT
C-             IWRT = -1 READ IN
C-   Outputs :
C-   Controls:
C-
C-   Created   4-JUN-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      INTEGER IHMUN,IERR,IWRT
      LOGICAL OK
      CHARACTER*80 FILNAM
      integer filnam_len
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        call ezgets('HMATRIX_FILE', 1, FILNAM, FILNAM_LEN, IERR)
        CALL GTSRCP('BEAM_ENERGY',BEAMEN,1)
        CALL EZRSET
      ENDIF
      CALL GTUNIT(501,IHMUN,IERR)
      IF(IWRT.EQ.1)THEN
        CALL D0OPEN (IHMUN,FILNAM,'OU',OK)
      ELSEIF(IWRT.EQ.-1)THEN
        CALL D0OPEN (IHMUN,FILNAM,'IU',OK)
      ENDIF
      IF(.NOT.OK)THEN
        CALL ERRMSG('CALORIMETER','CHMWRT',
     &    'ERROR OPENING HMATRIX SAVE FILE','W')
        RETURN
      ENDIF
      IF(IWRT.EQ.1)THEN
        WRITE(IHMUN)EMAT,HMAT,HMAT_VIS,AVR,
     &  EMATRL,HMATRL,HMATRL_VIS,AVERL,
     &  HMAT_INV,HMATRL_INV,
     &  NEVMTR,BEAMEN,HM_PHILIM,
     &  NET,NPH,NLYRH,NDIMH,NDIMVF,NDIMVFR,NDIML,NDIMVL
      ELSEIF(IWRT.EQ.-1)THEN
        READ(IHMUN)EMAT,HMAT,HMAT_VIS,AVR,
     &  EMATRL,HMATRL,HMATRL_VIS,AVERL,
     &  HMAT_INV,HMATRL_INV,
     &  NEVMTR,BEAMEN,HM_PHILIM
      ENDIF
      CLOSE(UNIT=IHMUN)
      CALL RLUNIT(501,IHMUN,IERR)
  999 RETURN
      END
