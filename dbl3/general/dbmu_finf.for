C----------------------------------------------------------------------
      LOGICAL FUNCTION DBMU_FINF(CTOP,FNAM,SVAL,EVAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given top directory, it will pass,
C-    for the currently open dbl3 file, file name, start and end 
C-    validity.
C-
C-   Returned value  : .true.  a file was currently open
C-                     .false. no file was currently open
C-   Inputs  : CTOP  (C) Top directory
C-   Outputs : FNAME (C) Name of file
C-             SVAL  (I) Start validity (dbl3 packed time)
C-             EVAL  (I) End validity (dbl3 packed time)
C-
C-   Created  26-JAN-1993   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DBMU_FNO
      LOGICAL D3U_SET_TOPD
C
      CHARACTER*(*) CTOP, FNAM
      INTEGER       SVAL, EVAL  
C
      CHARACTER*12  CHLP
      INTEGER       FID, IOP
C..........................SOOORYYY....................................
      INTEGER MAXDBTOPD
      PARAMETER (MAXDBTOPD=15)
      INTEGER MAXDBFILES
      PARAMETER (MAXDBFILES=100)
      INTEGER NDBTOPD
      CHARACTER*4 DBTOPD(MAXDBTOPD)
      INTEGER NDBFILES(MAXDBTOPD)
      CHARACTER*80 DBFILPOI(MAXDBTOPD)
      INTEGER DBFILOPN(MAXDBTOPD)
      CHARACTER*80 DBFILES(MAXDBFILES,MAXDBTOPD)
      INTEGER DBTIM(2,MAXDBFILES,MAXDBTOPD)
      INTEGER DBRUN(2,MAXDBFILES,MAXDBTOPD)
      COMMON/DBMFILES/
     &  NDBTOPD,DBTOPD,NDBFILES,DBFILOPN,DBTIM,DBRUN,DBFILES,DBFILPOI
C......................................................................
C----------------------------------------------------------------------
      DBMU_FINF = .FALSE.
      FNAM = ' '
      SVAL = 0
      EVAL = 0
      CHLP = CTOP
C
      FID = DBMU_FNO(CHLP,CHLP)
      IF (FID .LT. 0) THEN
         CALL MSGO
     &      ('w','DBMU_FINF','Top directory is invalid',0)
         RETURN
      ELSE IF (FID .EQ. 0) THEN
         CALL MSGO
     &      ('w','DBMU_FINF','Top directory not defined',0)
         RETURN
      ELSE IF (.NOT. D3U_SET_TOPD(CHLP,0)) THEN
         CALL MSGO
     &      ('w','DBMU_FINF','Top directory is invalid',0)
         RETURN
      END IF
C
      IOP = DBFILOPN(FID)
      IF (IOP .LE. 0) RETURN
      FNAM = DBFILES(IOP,FID)
      SVAL = DBTIM(1,IOP,FID)
      EVAL = DBTIM(2,IOP,FID)
      DBMU_FINF = .TRUE.
C
  999 RETURN
      END
