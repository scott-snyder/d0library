C----------------------------------------------------------------------
      INTEGER FUNCTION DBMU_FNO (CTOP,COUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will return the file id number corresponding
C-    to the file id top directory
C-
C-   Returned value  : file id number, if less that zero :
C-                     0 = is not defined
C-                    -1 = invalid top directory name 
C-
C-   Inputs  : CTOP (C*3)   An top directory assigned to a list of 
C-                          dbl3 files
C-   Outputs : COUT (C*3)   Trimmed version of CTOP
C-   Controls: 
C-
C-   Created  24-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICFNBL
C
      CHARACTER*(*) CTOP,COUT
      INTEGER I,J
      INTEGER K2
      CHARACTER CHLP*12, C4*4
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
      DBMU_FNO = -1
C
C- Get the number 
C
      CHLP = CTOP
      J = ICFNBL (CHLP,1,LEN(CHLP))
      IF (J .LE. 0 .OR. J .GT. LEN(CHLP)) RETURN
      C4 = CHLP(J:)
      CALL STR$TRIM (C4,C4,K2)
      IF (K2 .LE. 0) RETURN
      COUT = C4
C
      DBMU_FNO = 0
      DO I = 1,NDBTOPD
         IF (C4 .EQ. DBTOPD(I)) THEN
            DBMU_FNO = I
            RETURN
         END IF
      END DO
C
  999 RETURN
      END
