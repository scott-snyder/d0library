C----------------------------------------------------------------------
      LOGICAL FUNCTION DBMU_FILE_INIT (CTOP,FILP,INUM,CHOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will assign the DBL3 files, listed in the file
C-    pointed to by FILP, to the top directory CTOP. If that top directory
C-    is already assigned you will get whatewer already is assigned.
C-
C-   Returned value  : .true. = ok
C-   Inputs  :  CTOP  (C)  Corresponding top directory (max 4 bytes)
C-              FILP  (C)  Pointer to a set of dbl3 files
C-              CHOP  (C)  Currently not used
C-   Outputs :  INUM  (I)  Dummy
C-   Controls: 
C-
C-   Created  13-OCT-1992   Lars Rasmussen
C-
C-   Entry points :
C-
C-   DBMU_FILE_INIT   to define a new set of dbl3 files. to assing a top
C-                    directory to that set of files.
C-   DBMU_FILE_OPEN   check if current dbl3 file (for that top directory
C-                    is ok, else try to open a better one.
C-   DBMU_FILE_CLOSE  to close a a dbl3 file.
C-   DBMU_FILE_GETFI  to get a valid dbl3 file for a given top directory
C-                    and time
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER ICFILA,ICFNBL,DBMU_FNO
      LOGICAL D3U_SET_TOPD
      LOGICAL DBMU_FILE_OPEN
      LOGICAL DBMU_FILE_CLOSE
      LOGICAL DBMU_FILE_GETFI
C
      CHARACTER*(*) FILP,CTOP,CHOP
      INTEGER INUM
C
      INTEGER I,J,K,IER,LCC,ICC,FID,NF,NFF
      INTEGER ST(2),ET(2),SR,ER,KT,IFI,IOP
      INTEGER*2 K2
      CHARACTER*4 ZTOP
      CHARACTER*80 FHERE,FTHERE
      CHARACTER*48 FHLP
      INTEGER SV,EV,IDUM,IUNI,IRET
      LOGICAL FIRST,LOK
C
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
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      DBMU_FILE_INIT = .FALSE.
      IF (FIRST) THEN
         NDBTOPD = 0
         DO J = 1,MAXDBTOPD
         NDBFILES(J) = 0
         DBFILOPN(J) = 0
         DBFILPOI(J) = ' '
         DBTOPD(J) = ' '
         DO I = 1,MAXDBFILES
            DBTIM(1,I,J) = 0
            DBTIM(2,I,J) = 0
            DBRUN(1,I,J) = 0
            DBRUN(2,I,J) = 0
            DBFILES(I,J) = ' '
         END DO
         END DO
         FIRST = .FALSE.
      END IF
C
C- Set of files already assigned ?
C
      FID = DBMU_FNO (CTOP,ZTOP)
      IF (FID .LT. 0) THEN
         CALL MSGO
     &      ('e','DBMU_FILE_INIT','Top directory invalid',0)
         RETURN
      ELSE IF (FID .GT. 0) THEN
         DBMU_FILE_INIT = .TRUE.
         RETURN
      END IF
C
C- Trim the passed file name
C
      FHERE = FILP
      ICC = ICFNBL (FHERE,1,LEN(FHERE))
      IF (ICC .LE. 0 .OR. ICC .GT. LEN(FHERE)) THEN
         CALL MSGO
     &      ('e','DBMU_FILE_INIT','File pointer invalid',0)
         RETURN
      END IF
      FHERE = FHERE(ICC:)
      CALL STR$TRIM (FHERE,FHERE,K2)
      IF (K2 .LE. 0) THEN
         CALL MSGO
     &      ('e','DBMU_FILE_INIT','File pointer invalid',0)
         RETURN
      END IF
C
C- Space for more files ?
C
      IF (NDBTOPD .GE. MAXDBTOPD-1) THEN
         CALL MSGO
     &      ('e','DBMU_FILE_INIT','To many sets of dbl3 databases',0)
         RETURN
      END IF
C
C- Get a new list of files
C
      CALL GTUNIT(984,IUNI,IRET)
      IF (IRET .NE. 0) THEN
         CALL MSGO ('en','DBMU_FILE_INIT','Error from GTUNIT',IRET)
         RETURN
      END IF
      CALL D0OPEN (IUNI,FILP,'IF',LOK)
      IF (.NOT. LOK) RETURN
C
      FID = NDBTOPD + 1
      NDBFILES(FID) = 0
      NFF = 0
      DO WHILE (.TRUE.)
         READ(IUNI,801,END=11) SV,EV,IDUM,FHLP
         NFF = NFF + 1
         DBFILES(NFF,FID) = FHLP
         DBTIM(1,NFF,FID) = SV
         DBTIM(2,NFF,FID) = EV
         DBRUN(1,NFF,FID) = 0
         DBRUN(2,NFF,FID) = 0
      END DO
11    CLOSE (IUNI)
      CALL RLUNIT(984,IUNI,IRET)
801   FORMAT (2I12,I10,A48)
C
      IF (NFF .LE. 0) THEN
         CALL MSGO
     &      ('w','DBMU_FILE_INIT','No valid files in file list',0)
         RETURN
      END IF
C
C- Prevent pit falls, create end vality in such a way that there is no
C- gabs between files.
C
      DO I = 1,NFF-1
         IF (D3_NOT) THEN
            DBTIM(2,I,FID) = DBTIM(1,I+1,FID) - 1
         ELSE
            CALL D0DBL3_DBINCT(DBTIM(1,I+1,FID),-1,DBTIM(2,I,FID))
         END IF
      END DO
C
      NDBTOPD = FID
      DBTOPD(FID) = ZTOP
      DBFILPOI (FID) = FHERE(1:K2)
      NDBFILES(FID) = NFF
      DBMU_FILE_INIT = .TRUE.
C
      RETURN
C
C----------------------------------------------------------------------
      ENTRY DBMU_FILE_OPEN (CTOP,FILP,INUM,CHOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will check if the current open dbl3 file,
C-    for top directory CTOP, cover the time INUM. If not, it will open
C-    a valid database.
C-
C-   Returned value  : .true. = ok
C-   Inputs  :  CTOP  (C)  Top directory
C-              INUM  (I)  Validity time (dbl3 packed time)
C-              CHOP  (C)  Currently not used
C-   Outputs :  
C-   Controls: 
C-
C-   Created  13-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      DBMU_FILE_OPEN = .FALSE.
      FID = DBMU_FNO(CTOP,ZTOP)
      IF (FID .LT. 0) THEN
         CALL MSGO
     &      ('w','DBMU_FILE_OPEN','Top directory is invalid',0)
         RETURN
      ELSE IF (FID .EQ. 0) THEN
         CALL MSGO
     &      ('w','DBMU_FILE_OPEN','Top directory not defined',0)
         RETURN
      ELSE IF (.NOT. D3U_SET_TOPD(ZTOP,0)) THEN
         CALL MSGO
     &      ('w','DBMU_FILE_OPEN','Top directory is invalid',0)
         RETURN
      END IF
C
C- Database still valid ?
C
      IOP = DBFILOPN(FID)
      IF (IOP .GT. 0) THEN
      IF (INUM .GE. DBTIM(1,IOP,FID) .AND. 
     &    INUM .LE. DBTIM(2,IOP,FID)) THEN
            DBMU_FILE_OPEN = .TRUE.
            RETURN
      END IF
      END IF
C
C- Not good anymore, close the current one (if nothing open, it wont harm) ...
C
      CALL D3U_END
C
C- ... and find a better one
C
      IFI = 0
      DO I = 1,NDBFILES (FID)
         IF (INUM .GE. DBTIM(1,I,FID) .AND. 
     &       INUM .LT. DBTIM(2,I,FID)) IFI = I
      END DO
      IF (IFI .EQ. 0) THEN
         CALL MSGO
     &      ('w','DBMU_FILE_OPEN','Couldn''t find a valid dbl3 file',0)
         RETURN
      END IF
C
      CALL D3U_START (DBFILES(IFI,FID),' ',IER)
      IF (IER .NE. 0) THEN
         CALL D3U_END
         RETURN
      END IF
C
      CALL MSGO('i','DBMU_FILE_OPEN','open - '//DBFILES(IFI,FID),0)
      DBMU_FILE_OPEN = .TRUE.
      DBFILOPN(FID) = IFI
C
      RETURN
C
C----------------------------------------------------------------------
      ENTRY DBMU_FILE_CLOSE (CTOP,FILP,INUM,CHOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will close dbl3 database, corresponding
C-    to top directory CTOP
C-
C-   Returned value  : .true. = ok
C-   Inputs  :  CTOP  (C)  top directory
C-              INUM  (I)  dummy
C-              CHOP  (C)  Currently not used
C-   Outputs :  FILP  (C)  dummy
C-   Controls: 
C-
C-   Created  13-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      DBMU_FILE_CLOSE = .FALSE.
      FID = DBMU_FNO(CTOP,ZTOP)
      IF (FID .LE. 0 .OR. FID .GT. NDBTOPD) RETURN
      IF (D3U_SET_TOPD(DBTOPD(FID),0)) THEN
         DBFILOPN(FID) = 0
         CALL D3U_END
      END IF
      DBMU_FILE_CLOSE = .TRUE.
C
      RETURN      
C
C----------------------------------------------------------------------
      ENTRY DBMU_FILE_GETFI (CTOP,FILP,INUM,CHOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will return the DBL3 file defined by the 
C-    top directory CTOP and time INUM
C-
C-   Returned value  : .true. = ok
C-   Inputs  :  CTOP  (C)  Top directory
C-              INUM  (I)  Validity time (dbl3 packed time or run number)
C-              CHOP  (C)  Currently not used
C-   Outputs :  FILP  (C)  Name of valid DBL3 file
C-   Controls: 
C-
C-   Created  13-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      DBMU_FILE_GETFI = .FALSE.
      FILP = ' '
      FID = DBMU_FNO(CTOP,ZTOP)
      IF (FID .LE. 0) RETURN
      DO I = 1,NDBFILES (FID)
         IF (INUM .GE. DBTIM(1,I,FID) .AND. 
     &       INUM .LT. DBTIM(2,I,FID)) THEN
            FILP = DBFILES(I,FID)
            DBMU_FILE_GETFI = .TRUE.
            RETURN
         END IF
      END DO
C
      RETURN
C
      END
