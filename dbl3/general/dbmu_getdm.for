C----------------------------------------------------------------------
      SUBROUTINE DBMU_GETDM 
     &  (CLASS,DEV,NDEV,STIM,NVAL,CHOP,VAL,XHOU,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will fetch global DBMON values for a given 
C-    class for a given time.
C-    It will:
C-             1) At fist entry it will read the known list of DBMON dbl3 
C-                files, from DBL3$DBM:DBM_DBFILES.DAT or if CHOP= 
C-                'topdirectory' it will use the list of dbl3 files already
C-                assigned to that top directory.
C-             1) Check if current dbl3 file is valid, if not it will 
C-                open a new file.
C-             2) Check if the data in memory is still valid, if not
C-                it will fetch new data from current dbl3 file
C-
C-   Inputs  : CLASS (C)    The class of which the devices belong to
C-             DEV   (C)    Devices to fetch
C-             NDEV  (I)    Number of devices (length of DEV and second array
C-                          index for VAL)
C-             STIM  (2*I)  Search time, VMS system time
C-             NVAL  (I)    Number of data words for each device.att (first
C-                          array index of VAL)
C-             CHOP  (C)    ' ' Use DBM as topdirectory (default)
C-                          else it will be used as top directory name (is 
C-                          usefull if you have called your DBMU_FILE_INI).
C-   Outputs : VAL   (R)    Array of read values (ok, if value # -999.0)
C-             XHOU  (R)   Time between requested time for data 
C-                         and time for the found data, in hours
C-             IRET  (I)    Number of devices found
C-                          = 0  no channels found
C-                          =-1  something went wrong
C-
C-   Created   3-OCT-1992   Lars Rasmussen
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL DBMU_FILE_INIT,DBMU_FILE_OPEN,D3U_SET_TOPD
      LOGICAL DBMU_FINF
      LOGICAL D3UPT
      REAL D3UXH
C
      CHARACTER *(*)   CLASS,CHOP
      INTEGER          STIM(2),NDEV,NVAL,IRET
      CHARACTER *(*)   DEV(NDEV)
      REAL             VAL(NVAL,NDEV),XHOU
C
      INTEGER I,J,K,ISTA,KT
      CHARACTER CLS*12,ZTOP*4
      INTEGER KYS(D3_NK),LINK,KF,LCC
      LOGICAL FIRST,LRET
      CHARACTER*12 CDUM
      DATA FIRST /.TRUE./
      EQUIVALENCE   (D3_LNK(3),LINK)
C
      INTEGER DBKEYS(15)
      COMMON /DBMCURKEYS/ DBKEYS
C----------------------------------------------------------------------
      IRET = -1
      XHOU = 0.
      LINK = 0
      DO I = 1,15
         DBKEYS(I) = 0
      END DO
      CALL D3U_INI (IDVSTP,'//DBM/DBMD','S348910',' ',IRET)
      ZTOP = CHOP
      CALL UPCASE (ZTOP,ZTOP)
      IF (ZTOP(1:1) .EQ. ' ') ZTOP = 'DBM'
      IF (FIRST) THEN
         FIRST = .FALSE.
         IF (.NOT. DBMU_FILE_INIT 
     &      (ZTOP,'DBL3$DBM:DBM_DBFILES',0,' ')) RETURN
      END IF
      LRET = D3U_SET_TOPD(ZTOP,0)
      IF (NDEV .LE. 0) THEN
         RETURN
      ELSE IF (.NOT. D3UPT(STIM,KT)) THEN
         CALL MSGO ('w','DBMU_GETHV','Time invalid',0)
         RETURN
      END IF
C
C- Is current file ok ?, if not, open new one
C- (it will also set the right top directory)
C
      IF (.NOT. DBMU_FILE_OPEN (ZTOP,' ',KT,' ')) RETURN
C
      CLS = CLASS
      KYS(3)=KT
      KYS(4)=KT     
      CALL DBMCTOH (CLS,KYS(8),12)
C
C- Is current data in memory ok ?, if not, get some new
C
      CALL D3U_FETCM (KYS,LINK,ISTA)
      IF (ISTA .LE. -2) THEN
         IRET = -1
         RETURN
      ELSE IF (ISTA .EQ. -1 .OR. LINK .LE. 0) THEN
         IRET = 0
         RETURN
      END IF
C
      DO I = 1,15
         DBKEYS(I) = IC(LC(LINK+1)+I)
      END DO
      IF (DBKEYS(4) .GE. 999999999) THEN
         IF (.NOT. DBMU_FINF( ZTOP,CDUM,I,DBKEYS(4) )) THEN
            IRET = -1
            RETURN
         END IF
      END IF
      IF (DBKEYS(3) .LE. 2) THEN
         IRET = 0
         RETURN
      END IF
      XHOU = D3UXH(KT,DBKEYS(3))
C
C- Unpack it
C
      CALL DBMU_UNPDM (LC(LINK-1),DEV,NDEV,NVAL,VAL,IRET)
C
999   RETURN
      END
