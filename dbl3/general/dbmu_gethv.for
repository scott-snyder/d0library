C----------------------------------------------------------------------
      SUBROUTINE DBMU_GETHV (CLASS,STIM,CHOP,NCHN,CHN,XHOU,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will fetch HV values for a given class for
C-    a given time (class = CAL, MUO, LV0, TRD, CDC, VTX, etc).
C-    It will:
C-             1) At fist entry it will read the known list of HV dbl3 
C-                files, from DBL3$HVM:HVM_DBFILES.DAT
C-             1) Check if current dbl3 file is valid, if not it will 
C-                open a new file.
C-             2) Check if the data in memory is still valid, if not
C-                it will fetch new data from current dbl3 file
C-
C-   Inputs  : CLASS         (C)    Class name
C-             STIM          (2*I)  Search time (VMS system time)
C-             CHOP          (C)    Not used (yet)
C-             NCHN          (I)    Maximum number of channels
C-   Outputs : CHN (4,NCHN)  (R)    Voltage, current, status and
C-                                  found/not found (1/0) for each channel
C-             XHOU          (R)    Time between requested time for data 
C-                                  and time for the found data, in hours
C-             IRET          (I)    Number of devices found
C-                                  = 0  no channels found
C-                                  =-1  something went wrong
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
      CHARACTER*(*) CLASS, CHOP
      INTEGER STIM(2),NCHN,IRET
      REAL CHN(4,*),XHOU
C
      INTEGER I,J,K,ISTA,KT
      CHARACTER CHLP*12,ZTOP*4
      INTEGER KYS(D3_NK),LINK
      LOGICAL FIRST
      CHARACTER*12 CDUM
      DATA FIRST /.TRUE./
      EQUIVALENCE   (D3_LNK(3),LINK)
C
      INTEGER DBKEYS(15)
      COMMON /DBMCURKEYS/ DBKEYS
C----------------------------------------------------------------------
      XHOU = 0.
      IRET = -1
      LINK = 0
      DO I = 1,15
         DBKEYS(I) = 0
      END DO
      CALL D3U_INI (IDVSTP,'//HVM/DBMD','S348910',' ',IRET)
      IF (FIRST) THEN
         FIRST = .FALSE.
         IF (.NOT. DBMU_FILE_INIT 
     &      ('HVM','DBL3$HVM:HVM_DBFILES',0,' ')) RETURN
      END IF
      IF (NCHN .LE. 0) THEN
         RETURN
      ELSE IF (.NOT. D3UPT(STIM,KT)) THEN
         CALL MSGO ('w','DBMU_GETHV','Time invalid',0)
         RETURN
      END IF
C
C- Is current file ok ?, if not, open new one
C- (it will also set the right top directory)
C
      IF (.NOT. DBMU_FILE_OPEN ('HVM',' ',KT,' ')) RETURN
C
      CHLP = CLASS
      KYS(3)=KT
      KYS(4)=KT     
      CALL DBMCTOH (CHLP,KYS(8),12)
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
      IRET = 0
      DO I = 1,15
         DBKEYS(I) = IC(LC(LINK+1)+I)
      END DO
      IF (DBKEYS(4) .GE. 999999999) THEN
         IF (.NOT. DBMU_FINF( 'HVM',CDUM,I,DBKEYS(4) )) THEN
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
      CALL DBMU_UNPHV (LC(LINK-1),NCHN,CHN,IRET)
C
999   RETURN
      END
