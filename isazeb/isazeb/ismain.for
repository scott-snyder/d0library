      PROGRAM ISMAIN
C     ***************************************************************   
C          ALTERNATE MAIN PROGRAM FOR ISAJET.  
C
C       Needs following assigments:
C       ISAJET command file to FILCOM
C       DECAY.DAT to FILDKY   
C
C       data will be in FILEVT (no calorimeter banks)
C       listing will be in FILLIS
C
C          S. LINN, Aug. 1986, Rev. SDP Sept., 1986
C
C     ***************************************************************
      IMPLICIT NONE
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$INC:ITAPES.INC'
      CHARACTER*40 V,VISAZE
      DATA BANK/'ISAP'/
      DATA ITEVT/2/,ITDKY/21/,ITCOM/11/,ITLIS/12/    
C     ----------------------------------------------------------------   
C   
      OPEN(ITCOM,ERR=100,FILE='FILCOM',STATUS='OLD',
     +                             FORM='FORMATTED')  
C
      OPEN(ITLIS,ERR=200,FILE='FILLIS',STATUS='NEW',
     +                             FORM='FORMATTED')    
C
      OPEN(ITDKY,ERR=400,FILE='FILDKY',STATUS='OLD',
     +                             FORM='FORMATTED')    
C                                                  
      ISUNIT=ITEVT
      FILISA='FILEVT'
      V=VISAZE()
      PRINT *,' You are running with ',V
      CALL ISAZEB('O')
C
      CALL ISAJET(-ITDKY,-ITEVT,ITCOM,ITLIS) 
C
      PRINT*,'NORMAL END'
      STOP
 100  PRINT*,'ERROR OPENING FILCOM'
      STOP
 200  PRINT*,'ERROR OPENING FILLIS'
      STOP
 300  PRINT*,'ERROR OPENING FILEVT'
      STOP
 400  PRINT*,'ERROR OPENING FILDKY'
      END   
