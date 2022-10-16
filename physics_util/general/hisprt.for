C======================================================================
      SUBROUTINE HISPRT(IUNIT,MODE)
C======================================================================
C
C  Description: 
C  ============
C
C             Sets up and outputs histograms for SUMMARIES
c
C
C  ================
C  
C-    INPUT:
C             IUNIT - Output unit for SUMMARIES
C             MODE='USER' to ouput selected set of histograms
C             MODE='STANDARD' to ouput standard set of histograms
C
C-    OUTPUT:
C  =================
C       No ouput arguments
C
C-    ENTRYS:
C  =================
C      ENTRY SETHPR - SETUP USER or STANDARD SET OF HISTOGRAMS FOR SUMMARY
C      ENTRY HISPRT - OUTPUT USER or STANDARD SET OF HISTOGRAMS FOR SUMMARY
C
C  Preconditions necessary before call:
C  ====================================
C      Initialize HBOOK 
C      DEFAULTS if call HISPRT without calling SETHPR:
C          MODE='STANDARD' - all histograms outputted
C          MODE='USER' - no histograms outputted
C
C  Author:
C  =======
C  SHARON HAGOPIAN
C
C  Revision History:
C  =================
C  MAR 24, 1987 - Original creation
C  September 1, 1988 - Take out DHS calls. TLK
C
C======================================================================
C
      IMPLICIT NONE
C
C  Argument Declarations:
C  ======================
C
      INTEGER IUNIT
      CHARACTER*8 MODE
C             
C  Local Declarations:
C  ===================
C
      CHARACTER*8 NAME
      INTEGER MXNAM
      PARAMETER (MXNAM=120)
      CHARACTER*12 HNAME1(MXNAM),HNAME2(MXNAM)
      CHARACTER*12 IBLANK
      CHARACTER*1 IBL1
      INTEGER IP ! LOOP INDEX
      INTEGER ID1(3*MXNAM),ID2(3*MXNAM)
      LOGICAL YES
      CHARACTER*72 NAMLIS
      INTEGER NH1,NH2,NID1,NID2
C   
C  Executable Code:
C  ================
C
      CALL HOUTPU(IUNIT)
      IF(MODE(1:4).EQ.'USER')THEN
C        DEFAULT FOR USER SUMMARY IS NO HISTOGRAMS
         IF(NH1.EQ.0)GO TO 90
      ENDIF
      IF(MODE(1:8).EQ.'STANDARD')THEN
C     DEFAULT IS TO OUTPUT ALL HISTOGRAMS
        IF(NH2.EQ.0)THEN
          CALL HPRINT(0)
        ENDIF
      ENDIF
   90   CALL HOUTPU(6)
      RETURN
C
C !!!! ENTRY SETHPR - SECOND ENTRY POINT - !!!!!!!!!!!!!!!!!!!!!!!!!
C
C================================================================
C
      ENTRY SETHPR(MODE)
C
C  Description: 
C  ============
C       Stores number and names of STANDARD histogram summary list
C       Stores number and names of USER histogram summary list
C   
C  Data Statements:
C  ================
      DATA NH1,NH2/0,0/
      DATA IBLANK/'            '/
      DATA IBL1/' '/
C
C  Executable Code:
C  ================
      IF(MODE(1:4).EQ.'USER')THEN
      DO 1 IP=1,MXNAM
    1 HNAME1(IP)=IBLANK
        CALL INTMSG(' Enter hist name for USER SUMMARY; END to end')
    2   CONTINUE
        CALL GETPAR(1,'>','C',NAME)
        IF(NAME(1:3).EQ.'END') GO TO 5
        NH1=NH1+1
        HNAME1(NH1)=NAME
        GO TO 2
    5   CALL GETPAR1l('List histograms for USER summary? Y/N',
     x  'L',YES)
      IF(YES)THEN
         DO 6 IP=1,NH1,6
           NAMLIS=IBL1//HNAME1(IP)//HNAME1(IP+1)//HNAME1(IP+2)
     X          //HNAME1(IP+3)//HNAME1(IP+4)//HNAME1(IP+5)
         CALL INTMSG(NAMLIS)
    6   CONTINUE
      ENDIF
      ENDIF
      IF(MODE(1:8).EQ.'STANDARD')THEN
      DO 10 IP=1,MXNAM
   10 HNAME2(IP)=IBLANK
   12   CONTINUE
        CALL INTMSG(' Enter hist name for STANDARD SUMMARY; END to end')
        CALL GETPAR(1,'>','C',NAME)
        IF(NAME(1:3).EQ.'END') GO TO 15
        NH2=NH2+1
        HNAME2(NH2)=NAME
        GO TO 12
   15   CALL GETPAR1l('List histograms for STANDARD summary? Y/N',
     x  'L',YES)
      IF(YES)THEN
         DO 16 IP=1,NH2,6
           NAMLIS=IBL1//HNAME2(IP)//HNAME2(IP+1)//HNAME2(IP+2)
     X          //HNAME2(IP+3)//HNAME2(IP+4)//HNAME2(IP+5)
         CALL INTMSG(NAMLIS)
   16   CONTINUE
      ENDIF
      ENDIF
      RETURN
      END
