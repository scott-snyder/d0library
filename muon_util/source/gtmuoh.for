C DEC/CMS REPLACEMENT HISTORY, Element GTMUOH.FOR
C *1    27-OCT-1988 22:48:47 HEDIN "FROM ZEBRA_UTIL"
C DEC/CMS REPLACEMENT HISTORY, Element GTMUOH.FOR
C DEC/CMS REPLACEMENT HISTORY, Element GTMUOH.FOR
C *1     7-FEB-1988 17:36:00 TAMI "GET MUON PROCESSED HIT INFORMATION FROM MUOH"
C DEC/CMS REPLACEMENT HISTORY, Element GTMUOH.FOR
C====================================================================
      SUBROUTINE GTMUOH(IPHIT,IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR,
     X                  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     X                  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C=======================================================================
C
C  Description:  Returns information about processed muon hit IPHIT from
C  ============  bank MUOH
C
C  Argument Declarations:
C  =======================
C
C  IPHIT - Integer - Input - Number of the processed muon hit for which
C                            one would like information.
C  IWADD - Integer - Output - Wire Address
C  IFW1  - Integer - Output - Flagging Word
C  IFW2  - Integer - Output - Flagging Word
C  INRAW - Integer - Output - Raw Data Hit Number
C  IORIEN- Integer - Output - Module Orientation
C  NHWIR - Integer - Output - Number of times on wire
C  CORT1 - Real    - Output - Corrected time 1 (NS)
C  CORT2 - Real    - Output - Corrected time 2 (NS)
C  CORP1 - Real    - Output - Corrected pad  1 
C  CORP2 - Real    - Output - Corrected pad  2
C  CORDT1- Real    - Output - Corrected Delta Time 1 (NS)
C  CORDT2- Real    - Output - Corrected Delta Time 2 (NS)
C  DDIS1 - Real    - Output - Drift Distance 1 (CM)
C  DDIS2 - Real    - Output - Drift Distance 2 (CM)
C  TDIV1 - Real    - Output - Time Division 1 (CM from center of wire)
C  TDIV2 - Real    - Output - Time Division 2 (CM from center of wire)
C  VERD1 - Real    - Output - Vernier Distance First Solution (CM)
C  VERD2 - Real    - Output - Vernier Distance Second Solution (CM)
C  XCWIR - Real    - Output - X center of wire in global coordinates (CM)
C  YCWIR - Real    - Output - Y center of wire in global coordinates (CM)
C  ZCWIR - Real    - Output - Z center of wire in global coordinates (CM)
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - August 3,1987
C  D. Hedin 28 words per hit   3/92 (aalow for old)
C  DH 9/92 add rotation corrections to X,Y,ZCWIR
C==========================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Argument Declarations:
C  ======================
C
      INTEGER IPHIT,IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1
      REAL DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
C
C  Local Declarations:
C  ===================
C
      INTEGER LMUOH,GZMUOH,FIRST,LMUHT,GZMUHT,NMUSRT,NWD,L
      DATA FIRST,NWD/0,28/
C 
C  Executable Code:
C  ================
C
      LMUOH = GZMUOH(0)
      IF(FIRST.EQ.0) THEN
        LMUHT=GZMUHT(0)
        NMUSRT=IQ(LMUHT+2)
        IF(LMUHT.NE.0.AND.NMUSRT.NE.0.AND.LMUOH.NE.0) THEN
          NWD=28
          L=IQ(LMUOH-1)/NMUSRT
          IF(L.EQ.25) NWD=25
          FIRST=1
        ENDIF
      ENDIF
      IF (LMUOH .EQ. 0) THEN
          CALL INTMSG(' ERROR IN GTMUOH - MUOH NOT BOOKED')
      ELSE
         IWADD = IQ(LMUOH+1+NWD*(IPHIT-1))
         IFW1  = IQ(LMUOH+2+NWD*(IPHIT-1))
         IFW2  = IQ(LMUOH+3+NWD*(IPHIT-1))
         INRAW = IQ(LMUOH+4+NWD*(IPHIT-1))
         IORIEN = IQ(LMUOH+5+NWD*(IPHIT-1))
         NHWIR  = IQ(LMUOH+6+NWD*(IPHIT-1))
         CORT1  = Q(LMUOH+9+NWD*(IPHIT-1))
         CORT2  = Q(LMUOH+10+NWD*(IPHIT-1)) 
         CORP1  = Q(LMUOH+11+NWD*(IPHIT-1))
         CORP2  = Q(LMUOH+12+NWD*(IPHIT-1))
         CORDT1 = Q(LMUOH+13+NWD*(IPHIT-1))
         CORDT2 = Q(LMUOH+14+NWD*(IPHIT-1))
         DDIS1  = Q(LMUOH+15+NWD*(IPHIT-1))
         DDIS2  = Q(LMUOH+16+NWD*(IPHIT-1))
         TDIV1  = Q(LMUOH+17+NWD*(IPHIT-1))
         TDIV2  = Q(LMUOH+18+NWD*(IPHIT-1))
         VERD1  = Q(LMUOH+19+NWD*(IPHIT-1))
         VERD2  = Q(LMUOH+20+NWD*(IPHIT-1))
        IF(NWD.EQ.28)THEN
           XCWIR=Q(LMUOH+21+NWD*(IPHIT-1))+Q(LMUOH+26+NWD*(IPHIT-1))
           YCWIR=Q(LMUOH+22+NWD*(IPHIT-1))+Q(LMUOH+27+NWD*(IPHIT-1))
           ZCWIR=Q(LMUOH+23+NWD*(IPHIT-1))+Q(LMUOH+28+NWD*(IPHIT-1))
        ELSE
           XCWIR=Q(LMUOH+21+NWD*(IPHIT-1))
           YCWIR=Q(LMUOH+22+NWD*(IPHIT-1))
           ZCWIR=Q(LMUOH+23+NWD*(IPHIT-1))
        ENDIF
      ENDIF
      RETURN
      END
