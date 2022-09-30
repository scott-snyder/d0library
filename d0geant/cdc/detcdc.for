C DETCDC.FOR
      SUBROUTINE DETCDC
C======================================================================
C
C   Purpose and Methods : Define sets and detectors in the CDC
C                         to store the hits.
C                         Define variables to be stored for each hit.
C                         Those variables are:
C-            HITSV(1)   X-GLOBAL incoming track
C-            HITSV(2)   Y-GLOBAL
C-            HITSV(3)   Z-GLOBAL
C-            HITSV(4)   X-GLOBAL outgoing track
C-            HITSV(5)   Y-GLOBAL
C-            HITSV(6)   Z-GLOBAL
C-            HITSV(7)   PULSE HEIGHT ( Integrated charge)
C-            HITSV(8)   Track length in the cell ( dx**2 + dy**2 + dz**2)
C-            HITSV(9)   Track id.=2**11*Secondary track #+Primary track#
C-            HITSV(10)  Empty
C
C   Inputs  :
C   Outputs :
C
C   Created  FEB- 4-1986   K. Ng
C            JAN-21-1987   K. Nishikawa  : New wire configuration 
C                                          and other changes
C            MAR-31-1987   G. Rahal-Callot                           
C-   Updated  23-FEB-1988   Ghita Rahal-Callot  : Change the data to store
C-   Updated   9-APR-1992   Qizhong Li-Demarteau  Change NBITSH to 32 to
C                                             solve the overflow in GSAHIT
C                                          
C
C======================================================================
      IMPLICIT NONE
C     
      INTEGER NDAT
      PARAMETER (NDAT = 10)
C
      INTEGER NBITC(2), NBITE(1)
      INTEGER ISET,IDET
      CHARACTER*4 NAMES1(2), NAMES2(2), NAMES3(2), NAMES4(2)
      CHARACTER*4 NAME11,    NAME13,    NAME21,    NAME23
      CHARACTER*4 NAME31,    NAME33,    NAME41,    NAME43
C
      REAL    ORIG (NDAT),  FACT (NDAT)
      character*4  NAMESH (NDAT)
      INTEGER NBITSH (NDAT)
C======================================================================
C
      DATA NAMES1 /'DFS1', 'DFT1'/
      DATA NAMES2 /'DFS2', 'DFT2'/
      DATA NAMES3 /'DFS3', 'DFT3'/
      DATA NAMES4 /'DFS4', 'DFT4'/
      DATA NBITC  /6, 6/
      DATA NAME11 / 'DFS1'/
      DATA NAME13 / 'DFS1'/
      DATA NAME21 / 'DFS2'/
      DATA NAME23 / 'DFS2'/
      DATA NAME31 / 'DFS3'/
      DATA NAME33 / 'DFS3'/
      DATA NAME41 / 'DFS4'/
      DATA NAME43 / 'DFS4'/
      DATA NBITE  /6/
C
      DATA NAMESH / 'XIN ', 'YIN ', 'ZIN ', 'XOUT', 'YOUT',
     &              'ZOUT', 'PULS', 'TLEN' , 'ISAT', '    '/
      DATA NBITSH / 10*32 /
      DATA ORIG   / 6*1000., 4*0. /
      DATA FACT   / 6*100., 10., 1000., 2*1. /
C
C
C ****  Define set CDC and detectors DFij
C ****  The DFij detecteurs have differents IDTYPE to identify
C ****  them easily
C
      CALL GSDET('CDC ','DF11',1,NAME11,NBITE,101,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DFT1',2,NAMES1,NBITC,102,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DF13',1,NAME13,NBITE,103,1500,500,ISET,IDET)
C
      CALL GSDET('CDC ','DF21',1,NAME21,NBITE,201,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DFT2',2,NAMES2,NBITC,202,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DF23',1,NAME23,NBITE,203,1500,500,ISET,IDET)
C
      CALL GSDET('CDC ','DF31',1,NAME31,NBITE,301,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DFT3',2,NAMES3,NBITC,302,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DF33',1,NAME33,NBITE,303,1500,500,ISET,IDET)
C
      CALL GSDET('CDC ','DF41',1,NAME41,NBITE,401,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DFT4',2,NAMES4,NBITC,402,1500,500,ISET,IDET)
      CALL GSDET('CDC ','DF43',1,NAME43,NBITE,403,1500,500,ISET,IDET)
C
C
C ****  Define HIT parameters for each detector
C
      CALL GSDETH('CDC ','DF11',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DFT1',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DF13',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DF21',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DFT2',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DF23',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DF31',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DFT3',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DF33',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DF41',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DFT4',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('CDC ','DF43',NDAT,NAMESH,NBITSH,ORIG,FACT)
C
  999 CONTINUE
      RETURN
      END
