C
C======================================================================
C      COMMON /CALBUF/ contains the calorimeter buffers
C
       INTEGER
     1      NDEP,NPHI,NPCLO,NPCHI,NECLO,NECHI,NCCHI
C
       PARAMETER (NDEP=11)       
       PARAMETER (NPHI=63)       
       PARAMETER (NPCLO=1)       
       PARAMETER (NPCHI=24)      
       PARAMETER (NECLO=1)       
       PARAMETER (NECHI=24)      
       PARAMETER (NCCHI=12)      
C
       REAL     
     1      CENCAL(0:NDEP,0:NPHI,-NCCHI:NCCHI),
     2      POSCAP(0:NDEP,0:NPHI,NECLO:NECHI),
     3      NEGCAP(0:NDEP,0:NPHI,-NECHI:-NECLO),
     4      POSPLG(0:NDEP,0:NPHI,NPCLO:NPCHI),
     5      NEGPLG(0:NDEP,0:NPHI,-NPCHI:-NPCLO)
C
       COMMON/CALBUF/CENCAL,POSCAP,NEGCAP,POSPLG,NEGPLG
C      =======================================================================
C
