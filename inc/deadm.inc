C DEC/CMS REPLACEMENT HISTORY, Element DEADM.INC
C *3    24-MAY-1989 14:48:22 RAJA "EDEADT added"
C *2    24-MAY-1989 14:42:45 RAJA "NDEADM"
C *1    24-MAY-1989 12:21:25 RAJA "Dead materials enetrgy"
C DEC/CMS REPLACEMENT HISTORY, Element DEADM.INC
C----------------------------------------------------------------------
C-
C-   Created  24-MAY-1989   Rajendran Raja
C-   Should have Dead_materials. PARAMS Loaded before
C----------------------------------------------------------------------
      COMMON /DEADM/ EDEADM(DEADLO:DEADHI),NDEADM,EDEADT
      REAL    EDEADM                    
      REAL    EDEADT                    
      INTEGER NDEADM                    
                                        
