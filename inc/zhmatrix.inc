C----------------------------------------------------------------------
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-   Updated  23-JAN-1991   Harrison B. Prosper  
C-      Added space for multiple LHMTR addresses
C-      (HST_BASE + 1 ... HST_LNKMX)
C-
C----------------------------------------------------------------------
      INTEGER HST_MAX
      PARAMETER( HST_MAX   = 100)       
      INTEGER HMTR_MAX
      PARAMETER( HMTR_MAX  = 100 )       
      INTEGER HST_LNKMX
      PARAMETER(HST_LNKMX  = HST_MAX + HMTR_MAX ) 
      INTEGER HRF_LNKMX
      PARAMETER( HRF_LNKMX = 1)
      INTEGER HSTLNK                    
      INTEGER HRFLNK                    
      INTEGER HMTR_TOTAL                
      INTEGER HMTR_PTR                  
C
      COMMON /ZHMATRIX/HSTLNK(HST_LNKMX),HRFLNK(HRF_LNKMX),
     &                 HMTR_TOTAL,HMTR_PTR
C
C the user can add more links  by equivalencing to HSTLNK,HRFLNK
C arrays.
C
C
       INTEGER LHMTR,LAVER             
       INTEGER LEMAT,LHMAT
       INTEGER LHVIS,LHINV
       INTEGER LEIGN,LUMAT
       INTEGER LCRCP,LQUAN
       INTEGER LDIAG,LWORK
       INTEGER LPROD,LTEMP
       INTEGER LCHIS
       INTEGER HMTR_LINKS(HMTR_MAX)    
C
       EQUIVALENCE (HSTLNK(1),LHMTR),(HSTLNK(2),LAVER)
       EQUIVALENCE (HSTLNK(3),LEMAT),(HSTLNK(4),LHMAT)
       EQUIVALENCE (HSTLNK(5),LHVIS),(HSTLNK(6),LHINV)
       EQUIVALENCE (HSTLNK(7),LEIGN),(HSTLNK(8),LUMAT)
       EQUIVALENCE (HSTLNK(9),LCRCP),(HSTLNK(10),LQUAN)
       EQUIVALENCE (HSTLNK(11),LDIAG),(HSTLNK(12),LWORK)
       EQUIVALENCE (HSTLNK(13),LPROD),(HSTLNK(14),LTEMP)
       EQUIVALENCE (HSTLNK(15),LCHIS)
C
       EQUIVALENCE (HSTLNK(HST_MAX+1),HMTR_LINKS(1))
C
