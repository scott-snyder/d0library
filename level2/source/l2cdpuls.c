/*
        USR$ROOT2:[KLOPFENSTEIN.LEVEL2.TEST]L2CDPULS.C;
         c-version created : 19-SEP-1993 by Chris Klopfenstein

note : all function arguments are pointers since fortran
       calls by reference.

     Purpose and Methods : Find hits on one CDC channel, for
                           use in level-2.

     Inputs  : integer channel_id : logical channel number
               integer iq[] : pointer to zebra data
               integer end : pointer to end of data for this channel
               integer iped : (integerized) pedestal for this channel

     Outputs : integer maxhit : maximum number of hits allowed
               integer npulse : number of hits found
               integer time(maxhit) : drift times of hits, in
                                      units 1/128 ns
               integer area(maxhit) : pulse area of hits
               integer peak(maxhit) : peak height of hits
               integer status(maxhit) : status byte for each hit,
                       bit 0 - saturation flag
                       bit 1 - overlap flag
                       bits 2/3 - unused as yet
                       bits 4-7 - pulse width in units of 4 FADC bins
                                  (range 0 - 59 + overflow)
               integer error : = 0 if all ok, non-zero if
                               an unpack error is detected.
     Controls: none.

     Created  11-DEC-1992   Chris Klopfenstein
     (adapted from CDPULS: Qizhong Li-Demarteau, O. Callot, D. Pizzuto)
     Note - this version corrects an oversight in CDPULS - hits for
     which the leading edge overlaps the cluster end are handled properly.

     Modified March 1994 - add protection against corrupt FADC data.
                           CK
*/

int l2cdpuls(unsigned long *iq, long *end, long *ped, long *channel_id,
             long *maxhit, long *npulse, long *L2time,
             long *L2area, long *L2peak, long *L2status, long *error)
{

#include "c$include:table.h"
#include "c$include:cdc_rcp.h"      /* parameters normally gotten from RCP */
/*#include <macros.h> */
#define min(a,b)    (a>b ? b : a)
#define max(a,b)    (a<b ? b : a)
#define LFADC     512
#define NBSENS    7
#define NBDELY    2
#define LPULSE    8
#define MXHTOT    500
#define MASK16    0x0000FFFF
#define MASK4     0x0000000F
#define OVLMASK   0x00000002
#define SATMASK   0x00000001
#define LTABLE 256
#define MAXVAL 1024

	static long elbat[1024];
    static float coeff_sw[50], coeff_dl[50];
    static float coeff2_sw[50], coeff2_dl[50];
    static long maxcnt;
    static int thr1[2], thr2[2], thr3[2], maxtail[2];
    static long point, start, pout, bin;
    static union { unsigned long WordData;
                struct { unsigned int byte0 : 8;
                         unsigned int byte1 : 8;
                         unsigned int byte2 : 8;
                         unsigned int byte3 : 8;
                       } ByteData;
                 } udata;
    static long expdat[2 * LFADC];
    static long diff[2*LFADC + 1];
    static float time, sum, sumx;
    static long channel_length, cluster_length, cluster_end;
    static long natur, iflag, ntail;
    static int area, width;
    static long layer, sector, wire;
    static int lead, startlead, endlead;
    static int startarea, endarea;
    static int first = 1;
    static long nextval, lastval, ind;
    static long dummy1;
    static long dummy2;
    static long dummy3;
    static long dummy4;
    int loop;
/*---------------------------------------------------------------------------*/

/* initialize */
    error = 0;
    if (first) {
        first = 0;
/*        init_cdc_tables(table, elbat);	*/
/* construct inverse table */
        lastval = 0;
        for (ind = 0; ind < LTABLE; ++ind) {
        	nextval = table[ind];
        	while (lastval <= nextval) {
        		elbat[lastval] = ind;
        		++lastval;
        	}
        }
        maxcnt = table[MAXCNT];
        diff[1] = 0;
        coeff_sw[0] = 1.0;
        coeff2_sw[0] = 1.0;
        coeff_dl[0] = 1.0;
        coeff2_dl[0] = 1.0;
        thr1[0] = PULTH1_SW;
        thr1[1] = PULTH1_DL;
        thr2[0] = PULTH2_SW;
        thr2[1] = PULTH2_DL;
        thr3[0] = PULTH3_SW;
        thr3[1] = PULTH3_DL;
        maxtail[0] = PULMAX_SW;
        maxtail[1] = PULMAX_DL;
        coeff_sw[0] = 1.0;
        coeff2_sw[0] = coeff_sw[0];
        coeff_dl[0] = 1.0;
        coeff2_dl[0] = coeff_dl[0];
        for (loop = 1; loop < 50; ++loop) {
            coeff_sw[loop] = coeff_sw[loop - 1] * PULWEI_SW;
            coeff2_sw[loop] = coeff_sw[loop] * (loop + 1);
            coeff_dl[loop] = coeff_dl[loop - 1] * PULWEI_DL;
            coeff2_dl[loop] = coeff_dl[loop] * (loop + 1);
        }
    }
    *npulse = 0;
    channel_length = iq[*end - 1] & MASK16;
    if (channel_length <= 4) return 0; 
    wire = *channel_id & MASK4;
    if (wire < NBSENS) { natur = 0;
    }
    else { natur = 1;
    }

/* unpack channel data */

    start = *end - channel_length / 4;
    point = *end - 1;
    pout = 0;
    while (point > start) {
        cluster_end = iq[point - 1] >> 16;
        cluster_length = (iq[point - 1] & MASK16) - 4;
        if ( (cluster_length >= channel_length) ||
        	 (cluster_end >= LFADC) ) {
          error = 1;
          return 0;
        }
        point -= (cluster_length + 4)/4;
        expdat[pout] = cluster_length;
        expdat[pout + 1] = cluster_end - cluster_length + 1;

/* extract packed data, subtract pedestal, do bilinear conversion */

        for (bin = 0; bin < cluster_length; bin = bin + 4) {
          udata.WordData = iq[point + bin/4];
          if (udata.ByteData.byte3 > *ped)
            expdat[pout + bin + 2] = table[udata.ByteData.byte3 - *ped];
          else 
            expdat[pout + bin + 2] = udata.ByteData.byte3 - *ped;
          if (udata.ByteData.byte2 > *ped)
            expdat[pout + bin + 3] = table[udata.ByteData.byte2 - *ped];
          else 
           expdat[pout + bin + 3] = udata.ByteData.byte2 - *ped;
          if (udata.ByteData.byte1 > *ped)
            expdat[pout + bin + 4] = table[udata.ByteData.byte1 - *ped];
          else 
             expdat[pout + bin + 4] = udata.ByteData.byte1 - *ped;
          if (udata.ByteData.byte0 > *ped)
            expdat[pout + bin + 5] = table[udata.ByteData.byte0 - *ped];
          else 
            expdat[pout + bin + 5] = udata.ByteData.byte0 - *ped;
        }
        pout = pout + cluster_length + 2;
    }
    
/* mark end of data */
    expdat[pout] = 0;
    
/* loop over clusters */

    point = 0;
    while ((expdat[point] > 0) && (*npulse < *maxhit)) {
      cluster_length = expdat[point];
      cluster_end = expdat[point + 1];
      startlead = 0;
      endlead = 1;
      start = point;
      point = point + cluster_length + 2;

/* calculate first differences */

      for (bin = 2; bin <= cluster_length; ++bin) {
            diff[bin] = expdat[start + bin + 1] -
                            expdat[start + bin];
        }
/* find a leading edge */
      while ((bin = endlead + 1) < cluster_length) {
          lead = 0;
          while ((lead == 0) && (bin < cluster_length)) {
            if (diff[bin] >= thr1[natur]) {
              if (diff[bin - 1] >= thr1[natur]) {
                if (expdat[start + bin + 1] >= 0) {
                  if ((diff[bin + 1] >= thr1[natur]) ||
                      ((diff[bin-1] + diff[bin]) >= thr2[natur])) {
                    lead = 1;
                    startarea = bin - 2;
                    startlead = bin - 2;
                    if (diff[startlead] <= 0) startlead = bin - 1;
                  }
                  else
                    bin = bin + 3;
                }
                else
                  bin = bin + 1;
              }
              else
                bin = bin + 1;
            }
            else
              bin = bin + 2;
          } /* end loop to find leading edge */
          
/* go to next cluster if no edge found */
          
          if (bin >= cluster_length) break;
          
/* find end of leading edge, i.e. where difference goes to zero,
   at the same time, calculate sums needed for drift time calculation */

          endlead = startlead;
          endarea = startarea;
          sum = 0.0;
          sumx = 0.0;
/* use different weights for sense wires and delay lines */
          if (wire < NBSENS) {
            bin = startlead;
            while ((bin <= cluster_length) && (diff[bin] > 0)) {
                sum += diff[bin] * coeff_sw[bin - startlead];
                sumx += diff[bin] * coeff2_sw[bin - startlead];
                ++bin;
            }
            endlead = min(bin, cluster_length);
            endarea = min(bin, cluster_length);
          }
          else {
            bin = startlead;
            while ((bin <= cluster_length) && (diff[bin] > 0)) {
                sum += diff[bin] * coeff_dl[bin - startlead];
                sumx += diff[bin] * coeff2_dl[bin - startlead];
                ++bin;
            }
            endlead = min(bin, cluster_length);
            endarea = min(bin, cluster_length);
          } /* found end of leading edge */

/* check that peak height exceeds threshold */

          if ((expdat[start + endlead] -
               expdat[start + startlead]) > thr3[natur]) {
/* Find end of pulse. Truncate pulse if another leading edge is found
   (set overlap flag in this case), or if 3 consecutive differences
   are less than threshold. If these conditions are not met truncate
   after maxtail bins. */

            iflag = 0;
            ntail = 0;
            bin = endarea;
            endarea = cluster_length;
            while (bin < (cluster_length - 3)) {
              ++bin;
              ++ntail;
              if ((ntail + 3) >= maxtail[natur]) {
                  endarea = min(bin + 2, cluster_length);
                  break;    /* tail too long */
              }
              if (((diff[bin] >= thr1[natur]) && 
                   (diff[bin + 1] >= thr1[natur])) &&
                  ((diff[bin + 2] >= thr1[natur]) ||
                 ((diff[bin] + diff[bin + 1]) >= thr2[natur]))) {
                     iflag = iflag | OVLMASK;    /* set overlap flag */
                     endarea = bin - 1;
                     break;    /* found overlapping leading edge */
              }
              if ((-diff[bin] <= thr1[natur]) 
                && (-diff[bin + 1] <= thr1[natur])
                 && (-diff[bin + 2] <= thr1[natur])) {
                endarea = bin;
                if (diff[bin + 1] <= 0) {
                  if (diff[bin + 2] < 0) {
                      endarea = bin + 2;
                  }
                  else
                    endarea = bin + 1;
                }
                break;    /* found end of pulse tail */
              }
            } /* end of loop to find end of pulse tail */
/* compute pulse area */
            area = 0;
            for (bin = startarea; bin <= endarea; ++bin) {
              if (expdat[start + 1 + bin] >= maxcnt) {
                  iflag = iflag | SATMASK;    /* set saturation flag */
              }
              area = area + expdat[start + 1 + bin];
            }
            if (area >= thr3[natur]) {
/* record pulse parameters */
              ++*npulse;
              time = BINTIME * (sumx/sum + (startlead - 1) +
                               expdat[start + 1] - 0.5);
              L2time[*npulse - 1] = min(time * TSCALE, (TMAX * TSCALE - 1));
              L2area[*npulse - 1] = min(area, ARMAX);
/* undo bilinear conversion of peak height (so can store it in 8 bits) */
              L2peak[*npulse - 1] = elbat[expdat[start + endlead]];
              L2status[*npulse - 1] = iflag;
              width = min((endarea - startarea)/4, WIDMAX);
              L2status[*npulse - 1] += width << 4;
            }
          }
        } /* continue processing this cluster */
    }    /* go on to next cluster */
  return 0;
}

