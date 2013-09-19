#ifndef _ebcalcul_h
#define _ebcalcul_h

#ifndef Pb_M
#define Pb_M(vect,id) if(vect==NULL) {Rprintf("%d,Probleme de memoire\n",id); exit(0);}
#endif

extern void ebcalcul_intersection();
extern DOUBLE ebcalcul_distance(); //carré de la distance!!
extern DOUBLE ebcalcul_produit();
extern void ebcalcul_normale();
extern void ebcalcul_centre_triangle();
#endif
