
#ifndef krd_h 
#define krd_h
#include <stdio.h>

double GetMM(double Ei,double xpk,double ypk,double pk,
					double xpe,double ype,double pe,int TargetFlag);
float calcf2t(float* P, float xf, float xpf, 
                 float yf, float ypf, int nMat);
float calct2s(float* P, float xp, float yp, 
                 float mom, int nMat);
double corRaster(float* P, double xt, double yt);

#endif
