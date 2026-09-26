/////////////////////////////
// HES Forward Matrix making Program w/ SVD 
//
// D.Kawama
//////////////////////////////

#include <fstream>
#include <iostream>
#include <TMatrixD.h>
#include <TVectorD.h>
#include <TDecompSVD.h>
#include <math.h>
#include "mtune.h"
using namespace std;

//////////////////////////////////////////////////////////////////////////
void calcMatrix(int Nevent, double* yVal, double* eVal, 
      double* xpt, double* ypt, double* mom,
      double* par, char* Mname)
//////////////////////////////////////////////////////////////////////////
{

   int pxpt[nParamF],pypt[nParamF],pmom[nParamF];
   double norm[nTrig];
   for (int i=0;i<nTrig;i++){
      norm[i]=1.;
   }
   TMatrixD A(Nevent,nParamF);
   TVectorD x(Nevent); 
   TVectorD y,e; 
   TVectorD v0,v1,v2,v3;
   v0.Use(Nevent, norm);
   v1.Use(Nevent, xpt);
   v2.Use(Nevent, ypt);
   v3.Use(Nevent, mom);
   TMatrixD v(Nevent,4);
   TMatrixDColumn(v,0) = v0;
   TMatrixDColumn(v,1) = v1;
   TMatrixDColumn(v,2) = v2;
   TMatrixDColumn(v,3) = v3;
   ofstream matrix;
   y.Use(Nevent, yVal);
   e.Use(Nevent, eVal);
   matrix.open(Mname);
   int npar=0;


   int n=0;
   int a=0,b=0,c=0;
   for (n=0;n<nMatF+1;n++){
      for (c=0;c<n+1;c++){
         for (b=0;b<n+1;b++){
            for (a=0;a<n+1;a++){

               if (a+b+c==n){
                  pxpt[npar]=a;
                  pypt[npar]=b;
                  pmom[npar]=c;
                  if (a<=nXptF && b<=nYptF && c<=nMomF ){
                     for (int event=0;event<Nevent;event++){ 
                        x(event) = pow(xpt[event],double(a))*pow(ypt[event],double(b))*
                           pow(mom[event],double(c));
                     }
                  }
                  else{
                     for (int event=0;event<Nevent;event++){ 
                        x(event) = 0.;
                     }
                  }
                  TMatrixDColumn(A,npar) = x;
                  npar++;
               }

            }
         }
      }
   }

   TMatrixD Aw = A;
   TVectorD yw = y;
   /*for (int irow = 0; irow < A.GetNrows(); irow++) {
     TMatrixDRow(Aw,irow) *= 1/e(irow);
     yw(irow) /= e(irow);
     }*/
   //TDecompSVD svd(Aw,1e-5);
   TDecompSVD svd(Aw,1e-3);
   //TDecompQRH svd(Aw);
   Bool_t ok;
   const TVectorD c_svd = svd.Solve(yw,ok);
   /*if (Flag==0){
     const TVectorD sig = svd.GetSig();
     ofstream fsig("sig_svd.dat");
     for (int irow = 0; irow < sig.GetNrows(); irow++) {
     fsig << sig(irow) <<endl;
     }
     fsig.close();
     }*/

   //const TVectorD c_svd = NormalEqn(A,y,e);

   TMatrixD Params(1,nParamF);
   TMatrixDRow(Params,0) = c_svd;
   for(int i=0;i<nParamF;i++){
      par[i] = Params(0,i);
      if (fabs(par[i])<1e-4){par[i]=0.;}
      matrix << par[i] << " " 
         << pxpt[i] <<" "<< pypt[i]<<" " 
         << pmom[i] 
         << endl;
   }
   matrix.close();
}

