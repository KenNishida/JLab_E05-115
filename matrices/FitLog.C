/*
  "FitLog.C"
  Fitting Result logger.
  Present log file is used for next fitting
  as default parameters.
  
       Toshi Gogami , 2Feb2013
*/

#include"FitLog.hh"
#include<fstream>
#include<iostream>
#include<TF1.h>
using namespace std;

FitLog::FitLog()
{}

FitLog::FitLog(char* name_, TF1* func_ ,int flag)
  :name( name_ ) , func( func_ ) ,
   npar( 0 ) , already( false )
{
  char tempc[200];
  double temp;
  sprintf(filename,"./fitlog/%s_fit.dat",name);
  npar = func->GetNpar();
  ifs = new ifstream(filename);
  
  
  if( !*ifs ){
    cout << " There is no file : " << filename << endl;
    cout << " " << filename 
	 << " will be created when you do LogWrite()." 
	 << endl;
    already = false;
  }
  else{
    *ifs >> tempc >> tempc;
    *ifs >> temp;
    for(int i=0 ; i<npar ; i++){
      *ifs >> par[i] >> par_er[i];
      func->SetParameter(i,par[i]);
    }
    already = true;
  }
  
  
}

FitLog::~FitLog(){}

void FitLog::LogWrite(){
  const int n = npar;
  for(int i=0 ; i<n ; i++){
    par[i] = func->GetParameter(i);
    par_er[i] = func->GetParError(i);
  }
  ofs = new ofstream(filename);
  *ofs << name << " "<< func->GetExpFormula() << endl;
  *ofs << n << endl;
  for(int i=0 ; i<n ; i++){
    *ofs << par[i] << " " << par_er[i] << endl;
  }
  ofs->close();
}

void FitLog::check(){
  if( already == true ){
    cout << endl;
    cout << " ############ CHECK ( FITLOG.C ) ############ "
	 <<endl;
    cout << " NAME: " << name << endl;
    cout << " FILE: " << filename << endl;
    cout << " FUNC: " << func->GetExpFormula() << endl;
    cout << " NPAR: " << npar << endl;
    cout << " PARA: " << endl;
    for(int i=0 ; i<npar ; i++){
      cout << "       " << par[i] 
	 << " " << par_er[i] << endl;
    }
    cout << endl;
  }
  else{
    cout << " ############ CHECK ( FITLOG.C ) ############ "
	 <<endl;
    cout << " There is no fitting data yet..... " << endl;
    cout << endl;
  }
}
