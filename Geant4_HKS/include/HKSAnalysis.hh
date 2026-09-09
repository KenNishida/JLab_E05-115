/*
  HKSAnalysis.hh
 
  M.Kawai
*/

#ifndef HKSAnalysis_h

#define HKSAnalysis_h 1


#include "globals.hh"
#include "G4ThreeVector.hh"
#include "Randomize.hh"
#include <TTree.h> 
#include <TFile.h> 

#ifdef USE_CSTREAM
#include <cstdio>
#else
#include <fstream>
#endif
#include <iostream>

class G4Run;
class G4Event;
class G4Step;


class HKSAnalysis 
{
public:
  HKSAnalysis();
  virtual ~HKSAnalysis();

public:
  void BeginOfRun( const G4Run *aRun );
  void EndOfRun( const G4Run *aRun );
  void BeginOfEvent( const G4Event *anEvent );
  void EndOfEvent( const G4Event *anEvent );
  void PrimaryGeneration( const G4ThreeVector &pos,
			  const G4ThreeVector &mom, 
			  const G4int ID);
  void SaveFile( void ) const;
  void Terminate( void ) const;
  void SetDataFile( const char *datafile );
  void InitializeEvent( void );
  void SetActive( void ) { fActive_=true; }
  void SetInActive( void ) { fActive_=false; }

private:
  G4String filename_,title;
  G4int trigNum;
  G4ThreeVector gPos_, gMom_;
  G4bool fTriggered;
  G4bool fActive_;
  G4bool fOutput;
  G4int evID;
  G4double event[100];

  G4double Mom;
  G4double Theta;
  G4double Phi;
  G4double Xpt,Ypt;
  G4double Xt,Yt,Zt;
  
  G4double X[15],Y[15],Z[15];
  G4double Xp[15],Yp[15];
  G4double Xv[15],Yv[15],Zv[15];
  G4double E[15],P[15],L[15];
  //for (int i=1; i<=14 ; ++i){
  // Xp[i] = -1000.0;Yp[i] = -1000.0;
  // X[i] = -1000.0;Y[i] = -1000.0;
  // Xv[i] = -1000.0;Yv[i] = -1000.0;Zv[i]=-1000.0;
  //}

  G4bool Trig,TOFTrig,KDCTrig;
  G4bool TOF1Trig, TOF2Trig;
  G4bool AC1Trig, AC2Trig, AC3Trig;
  G4bool WC1Trig, WC2Trig;
  G4double TargetEdep;
  G4bool VD;

  G4int TOFseg[3], ACseg[3], WCseg[2];
  G4double TOFadc[3][18], TOFtdc[3][18], TOFx[3][18], TOFy[3][18];
  G4double ACadc[3][7], ACtdc[3][7], ACx[3][7],ACy[3][7];
  G4double WCadc[2][12], WCtdc[2][12], WCx[2][12], WCy[2][12], WCz[2][12];
  G4double TOFtdcSum[3];
  G4double TOFLength[3];
  G4double ACadcSumLayer[3];
  G4double WCadcSumLayer[2];
  G4double ACsum;
  G4double WCsum;
  G4int TOFpid[3],ACpid[3],WCpid[2],VDpid[14];
  G4int DC1hit[6], DC2hit[6];
  G4double DC1tdc[6], DC2tdc[6];
  
  //G4ThreeVector KDC1X[6], KDC2X[6];
  //G4ThreeVector KDC1Y[6], KDC2Y[6];
  //G4ThreeVector KDC1Z[6], KDC2Z[6];
  //G4ThreeVector KDC1P[6], KDC2P[6];
  //G4ThreeVector KDC1Xp[6], KDC2Xp[6];
  //G4ThreeVector KDC1Yp[6], KDC2Yp[6];
  
  //--- By Toshi , 21/Oct/2012---
  G4double KDC1X[6], KDC2X[6];
  G4double KDC1Y[6], KDC2Y[6];
  G4double KDC1Z[6], KDC2Z[6];
  G4double KDC1P[6], KDC2P[6];
  G4double KDC1Xp[6], KDC2Xp[6];
  G4double KDC1Yp[6], KDC2Yp[6];
  // --- (Bug fix)
  
  std::ofstream DataFile_;

  void NormalAnalysis( const G4Event *anEvent );

  FILE *fprim, *ftrig;
  FILE *fDummy;
#ifdef USE_CSTREAM
  FILE *fpout;
#else
  std::ofstream fout;
#endif

  void outputData( const G4Event *anEvent );
  void defineHistograms( void );

public:
  G4bool GetTriggerStatus( void ) const { return fTriggered; }
  //void SetEventID( G4int id ) { evID_ = id; }

private:
  G4double RandGauss( G4double center, G4double dev );

};

#endif
