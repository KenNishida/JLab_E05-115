/*
  HESAnalysis.hh
 
  D.Kawama
*/

#ifndef HESAnalysis_h

#define HESAnalysis_h 1


#include "globals.hh"
#include "G4ThreeVector.hh"
#include "Randomize.hh"

#ifdef USE_CSTREAM
#include <cstdio>
#else
#include <fstream>
#endif

class G4Run;
class G4Event;
class G4Step;


class HESAnalysis 
{
public:
  HESAnalysis();
  virtual ~HESAnalysis();

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
  G4String filename_;
  G4int trigNum;
  G4ThreeVector gPos_, gMom_;
  G4bool fTriggered;
  G4bool fActive_;
  G4bool fOutput;
  G4int evID;
  G4double event[100];

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
