/*
  HKSParamMan.hh

  D.Kawama
*/

#ifndef HKSParamMan_h
#define HKSParamMan_h 1

#define MAXCHAR 144

#include "globals.hh"


class HKSParamMan
{
public:
  HKSParamMan( const char *fileName );

private:
  static HKSParamMan *thisPointer;

  bool ReadParam( void );

private:
  // Input file name
  const char *fileName;

  /////////////////////
  // FOR HKSAnalysis //
  /////////////////////

  // Hbook file name
  char HbookFileName[MAXCHAR];
  char LogFileName[MAXCHAR];
  // Output file name
  char ErikaFileName[MAXCHAR]; // for ERIKA study
  char TulFileName[MAXCHAR]; // for grouping study
  char dumFileName[MAXCHAR]; // for dummy data

  ///////////////////////////////////
  // FOR HKSPrimaryGeneratorAction //
  ///////////////////////////////////

  // Generation Type
  int GenerationID;
  // Particle Type
  int ParticleID;
  // input for dummy data
  char InputDataName[MAXCHAR];

  ///////////////
  // Field Map //
  ///////////////

  std::string DFieldMap;
  std::string SplitterFieldMap;
  std::string Q1FieldMap;
  std::string Q2FieldMap;
  double Sscale;
  double Dscale;
  double Q1scale;
  double Q2scale;
  int Q1mapID, Q2mapID;
  double CentMom;
  double AcptMom;
  //double CentXp;
  //double AcptXp;
  //double CentYp;
  //double AcptYp;
  double CentTheta;
  double AcptTheta;
  double CentPhi;
  double AcptPhi;
  
  double ThetaLimit;
  double RasterX;
  double RasterY;
  double RasterZ;
  double TargetThickness;
  std::string TargetMaterial;
  int EventFlag;
  int KDCFlag;
  int EMFlag;
  int SSFlag;
  int DecayFlag;
  int HadronFlag;
  double CutValue;
  double Timeresolution;
  double Edet;

public:
  static HKSParamMan *GetParamMan(){ return thisPointer; }

  char *GetHbookFileName( void ){ return HbookFileName; }
  char *GetErikaFileName( void ){ return ErikaFileName; }
  char *GetTulFileName( void ){ return TulFileName; }
  char *GetDumFileName( void ){ return dumFileName; }
  char *GetInputDataName( void ){ return InputDataName; }

  int GetGenerationID( void ){ return GenerationID; }
  int GetParticleID( void ){ return ParticleID; }
  std::string GetDFieldMap( void ){ return DFieldMap; }
  double GetSplitterScaleFactor( void ){ return Sscale; }
  double GetDScaleFactor( void ){ return Dscale; }
  double GetQ1ScaleFactor( void ){ return Q1scale; }
  double GetQ2ScaleFactor( void ){ return Q2scale; }
  std::string GetSplitterFieldMap( void ){ return SplitterFieldMap; }
  std::string GetQ1FieldMap( void ){ return Q1FieldMap; }
  std::string GetQ2FieldMap( void ){ return Q2FieldMap; }
  int GetQ1MapID( void ){ return Q1mapID; }
  int GetQ2MapID( void ){ return Q2mapID; }
  double GetCentMom( void ){ return CentMom; }
  double GetAcptMom( void ){ return AcptMom; }
  double GetCentTheta( void ){ return CentTheta; }
  double GetAcptTheta( void ){ return AcptTheta; }
  double GetCentPhi( void ){ return CentPhi; }
  double GetAcptPhi( void ){ return AcptPhi; }
  double GetThetaLimit( void ){ return ThetaLimit; }
  double GetRasterX( void ){ return RasterX; }
  double GetRasterY( void ){ return RasterY; }
  double GetRasterZ( void ){ return RasterZ; }
  double GetTargetThickness( void ){ return TargetThickness; }
  double GetTimeresolution( void ){ return Timeresolution; }
  double GetEdet( void ){ return Edet; }
  double GetCutValue( void ){ return CutValue; }
  std::string GetTargetMaterial( void ){ return TargetMaterial; }
  int GetEventFlag( void ){ return EventFlag; }
  int GetKDCFlag( void ){ return KDCFlag; }
  int GetEMFlag( void ){ return EMFlag; }
  int GetSSFlag( void ){ return SSFlag; }
  int GetDecayFlag( void ){ return DecayFlag; }
  int GetHadronFlag( void ){ return HadronFlag; }

};

#endif

// #define SIEVE_SLIT 1
#undef SIEVE_SLIT
