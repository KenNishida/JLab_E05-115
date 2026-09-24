/*
  HESParamMan.hh

  D.Kawama
*/

#ifndef HESParamMan_h
#define HESParamMan_h 1

#define MAXCHAR 144

#include "globals.hh"


class HESParamMan
{
public:
  HESParamMan( const char *fileName );

private:
  static HESParamMan *thisPointer;

  bool ReadParam( void );

private:
  // Input file name
  const char *fileName;

  /////////////////////
  // FOR HESAnalysis //
  /////////////////////

  // Hbook file name
  char HbookFileName[MAXCHAR];
  char LogFileName[MAXCHAR];
  // Output file name
  char ErikaFileName[MAXCHAR]; // for ERIKA study
  char TulFileName[MAXCHAR]; // for grouping study
  char dumFileName[MAXCHAR]; // for dummy data

  ///////////////////////////////////
  // FOR HESPrimaryGeneratorAction //
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
  double TiltAngle;
  double RotAngle;
  double FPAngle;
  double Shift;
  double PivAdj;
  double Offset;
  double CentMom;
  double AcptMom;
  double CentTheta;
  double AcptTheta;
  double CentPhi;
  double AcptPhi;
  double RasterX;
  double RasterY;
  double RasterZ;
  double TargetThickness;
  std::string TargetMaterial;
  int EventFlag;
  int PhysicsFlag;
  int EDCFlag;
  int SSFlag;
  double p0,p1;// incident beam momentum

public:
  static HESParamMan *GetParamMan(){ return thisPointer; }

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
  double GetTiltAngle( void ){ return TiltAngle; }
  double GetRotAngle( void ){ return RotAngle; }
  double GetFPAngle( void ){ return FPAngle; }
  double GetShift( void ){ return Shift; }
  double GetPivAdj( void ){ return PivAdj; }
  double GetOffset( void ){ return Offset; }
  double GetCentMom( void ){ return CentMom; }
  double GetAcptMom( void ){ return AcptMom; }
  double GetCentTheta( void ){ return CentTheta; }
  double GetAcptTheta( void ){ return AcptTheta; }
  double GetCentPhi( void ){ return CentPhi; }
  double GetAcptPhi( void ){ return AcptPhi; }
  double GetRasterX( void ){ return RasterX; }
  double GetRasterY( void ){ return RasterY; }
  double GetRasterZ( void ){ return RasterZ; }
  double GetTargetThickness( void ){ return TargetThickness; }
  std::string GetTargetMaterial( void ){ return TargetMaterial; }
  int GetEventFlag( void ){ return EventFlag; }
  int GetPhysicsFlag( void ){ return PhysicsFlag; }
  int GetEDCFlag( void ){ return EDCFlag; }
  int GetSSFlag( void ){ return SSFlag; }
  void SetpBeam(double bp0,double bp1){
    p0 = bp0;
    p1 = bp1;
  };
  double GetpBeam0(){ return p0; };
  double GetpBeam1(){ return p1; };

};

#endif

// #define SIEVE_SLIT 1
#undef SIEVE_SLIT
