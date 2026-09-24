/*
  define HES Simulation Parameter
*/

#include "HESParamMan.hh"

#include <string>
#include <fstream>
#include <iostream>
using namespace std;

ofstream LogFile;
namespace{
  const int SizeOfBuffer = 144;
}

/////////////////////////////////////////
HESParamMan *HESParamMan::thisPointer=0;
/////////////////////////////////////////

//////////////////////////////////////////////////////
HESParamMan::HESParamMan( const char *inputFileName )
  : fileName(inputFileName)
//////////////////////////////////////////////////////
{
  ReadParam();
  LogFile.open(LogFileName);
  LogFile << "Hbook file name : " << HbookFileName << G4endl;
  LogFile << "Input Data Name : " << InputDataName << G4endl;
  LogFile << "Splitter field map : " << SplitterFieldMap << G4endl;
  LogFile << "Splitter scale factor : " << Sscale << G4endl;
  LogFile << "Q1 field map : " << Q1FieldMap << G4endl;
  LogFile << "Q1scale factor : " << Q1scale << G4endl;
  LogFile << "Q2 field map : " << Q2FieldMap << G4endl;
  LogFile << "Q2scale factor : " << Q2scale << G4endl;
  LogFile << "D field map : " << DFieldMap << G4endl;
  LogFile << "Dscale factor : " << Dscale << G4endl;
  LogFile << "Tilt Angle(degree) = " << TiltAngle << G4endl;
  LogFile << "Rot Angle(degree) = " << RotAngle << G4endl;
  LogFile << "FP Angle(degree) = " << FPAngle << G4endl;
  LogFile << "HES Offset(cm) = " << Offset << G4endl;
  LogFile << "Shift(cm) = " << Shift << G4endl;
  LogFile << "Pivot Adjustment(cm) = " << PivAdj << G4endl;
  LogFile << "Central Momentum(GeV) = " << CentMom << G4endl;
  LogFile << "Momentum Emittance(GeV) = " << AcptMom << G4endl;
  LogFile << "Theta central(rad) = " << CentTheta << G4endl;
  LogFile << "Theta Gen. Range(rad) = " << AcptTheta << G4endl;
  LogFile << "Phi central(rad) = " << CentPhi << G4endl;
  LogFile << "Phi Gen. Range(rad) = " << AcptPhi << G4endl;
  LogFile << "Raster X (cm) = " << RasterX << G4endl;
  LogFile << "Raster Y (cm) = " << RasterY << G4endl;
  LogFile << "Raster Z (cm) = " << RasterZ << G4endl;
  LogFile << "Target Material: " << TargetMaterial << G4endl;
  LogFile << "Target Thickness: " << TargetThickness << G4endl;
  LogFile << "EventFlag: " << EventFlag << G4endl;
  LogFile << "PhysicsFlag: " << PhysicsFlag << G4endl;
  LogFile << "EDCFlag: " << EDCFlag << G4endl;
  LogFile << "Particle ID:  " << ParticleID << G4endl;

  thisPointer = this;
}

/////////////////////////////
bool HESParamMan::ReadParam()
////////////////////////////
{
  // define initial value
  strcpy( HbookFileName, "hbook/geant.hbook" );
  strcpy( ErikaFileName, "data/erika.dat" );
  strcpy( TulFileName,   "data/tul.dat" );
  strcpy( dumFileName,   "dummy/dum.dat" );

  strcpy( InputDataName, "" );
  GenerationID = 0;
  // ID = 0 : uniform
  // ID = 1 : from file
  ParticleID = 1;
  // ID = 0 : geantino
  // ID = 1 : Electron 

  SplitterFieldMap = "param/HES_SPL_A.map";
  Dscale = 1.0;
  Q1scale = 1.0;
  Q2scale = 1.0;
  Q1mapID = 1;
  Q2mapID = 1;
  //1 for TOSCA MAP
  //else for Kato eq
  Q1FieldMap = "param/HES_Q1_test.map";
  Q2FieldMap = "param/HES_Q2_test.map";
  DFieldMap = "param/HES_D_test.map";
  
  TargetThickness = 100.0;
  TargetMaterial = "Vanadium";

  EventFlag = 0;
  PhysicsFlag = 0;
  EDCFlag = 0;
  SSFlag = 0;
  
  Shift = 0.;
  Offset = 0.;
  PivAdj = 0.;
  RotAngle = 0.;
  TiltAngle = 0.0;
  CentMom = 0.0;
  AcptMom = 0.0;
  AcptTheta = 0.0;
  CentTheta = 0.0;
  AcptPhi = 0.0;
  CentPhi = 0.0;
  RasterX = 0.0;
  RasterY = 0.0;
  RasterZ = 0.0;
  
  
  FILE *fp = fopen( fileName, "r" );
  if( !fp ){
    G4cerr << "HESParamMan: file open fail \"" << fileName << "\"" 
	   << G4endl;
    exit( -1 );
  }

  char str[SizeOfBuffer];
  char str1[SizeOfBuffer];

  while( fgets( str, SizeOfBuffer, fp ) ){
    if( str[0] == '#' ) continue;

    if( sscanf( str, "HBOOK: %s", str1 )==1 ){
      strcpy( HbookFileName, str1 );
      strcpy( LogFileName, str1 );
      strcat( LogFileName, "_Log" );
      G4cout <<  LogFileName << G4endl;
    }
    
    else if( sscanf( str, "INPUT: %s", str1 )==1 ){
      GenerationID = 1;
      strcpy( InputDataName, str1 );
    }
    
    else if( sscanf( str, "SplitterMAP: %s %lf", str1, &Sscale )==2 ){
      SplitterFieldMap = str1;
    }
    else if( sscanf( str, "DMAP: %s %lf", str1, &Dscale )==2 ){
      DFieldMap = str1;
    }
    else if( sscanf( str, "Q1MAP: %s %lf", str1, &Q1scale )==2 ){
      Q1FieldMap = str1;
    }
    else if( sscanf( str, "Q2MAP: %s %lf", str1, &Q2scale )==2 ){
      Q2FieldMap = str1;
    }
    else if( sscanf( str, "TiltAngle(degree): %lf", &TiltAngle )==1 ){
    }
    else if( sscanf( str, "Rot Angle(degree): %lf", &RotAngle )==1 ){
    }
    else if( sscanf( str, "FP Angle(degree): %lf", &FPAngle )==1 ){
    }
    else if( sscanf( str, "HES Offset(cm): %lf", &Offset )==1 ){
    }
    else if( sscanf( str, "Shift(cm): %lf", &Shift )==1 ){
    }
    else if( sscanf( str, "Pivot Adjustment(cm): %lf", &PivAdj )==1 ){
    }
    else if( sscanf( str, "CentralMomentum(GeV): %lf", &CentMom )==1 ){
    }
    else if( sscanf( str, "MomentumWidth(GeV): %lf", &AcptMom )==1 ){
    }
    else if( sscanf( str, "Theta Central(rad): %lf", &CentTheta )==1 ){
    }
    else if( sscanf( str, "Theta Width(rad): %lf", &AcptTheta )==1 ){
    }
    else if( sscanf( str, "Phi Central(rad): %lf", &CentPhi )==1 ){
    }
    else if( sscanf( str, "Phi Width(rad): %lf", &AcptPhi )==1 ){
    }
    else if( sscanf( str, "Raster X(cm): %lf", &RasterX )==1 ){
    }
    else if( sscanf( str, "Raster Y(cm): %lf", &RasterY )==1 ){
    }
    else if( sscanf( str, "Raster Z(cm): %lf", &RasterZ )==1 ){
    }
    else if( sscanf( str, "TargetMaterial & Thickness(mg/cm^2): %s %lf", str1, &TargetThickness )==2 ){
      TargetMaterial = str1;
    }
    else if( sscanf( str, "EventFlag: %d", &EventFlag )==1 ){
    }
    else if( sscanf( str, "PhysicsFlag: %d", &PhysicsFlag )==1 ){
    }
    else if( sscanf( str, "EDCFlag: %d", &EDCFlag )==1 ){
    }
    else if( sscanf( str, "SSFlag: %d", &SSFlag )==1 ){
    }
    else if( sscanf( str, "Particle ID: %d", &ParticleID )==1 ){
    }
  }

  return true;
}
