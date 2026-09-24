/*
  define HKS Simulation Parameter
*/

#include "HKSParamMan.hh"

#include <string>
#include <fstream>
#include <iostream>
using namespace std;

ofstream LogFile;
namespace{
  const int SizeOfBuffer = 144;
}

/////////////////////////////////////////
HKSParamMan *HKSParamMan::thisPointer=0;
/////////////////////////////////////////

//////////////////////////////////////////////////////
HKSParamMan::HKSParamMan( const char *inputFileName )
  : fileName(inputFileName)
//////////////////////////////////////////////////////
{
  ReadParam();
  LogFile.open(LogFileName);
  LogFile << "Hbook file name : " << HbookFileName << G4endl;
  LogFile << "Input Data Name : " << InputDataName << G4endl;
  LogFile << "Splitter field map : " << SplitterFieldMap << G4endl;
  LogFile << "Splitter scale factor : " << Sscale << G4endl;
  LogFile << "D field map : " << DFieldMap << G4endl;
  LogFile << "Dscale factor : " << Dscale << G4endl;
  LogFile << "Q1 field map : " << Q1FieldMap << G4endl;
  LogFile << "Q1scale factor : " << Q1scale << G4endl;
  LogFile << "Q2 field map : " << Q2FieldMap << G4endl;
  LogFile << "Q2scale factor : " << Q2scale << G4endl;
  LogFile << "Central Momentum(GeV) = " << CentMom << G4endl;
  LogFile << "Momentum Width(GeV) = " << AcptMom << G4endl;
  LogFile << "Theta central(rad) = " << CentTheta << G4endl;
  LogFile << "Theta Width(rad) = " << AcptTheta << G4endl;
  LogFile << "Phi central(rad) = " << CentPhi << G4endl;
  LogFile << "Phi Width(rad) = " << AcptPhi << G4endl;
  LogFile << "Target Material is " << TargetMaterial << G4endl;
  LogFile << "Target Thickness is " << TargetThickness << G4endl;
  LogFile << "KDCFlag : " << KDCFlag << G4endl;
  LogFile << "EMFlag : " << EMFlag << G4endl;
  LogFile << "DecayFlag : " << DecayFlag << G4endl;
  LogFile << "HadronFlag : " << HadronFlag << G4endl;

  thisPointer = this;
}

////////////////////////////////////////////////////////
//HKSParamMan::HKSParamMan( const char *inputFileName , int runnum )
//  : fileName(inputFileName)
////////////////////////////////////////////////////////
//{
//  ReadParam();
//  LogFile.open(LogFileName);
//  LogFile << "Hbook file name : " << HbookFileName << G4endl;
//  LogFile << "Input Data Name : " << InputDataName << G4endl;
//  LogFile << "Splitter field map : " << SplitterFieldMap << G4endl;
//  LogFile << "Splitter scale factor : " << Sscale << G4endl;
//  LogFile << "D field map : " << DFieldMap << G4endl;
//  LogFile << "Dscale factor : " << Dscale << G4endl;
//  LogFile << "Q1 field map : " << Q1FieldMap << G4endl;
//  LogFile << "Q1scale factor : " << Q1scale << G4endl;
//  LogFile << "Q2 field map : " << Q2FieldMap << G4endl;
//  LogFile << "Q2scale factor : " << Q2scale << G4endl;
//  LogFile << "Central Momentum(GeV) = " << CentMom << G4endl;
//  LogFile << "Momentum Width(GeV) = " << AcptMom << G4endl;
//  LogFile << "Theta central(rad) = " << CentTheta << G4endl;
//  LogFile << "Theta Width(rad) = " << AcptTheta << G4endl;
//  LogFile << "Phi central(rad) = " << CentPhi << G4endl;
//  LogFile << "Phi Width(rad) = " << AcptPhi << G4endl;
//  LogFile << "Target Material is " << TargetMaterial << G4endl;
//  LogFile << "Target Thickness is " << TargetThickness << G4endl;
//  LogFile << "KDCFlag : " << KDCFlag << G4endl;
//  LogFile << "EMFlag : " << EMFlag << G4endl;
//  LogFile << "DecayFlag : " << DecayFlag << G4endl;
//  LogFile << "HadronFlag : " << HadronFlag << G4endl;
//
//  char tempc[300];
//  sprintf(tempc,"_%d_%s",runnum,HbookFileName);
//  sprintf(HbookFileName,tempc);
//  
//  thisPointer = this;
//}

/////////////////////////////
bool HKSParamMan::ReadParam()
////////////////////////////
{
  // define initial value
  strcpy( HbookFileName, "hbook/geant.hbook" );
  strcpy( InputDataName, "" );
  GenerationID = 0;
  // ID = 0 : uniform
  // ID = 1 : from file
  ParticleID = 1;
  // ID = 0 : geantino
  // ID = 1 : Electron 

  SplitterFieldMap = "param/HKS_SPL_A.map";
  Dscale = 1.0;
  Q1scale = 1.0;
  Q2scale = 1.0;
  Q1mapID = 1;
  Q2mapID = 1;
  //1 for TOSCA MAP
  //else for Kato eq
  Q1FieldMap = "paramhks/Q1.map";
  Q2FieldMap = "paramhks/Q2.map";
  DFieldMap = "paramhks/ohtaniD6th.map";
  
  TargetThickness = 100.0;
  TargetMaterial = "Vacuum";
  
  EventFlag = 0 ;
  KDCFlag = 0;
  DecayFlag = 0;
  HadronFlag = 0;
  EMFlag = 0;
  SSFlag = 0;
  
  CentMom = 0.0;
  AcptMom = 0.0;
  //CentXp = 0.0;
  //AcptXp = 0.0;
  //CentYp = 0.0;
  //AcptYp = 0.0;
  CentTheta = 0.0;
  AcptTheta = 0.0;
  CentPhi = 0.0;
  AcptPhi = 0.0;
  
  CutValue = 0.001;
  Edet = 0.2;
  ThetaLimit = 3.14;

  FILE *fp = fopen( fileName, "r" );
  if( !fp ){
    G4cerr << "HKSParamMan: file open fail \"" << fileName << "\"" 
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
      G4cout << LogFileName << G4endl;
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
    else if( sscanf( str, "CentralMomentum(GeV): %lf", &CentMom )==1 ){
    }
    else if( sscanf( str, "MomentumWidth(GeV): %lf", &AcptMom )==1 ){
    }
    else if( sscanf( str, "ThetaCentral(rad): %lf", &CentTheta )==1 ){
    }
    else if( sscanf( str, "ThetaWidth(rad): %lf", &AcptTheta )==1 ){
    }
    else if( sscanf( str, "PhiCentral(rad): %lf", &CentPhi )==1 ){
    }
    else if( sscanf( str, "PhiWidth(rad): %lf", &AcptPhi )==1 ){
    }
    else if( sscanf( str, "ThetaLimit(mrad): %lf", &ThetaLimit )==1 ){
    }
    else if( sscanf( str, "RasterX(cm): %lf", &RasterX )==1 ){
    }
    else if( sscanf( str, "RasterY(cm): %lf", &RasterY )==1 ){
    }
    else if( sscanf( str, "RasterZ(cm): %lf", &RasterZ )==1 ){
    }
    else if( sscanf( str, "TargetMaterial & Thickness(mg/cm^2): %s %lf",
				str1, &TargetThickness )==2 ){
      TargetMaterial = str1;
    }
    else if( sscanf( str, "EventFlag: %d", &EventFlag )==1 ){
    }
    else if( sscanf( str, "KDCFlag: %d", &KDCFlag )==1 ){
    }
    else if( sscanf( str, "SSFlag: %d", &SSFlag )==1 ){
    }
    else if( sscanf( str, "EMFlag: %d", &EMFlag )==1 ){
    }
    else if( sscanf( str, "DecayFlag: %d", &DecayFlag )==1 ){
    }
    else if( sscanf( str, "HadronFlag: %d", &HadronFlag )==1 ){
    }
    else if( sscanf( str, "Particle ID: %d", &ParticleID )==1 ){
    }
  }

  return true;
}
