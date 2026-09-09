/*
  HKSGeometryParameter.hh
  H.Nomura 2004/Mar/11
*/

#ifndef HKSGeometryParameter_h

#define HKSGeometryParameter_h 1

#include "G4ThreeVector.hh"
#include "G4RotationMatrix.hh"

G4double Drot = 0.0*degree;

G4double K_Theta = 21.4*degree;
G4double VT_Phi  = 14.5*degree;
G4double Exit_ang = 1.4*degree + Drot;

////////////
// Target //
////////////

// Size
G4double TargetSizeX = 1.*cm;
G4double TargetSizeY = 1.*cm;
G4double TargetSizeZ = 0.44*mm;

// Virtual Target Position
G4double VTposX = 8.99*cos(VT_Phi)*cm;
G4double VTposZ = 8.99*sin(VT_Phi)*cm;

//////////////
// Splitter //
//////////////

// Size
G4double SplitterYokeSizeX = 94.3*cm;
G4double SplitterYokeSizeY = 96.52*cm;
G4double SplitterYokeSizeZ = 76.2*cm;

G4double SplitterGapSizeX = 32.1*cm;
G4double SplitterGapSizeY = 7.60*cm;
G4double SplitterGapSizeZ = 76.2*cm;

// Position
G4double SpX = -33.85*cm;
G4double SpZ = 34.2*cm;
G4double RotSp = 8.51*degree;
// G4double SplitterPositionX = -38.15*cm;
G4double SplitterPositionX = SpX*cos(RotSp) - SpZ*sin(RotSp);
G4double SplitterPositionY = 0.*cm;
// G4double SplitterPositionZ = 28.9*cm;
G4double SplitterPositionZ = SpX*sin(RotSp) + SpZ*cos(RotSp);

/////////////////////////////////
// Target Chamber (HKS fringe) //
/////////////////////////////////
// Top & Botom part
G4double VTtoTCt = 82.01*cm;
G4double TCPositionX = -VTtoTCt*sin(K_Theta) -(0.09*cm)*cos(K_Theta) + VTposX;
G4double TCPositionY = 0.*cm;
G4double TCPositionZ =  VTtoTCt*cos(K_Theta) -(0.09*cm)*sin(K_Theta) + VTposZ;

////////////////
// Sieve Slit //
////////////////
// Position

G4double VTtoSS = 88.5*cm;

G4double SSPositionX = -VTtoSS*sin(K_Theta) + VTposX;
G4double SSPositionY = 0.*cm;
G4double SSPositionZ =  VTtoSS*cos(K_Theta) + VTposZ;

///////////////
// Q1 Magnet //
///////////////

// Size
G4double Q1YokeSizeX = 76.*cm;
G4double Q1YokeSizeY = 175.*cm;
G4double Q1YokeSizeZ = 84.*cm;

// Position
G4double VTtoQ1 = 150.5*cm;

G4double Q1PositionX = -VTtoQ1*sin(K_Theta) + VTposX;
G4double Q1PositionY = 0.*cm;
G4double Q1PositionZ =  VTtoQ1*cos(K_Theta) + VTposZ;

///////////////
// Q2 Magnet //
///////////////

// Size
G4double Q2YokeSizeX = 110.*cm;
G4double Q2YokeSizeY = 190.*cm;
G4double Q2YokeSizeZ = 60.*cm;

// Position
G4double VTtoQ2 = 252.5*cm;

G4double Q2PositionX = -VTtoQ2*sin(K_Theta) + VTposX;
G4double Q2PositionY = 0.*cm;
G4double Q2PositionZ =  VTtoQ2*cos(K_Theta) + VTposZ;

//////////////
// D Magnet //
//////////////

// Size
G4double DAngle = 70.*degree + Drot; 
G4double DYokeInRad  = 92.4*cm;
G4double DYokeOutRad = 450.*cm;
G4double DYokeHeight = 125.*cm;

G4double DGapInRad  = 190.5*cm;
G4double DGapOutRad = 349.5*cm;
G4double DGapHeight = 20.0*cm;

// Position
G4double VTtoD = 322.5*cm;
G4double DCenter = 266.3*cm;
G4double DrotAng = K_Theta + Drot;

G4double DPositionX = -VTtoD*sin(K_Theta) + VTposX - DCenter*cos(DrotAng);
G4double DPositionY = 0.*cm;
G4double DPositionZ =  VTtoD*cos(K_Theta) + VTposZ - DCenter*sin(DrotAng);

G4double DExitX = DPositionX - DCenter*sin(Exit_ang);
G4double DExitY = 0.*cm;
G4double DExitZ = DPositionZ + DCenter*cos(Exit_ang);

///////////////
// Detectors //
///////////////

//=== Virtual Detector ===//

// VD0 (Q1 Entrance)
G4double VTtoVD0 = 108.*cm;
G4double PositionVD0X = -VTtoVD0*sin(K_Theta) + VTposX;
G4double PositionVD0Y = 0.*cm;
G4double PositionVD0Z =  VTtoVD0*cos(K_Theta) + VTposZ;

// VD1 (Q1 Exit)
G4double VTtoVD1 = 193.*cm;
G4double PositionVD1X = -VTtoVD1*sin(K_Theta) + VTposX;
G4double PositionVD1Y = 0.*cm;
G4double PositionVD1Z =  VTtoVD1*cos(K_Theta) + VTposZ;

// VD2 (Q2 Entrance)
G4double VTtoVD2 = 222.*cm;
G4double PositionVD2X = -VTtoVD2*sin(K_Theta) + VTposX;
G4double PositionVD2Y = 0.*cm;
G4double PositionVD2Z =  VTtoVD2*cos(K_Theta) + VTposZ;

// VD3 (Q2 Exit)
G4double VTtoVD3 = 283.*cm;
G4double PositionVD3X = -VTtoVD3*sin(K_Theta) + VTposX;
G4double PositionVD3Y = 0.*cm;
G4double PositionVD3Z =  VTtoVD3*cos(K_Theta) + VTposZ;

// VD4 (Dipole Entrance)
G4double VTtoVD4 = 322.*cm;
G4double PositionVD4X = -VTtoVD4*sin(K_Theta) + VTposX;
G4double PositionVD4Y = 0.*cm;
G4double PositionVD4Z =  VTtoVD4*cos(K_Theta) + VTposZ;

G4double VD4SizeX = 166.*cm;
G4double VD4SizeY = 20.*cm;
G4double VD4SizeZ = 1.*cm;

// VD5 (Dipole Exit)
G4double DtoVD5 = 0.5*cm;
G4double PositionVD5X = -DtoVD5*cos(Exit_ang) + DExitX;
G4double PositionVD5Y = 0.*cm;
G4double PositionVD5Z = -DtoVD5*sin(Exit_ang) + DExitZ;

G4double VD5SizeX = 166.*cm;
G4double VD5SizeY = 20.*cm;
G4double VD5SizeZ = 0.2*cm;

//=== Real Detector ===//

// Detector Space (Air)
G4double DtoDS = 380.*cm;
G4double PositionDSX = -DtoDS*cos(Exit_ang) + DExitX;
G4double PositionDSY = 0.*cm;
G4double PositionDSZ = -DtoDS*sin(Exit_ang) + DExitZ;

#endif
